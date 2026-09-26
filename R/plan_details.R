# ============================================================
# plan_details.R — Plan characteristics by contract year
#
# make_plan_details() builds one row per contract year x contract x plan x
# segment for the individual-market MA and SNP plans in the landscape
# files: organization and parent, plan name and type, premiums, Part D
# deductible and benefit type, in-network out-of-pocket maximum, star
# ratings and SNP details. Sources, all in raw/:
#   landscape files (every year)                premium, deductible, MOOP,
#                                               stars, SNP details
#   Part D Plan and Premium reports (to CY2024) Part C and Part D premiums,
#                                               Part C / D stars (CY2024)
#   CPSC Contract Info (December, else January) organization, parent,
#                                               plan type group, Part D
# Details are kept at the segment level: some plans charge different
# premiums in different segments.
# ============================================================


# C-SNP condition flag columns of the CY2016-CY2024 SNP landscape files
.CSNP_CONDITIONS <- c(
  "Cardiovascular Disorders", "Chronic Heart Failure", "Dementia", "Diabetes Mellitus",
  "End-stage Renal Disease Requiring Dialysis (any mode of dialysis)", "HIV/AIDS",
  "Chronic Lung Disorders", "Chronic and Disabling Mental Health Conditions",
  "Cardiovascular Disorders and Chronic Heart Failure", "Cardiovascular Disorders and Diabetes",
  "Chronic Heart Failure and Diabetes", "Cardiovascular Disorders, Chronic Heart Failure and Diabetes")

# Part D benefit type codes (CY2016-CY2024 landscape) and label variants
.DRUG_BENEFIT_TYPES <- c(
  DS = "Defined Standard", AE = "Actuarially Equivalent", BA = "Basic Alternative",
  EA = "Enhanced Alternative", "Defined Standard Benefit" = "Defined Standard",
  "Defined Standard" = "Defined Standard",
  "Actuarially Equivalent Standard" = "Actuarially Equivalent",
  "Basic Alternative" = "Basic Alternative", "Enhanced Alternative" = "Enhanced Alternative")


# --- Parsing helpers ------------------------------------------------------------

#' Normalize CMS column headers across years
#'
#' Collapses line breaks and runs of spaces, drops asterisks, footnote
#' digits ("Part C Premium2") and leading year labels ("2024 Overall Star
#' Rating"), fixes the CY2023 "Overal Star Rating" spelling, and marks
#' numbers repeated names (the second "Drug Benefit Type" becomes
#' `Drug Benefit Type [2]`).
#' @keywords internal
.norm_header <- function(x) {
  x <- gsub("\ufeff", "", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  x <- trimws(gsub("\\*+", "", x))
  x <- sub("^[^A-Za-z]+", "", x)
  x <- trimws(sub(" ?[0-9]+$", "", x))
  x <- sub("^Overal Star", "Overall Star", x)
  k <- stats::ave(seq_along(x), x, FUN = seq_along)
  x[k > 1] <- paste0(x[k > 1], " [", k[k > 1], "]")
  x
}


#' Parse CMS money strings ("$1,234.50", "$-", "($7.20)") to numbers
#'
#' "$-" is zero; parentheses or a leading minus mean negative; blank,
#' "N/A" and "Not Applicable" are NA.
#' @keywords internal
.parse_money <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "N/A", "NA", "Not Applicable")] <- NA
  neg <- !is.na(x) & grepl("^-|^\\(|^\\$\\(|\\)$", x)
  y <- gsub("[$,() -]", "", x)
  y[!is.na(x) & y == ""] <- "0"
  out <- suppressWarnings(as.numeric(y))
  out[neg & !is.na(out)] <- -out[neg & !is.na(out)]
  out
}


#' Parse money and stop if any non-blank value is not a dollar amount
#' @keywords internal
.money <- function(x, what) {
  out <- .parse_money(x)
  bad <- !is.na(x) & !trimws(x) %in% c("", "N/A", "NA", "Not Applicable") & is.na(out)
  .vcheck(!any(bad), "%s: %d value(s) are not dollar amounts (e.g. %s); extend .parse_money()",
          what, sum(bad), paste(utils::head(unique(x[bad]), 3), collapse = ", "))
  out
}


# Out-of-pocket maximum: "$-" and "N/A" mean the plan reports none
.parse_moop <- function(x, what) {
  x <- trimws(as.character(x))
  x[x %in% c("$-", "-")] <- NA
  .money(x, what)
}


# Stop if none of a field's header aliases is in a file
.need <- function(d, aliases, what) {
  .vcheck(any(aliases %in% names(d)), paste0(
    "%s: no column for %s (expected one of: %s); CMS may have renamed it; add the new ",
    "name to the aliases in R/plan_details.R"), attr(d, "src") %||% "file", what,
    paste(aliases, collapse = " | "))
}


#' Parse star ratings ("4 Stars", "3.5", "4.0") to numbers; labels are NA
#' @keywords internal
.parse_star <- function(x) {
  x <- trimws(as.character(x))
  ok <- !is.na(x) & grepl("^[0-9](\\.[05])?( Stars?)?$", x)
  out <- rep(NA_real_, length(x))
  out[ok] <- as.numeric(sub("^([0-9](\\.[05])?).*$", "\\1", x[ok]))
  out
}


#' Star rating status: rated, not enough data, plan too new, not applicable
#' @keywords internal
.star_status <- function(x) {
  x <- tolower(trimws(as.character(x)))
  fcase(!is.na(x) & grepl("^[0-9](\\.[05])?( stars?)?$", x), "rated",
        grepl("not enough data", x), "not enough data",
        grepl("too new", x), "plan too new",
        grepl("not applicable", x), "not applicable",
        default = NA_character_)
}


.yes_no <- function(x) {
  x <- tolower(trimws(as.character(x)))
  fifelse(x %in% c("yes", "y"), TRUE, fifelse(x %in% c("no", "n"), FALSE, NA))
}


.na_text <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "Not Applicable", "N/A")] <- NA
  x
}


.fix_utf8 <- function(x) {
  bad <- !is.na(x) & !validUTF8(x)
  x[bad] <- iconv(x[bad], "latin1", "UTF-8")
  x
}


.pick <- function(d, aliases) {
  hit <- intersect(aliases, names(d))
  if (length(hit)) trimws(as.character(d[[hit[1]]])) else rep(NA_character_, nrow(d))
}


#' Read a CMS csv with a title preamble
#'
#' Finds the header row (the first line starting with "State" or "Contract
#' Year"), reads everything as text and normalizes the headers. Returns NULL
#' for a file with no header row (CMS ships some empty sanctioned-plan
#' files).
#' @keywords internal
.read_detail_csv <- function(path) {
  lines <- readLines(path, n = 40, warn = FALSE)
  hdr <- which(grepl("^[^A-Za-z]*\"?(State|Contract Year)\"?,", lines, useBytes = TRUE))[1]
  if (is.na(hdr)) return(NULL)
  d <- fread(path, skip = hdr - 1L, colClasses = "character", showProgress = FALSE)
  d <- d[, !grepl("^V[0-9]+$", names(d)), with = FALSE]
  setnames(d, .norm_header(names(d)))
  .vcheck("Contract ID" %in% names(d), "%s has no Contract ID column after its header row", path)
  d <- d[!is.na(`Contract ID`) & nzchar(trimws(`Contract ID`))]
  setattr(d, "src", basename(path))
  d
}


# --- Sources --------------------------------------------------------------------

# Landscape rows for one contract year with the plan-detail fields, as text
.detail_landscape <- function(cy) {
  cy <- as.integer(cy)
  frames <- if (cy <= 2023L) {
    paths <- list.files(here("raw", "landscape", paste0("CY", cy)), pattern = "\\.csv$",
                        recursive = TRUE, full.names = TRUE)
    b <- basename(paths)
    snp <- grepl("SNP", b)
    ma <- grepl("MA", b) & !snp
    .vcheck(any(ma) && any(snp), "landscape CY%d: expected MA and SNP csv files", cy)
    keep <- ma | snp
    Map(function(p, s, sanc) list(d = .read_detail_csv(p), snp = s, sanctioned = sanc),
        paths[keep], snp[keep], grepl("anction", b[keep]))
  } else if (cy == 2024L) {
    p <- .landscape_2024_paths()
    Map(function(p, s, sanc) list(d = .read_detail_csv(p), snp = s, sanctioned = sanc),
        p, grepl("snp", names(p)), grepl("sanctioned", names(p)))
  } else {
    d <- .read_detail_csv(.landscape_file(cy))
    d <- d[!`Contract Category Type` %in% MAEXITS_EXCLUDED_CATEGORIES]
    list(list(d = d, snp = NA, sanctioned = NA))
  }
  rbindlist(lapply(frames, function(f) {
    d <- f$d
    if (is.null(d)) return(NULL)
    lbl <- sprintf("landscape CY%d %s", cy, attr(d, "src") %||% "")
    .need(d, c("Monthly Consolidated Premium (Includes Part C + D)",
               "Monthly Consolidated Premium (Part C + D)"), "the monthly premium")
    .need(d, c("Annual Drug Deductible", "Annual Part D Deductible Amount"), "the Part D deductible")
    .need(d, "Overall Star Rating", "the overall star rating")
    if (!isTRUE(f$snp)) {
      .need(d, c("In-network MOOP Amount", "In-Network Maximum Out-of-Pocket (MOOP) Amount"),
            "the in-network MOOP")
    }
    if (cy >= 2025L) {
      .need(d, "Part C Premium", "the Part C premium")
      .need(d, "Part D Total Premium", "the Part D total premium")
    }
    if (!nrow(d)) return(NULL)
    csnp <- if (any(.CSNP_CONDITIONS %in% names(d))) {
      cols <- intersect(.CSNP_CONDITIONS, names(d))
      flag <- as.matrix(d[, lapply(.SD, function(v) !is.na(v) & nzchar(trimws(v))), .SDcols = cols])
      apply(flag, 1, function(r) if (any(r)) paste(cols[r], collapse = "; ") else NA_character_)
    } else {
      gsub(",", "; ", .na_text(.pick(d, "Chronic or Disabling Condition SNP (C-SNP) Condition Type")))
    }
    data.table(
      year = cy,
      contract_id = .pick(d, "Contract ID"),
      plan_id = suppressWarnings(as.integer(.pick(d, "Plan ID"))),
      segment_id = suppressWarnings(as.integer(.pick(d, "Segment ID"))),
      ls_plan_name = .fix_utf8(.pick(d, "Plan Name")),
      plan_type = .pick(d, c("Type of Medicare Health Plan", "Plan Type")),
      snp = if (is.na(f$snp)) .pick(d, "Special Needs Plan (SNP) Indicator") else if (f$snp) "Yes" else "No",
      snp_type = .na_text(.pick(d, c("Special Needs Plan Type", "SNP Type"))),
      sanctioned = if (is.na(f$sanctioned)) .yes_no(.pick(d, "Sanctioned Plan")) else f$sanctioned,
      premium_total = .money(.pick(d, c("Monthly Consolidated Premium (Includes Part C + D)",
                                        "Monthly Consolidated Premium (Part C + D)")), lbl),
      premium_part_c = .money(.pick(d, "Part C Premium"), lbl),
      premium_part_d_basic = .money(.pick(d, "Part D Basic Premium"), lbl),
      premium_part_d_supp = .money(.pick(d, "Part D Supplemental Premium"), lbl),
      premium_part_d_total = .money(.pick(d, "Part D Total Premium"), lbl),
      premium_part_d_lis = .money(.pick(d, "Part D Low Income Beneficiary Premium Amount"), lbl),
      part_d_deductible = .money(.pick(d, c("Annual Drug Deductible", "Annual Part D Deductible Amount")), lbl),
      drug_benefit_raw = .pick(d, c("Drug Benefit Type Detail", "Drug Benefit Type [2]", "Drug Benefit Type")),
      gap_raw = .pick(d, c("Additional Coverage Offered in the Gap",
                           "Type of Additional Coverage Offered in the Gap")),
      moop_in_network = .parse_moop(.pick(d, c("In-network MOOP Amount",
                                              "In-Network Maximum Out-of-Pocket (MOOP) Amount")), lbl),
      star_raw = .pick(d, "Overall Star Rating"),
      star_part_c = .parse_star(.pick(d, c("Part C Summary Star Rating", "Star Rating Part C"))),
      star_part_d = .parse_star(.pick(d, c("Part D Summary Star Rating", "Star Rating Part D"))),
      dsnp_integration = .na_text(.pick(d, c("Integration Status",
                                             "Dual Eligible SNP (D-SNP) Integration Status"))),
      dsnp_aip = .yes_no(.pick(d, c("AIP Status", "D-SNP Applicable Integrated Plan (AIP) Identifier"))),
      csnp_conditions = csnp,
      snp_institutional_type = .na_text(.pick(d, "SNP Institutional Type")),
      zero_dollar_dsnp = .yes_no(.pick(d, "Medicare Zero-Dollar Cost Sharing D-SNP Plan"))
    )
  }), fill = TRUE)
}


# Part D Plan and Premium report rows for MA plans (CY2018-CY2024)
.detail_partd_report <- function(cy) {
  key <- as.character(cy)
  if (!key %in% names(MAEXITS_PARTD_REPORT_FILES)) return(NULL)
  files <- here("raw", "landscape", MAEXITS_PARTD_REPORT_FILES[[key]])
  miss <- files[!file.exists(files)]
  .vcheck(length(miss) == 0, "Part D premium report for CY%s not found: %s", key,
          paste(miss, collapse = ", "))
  paths <- if (length(files) == 1L && grepl("\\.zip$", files)) {
    members <- utils::unzip(files, list = TRUE)$Name
    want <- members[grepl("508_(AlabamatoMontana|NebraskatoWyoming|Sanctioned)", basename(members))]
    .vcheck(length(want) >= 2, "%s does not contain the 508_ Part D report csvs", basename(files))
    exdir <- tempfile("partd_")
    on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
    utils::unzip(files, files = want, exdir = exdir, junkpaths = TRUE)
    file.path(exdir, basename(want))
  } else files
  d <- rbindlist(lapply(paths, function(p) {
    x <- .read_detail_csv(p)
    if (!is.null(x)) {
      for (need in c("Part C Premium", "Part D Basic Premium", "Part D Total Premium")) .need(x, need, need)
    }
    x
  }), fill = TRUE)
  d <- d[!startsWith(`Contract ID`, "S")]
  lbl <- sprintf("Part D report CY%s", key)
  data.table(
    contract_id = .pick(d, "Contract ID"),
    plan_id = suppressWarnings(as.integer(.pick(d, "Plan ID"))),
    segment_id = suppressWarnings(as.integer(.pick(d, "Segment ID"))),
    pd_part_c = .money(.pick(d, "Part C Premium"), lbl),
    pd_part_d_basic = .money(.pick(d, "Part D Basic Premium"), lbl),
    pd_part_d_supp = .money(.pick(d, "Part D Supplemental Premium"), lbl),
    pd_part_d_total = .money(.pick(d, "Part D Total Premium"), lbl),
    pd_part_d_lis = .money(.pick(d, "Part D Premium Obligation with Full Premium Assistance"), lbl),
    pd_star_c = .parse_star(.pick(d, "Star Rating Part C")),
    pd_star_d = .parse_star(.pick(d, "Star Rating Part D"))
  )
}


#' Plan rows of one CPSC Contract Info file
#' @param year,month Year and month of the file.
#' @return data.table keyed by contract_id and plan_id, or NULL if the file
#'   is not in raw/.
#' @keywords internal
.contract_info <- function(year, month) {
  folder <- if (as.integer(month) == 12L) "december enrollment" else "january enrollment"
  f <- list.files(here("raw", folder),
                  pattern = sprintf("^CPSC_Contract_Info_%d_%02d\\.csv$", as.integer(year), as.integer(month)),
                  recursive = TRUE, full.names = TRUE)
  if (!length(f)) return(NULL)
  d <- fread(f[1], colClasses = "character", showProgress = FALSE)
  for (c in names(d)) set(d, j = c, value = trimws(.fix_utf8(d[[c]])))
  d <- d[nzchar(`Plan ID`)]
  out <- data.table(
    contract_id = d$`Contract ID`,
    plan_id = suppressWarnings(as.integer(d$`Plan ID`)),
    org_type = .na_text(d$`Organization Type`),
    plan_type_group = .na_text(d$`Plan Type`),
    part_d = .yes_no(d$`Offers Part D`),
    org_legal_name = .na_text(d$`Organization Name`),
    org_marketing_name = .na_text(d$`Organization Marketing Name`),
    ci_plan_name = .na_text(d$`Plan Name`),
    parent_org = .na_text(d$`Parent Organization`),
    contract_effective_date = as.Date(sub("\\s.*$", "", d$`Contract Effective Date`), "%m/%d/%Y"))
  unique(out, by = c("contract_id", "plan_id"))
}


# First non-missing value per key for every column; reports keys where a
# column holds more than one distinct value
.collapse_first <- function(d, key, label) {
  vars <- setdiff(names(d), key)
  n <- d[, .N, by = key][N > 1L, .N]
  if (n) {
    conf <- d[, lapply(.SD, function(v) uniqueN(v[!is.na(v)]) > 1L), by = key, .SDcols = vars]
    bad <- colSums(conf[, vars, with = FALSE])
    bad <- bad[bad > 0]
    if (length(bad)) {
      nkeys <- uniqueN(d, by = key)
      .vcheck(max(bad) <= 0.005 * nkeys, paste0(
        "%s: %s differ within a plan-segment for more than 0.5%% of plan-segments; ",
        "check the file for a new layout"), label,
        paste(sprintf("%s (%d)", names(bad), bad), collapse = ", "))
      message(sprintf("%s: first value kept where a plan-segment has several (%s)", label,
                      paste(sprintf("%s: %d", names(bad), bad), collapse = ", ")))
    }
  }
  d[, lapply(.SD, function(v) { w <- v[!is.na(v)]; if (length(w)) w[1] else v[1] }),
    by = key, .SDcols = vars]
}


# --- Build ----------------------------------------------------------------------

#' Build Plan Details by Contract Year
#'
#' One row per contract year x contract x plan x segment for the
#' individual-market MA and SNP plans in the landscape files (standalone
#' drug plans and Medicare-Medicaid Plans excluded), with the plan's
#' organization, premiums, Part D deductible and benefit type, in-network
#' out-of-pocket maximum (MOOP), star ratings and SNP details. See
#' [plan_details] for every column and its coverage by year.
#'
#' @param years Contract years (default: `MAEXITS_PLAN_DETAIL_FIRST_YEAR`
#'   through the last landscape year).
#' @param save If TRUE, write `trunk/derived/plan_details.csv`.
#' @return data.table.
#' @export
make_plan_details <- function(years = MAEXITS_PLAN_DETAIL_FIRST_YEAR:max(.landscape_years()),
                              save = TRUE) {
  key <- c("year", "contract_id", "plan_id", "segment_id")
  out <- rbindlist(lapply(as.integer(years), function(cy) {
    ls <- .detail_landscape(cy)
    ls <- ls[!is.na(plan_id)]
    det <- .collapse_first(ls, key, sprintf("landscape CY%d", cy))

    pd <- .detail_partd_report(cy)
    if (!is.null(pd)) {
      pd <- .collapse_first(pd[!is.na(plan_id)], c("contract_id", "plan_id", "segment_id"),
                            sprintf("Part D report CY%d", cy))
      det <- merge(det, pd, by = c("contract_id", "plan_id", "segment_id"), all.x = TRUE, sort = FALSE)
      det[, `:=`(premium_part_c = fcoalesce(premium_part_c, pd_part_c),
                 premium_part_d_basic = fcoalesce(premium_part_d_basic, pd_part_d_basic),
                 premium_part_d_supp = fcoalesce(premium_part_d_supp, pd_part_d_supp),
                 premium_part_d_total = fcoalesce(premium_part_d_total, pd_part_d_total),
                 premium_part_d_lis = fcoalesce(premium_part_d_lis, pd_part_d_lis),
                 star_part_c = fcoalesce(star_part_c, pd_star_c),
                 star_part_d = fcoalesce(star_part_d, pd_star_d))]
      det[, grep("^pd_", names(det), value = TRUE) := NULL]
    }

    # Organization, parent and plan type group: December Contract Info of
    # the contract year, else January
    ci <- .contract_info(cy, 12L)
    if (is.null(ci)) ci <- .contract_info(cy, 1L)
    if (!is.null(ci)) {
      det <- merge(det, ci, by = c("contract_id", "plan_id"), all.x = TRUE, sort = FALSE)
      det[, plan_name := fcoalesce(ci_plan_name, ls_plan_name)]
      det[, ci_plan_name := NULL]
    } else {
      message("CPSC Contract Info for CY", cy, " not found: organization fields left NA")
      det[, `:=`(org_type = NA_character_, plan_type_group = NA_character_, part_d = NA,
                 org_legal_name = NA_character_, org_marketing_name = NA_character_,
                 parent_org = NA_character_, contract_effective_date = as.Date(NA),
                 plan_name = ls_plan_name)]
    }
    det[, ls_plan_name := NULL]
    det
  }), fill = TRUE)

  out[, part_d := fcoalesce(part_d, !is.na(premium_part_d_total))]
  # Premiums: without Part D, the consolidated premium is the Part C premium;
  # 1876 Cost plans report no Part C premium in the Part D report. A Part D
  # plan missing from the Part D report keeps an NA Part C premium.
  out[is.na(premium_part_c) & part_d %in% FALSE & !is.na(premium_total),
      premium_part_c := premium_total]
  out[is.na(premium_part_c) & !is.na(premium_total) & !is.na(premium_part_d_total),
      premium_part_c := pmax(premium_total - premium_part_d_total, 0)]
  out[is.na(premium_total) & !is.na(premium_part_c),
      premium_total := premium_part_c + fcoalesce(premium_part_d_total, 0)]
  # In-network MOOP: none for PFFS and MSA plans (from CY2025 CMS reports a
  # combined in- and out-of-network amount for PFFS in this column); a $0
  # on a Cost plan means none. Other $0 amounts are kept as reported.
  out[org_type %in% c("PFFS", "MSA") | grepl("^(PFFS|MSA)", plan_type), moop_in_network := NA_real_]
  out[grepl("Cost", org_type) & moop_in_network %in% 0, moop_in_network := NA_real_]

  out[, drug_benefit_type := fifelse(drug_benefit_raw %in% names(.DRUG_BENEFIT_TYPES),
                                     unname(.DRUG_BENEFIT_TYPES[drug_benefit_raw]),
                                     .na_text(drug_benefit_raw))]
  out[, gap_coverage := fifelse(is.na(.na_text(gap_raw)), NA, !grepl("^No", trimws(gap_raw)))]
  out[, `:=`(star_overall = .parse_star(star_raw), star_status = .star_status(star_raw))]
  out[, c("drug_benefit_raw", "gap_raw", "star_raw") := NULL]

  cols <- c(key, "plan_name", "org_marketing_name", "org_legal_name", "parent_org", "org_type",
            "plan_type", "plan_type_group", "contract_effective_date", "snp", "snp_type",
            "dsnp_integration", "dsnp_aip", "csnp_conditions", "snp_institutional_type",
            "zero_dollar_dsnp", "sanctioned", "part_d", "premium_total", "premium_part_c",
            "premium_part_d_basic", "premium_part_d_supp", "premium_part_d_total",
            "premium_part_d_lis", "part_d_deductible", "drug_benefit_type", "gap_coverage",
            "moop_in_network", "star_overall", "star_status", "star_part_c", "star_part_d")
  out <- out[, cols, with = FALSE]
  setorderv(out, key)

  # Coverage floors (CY2018-2026 values are 99-100% for premium and parent,
  # 85-92% rated or labelled stars); a drop means a file changed layout
  cov <- out[, .(premium = mean(!is.na(premium_total)), parent = mean(!is.na(parent_org)),
                 star = mean(!is.na(star_status))), by = year]
  low <- cov[premium < 0.97 | parent < 0.95 | (star < 0.9 & !year %in% MAEXITS_LANDSCAPE_BLANK_STARS)]
  .vcheck(nrow(low) == 0, paste0(
    "plan details coverage dropped in CY%s (premium %s, parent %s, star status %s); check the ",
    "landscape and Contract Info files for renamed columns"),
    paste(low$year, collapse = ","), paste(round(100 * low$premium), collapse = ","),
    paste(round(100 * low$parent), collapse = ","), paste(round(100 * low$star), collapse = ","))
  .vcheck(!anyDuplicated(out, by = key), "plan_details has duplicate year-contract-plan-segment rows")
  message("Plan details: ", nrow(out), " plan-segments, CY", min(years), "-CY", max(years))
  if (save) fwrite(out, here("trunk", "derived", "plan_details.csv"))
  invisible(out)
}
