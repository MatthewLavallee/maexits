# ============================================================
# pbp_benefits.R — Supplemental benefits and Part B givebacks from CMS's
# Plan Benefit Package (PBP) Benefits files
#
#   make_pbp_benefits()   plan x segment x contract year table of salient
#                         supplemental benefits
#
# The landscape gives premiums, MOOP and stars but not the benefits plans
# advertise most: dental, vision and hearing coverage, over-the-counter
# allowances, fitness, meals, transportation, and the Part B premium
# reduction ("giveback"). CMS publishes them in the PBP Benefits files, one
# zip of tab-delimited extracts per contract year (registered in
# MAEXITS_PBP_FILES). Each extract has one row per contract, plan and
# segment.
#
# Flags are TRUE when the plan covers the benefit as a mandatory
# supplemental benefit (in every enrollee's package). From CY2025 CMS
# restructured the dental section: preventive dental moved from 16a to 16b
# and comprehensive from 16b to 16c, and the "offered" questions became
# per-service mandatory/optional codes. Dental is read accordingly.
# ============================================================


# Annualizing factor for CMS's OTC allowance period codes
.PBP_PERIODS <- c("1" = 1 / 3, "2" = 1 / 2, "3" = 1, "4" = 2, "5" = 4, "6" = 12, "7" = 12)


#' Read one PBP extract from a registered zip
#' @keywords internal
.pbp_read <- function(zip, member, cols, unique_rows = TRUE) {
  files <- utils::unzip(zip, list = TRUE)$Name
  hit <- files[tolower(files) == tolower(paste0(member, ".txt"))]
  .vcheck(length(hit) == 1, "%s not found in %s", member, zip)
  key <- c("pbp_a_hnumber", "pbp_a_plan_identifier", "segment_id")
  x <- fread(cmd = sprintf("unzip -p %s %s", shQuote(zip), shQuote(hit)), sep = "\t",
             quote = "", colClasses = "character", na.strings = NULL, encoding = "Latin-1",
             select = NULL, showProgress = FALSE)
  setnames(x, tolower(trimws(names(x))))
  miss <- setdiff(c(key, cols), names(x))
  .vcheck(length(miss) == 0, "%s in %s lacks column(s): %s", member, zip, paste(miss, collapse = ", "))
  x <- x[, c(key, cols), with = FALSE]
  for (j in names(x)) set(x, j = j, value = trimws(x[[j]]))
  setnames(x, key, c("contract_id", "plan_id", "segment_id"))
  x[, `:=`(plan_id = as.integer(plan_id), segment_id = as.integer(segment_id))]
  .vcheck(!unique_rows || !anyDuplicated(x, by = c("contract_id", "plan_id", "segment_id")),
          "%s in %s has duplicate contract-plan-segment rows", member, zip)
  x
}


# Yes/No codes: 1 = Yes, 2 = No, blank = question not reached (No)
.pbp_yes <- function(v, what) {
  .vcheck(all(v %in% c("1", "2", "")), "unexpected %s code(s): %s", what,
          paste(head(unique(setdiff(v, c("1", "2", ""))), 5), collapse = ", "))
  v == "1"
}


.pbp_num <- function(v, what) {
  bad <- v != "" & !grepl("^-?[0-9]*\\.?[0-9]+$", v)
  .vcheck(!any(bad), "unexpected %s value(s): %s", what, paste(head(unique(v[bad]), 5), collapse = ", "))
  suppressWarnings(as.numeric(v))
}


#' Supplemental benefits for one contract year
#' @keywords internal
.pbp_year <- function(year, zip) {
  d <- .pbp_read(zip, "pbp_Section_D", c("pbp_d_mco_pay_reduct_yn", "pbp_d_mco_pay_reduct_amt"))
  d[, `:=`(partb_giveback = .pbp_yes(pbp_d_mco_pay_reduct_yn, "Part B reduction"),
           partb_giveback_amt = .pbp_num(pbp_d_mco_pay_reduct_amt, "Part B reduction amount"))]
  d[partb_giveback == FALSE, partb_giveback_amt := NA_real_]
  out <- d[, .(contract_id, plan_id, segment_id, partb_giveback, partb_giveback_amt)]

  if (year <= 2024L) {
    dn <- .pbp_read(zip, "pbp_b16_dental", c("pbp_b16a_bendesc_yn", "pbp_b16b_bendesc_yn"))
    dn[, `:=`(dental_preventive = .pbp_yes(pbp_b16a_bendesc_yn, "preventive dental"),
              dental_comprehensive = .pbp_yes(pbp_b16b_bendesc_yn, "comprehensive dental"))]
  } else {
    # per-service codes: 2 = mandatory, 3 = optional, 4 = both, blank = not covered
    pv <- c("oe", "dx", "ods", "pc", "ft", "ops")
    cm <- c("rs", "end", "peri", "prm", "mxpr", "impl", "prf", "omsg", "orth", "ags")
    cols <- c(sprintf("pbp_b16b_bendesc_%s_amo", pv), sprintf("pbp_b16c_bendesc_%s_amo", cm))
    dn <- .pbp_read(zip, "pbp_b16_dental", cols)
    codes <- unique(unlist(dn[, ..cols]))
    .vcheck(all(codes %in% c("", "2", "3", "4")), "unexpected dental code(s) in CY%d: %s", year,
            paste(setdiff(codes, c("", "2", "3", "4")), collapse = ", "))
    mand <- function(cs) Reduce(`|`, lapply(cs, function(cc) dn[[cc]] %in% c("2", "4")))
    dn[, `:=`(dental_preventive = mand(cols[seq_along(pv)]),
              dental_comprehensive = mand(cols[-seq_along(pv)]))]
  }
  out <- merge(out, dn[, .(contract_id, plan_id, segment_id, dental_preventive, dental_comprehensive)],
               by = c("contract_id", "plan_id", "segment_id"), all = TRUE)

  yn <- list(
    list("pbp_b17_eye_exams_wear", c(eye_exam = "pbp_b17a_bendesc_yn", eyewear = "pbp_b17b_bendesc_yn")),
    list("pbp_b18_hearing_exams_aids", c(hearing_exam = "pbp_b18a_bendesc_yn", hearing_aids = "pbp_b18b_bendesc_yn")),
    list("pbp_b10_amb_trans", c(transportation = "pbp_b10b_bendesc_yn")),
    list("pbp_b4_emerg_urgent", c(worldwide_emergency = "pbp_b4c_bendesc_yn")),
    list("pbp_b13_other_services", c(otc = "pbp_b13b_bendesc_otc", meals = "pbp_b13c_bendesc_service"))
  )
  for (s in yn) {
    x <- .pbp_read(zip, s[[1]], unname(s[[2]]))
    for (nm in names(s[[2]])) set(x, j = nm, value = .pbp_yes(x[[s[[2]][[nm]]]], nm))
    out <- merge(out, x[, c("contract_id", "plan_id", "segment_id", names(s[[2]])), with = FALSE],
                 by = c("contract_id", "plan_id", "segment_id"), all = TRUE)
  }

  # OTC allowance per year: plan maximum times the number of periods a year
  o <- .pbp_read(zip, "pbp_b13_other_services", c("pbp_b13b_bendesc_otc", "pbp_b13b_maxplan_amt", "pbp_b13b_otc_maxplan_per"))
  per <- unique(o[pbp_b13b_bendesc_otc == "1" & pbp_b13b_maxplan_amt != "", pbp_b13b_otc_maxplan_per])
  .vcheck(all(per %in% names(.PBP_PERIODS)), "unexpected OTC period code(s) in CY%d: %s", year,
          paste(setdiff(per, names(.PBP_PERIODS)), collapse = ", "))
  o[, otc_annual_amt := fifelse(pbp_b13b_bendesc_otc == "1",
                                .pbp_num(pbp_b13b_maxplan_amt, "OTC amount") * .PBP_PERIODS[pbp_b13b_otc_maxplan_per], NA_real_)]
  out <- merge(out, o[, .(contract_id, plan_id, segment_id, otc_annual_amt)],
               by = c("contract_id", "plan_id", "segment_id"), all = TRUE)

  # Other defined supplemental benefits (14c): fitness and in-home support
  p <- .pbp_read(zip, "pbp_b14_preventive", c("pbp_b14c_bendesc_yn", "pbp_b14c_bendesc_ehc"))
  has <- function(code) .pbp_yes(p$pbp_b14c_bendesc_yn, "14c") &
    grepl(sprintf("(^|;)%s(;|$)", code), p$pbp_b14c_bendesc_ehc)
  p[, `:=`(fitness = has("14c4"), in_home_support = if (year >= 2020L) has("14c21") else NA)]
  out <- merge(out, p[, .(contract_id, plan_id, segment_id, fitness, in_home_support)],
               by = c("contract_id", "plan_id", "segment_id"), all = TRUE)

  # Food and produce (a special supplemental benefit for the chronically ill), CY2020 on
  if (year >= 2020L) {
    # one row per VBID/SSBCI package; a plan offers the benefit if any package does
    f <- .pbp_read(zip, "pbp_b13i_b19b_services_vbid_ssbci", "pbp_b13i_fd_bendesc_yn", unique_rows = FALSE)
    f <- f[, .(food_produce = any(.pbp_yes(pbp_b13i_fd_bendesc_yn, "food and produce"))),
           by = .(contract_id, plan_id, segment_id)]
    out <- merge(out, f[, .(contract_id, plan_id, segment_id, food_produce)],
                 by = c("contract_id", "plan_id", "segment_id"), all.x = TRUE)
    out[is.na(food_produce), food_produce := FALSE]
    out[is.na(in_home_support), in_home_support := FALSE]
  } else {
    out[, food_produce := NA]
  }
  out[, year := as.integer(year)]
  setcolorder(out, "year")
  out[]
}


#' Build the Plan Benefit Package (Supplemental Benefits) Table
#'
#' Reads CMS's Plan Benefit Package (PBP) Benefits files (registered in
#' `MAEXITS_PBP_FILES`, under raw/pbp/) and returns one row per contract
#' year, contract, plan and segment, with the Part B premium reduction
#' ("giveback") and flags for salient supplemental benefits: preventive and
#' comprehensive dental, eye exams, eyewear, hearing exams, hearing aids,
#' over-the-counter allowance (and its annual amount), meals,
#' transportation, worldwide emergency coverage, fitness, in-home support
#' services, and food and produce (CY2020 on). The file covers every PBP
#' CMS approved, including PDPs, employer plans and cost plans; join to
#' other tables on `year`, `contract_id`, `plan_id` and `segment_id`.
#'
#' @param years Contract years to build (default: every registered year).
#' @param save Logical. If TRUE (default), writes
#'   trunk/derived/pbp_benefits.csv.
#' @return Invisibly, a data.table with one row per plan-segment and year.
#' @export
make_pbp_benefits <- function(years = as.integer(names(MAEXITS_PBP_FILES)), save = TRUE) {
  years <- as.integer(years)
  .vcheck(all(as.character(years) %in% names(MAEXITS_PBP_FILES)),
          "PBP files not registered in MAEXITS_PBP_FILES for: %s",
          paste(setdiff(years, as.integer(names(MAEXITS_PBP_FILES))), collapse = ", "))
  out <- rbindlist(lapply(years, function(y) {
    zip <- here("raw", "pbp", MAEXITS_PBP_FILES[[as.character(y)]])
    .vcheck(file.exists(zip), "PBP file for CY%d not found: %s", y, zip)
    .pbp_year(y, zip)
  }), use.names = TRUE)
  flags <- setdiff(names(out), c("year", "contract_id", "plan_id", "segment_id",
                                 "partb_giveback_amt", "otc_annual_amt", "food_produce", "in_home_support"))
  for (j in flags) set(out, i = which(is.na(out[[j]])), j = j, value = FALSE)
  setorder(out, year, contract_id, plan_id, segment_id)

  .vcheck(!anyDuplicated(out, by = c("year", "contract_id", "plan_id", "segment_id")),
          "pbp_benefits has duplicate year-contract-plan-segment rows")
  n <- out[, .N, by = year]
  .vcheck(all(n$N > 3000), "PBP CY%s has fewer than 3,000 plan-segments",
          paste(n[N <= 3000, year], collapse = ", "))
  gb <- out[partb_giveback == TRUE, range(partb_giveback_amt, na.rm = TRUE)]
  .vcheck(gb[1] > 0 && gb[2] < 250, "Part B reduction amounts outside (0, 250): %s",
          paste(gb, collapse = " to "))

  if (save) fwrite(out, here("trunk", "derived", "pbp_benefits.csv"))
  message("PBP benefits: ", nrow(out), " plan-segments, CY", min(years), "-CY", max(years))
  invisible(out)
}
