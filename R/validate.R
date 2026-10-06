# ============================================================
# validate.R — Fail-loud checks the pipeline runs every cycle
#
# Each check stop()s with a "[validate]" message instead of letting a
# missing, misfiled or reformatted input flow silently into the outputs
# (e.g. a missing CPSC month being imputed as 10 enrollees per row).
# Thresholds were calibrated on dec_year 2018-2025; the historical range
# is noted next to each one, and every check passes on the 2018-2025 data.
# ============================================================


.vcheck <- function(ok, ...) {
  if (!isTRUE(ok)) stop("[validate] ", sprintf(...), call. = FALSE)
  invisible(TRUE)
}


# --- Input discovery ----------------------------------------------------

#' Find the CPSC enrollment file(s) for one year and month
#'
#' @param month_folder Folder under \code{raw/}.
#' @param year,month Integers.
#' @return Character vector of matching paths (length 1 when staged correctly).
#' @keywords internal
.cpsc_files <- function(month_folder, year, month) {
  target <- sprintf("^CPSC_Enrollment_Info_%d_%02d\\.csv$",
                    as.integer(year), as.integer(month))
  list.files(here("raw", month_folder), pattern = target,
             recursive = TRUE, full.names = TRUE)
}


#' Whether a landscape year can be read
#' @keywords internal
.landscape_available <- function(cy) {
  cy <- as.integer(cy)
  if (cy <= 2023L) {
    files <- list.files(here("raw", "landscape", paste0("CY", cy)),
                        pattern = "csv", recursive = TRUE)
    return(any(grepl("MA", files)) && any(grepl("SNP", files)))
  }
  if (cy == 2024L) return(all(file.exists(.landscape_2024_paths())))
  key <- as.character(cy)
  key %in% names(MAEXITS_LANDSCAPE_FILES) &&
    file.exists(here("raw", "landscape", MAEXITS_LANDSCAPE_FILES[[key]]))
}


#' Check that every input for the requested crosswalk years is present
#'
#' Run this before \code{run_data_pipeline()} (which also calls it). It
#' lists everything missing at once: unregistered or absent crosswalk
#' files, every landscape year the build reads, missing or duplicated
#' December/January CPSC files, missing NBER ratebook years, the FIPS
#' and penetration reference files, and, for the exits table, the September
#' CPSC file of every year in \code{exits_years} with the crosswalk and
#' landscape of any of those years past \code{xwalk_years}, and the PDP
#' landscapes for the PDP exits table. It also notes
#' inputs already staged for a later crosswalk year that is not yet in
#' \code{MAEXITS_XWALK_YEARS}.
#'
#' @param xwalk_years Integer vector of crosswalk years to check.
#' @param exits_years Crosswalk years of the exits table (default
#'   \code{MAEXITS_EXITS_YEARS}).
#' @return Invisibly TRUE; stops with the list of problems otherwise.
#' @export
check_inputs <- function(xwalk_years = MAEXITS_XWALK_YEARS, exits_years = MAEXITS_EXITS_YEARS) {
  .vcheck(!anyDuplicated(xwalk_years), "duplicated crosswalk year(s): %s",
          paste(xwalk_years[duplicated(xwalk_years)], collapse = ", "))
  problems <- character(0)
  for (cy in .landscape_years(xwalk_years)) {
    if (!.landscape_available(cy)) problems <- c(problems, sprintf(
      "landscape CY%d is missing or not registered in MAEXITS_LANDSCAPE_FILES", cy))
  }
  for (n in as.integer(xwalk_years)) {
    key <- as.character(n)
    if (!key %in% names(MAEXITS_XWALK_FILES)) {
      problems <- c(problems, sprintf(
        "crosswalk %d is not registered in MAEXITS_XWALK_FILES (R/config.R)", n))
    } else {
      p <- here("raw", "plan crosswalk", MAEXITS_XWALK_FILES[[key]])
      if (!file.exists(p)) problems <- c(problems, sprintf(
        "crosswalk %d: registered file not found: %s", n, p))
    }
    for (spec in list(list("december enrollment", n - 1L, 12L),
                      list("january enrollment", n, 1L))) {
      hits <- .cpsc_files(spec[[1]], spec[[2]], spec[[3]])
      want <- sprintf("CPSC_Enrollment_Info_%d_%02d.csv", spec[[2]], spec[[3]])
      if (length(hits) == 0) {
        problems <- c(problems, sprintf("raw/%s: %s not found", spec[[1]], want))
      } else if (length(hits) > 1) {
        problems <- c(problems, sprintf("raw/%s: %d copies of %s: %s",
          spec[[1]], length(hits), want, paste(hits, collapse = ", ")))
      }
    }
    rb <- here("raw", "ratebook", sprintf("countyrate%d.csv", n - 1L))
    if (!file.exists(rb)) problems <- c(problems, sprintf(
      "ratebook countyrate%d.csv not found (download from NBER)", n - 1L))
    # Organizations and parents for plan details and the displacement table
    for (spec in list(list("december enrollment", n - 1L, 12L),
                      list("january enrollment", n, 1L))) {
      want <- sprintf("^CPSC_Contract_Info_%d_%02d\\.csv$", spec[[2]], spec[[3]])
      hits <- list.files(here("raw", spec[[1]]), pattern = want, recursive = TRUE)
      if (!length(hits)) {
        problems <- c(problems, sprintf("raw/%s: CPSC_Contract_Info_%d_%02d.csv not found",
                                        spec[[1]], spec[[2]], spec[[3]]))
      } else if (length(hits) > 1) {
        problems <- c(problems, sprintf("raw/%s: %d copies of CPSC_Contract_Info_%d_%02d.csv",
                                        spec[[1]], length(hits), spec[[2]], spec[[3]]))
      }
    }
  }
  for (cy in .plan_detail_years(xwalk_years)) {
    key <- as.character(cy)
    # Part D premiums come from the report to CY2024 (and any later year
    # registered there), from the combined landscape after that
    if (cy <= 2024L || key %in% names(MAEXITS_PARTD_REPORT_FILES)) {
      if (!key %in% names(MAEXITS_PARTD_REPORT_FILES)) {
        problems <- c(problems, sprintf(
          "Part D premium report for CY%d is not registered in MAEXITS_PARTD_REPORT_FILES", cy))
      } else {
        f <- here("raw", "landscape", MAEXITS_PARTD_REPORT_FILES[[key]])
        if (!all(file.exists(f))) problems <- c(problems, sprintf(
          "Part D premium report for CY%d not found: %s", cy, paste(f[!file.exists(f)], collapse = ", ")))
      }
    }
  }
  # The exits table: September enrollment for each of its years, and the
  # crosswalk and landscape of years past xwalk_years
  missing_dec <- setdiff(as.integer(xwalk_years), as.integer(exits_years))
  if (length(missing_dec)) problems <- c(problems, sprintf(
    "exits_years (MAEXITS_EXITS_YEARS) must include every crosswalk year; missing %s",
    paste(missing_dec, collapse = ", ")))
  for (n in as.integer(exits_years)) {
    hits <- .cpsc_files("monthly enrollment", n - 1L, MAEXITS_EXITS_MONTH)
    want <- sprintf("CPSC_Enrollment_Info_%d_%02d.csv", n - 1L, MAEXITS_EXITS_MONTH)
    if (length(hits) != 1L) problems <- c(problems, sprintf(
      "raw/monthly enrollment: %s (for the exits table) %s", want,
      if (length(hits)) sprintf("found %d times", length(hits)) else "not found"))
    if (!n %in% as.integer(xwalk_years)) {
      key <- as.character(n)
      if (!key %in% names(MAEXITS_XWALK_FILES)) {
        problems <- c(problems, sprintf(
          "crosswalk %d (for the exits table) is not registered in MAEXITS_XWALK_FILES", n))
      } else if (!file.exists(here("raw", "plan crosswalk", MAEXITS_XWALK_FILES[[key]]))) {
        problems <- c(problems, sprintf("crosswalk %d: registered file not found", n))
      }
      if (!.landscape_available(n)) problems <- c(problems, sprintf(
        "landscape CY%d (for the exits table) is missing or not registered in MAEXITS_LANDSCAPE_FILES", n))
    }
  }
  # The PDP exits table: PDP landscapes for December and January of each
  # exits year (CY2025 on, the combined landscape checked above)
  for (cy in (min(as.integer(exits_years)) - 1L):max(as.integer(exits_years))) {
    key <- as.character(cy)
    if (key %in% names(MAEXITS_PDP_LANDSCAPE_FILES)) {
      f <- here("raw", "landscape", MAEXITS_PDP_LANDSCAPE_FILES[[key]])
      if (!all(file.exists(f))) problems <- c(problems, sprintf(
        "PDP landscape CY%d (for the PDP exits table) not found: %s", cy, paste(f[!file.exists(f)], collapse = ", ")))
    } else if (cy <= 2024L) {
      problems <- c(problems, sprintf(
        "PDP landscape CY%d is not registered in MAEXITS_PDP_LANDSCAPE_FILES", cy))
    } else if (!.landscape_available(cy)) {
      problems <- c(problems, sprintf(
        "landscape CY%d (PDP rows, for the PDP exits table) is missing or not registered", cy))
    }
  }
  for (f in c(MAEXITS_FIPS_LOOKUP_FILE, MAEXITS_PENETRATION_FILE)) {
    if (!file.exists(here("raw", f))) problems <- c(problems,
      sprintf("reference file raw/%s not found", f))
  }
  .vcheck(length(problems) == 0,
          "inputs incomplete for crosswalk years %s:\n  - %s",
          paste(range(xwalk_years), collapse = "-"),
          paste(unique(problems), collapse = "\n  - "))
  message("check_inputs: all inputs present for crosswalk years ",
          paste(range(xwalk_years), collapse = "-"))

  # Inputs for a later year are expected between the crosswalk release and
  # the January CPSC release; say so (only the exits table reads that year's
  # crosswalk and landscape, through its September enrollment).
  last <- max(as.integer(xwalk_years))
  later <- setdiff(as.integer(names(MAEXITS_XWALK_FILES)), xwalk_years)
  later <- later[later > last]
  if (length(later)) message("check_inputs: crosswalk ", paste(later, collapse = ", "),
    " is registered but not in MAEXITS_XWALK_YEARS; add it once December ",
    min(later) - 1L, " and January ", min(later), " CPSC are in raw/")
  for (spec in list(list("december enrollment", last - 1L, 12L),
                    list("january enrollment", last, 1L))) {
    newer <- .cpsc_files(spec[[1]], spec[[2]] + 1L, spec[[3]])
    if (length(newer)) message("check_inputs: ", basename(newer[1]), " is in raw/",
      spec[[1]], " but its year is not covered by MAEXITS_XWALK_YEARS, so it is not read")
  }
  invisible(TRUE)
}


# --- Enrollment ---------------------------------------------------------

#' Validate CPSC enrollment file names for one month folder
#'
#' Year and month are parsed from the file's basename. Every file must be
#' named CPSC_Enrollment_Info_YYYY_MM.csv, its month must match the folder,
#' and each year may appear only once (a re-extracted zip would otherwise
#' be read twice).
#'
#' @return Character vector of years, aligned with \code{files}.
#' @keywords internal
.check_enrollment_files <- function(files, month_folder) {
  .vcheck(length(files) > 0,
          "no CPSC_Enrollment_Info files found under raw/%s", month_folder)
  b <- basename(files)
  m <- regmatches(b, regexec("^CPSC_Enrollment_Info_(\\d{4})_(\\d{2})\\.csv$", b))
  bad <- lengths(m) != 3
  .vcheck(!any(bad), "unrecognised CPSC file name(s) in raw/%s: %s",
          month_folder, paste(b[bad], collapse = ", "))
  yr <- vapply(m, `[`, "", 2)
  mo <- vapply(m, `[`, "", 3)
  expect <- c("december enrollment" = "12", "january enrollment" = "01")[month_folder]
  if (!is.na(expect)) {
    .vcheck(all(mo == expect), "raw/%s contains file(s) for the wrong month: %s",
            month_folder, paste(b[mo != expect], collapse = ", "))
  }
  dup <- unique(yr[duplicated(yr)])
  .vcheck(length(dup) == 0,
          "raw/%s has more than one CPSC file for year(s) %s: %s", month_folder,
          paste(dup, collapse = ", "), paste(files[yr %in% dup], collapse = ", "))
  yr
}


#' Check that enrollment and landscape cover every year a run needs, and
#' that January totals are consistent with the prior December
#'
#' @param dfe_jan January enrollment, or NULL for a preliminary run (no
#'   January checks).
#' @keywords internal
.check_year_coverage <- function(dfl, dfe_dec, dfe_jan, xwalk_years) {
  for (n in as.integer(xwalk_years)) {
    .vcheck(nrow(dfl[year == n - 1L]) > 0, "landscape has no rows for CY%d", n - 1L)
    .vcheck(nrow(dfl[year == n]) > 0, "landscape has no rows for CY%d", n)
    d <- dfe_dec[year == as.character(n - 1L), sum(county_enrollment)]
    .vcheck(d > 0, "no December %d enrollment (raw/december enrollment)", n - 1L)
    .check_totals_vs_prior(dfe_dec, n - 1L, "December")
    if (is.null(dfe_jan)) next
    .check_totals_vs_prior(dfe_jan, n, "January")
    j <- dfe_jan[year == as.character(n), sum(county_enrollment)]
    .vcheck(j > 0, "no January %d enrollment (raw/january enrollment)", n)
    # Historical Jan(N) vs Dec(N-1): -0.6% to +2.5%
    .vcheck(abs(j / d - 1) <= 0.05,
            "January %d total (%s) differs from December %d total (%s) by %.1f%%; historical range is -0.6%% to +2.5%%. Check the CPSC files.",
            n, format(j, big.mark = ","), n - 1L, format(d, big.mark = ","),
            100 * (j / d - 1))
  }
  invisible(TRUE)
}


#' Check that every landscape county has a CMS enrollment spelling
#'
#' Rows are joined to CPSC enrollment by county and state name, so a
#' landscape spelling CMS enrollment files never use would silently find no
#' enrollment. make_landscape() applies MAEXITS_COUNTY_NAME_MAP first.
#' @param known Optional table of further CMS county spellings (state_name,
#'   county_name), e.g. from .fips_lookup(). The enrollment tables keep only
#'   plan-counties with enrollment, so a county where no individual-market
#'   MA plan has enrollees appears only there.
#' @keywords internal
.check_county_names <- function(dfl, dfe_dec, dfe_jan, xwalk_years, known = NULL) {
  yrs <- sort(unique(c(as.integer(xwalk_years) - 1L, as.integer(xwalk_years))))
  cps <- unique(rbind(dfe_dec[, .(state_name, county_name)],
                      if (!is.null(dfe_jan)) dfe_jan[, .(state_name, county_name)],
                      if (!is.null(known)) known[, .(state_name, county_name)]))
  keys <- unique(dfl[year %in% yrs, .(state_name, county_name)])
  miss <- keys[!cps, on = .(state_name, county_name)]
  .vcheck(nrow(miss) == 0, paste0(
    "landscape county name(s) with no match in CMS enrollment: %s. Add the CMS ",
    "spelling to MAEXITS_COUNTY_NAME_MAP in R/config.R."),
    paste(head(miss[, paste0(county_name, ", ", state_name)], 15), collapse = "; "))
}


#' Compare one year's CPSC total with the previous year's
#'
#' A file whose total exactly equals the prior year's is a copy; historical
#' year-over-year changes are +2.6% to +4.7% (December) and +1.6% to +5.7%
#' (January).
#' @keywords internal
.check_totals_vs_prior <- function(dfe, yr, side) {
  cur <- dfe[year == as.character(yr), sum(county_enrollment)]
  prev <- dfe[year == as.character(yr - 1L), sum(county_enrollment)]
  if (!length(prev) || prev == 0) return(invisible(TRUE))
  .vcheck(cur != prev, paste0(
    "%s %d CPSC total equals %s %d exactly (%s); the %d file looks like a copy ",
    "of the %d file"), side, yr, side, yr - 1L, format(cur, big.mark = ","), yr, yr - 1L)
  .vcheck(abs(cur / prev - 1) <= 0.10,
          "%s %d CPSC total (%s) differs from %s %d by %.1f%%; historical year-over-year change is +1.6%% to +5.7%%",
          side, yr, format(cur, big.mark = ","), side, yr - 1L, 100 * (cur / prev - 1))
}


#' Stop if too many enrollment values have no reported CMS count
#'
#' @param src Source codes from .attach_enrollment() ("reported",
#'   "suppressed", "mixed", "no_record").
#' @keywords internal
.check_imputation <- function(src, side, xwalk_year) {
  if (!length(src)) return(invisible(TRUE))
  share <- mean(src %in% c("suppressed", "no_record"))
  # Historical share without a CPSC match: December 0.26-0.37,
  # January (non-terminated rows) 0.24-0.34
  .vcheck(share < 0.5,
          "crosswalk %d: %.0f%% of %s enrollment values are CMS-suppressed or have no CPSC record (historical maximum 37%%). Is the %s file present and for the right year?",
          xwalk_year, 100 * share, side, side)
}


# --- Crosswalk ----------------------------------------------------------

#' Check a crosswalk's content against the landscape years around it
#'
#' Continuing MA plans (H/R contracts, plan ID < 800) listed on the
#' PREVIOUS side of crosswalk N must appear in landscape year N-1 more
#' often than in the adjacent years. Historically the correct year matches
#' 0.90-0.94 and beats both neighbours by at least 0.055; a mislabelled or
#' misregistered file fails this check.
#' @keywords internal
.check_xwalk_content <- function(dfx, dfl, xwalk_year) {
  cont <- names(MAEXITS_XWALK_STATUS_CLASS)[
    MAEXITS_XWALK_STATUS_CLASS %in% c("continuing", "service_area_reduction",
                                      "service_area_expansion")]
  prev <- unique(dfx[status %in% cont & grepl("^[HR]", prev_contract) &
                       !is.na(prev_plan) & prev_plan < 800,
                     paste(prev_contract, prev_plan)])
  if (!length(prev)) return(invisible(TRUE))
  share_in <- function(yr) {
    keys <- dfl[year == yr, unique(paste(contract_id, plan_id))]
    if (!length(keys)) return(NA_real_)
    mean(prev %in% keys)
  }
  right <- share_in(xwalk_year - 1L)
  near <- c(share_in(xwalk_year - 2L), share_in(xwalk_year))
  file <- MAEXITS_XWALK_FILES[as.character(xwalk_year)]
  # When landscape CY N matches as well as CY N-1, the CY N registration is
  # the likelier culprit (a copy of the prior year's file).
  suspect <- if (!is.na(near[2]) && !is.na(right) && near[2] >= right) {
    sprintf("the landscape registered for CY%d in MAEXITS_LANDSCAPE_FILES (it matches like CY%d)",
            xwalk_year, xwalk_year - 1L)
  } else {
    "the file registered in MAEXITS_XWALK_FILES"
  }
  # Historical margin over the best neighbouring year: at least 0.055
  .vcheck(!is.na(right) && right >= 0.85 &&
            all(right - near >= 0.03, na.rm = TRUE),
          "crosswalk %d (%s) does not look like the %d crosswalk: %.1f%% of its continuing plans are in landscape CY%d vs %s in CY%d/CY%d. Check %s.",
          xwalk_year, if (is.na(file)) "unregistered" else file, xwalk_year,
          100 * right, xwalk_year - 1L,
          paste(sprintf("%.1f%%", 100 * near), collapse = "/"),
          xwalk_year - 2L, xwalk_year, suspect)
}


#' Check the crosswalk's new-plan rows
#'
#' New plans are identified by a placeholder PREVIOUS_PLAN_ID ("NEW"). If CMS
#' stopped using the placeholder, new plans would silently vanish from the
#' January side. Historically each crosswalk has 701-897 new MA plans, and
#' at most 6 New Plan/Initial Contract rows carry a numeric previous ID.
#' @keywords internal
.check_xwalk_new_rows <- function(dfx, xwalk_year, min_new = 300L) {
  new_cls <- .statuses("new")
  n_numeric <- dfx[status %in% new_cls & !is.na(prev_plan), .N]
  .vcheck(n_numeric <= 25L, paste0(
    "crosswalk %d: %d New Plan/Initial Contract rows have a numeric ",
    "PREVIOUS_PLAN_ID (historical maximum 6); these plans would drop out of ",
    "the January side. Has CMS changed the new-plan convention?"), xwalk_year, n_numeric)
  n_new <- uniqueN(dfx[is.na(prev_plan) & grepl("^[HR]", curr_contract) &
                         !is.na(curr_plan) & curr_plan < 800,
                       .(curr_contract, curr_plan)])
  .vcheck(n_new >= min_new, paste0(
    "crosswalk %d: only %d new MA plans have a NEW placeholder previous ID ",
    "(historical 701-897). Has CMS changed the new-plan convention?"), xwalk_year, n_new)
}


#' Check that the crosswalk's new MA plans appear in landscape year N
#'
#' Historically 98-100% do. A low share means the landscape registered for
#' year N is really another year's file.
#' @keywords internal
.check_new_plans_landscape <- function(dfx, dfl, xwalk_year) {
  keys <- unique(dfx[status %in% .statuses("new") & is.na(prev_plan) &
                       grepl("^[HR]", curr_contract) & !is.na(curr_plan) &
                       curr_plan < 800, paste(curr_contract, curr_plan)])
  if (!length(keys)) return(invisible(TRUE))
  share <- mean(keys %in% dfl[year == xwalk_year, unique(paste(contract_id, plan_id))])
  .vcheck(share >= 0.9, paste0(
    "only %.0f%% of crosswalk %d's new MA plans appear in landscape CY%d ",
    "(historical 98-100%%); check the CY%d file registered in ",
    "MAEXITS_LANDSCAPE_FILES"), 100 * share, xwalk_year, xwalk_year, xwalk_year)
}


#' Check that plans new in landscape year Y have December Y enrollment
#'
#' Historically 88-94% do; a December file that is really the previous
#' year's scores near 0%.
#' @keywords internal
.check_dec_new_plans <- function(dfl, dfe_dec_year, dec_year) {
  if (!nrow(dfl[year == dec_year - 1L])) return(invisible(TRUE))
  k <- dfl[year == dec_year & grepl("^[HR]", contract_id) & plan_id < 800,
           unique(paste(contract_id, plan_id))]
  nk <- setdiff(k, dfl[year == dec_year - 1L, unique(paste(contract_id, plan_id))])
  if (!length(nk)) return(invisible(TRUE))
  share <- mean(nk %in% dfe_dec_year[, paste(contract_id, plan_id)])
  .vcheck(share >= 0.6, paste0(
    "December %d: only %.0f%% of %d MA plans new in CY%d have December enrollment ",
    "(historical 88-94%%). Is CPSC_Enrollment_Info_%d_12.csv really December %d?"),
    dec_year, 100 * share, length(nk), dec_year, dec_year, dec_year)
}


#' Check that the crosswalk's new MA plans appear in January enrollment
#'
#' Historically 77-92% of new MA plans have January enrollment.
#' @keywords internal
.check_new_plans <- function(dfnew, dfe_jan_year, xwalk_year) {
  new_ma <- unique(dfnew[grepl("^[HR]", curr_contract) & !is.na(curr_plan) &
                           curr_plan < 800, paste(curr_contract, curr_plan)])
  if (!length(new_ma)) return(invisible(TRUE))
  share <- mean(new_ma %in% dfe_jan_year[, paste(contract_id, plan_id)])
  .vcheck(share >= 0.6,
          "crosswalk %d: only %.0f%% of %d new MA plans have January enrollment (historical 77-92%%). Is the January %d CPSC file present?",
          xwalk_year, 100 * share, length(new_ma), xwalk_year)
}


# --- Augmented table ----------------------------------------------------

#' Checks run at the end of augment_analytic()
#' @keywords internal
.check_augmented <- function(at, ls_years, xwalk_years) {
  dec_years <- sort(unique(at$dec_year))
  .vcheck(setequal(dec_years, as.integer(xwalk_years) - 1L),
          "analytic table covers dec_year %s but augment_analytic() was asked for crosswalk years %s; both must use the same years",
          paste(dec_years, collapse = ","), paste(xwalk_years, collapse = ","))
  missing_jan <- setdiff(dec_years + 1L, ls_years)
  .vcheck(length(missing_jan) == 0,
          "landscape has no January year(s) %s; every SAR county would be marked as dropped",
          paste(missing_jan, collapse = ", "))
  # Historical SAR dropped share: 0.09-0.33 (1.00 when the January landscape is missing)
  sar <- at[status %in% .statuses("service_area_reduction"),
            .(share = mean(sar_dropped)), by = dec_year]
  .vcheck(all(sar$share < 0.6),
          "SAR dropped share out of range in dec_year %s (historical 0.09-0.33)",
          paste(sar[share >= 0.6, sprintf("%d (%.2f)", dec_year, share)], collapse = ", "))
  n_other <- at[role == "other", .N]
  .vcheck(n_other == 0,
          "%d rows have role 'other' (statuses: %s); update the role rules for the new status",
          n_other, paste(unique(at[role == "other", status]), collapse = ", "))
  invisible(TRUE)
}


#' Per-year FIPS coverage check (historical NA share <= 0.05%)
#'
#' Rows without a FIPS code also get no penetration or benchmark, so a
#' spike points at county-name mismatches between the landscape/CPSC and
#' the FIPS lookup.
#' @keywords internal
.check_fips <- function(at) {
  na <- at[, .(share = mean(is.na(fips))), by = dec_year]
  bad <- na[share >= 0.005]
  .vcheck(nrow(bad) == 0, paste0(
    "no FIPS code for more than 0.5%% of rows in dec_year %s (historical ",
    "maximum 0.05%%); unmatched county names include: %s. Check county name ",
    "spellings against MAEXITS_FIPS_LOOKUP_FILE."),
    paste(sprintf("%d (%.1f%%)", bad$dec_year, 100 * bad$share), collapse = ", "),
    paste(head(unique(at[is.na(fips) & dec_year %in% bad$dec_year,
                         paste0(county_name, ", ", state_name)]), 8), collapse = "; "))
}


#' Per-year benchmark coverage check (historical NA share <= 0.14%)
#' @keywords internal
.check_benchmark <- function(at) {
  na <- at[, .(share = mean(is.na(benchmark))), by = dec_year]
  .vcheck(all(na$share < 0.01),
          "benchmark missing for more than 1%% of rows in dec_year %s; check raw/ratebook",
          paste(na[share >= 0.01, sprintf("%d (%.1f%%)", dec_year, 100 * share)],
                collapse = ", "))
}
