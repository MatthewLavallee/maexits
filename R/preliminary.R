# ============================================================
# preliminary.R — Early forced-disenrollment estimate, before the
# December and January CPSC files are released
#
# When CMS publishes the crosswalk and landscape for year N (~Oct 1), the
# plans exiting at the end of the year are known, but December N-1 and
# January N enrollment are not. Exit exposure (displaced enrollment, exit
# rate, exit group) needs only a pre-exit enrollment month, so
# run_preliminary() substitutes the latest CPSC month for December.
# January-dependent measures are left NA. Outputs are written to
# trunk/derived/preliminary/ with the crosswalk year and source month in
# the file name, and never overwrite the final tables.
#
# Score every new proxy month with backtest_preliminary() against a
# completed year before quoting it.
# ============================================================


# County-panel columns that need January enrollment or January-only rows
# (n_plans_dec counts SAE-expansion and new-plan rows in the final panel)
.JAN_COLUMNS <- c("total_jan_enrollment", "incumbent_jan", "new_entrant_jan",
                  "enrollment_change", "net_enrollment_change", "incumbent_growth",
                  "n_plans_dec", "total_jan_enrollment_once",
                  "total_jan_enrollment_once_low", "incumbent_jan_once",
                  "new_entrant_jan_once", "enrollment_change_once",
                  "incumbent_growth_once")


#' Parse year and month from a CPSC_Enrollment_Info_YYYY_MM.csv file name
#' @keywords internal
.parse_cpsc_name <- function(path) {
  b <- basename(path)
  m <- regmatches(b, regexec("^CPSC_Enrollment_Info_(\\d{4})_(\\d{2})\\.csv$", b))[[1]]
  .vcheck(length(m) == 3, "%s is not named CPSC_Enrollment_Info_YYYY_MM.csv", b)
  list(year = as.integer(m[2]), month = as.integer(m[3]))
}


#' Preliminary Forced-Disenrollment Estimate
#'
#' Estimates exit exposure for crosswalk year N as soon as the N crosswalk
#' and CY N landscape are published, using a pre-exit CPSC month from year
#' N-1 in place of December N-1. Only exit measures are produced
#' (\code{displaced_enrollment}, \code{total_dec_enrollment} as the proxy
#' denominator, \code{exit_rate}, \code{exit_group}, \code{n_exiting_plans},
#' and their \code{_once} versions); January-dependent columns are NA. The
#' printed national summary uses the \code{_once} columns.
#'
#' The crosswalk and landscape must already be registered in R/config.R;
#' \code{MAEXITS_XWALK_YEARS} does not need to include \code{xwalk_year}.
#' Keep proxy months in \code{raw/monthly enrollment/}: a non-December or
#' non-January file in the december/january folders stops
#' \code{make_enrollment()} with a wrong-month error.
#'
#' Penetration quartiles use the cutpoints of the final county panel when
#' \code{trunk/derived/county_panel.csv} exists, so counties fall in the
#' same quartile as in the final panel. The input files are recorded in
#' the \code{prelim_*} columns.
#'
#' @param xwalk_year Integer crosswalk year N (e.g., 2027).
#' @param proxy_file Path to one CPSC_Enrollment_Info_YYYY_MM.csv, absolute
#'   or relative to \code{raw/}. Its year must be N-1.
#' @param save Logical. If TRUE (default), writes the county panel and
#'   analytic table to \code{out_dir}.
#' @param out_dir Output folder (default \code{trunk/derived/preliminary});
#'   may not be \code{trunk/derived} itself.
#' @param verbose Logical. If TRUE (default), prints the national summary.
#' @param overwrite Logical. If FALSE (default), refuses to replace an
#'   existing estimate for the same crosswalk year and source month.
#' @return Invisibly, a list with \code{panel}, \code{analytic} and
#'   \code{national}.
#' @export
run_preliminary <- function(xwalk_year, proxy_file, save = TRUE,
                            out_dir = here("trunk", "derived", "preliminary"),
                            verbose = TRUE, overwrite = FALSE) {
  n <- as.integer(xwalk_year)
  path <- if (file.exists(proxy_file)) proxy_file else here("raw", proxy_file)
  .vcheck(file.exists(path), "proxy CPSC file not found: %s", proxy_file)
  src <- .parse_cpsc_name(path)
  .vcheck(src$year == n - 1L, paste0(
    "the proxy month must be from %d (the calendar year of the December it ",
    "stands in for); %s is from %d"), n - 1L, basename(path), src$year)
  if (save) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    .vcheck(normalizePath(out_dir) != normalizePath(here("trunk", "derived")),
            "out_dir must not be trunk/derived: preliminary tables never replace final ones")
  }
  .resolve_crosswalk_path(n)
  for (cy in c(n - 1L, n)) {
    .vcheck(.landscape_available(cy),
            "landscape CY%d is missing or not registered in MAEXITS_LANDSCAPE_FILES", cy)
  }

  tag <- sprintf("xw%d_src%d%02d", n, src$year, src$month)
  outputs <- file.path(out_dir, paste0(c("county_panel_prelim_", "analytictable_prelim_"),
                                       tag, ".csv"))
  if (save && !overwrite) {
    .vcheck(!any(file.exists(outputs)), paste0(
      "a preliminary estimate for %s already exists in %s; use overwrite = TRUE ",
      "to replace it"), tag, out_dir)
  }
  if (verbose) message("Preliminary run ", tag, ": December ", n - 1L,
                       " proxied by ", basename(path))

  proxy <- .aggregate_cpsc(path, basename(path))  # year = N-1, as for December
  landscape <- make_landscape(save = FALSE, years = (n - 2L):n)
  at <- make_analytic(landscape, proxy, save = FALSE, xwalk_years = n,
                      preliminary = TRUE)
  at <- add_fips_and_penetration(at, save = FALSE)
  if (file.exists(here("raw", "ratebook", sprintf("countyrate%d.csv", n - 1L)))) {
    at <- add_benchmark(at, save = FALSE, verbose = FALSE)
  } else {
    message("countyrate", n - 1L, ".csv is not on disk: benchmark left NA")
    at[, benchmark := NA_real_]
  }
  at <- augment_analytic(at, save = FALSE, verbose = FALSE,
                         landscape = landscape, xwalk_years = n)
  panel <- make_county_panel(at, save = FALSE)
  panel[, (.JAN_COLUMNS) := NA_real_]

  # Use the final panel's penetration cutpoints (the single-year panel's
  # own quantiles differ slightly)
  final_path <- here("trunk", "derived", "county_panel.csv")
  if (file.exists(final_path)) {
    pen <- fread(final_path, select = "penetration_2018")$penetration_2018
    breaks <- quantile(unique(pen[!is.na(pen)]), probs = c(0, 0.25, 0.5, 0.75, 1))
    panel[, pen_quartile := cut(penetration_2018, breaks = breaks,
      labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"), include.lowest = TRUE)]
  } else {
    message("trunk/derived/county_panel.csv not found: penetration quartiles ",
            "use this run's own cutpoints")
  }

  cy_file <- function(cy) {
    if (cy <= 2024L) sprintf("CY%d (fixed reader)", cy) else MAEXITS_LANDSCAPE_FILES[[as.character(cy)]]
  }
  panel[, `:=`(prelim_xwalk_year = n,
               prelim_source = sprintf("%d-%02d", src$year, src$month),
               prelim_crosswalk_file = MAEXITS_XWALK_FILES[[as.character(n)]],
               prelim_landscape_files = paste(cy_file(n - 1L), cy_file(n), sep = " | "))]

  # Each plan-county counted once (the _once columns)
  national <- panel[, .(
    dec_year = n - 1L,
    proxy_month = sprintf("%d-%02d", src$year, src$month),
    displaced = sum(displaced_enrollment_once),
    proxy_enrollment = sum(total_dec_enrollment_once),
    exit_rate_pct = round(100 * sum(displaced_enrollment_once) / sum(total_dec_enrollment_once), 2),
    high_exit_counties = sum(exit_group_once == "High (>15%)")
  )]

  if (save) {
    fwrite(panel, outputs[1])
    fwrite(at, outputs[2])
    message("Wrote preliminary tables to ", out_dir)
  }
  if (verbose) print(national)
  invisible(list(panel = panel, analytic = at, national = national))
}


#' Backtest a Preliminary Estimate Against a Completed Year
#'
#' Runs \code{run_preliminary()} for a crosswalk year whose final December
#' data exist, and compares it with the final county panel: national exit
#' rate and displaced count, the gap between the proxy and December
#' denominators, county-level agreement, and state-level error bands, all
#' from the \code{_once} columns (each plan-county counted once).
#' Use it to decide how far to trust a proxy month (for example, backtest
#' September 2025 against December 2025 before quoting a September 2026
#' estimate), and report the state error bands alongside any state figure.
#'
#' @param xwalk_year Integer crosswalk year with final data (e.g., 2026).
#' @param proxy_file CPSC file from year \code{xwalk_year - 1}; see
#'   \code{run_preliminary()}.
#' @param panel Final county panel, or NULL to read
#'   \code{trunk/derived/county_panel.csv}.
#' @return A list with \code{national}, \code{county} and \code{state}
#'   comparison tables.
#' @export
backtest_preliminary <- function(xwalk_year, proxy_file, panel = NULL) {
  n <- as.integer(xwalk_year)
  panel <- .load_if_null(panel, "trunk/derived/county_panel.csv")
  final <- panel[dec_year == n - 1L]
  .vcheck(nrow(final) > 0, "the final panel has no dec_year %d to backtest against", n - 1L)
  prelim <- run_preliminary(n, proxy_file, save = FALSE, verbose = FALSE)$panel
  proxy_month <- prelim$prelim_source[1]

  # Score the measures that count each plan-county once
  key <- c("county_name", "state_name")
  once <- c(exit_rate = "exit_rate_once", displaced_enrollment = "displaced_enrollment_once",
            total_dec_enrollment = "total_dec_enrollment_once", exit_group = "exit_group_once")
  pick <- function(p) setnames(p[, c(key, once), with = FALSE], once, names(once))
  final <- pick(final)
  prelim <- pick(prelim)
  cmp <- merge(final, prelim, by = key, suffixes = c("_final", "_prelim"))

  national <- data.table(
    dec_year = n - 1L,
    proxy_month = proxy_month,
    exit_rate_final = round(100 * sum(final$displaced_enrollment) /
                              sum(final$total_dec_enrollment), 2),
    exit_rate_prelim = round(100 * sum(prelim$displaced_enrollment) /
                               sum(prelim$total_dec_enrollment), 2),
    displaced_final = sum(final$displaced_enrollment),
    displaced_prelim = sum(prelim$displaced_enrollment),
    displaced_diff_pct = round(100 * (sum(prelim$displaced_enrollment) /
                                        sum(final$displaced_enrollment) - 1), 1),
    denominator_diff_pct = round(100 * (sum(prelim$total_dec_enrollment) /
                                          sum(final$total_dec_enrollment) - 1), 1)
  )
  county <- cmp[!is.na(exit_rate_final) & !is.na(exit_rate_prelim), .(
    counties = .N,
    exit_rate_cor = round(cor(exit_rate_final, exit_rate_prelim), 4),
    mae_pp = round(100 * mean(abs(exit_rate_final - exit_rate_prelim)), 2),
    high_exit_agreement_pct = round(100 * mean(
      (exit_group_final == "High (>15%)") == (exit_group_prelim == "High (>15%)")), 1)
  )]
  state <- merge(
    final[, .(exit_final = 100 * sum(displaced_enrollment) / sum(total_dec_enrollment),
              displaced_final = sum(displaced_enrollment)), by = state_name],
    prelim[, .(exit_prelim = 100 * sum(displaced_enrollment) / sum(total_dec_enrollment),
               displaced_prelim = sum(displaced_enrollment)), by = state_name],
    by = "state_name")
  state[, `:=`(abs_error_pp = round(abs(exit_prelim - exit_final), 2),
               displaced_diff_pct = round(100 * (displaced_prelim /
                                                   displaced_final - 1), 1),
               exit_final = round(exit_final, 2), exit_prelim = round(exit_prelim, 2))]
  setorder(state, -abs_error_pp)

  list(national = national, county = county, state = state)
}
