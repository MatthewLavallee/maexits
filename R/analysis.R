# ============================================================
# analysis.R — Descriptive Summaries and Analysis Functions
#
# All functions take a data.table as their first argument
# (NULL triggers loading from the standard CSV path). None
# write files — they return data.tables for the caller to
# print, format, or pass to kable.
#
# Plan-level summaries (from the analytic table):
#   1. summarize_exits()       — terminated/non-renewed plans
#   2. summarize_sar()         — service area reductions
#   3. summarize_sae()         — service area expansions
#   4. summarize_new_plans()   — new plan entries
#   5. summarize_stable()      — stable renewals
#   6. summarize_by_status()   — cross-tabulations
#
# Geographic analysis (from the county panel):
#   7. tabulate_state_exits()        — state-level exit rates
#   8. tabulate_exit_dynamics()      — enrollment by exit group
#   9. tabulate_penetration_impacts() — by pen quartile
#  10. test_penetration_hypothesis() — correlation tests
#  11. run_all_analyses()            — convenience runner
# ============================================================


# --- Internal Helper (duplicated from data_build.R for self-containment) ---

.load_if_null_analysis <- function(dt, path) {
  if (is.null(dt)) {
    dt <- fread(here(path))
  }
  dt <- copy(dt)
  if ("pen_quartile" %in% names(dt)) {
    dt[pen_quartile == "", pen_quartile := NA_character_]
    dt[, pen_quartile := factor(pen_quartile,
      levels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"))]
  }
  for (g in intersect(c("exit_group", "exit_group_once"), names(dt))) {
    dt[, (g) := factor(get(g),
      levels = c("No exits", "Low (<5%)", "Medium (5-15%)", "High (>15%)"))]
  }
  dt
}


# ============================================================
# PLAN-LEVEL SUMMARIES
# ============================================================

#' Summarize Plan Exits by Year
#'
#' Counts terminated and non-renewed plans and their December enrollment
#' for each transition year.
#'
#' @param at data.table or NULL. If NULL, reads
#'   \code{trunk/derived/analytictable.csv}.
#' @return data.table with columns: dec_year, n_plans, dec_enrollment.
#' @export
summarize_exits <- function(at = NULL) {
  at <- .load_if_null_analysis(at, "trunk/derived/analytictable.csv")
  exit_statuses <- .statuses("terminated")
  exits <- at[status %in% exit_statuses]
  exits[, .(
    n_plans = uniqueN(paste(contract_id, plan_id)),
    dec_enrollment = sum(dec_enrollment, na.rm = TRUE)
  ), by = dec_year][order(dec_year)]
}


#' Summarize Service Area Reductions by Year
#'
#' Counts SAR plans and their enrollment changes for each transition year.
#'
#' @param at data.table or NULL. If NULL, reads
#'   \code{trunk/derived/analytictable.csv}.
#' @return data.table with columns: dec_year, n_plans, dec_enrollment,
#'   jan_enrollment, enroll_change.
#' @export
summarize_sar <- function(at = NULL) {
  at <- .load_if_null_analysis(at, "trunk/derived/analytictable.csv")
  sar <- at[status %in% .statuses("service_area_reduction")]
  out <- sar[, .(
    n_plans = uniqueN(paste(contract_id, plan_id)),
    dec_enrollment = sum(dec_enrollment, na.rm = TRUE),
    jan_enrollment = sum(jan_enrollment, na.rm = TRUE)
  ), by = dec_year][order(dec_year)]
  out[, enroll_change := jan_enrollment - dec_enrollment]
  out
}


#' Summarize Service Area Expansions by Year
#'
#' Counts SAE plans and splits enrollment between existing counties
#' (where the plan already operated) and expansion counties (new service areas).
#'
#' @param at data.table or NULL. If NULL, reads
#'   \code{trunk/derived/analytictable.csv}.
#' @return Named list with three data.tables:
#'   \describe{
#'     \item{existing}{SAE plans in existing counties (had Dec enrollment)}
#'     \item{expansion}{SAE plans in expansion counties (no Dec enrollment)}
#'     \item{combined}{All SAE plans combined}
#'   }
#' @export
summarize_sae <- function(at = NULL) {
  at <- .load_if_null_analysis(at, "trunk/derived/analytictable.csv")
  sae <- at[status %in% .statuses("service_area_expansion")]

  existing <- sae[!is.na(dec_enrollment), .(
    n_plans = uniqueN(paste(contract_id, plan_id)),
    existing_counties = .N,
    dec_enrollment = sum(dec_enrollment, na.rm = TRUE),
    jan_enrollment = sum(jan_enrollment, na.rm = TRUE)
  ), by = dec_year][order(dec_year)]
  existing[, enroll_change := jan_enrollment - dec_enrollment]

  expansion <- sae[is.na(dec_enrollment), .(
    n_plans = uniqueN(paste(contract_id, plan_id)),
    expansion_counties = .N,
    jan_enrollment = sum(jan_enrollment, na.rm = TRUE)
  ), by = dec_year][order(dec_year)]

  combined <- sae[, .(
    n_plans = uniqueN(paste(contract_id, plan_id)),
    dec_enrollment = sum(dec_enrollment, na.rm = TRUE),
    jan_enrollment = sum(jan_enrollment, na.rm = TRUE)
  ), by = dec_year][order(dec_year)]
  combined[, enroll_change := jan_enrollment - dec_enrollment]

  list(existing = existing, expansion = expansion, combined = combined)
}


#' Summarize New Plan Entries by Year
#'
#' Counts new plans and initial contracts and their January enrollment.
#'
#' @param at data.table or NULL. If NULL, reads
#'   \code{trunk/derived/analytictable.csv}.
#' @return data.table with columns: dec_year, n_plans, jan_enrollment.
#' @export
summarize_new_plans <- function(at = NULL) {
  at <- .load_if_null_analysis(at, "trunk/derived/analytictable.csv")
  new_statuses <- .statuses("new")
  newp <- at[status %in% new_statuses]
  newp[, .(
    n_plans = uniqueN(paste(contract_id, plan_id)),
    jan_enrollment = sum(jan_enrollment, na.rm = TRUE)
  ), by = dec_year][order(dec_year)]
}


#' Summarize Stable Renewals by Year
#'
#' Counts renewal and consolidated renewal plans and their enrollment changes.
#'
#' @param at data.table or NULL. If NULL, reads
#'   \code{trunk/derived/analytictable.csv}.
#' @return data.table with columns: dec_year, n_plans, dec_enrollment,
#'   jan_enrollment, enroll_change.
#' @export
summarize_stable <- function(at = NULL) {
  at <- .load_if_null_analysis(at, "trunk/derived/analytictable.csv")
  stable_statuses <- .statuses("continuing")
  stable <- at[status %in% stable_statuses]
  out <- stable[, .(
    n_plans = uniqueN(paste(contract_id, plan_id)),
    dec_enrollment = sum(dec_enrollment, na.rm = TRUE),
    jan_enrollment = sum(jan_enrollment, na.rm = TRUE)
  ), by = dec_year][order(dec_year)]
  out[, enroll_change := jan_enrollment - dec_enrollment]
  out
}


#' Summarize Plans and Enrollment by Status
#'
#' Produces two summary tables: a wide cross-tabulation of plan counts
#' by status and year, and a total enrollment table by status across all years.
#'
#' @param at data.table or NULL. If NULL, reads
#'   \code{trunk/derived/analytictable.csv}.
#' @return Named list with two data.tables:
#'   \describe{
#'     \item{plan_summary}{Wide format: dec_year x status with plan counts}
#'     \item{enrollment_by_status}{Per-status totals with pct_of_dec column}
#'   }
#' @export
summarize_by_status <- function(at = NULL) {
  at <- .load_if_null_analysis(at, "trunk/derived/analytictable.csv")

  plan_summary <- dcast(
    at[, .(n_plans = uniqueN(paste(contract_id, plan_id))),
       by = .(dec_year, status)],
    dec_year ~ status, value.var = "n_plans", fill = 0
  )

  enrollment_by_status <- at[, .(
    county_plan_obs = .N,
    total_dec_enrollment = sum(dec_enrollment, na.rm = TRUE),
    total_jan_enrollment = sum(jan_enrollment, na.rm = TRUE)
  ), by = status]
  enrollment_by_status[, enroll_change := total_jan_enrollment - total_dec_enrollment]
  enrollment_by_status[, pct_of_dec := round(
    total_dec_enrollment / sum(total_dec_enrollment) * 100, 1)]
  enrollment_by_status <- enrollment_by_status[order(-total_dec_enrollment)]

  list(plan_summary = plan_summary, enrollment_by_status = enrollment_by_status)
}


# ============================================================
# GEOGRAPHIC ANALYSIS
# ============================================================

#' Tabulate State-Level Exit Rates
#'
#' Computes enrollment-weighted exit rates by state and year, plus
#' a national summary.
#'
#' @param panel data.table or NULL. If NULL, reads
#'   \code{trunk/derived/county_panel.csv}.
#' @return Named list with two data.tables:
#'   \describe{
#'     \item{state_wide}{Wide format: state_name x year with exit rates (%)}
#'     \item{national}{National exit rate, displaced count, and total Dec
#'       enrollment by year}
#'   }
#' @export
tabulate_state_exits <- function(panel = NULL) {
  panel <- .load_if_null_analysis(panel, "trunk/derived/county_panel.csv")

  state_exit <- panel[, .(
    exit_rate = round(sum(displaced_enrollment) / sum(total_dec_enrollment) * 100, 1)
  ), by = .(state_name, dec_year)]

  state_wide <- dcast(state_exit, state_name ~ dec_year,
                      value.var = "exit_rate", fill = 0)
  # Order by the latest year; drop states with no December enrollment in it
  latest <- as.character(max(panel$dec_year))
  state_wide <- state_wide[!is.nan(get(latest))]
  setorderv(state_wide, latest, order = -1)

  national <- panel[, .(
    exit_rate_pct = round(sum(displaced_enrollment) /
                            sum(total_dec_enrollment) * 100, 1),
    displaced = sum(displaced_enrollment),
    total_dec = sum(total_dec_enrollment)
  ), by = dec_year][order(dec_year)]

  list(state_wide = state_wide, national = national)
}


#' Tabulate Enrollment Dynamics by Exit Exposure Group
#'
#' Computes mean enrollment changes, share of counties losing enrollment,
#' and county counts for each exit exposure group by year.
#'
#' @param panel data.table or NULL. If NULL, reads
#'   \code{trunk/derived/county_panel.csv}.
#' @return data.table with columns: dec_year, exit_group, n_counties,
#'   mean_exit_rate, mean_enroll_change, median_enroll_change,
#'   pct_lost_enrollment.
#' @export
tabulate_exit_dynamics <- function(panel = NULL) {
  panel <- .load_if_null_analysis(panel, "trunk/derived/county_panel.csv")

  panel[!is.na(enrollment_change), .(
    n_counties = .N,
    mean_exit_rate = round(mean(exit_rate, na.rm = TRUE) * 100, 1),
    mean_enroll_change = round(mean(enrollment_change, na.rm = TRUE) * 100, 1),
    median_enroll_change = round(median(enrollment_change, na.rm = TRUE) * 100, 1),
    pct_lost_enrollment = round(mean(enrollment_change < 0, na.rm = TRUE) * 100, 1)
  ), by = .(dec_year, exit_group)][order(dec_year, exit_group)]
}


#' Tabulate Exit Impacts by 2018 Penetration Quartile
#'
#' Computes both unweighted (county-mean) and enrollment-weighted exit rates,
#' enrollment changes, and share of counties losing enrollment by 2018 MA
#' penetration quartile and year.
#'
#' @param panel data.table or NULL. If NULL, reads
#'   \code{trunk/derived/county_panel.csv}.
#' @return data.table with columns: dec_year, pen_quartile, n_counties,
#'   unwt_exit_rate, wt_exit_rate, unwt_enroll_change, wt_enroll_change,
#'   pct_lost_enrollment, total_dec_enroll. The penetration quartile
#'   cutpoints are stored as an attribute \code{"pen_breaks"}.
#' @export
tabulate_penetration_impacts <- function(panel = NULL) {
  panel <- .load_if_null_analysis(panel, "trunk/derived/county_panel.csv")

  # Recompute pen_breaks (same logic as make_county_panel)
  pen_breaks <- quantile(
    panel[!is.na(penetration_2018), unique(penetration_2018)],
    probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

  result <- panel[!is.na(pen_quartile) & !is.na(exit_rate), .(
    n_counties = .N,
    unwt_exit_rate = round(mean(exit_rate, na.rm = TRUE) * 100, 1),
    wt_exit_rate = round(sum(displaced_enrollment) /
                           sum(total_dec_enrollment) * 100, 1),
    unwt_enroll_change = round(mean(enrollment_change, na.rm = TRUE) * 100, 1),
    wt_enroll_change = round(
      (sum(total_jan_enrollment) - sum(total_dec_enrollment)) /
        sum(total_dec_enrollment) * 100, 1),
    pct_lost_enrollment = round(mean(enrollment_change < 0, na.rm = TRUE) * 100, 1),
    total_dec_enroll = sum(total_dec_enrollment)
  ), by = .(dec_year, pen_quartile)][order(dec_year, pen_quartile)]

  attr(result, "pen_breaks") <- pen_breaks
  result
}


#' Test Penetration-Resilience Hypothesis
#'
#' For each specified year, computes correlations between 2018 MA penetration
#' and enrollment change, both unconditionally and conditional on having exits.
#' Also returns enrollment-weighted stats by penetration quartile.
#'
#' @param panel data.table or NULL. If NULL, reads
#'   \code{trunk/derived/county_panel.csv}.
#' @param years Integer vector. Transition years to test. Default NULL
#'   uses the two latest years in the panel.
#' @param verbose Logical. If TRUE (default), prints results to console.
#' @return data.table with columns: dec_year, cor_all, n_all,
#'   cor_exits_only, n_exits_only.
#' @export
test_penetration_hypothesis <- function(panel = NULL, years = NULL,
                                        verbose = TRUE) {
  panel <- .load_if_null_analysis(panel, "trunk/derived/county_panel.csv")
  if (is.null(years)) years <- tail(sort(unique(panel$dec_year)), 2)

  results <- rbindlist(lapply(years, function(yr) {
    p <- panel[dec_year == yr & !is.na(pen_quartile) & !is.na(enrollment_change)]
    cor_all <- cor(p$penetration_2018, p$enrollment_change, use = "complete.obs")

    p_exits <- p[exit_rate > 0]
    cor_exits <- if (nrow(p_exits) > 10) {
      cor(p_exits$penetration_2018, p_exits$enrollment_change, use = "complete.obs")
    } else {
      NA_real_
    }

    if (verbose) {
      message("--- dec_year = ", yr, " ---")
      message("Correlation(penetration_2018, enrollment_change): ",
              round(cor_all, 3), " (n = ", nrow(p), ")")
      if (!is.na(cor_exits)) {
        message("Among counties with exits: correlation = ",
                round(cor_exits, 3), " (n = ", nrow(p_exits), ")")
      }
      message("Enrollment-weighted stats by penetration quartile:")
      print(p[!is.na(exit_rate), .(
        wt_exit_rate = round(sum(displaced_enrollment) /
                               sum(total_dec_enrollment) * 100, 1),
        wt_enroll_chg = round(
          (sum(total_jan_enrollment) - sum(total_dec_enrollment)) /
            sum(total_dec_enrollment) * 100, 1),
        pct_lost = round(mean(enrollment_change < 0, na.rm = TRUE) * 100, 1),
        n = .N
      ), by = pen_quartile][order(pen_quartile)])
      message("")
    }

    data.table(
      dec_year = yr,
      cor_all = round(cor_all, 3),
      n_all = nrow(p),
      cor_exits_only = if (!is.na(cor_exits)) round(cor_exits, 3) else NA_real_,
      n_exits_only = nrow(p_exits)
    )
  }))

  results
}


#' Run All Analyses
#'
#' Convenience function that runs all analysis functions and returns
#' results as a named list.
#'
#' @param at data.table or NULL. Analytic table for plan-level summaries.
#'   If NULL, reads from \code{trunk/derived/analytictable.csv}.
#' @param panel data.table or NULL. County panel for geographic analysis.
#'   If NULL, reads from \code{trunk/derived/county_panel.csv}.
#' @param verbose Logical. If TRUE (default), prints results to console.
#' @return Named list with elements: exits, sar, sae, new_plans, stable,
#'   by_status, state_exits, exit_dynamics, penetration_impacts,
#'   penetration_test.
#' @export
run_all_analyses <- function(at = NULL, panel = NULL, verbose = TRUE) {
  if (verbose) message("=== Plan-level summaries ===\n")
  results <- list(
    exits = summarize_exits(at),
    sar = summarize_sar(at),
    sae = summarize_sae(at),
    new_plans = summarize_new_plans(at),
    stable = summarize_stable(at),
    by_status = summarize_by_status(at)
  )

  if (verbose) message("\n=== Geographic analysis ===\n")
  results$state_exits <- tabulate_state_exits(panel)
  results$exit_dynamics <- tabulate_exit_dynamics(panel)
  results$penetration_impacts <- tabulate_penetration_impacts(panel)
  results$penetration_test <- test_penetration_hypothesis(panel, verbose = verbose)

  if (verbose) message("=== All analyses complete ===")
  invisible(results)
}
