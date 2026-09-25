# Help pages for the datasets loaded with maexits_data() and
# cms_enrollment(). The variable lists are generated from the variable key
# in R/catalog.R.


#' County x transition-year panel
#'
#' One row per county and transition year (`dec_year` 2025 is December 2025
#' to January 2026). Load it with `maexits_data("county_panel")`.
#'
#' @section Counting rows:
#' The panel is built from [plan_county], a table of crosswalk links. The
#' columns without `_once` (`total_dec_enrollment`, `displaced_enrollment`,
#' `exit_rate`, ...) sum over links and use the link-level flag `forced`, so
#' a plan-county with several links (a plan split across successors, a move
#' into another contract, or a consolidation) enters them once per link. The
#' `_once` columns count each plan-county once and use the plan-county exit
#' flag `forced_county`; their December and January totals match CMS's
#' totals for the same plans.
#'
#' Suppressed CMS cells (1-10 enrollees) count as 10; `_low` columns count
#' them as 1.
#'
#' @eval .rd_format("county_panel")
#' @seealso [maexits_data()], [maexits_catalog()], [plan_county]
#' @name county_panel
#' @docType data
#' @keywords datasets
NULL


#' Plan x county x transition-year crosswalk links
#'
#' One row per crosswalk link and county: a December plan in a county and
#' where the CMS Part C&D Plan Crosswalk sends it for January, plus
#' January-only rows for new plans and counties a plan adds. Load it with
#' `maexits_data("plan_county")`.
#'
#' @section Counting rows:
#' Rows are crosswalk links, not plan-counties. When a December plan has
#' several crosswalk rows (a split across successor plans, or a move into
#' another contract listed as a Terminated/Non-renewed Contract row plus
#' consolidation rows), its December enrollment appears on every one of
#' those rows; `prev_links` says how many. When several December plans
#' consolidate into one January plan, the successor's January enrollment
#' appears on each of their rows; `curr_links` says how many.
#'
#' For totals that count each plan-county once, sum
#' `dec_enrollment_once` / `jan_enrollment_once` (the value on one row, 0 on
#' the others) or `dec_enrollment_split` / `jan_enrollment_split` (the value
#' shared equally across the links). `forced_county` (with `forced_reason`)
#' says whether a plan-county's enrollees were forced out; the link-level
#' `forced` flags individual links.
#'
#' The plan universe is individual-market Medicare Advantage plus special
#' needs plans in the CMS landscape, on both the December and January side;
#' standalone drug plans, employer group plans and Medicare-Medicaid Plans
#' are excluded.
#'
#' @eval .rd_format("plan_county")
#' @seealso [maexits_data()], [maexits_catalog()], [county_panel]
#' @name plan_county
#' @docType data
#' @keywords datasets
NULL


#' Plan service areas by contract year
#'
#' One row per plan segment and county in the CMS landscape files (CY2016
#' on), individual-market MA plus SNPs. County names use the CMS enrollment
#' spelling. Load it with `maexits_data("landscape")`.
#'
#' @eval .rd_format("landscape")
#' @seealso [maexits_data()], [maexits_catalog()]
#' @name landscape
#' @docType data
#' @keywords datasets
NULL


#' December and January plan x county enrollment used by the pipeline
#'
#' CMS Monthly Enrollment by Contract/Plan/State/County aggregated to plan
#' and county, for the December and January files the pipeline uses. Load it
#' with `maexits_data("enrollment")`; for any other month use
#' [cms_enrollment()].
#'
#' @eval .rd_format("enrollment")
#' @seealso [maexits_data()], [cms_enrollment()]
#' @name enrollment
#' @docType data
#' @keywords datasets
NULL


#' CMS monthly enrollment for any month
#'
#' The table returned by [cms_enrollment()]: CMS Monthly Enrollment by
#' Contract/Plan/State/County for the requested months, joined to the
#' month's contract information. It covers every plan type, including
#' standalone drug plans (use `ma_only = TRUE` to drop them).
#'
#' @eval .rd_format("cms_enrollment")
#' @seealso [cms_enrollment()], [maexits_catalog()]
#' @name cms_enrollment_data
#' @aliases cms_enrollment_data
#' @docType data
#' @keywords datasets
NULL
