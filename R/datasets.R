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


#' December plan x county: who lost their plan in January
#'
#' One row per December plan and county (`dec_year`, contract, plan,
#' county), for every individual-market MA plan and SNP the December
#' landscape and the crosswalk list, 2018-2025 (including 2,068
#' plan-counties with no CMS enrollment record, which have 0 enrollment). Each row says what happened to the plan in that
#' county the next January (`outcome`, `lost_coverage`), how far the exit
#' reached (the whole plan, the contract, the parent organization in the
#' county), what the county offers next January, and the plan's details.
#' Load it with `maexits_data("displacement")`.
#'
#' @section Counting people who lost their plan:
#' Each plan-county appears once, so `sum(dec_enrollment)` counts every
#' December enrollee once. The share of December enrollment that lost
#' coverage in a year is
#' `sum(dec_enrollment[lost_coverage]) / sum(dec_enrollment)`, and the
#' `outcome` splits that total by type. `dec_enrollment_low` gives the
#' lower bound when CMS-suppressed counts (1-10 enrollees) are counted as 1
#' instead of 10.
#'
#' Plan-, contract- and county-level columns (`plan_*`, `contract_exits`,
#' `n_plans_*`, `county_*`, 2018 county values, `benchmark`) repeat on
#' every row of their plan or county; the `counting` column of
#' [maexits_catalog()] flags them.
#'
#' @section Outcomes:
#' A plan-county's enrollees keep coverage when some January plan the
#' crosswalk links it to serves the county: the same plan renewed
#' (`renewed`, `renewed_sae`, `renewed_sar_kept_county`,
#' `consolidated_same_plan_id`), or another plan of the same or another
#' contract (`moved_plan_same_contract`, `moved_contract`,
#' `moved_new_plan`). Otherwise they lost it (`lost_coverage` TRUE):
#' * `terminated_contract_listed`, `terminated_contract_gone`: the plan
#'   was terminated; split by whether the contract still offers any plan in
#'   January. From the 2024 crosswalk on, CMS labels every termination
#'   "Terminated/Non-renewed Contract" (kept in `xwalk_statuses`), so the
#'   split uses the January landscape instead of the label.
#' * `sar_dropped_county`: the plan continues but dropped this county.
#' * `moved_plan_not_in_county`, `moved_contract_not_in_county`,
#'   `sar_successor_not_in_county`, `new_plan_not_in_county`: enrollees were
#'   moved to a plan that does not serve the county.
#' * `renewal_not_listed_in_county`: the plan renewed but the January
#'   landscape does not list it in the county. For about 69% of these
#'   enrollees the plan still reports January enrollment in the county, so
#'   this is the least certain category; drop it through `outcome` if
#'   needed.
#'
#' `lost_coverage` equals `forced_county` in [plan_county], and the
#' county totals equal the `_once` columns of [county_panel].
#'
#' @section Coverage gaps:
#' Plan details come from [plan_details]. The landscape files give no
#' out-of-pocket maximum for SNPs before CY2025 (`moop_in_network` NA),
#' Part C and Part D star ratings start in CY2024 (plans with Part D only
#' in CY2024), and D-SNP integration status starts in CY2023. The county
#' benchmark for the January year is `benchmark` on the next `dec_year`'s
#' rows (or in [county_panel]). The universe excludes employer group plans,
#' PACE, Medicare-Medicaid Plans and enrollees living outside a plan's
#' listed service area (about 3-6% of CPSC December MA enrollment).
#'
#' @eval .rd_format("displacement")
#' @seealso [maexits_data()], [plan_details], [county_panel]
#' @name displacement
#' @docType data
#' @keywords datasets
NULL


#' Plan details by contract year
#'
#' One row per contract year, contract, plan and segment for the
#' individual-market MA and SNP plans in the CMS landscape files, CY2018 on:
#' organization and parent, plan type, premiums, Part D deductible and
#' benefit type, in-network out-of-pocket maximum, star ratings and SNP
#' details. Built by [make_plan_details()] from the landscape files, the
#' Part D Plan and Premium reports (CY2018-CY2024) and CPSC Contract Info.
#' Load it with `maexits_data("plan_details")`; join it to other tables on
#' `year` (= `dec_year` for December plans), `contract_id`, `plan_id` and
#' `segment_id`. Money amounts are monthly dollars except deductibles and
#' the out-of-pocket maximum (annual).
#'
#' @eval .rd_format("plan_details")
#' @seealso [maexits_data()], [displacement]
#' @name plan_details
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
