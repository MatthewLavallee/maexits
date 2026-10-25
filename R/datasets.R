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
#' @section Spring enrollment:
#' The `feb`, `mar` and `apr` columns give the same January plans'
#' enrollment in February, March and April of the January year, through
#' the Medicare Advantage Open Enrollment Period (January-March, changes
#' effective the first of the next month); April is the first month after
#' it. They are NA for a transition until CMS posts that month.
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
#' The `feb_`, `mar_` and `apr_` enrollment columns follow the January
#' rules for February, March and April of the January year (NA until CMS
#' posts the month), with the same `_once` and `_split` versions.
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
#' @section Benchmarks after January:
#' `successor_jan_enrollment` through `successor_apr_enrollment` give the
#' enrollment of the plan's successor in the county in January-April of the
#' January year, and `county_jan_enrollment` through
#' `county_apr_enrollment` the county's MA enrollment in the January plans,
#' to follow the county after the transition and the MA Open Enrollment
#' Period. Months CMS has not posted are NA.
#'
#' @section Coverage gaps:
#' Plan details come from [plan_info]. The landscape files give no
#' out-of-pocket maximum for SNPs before CY2025 (`moop_in_network` NA),
#' Part C and Part D star ratings start in CY2024 (plans with Part D only
#' in CY2024), and D-SNP integration status starts in CY2023. The county
#' benchmark for the January year is `benchmark` on the next `dec_year`'s
#' rows (or in [county_panel]). The universe excludes employer group plans,
#' PACE, Medicare-Medicaid Plans and enrollees living outside a plan's
#' listed service area (about 3-6% of CPSC December MA enrollment).
#'
#' @eval .rd_format("displacement")
#' @seealso [maexits_data()], [plan_info], [county_panel]
#' @name displacement
#' @docType data
#' @keywords datasets
NULL


#' December plan x county: terminated or cut from the county the next January
#'
#' One row per December plan and county (`dec_year`, contract, plan,
#' county) for every individual-market MA plan and SNP the December
#' landscape and the crosswalk list, with whether CMS's crosswalk
#' terminated the plan or cut the county from its service area for the
#' next January (`exit_type`), and the plan-county's December enrollment.
#' It covers the transitions with December enrollment
#' (`MAEXITS_XWALK_YEARS`); [exit_sept] gives the same plan-counties on
#' September enrollment, including the newest transition. Built by
#' [make_exits()]. Load it with `maexits_data("exits")`.
#'
#' @section Counting:
#' Each plan-county appears once, so sums count every enrollee once. The
#' share of December enrollment in plans CMS terminated or cut from the
#' county is
#' `sum(dec_enrollment[exit_type != "none"]) / sum(dec_enrollment)`, and
#' `exit_type` splits it into terminations and service-area reductions.
#'
#' @section What counts as an exit:
#' `exit_type` uses only CMS's crosswalk labels, checked against the
#' January landscape for the county: `terminated` when every crosswalk link
#' of the plan-county is a termination, `service_area_reduction` when the
#' plan renews with a service-area reduction and no plan it maps to serves
#' the county. [displacement] also counts as lost coverage the
#' plan-counties CMS moved to a plan that does not serve the county (by
#' consolidation or a New Plan link), or renewed without listing in the
#' county in January (together 0.03-0.31% of December enrollment a year);
#' here they are `none`.
#'
#' @eval .rd_format("exits")
#' @seealso [maexits_data()], [exit_sept], [displacement], [plan_info]
#' @name exits
#' @docType data
#' @keywords datasets
NULL


#' Plan exits on September enrollment, including the newest transition
#'
#' The plan-counties and `exit_type` of [exits], with the plan-county's
#' enrollment in September of `dec_year` instead of December. It covers
#' every transition in `MAEXITS_EXITS_YEARS`, which adds the newest one
#' once its crosswalk is registered (early October), about two months
#' before its December enrollment comes out. Use it to compare the newest
#' transition with earlier years on the same basis. Built by
#' [make_exit_sept()]. Load it with `maexits_data("exit_sept")`.
#'
#' @section Counting:
#' Each plan-county appears once. The share of September enrollment in
#' plans CMS terminated or cut from the county is
#' `sum(sep_enrollment[exit_type != "none"]) / sum(sep_enrollment)`.
#' September gives shares 0.01-0.28 points higher than December
#' (2018-2025).
#'
#' @eval .rd_format("exit_sept")
#' @seealso [maexits_data()], [exits]
#' @name exit_sept
#' @docType data
#' @keywords datasets
NULL


#' Standalone Part D plans (PDPs) CMS terminated for the next January
#'
#' One row per December standalone prescription drug plan (PDP) and state
#' (`dec_year`, contract, plan, state) for every individual-market PDP the
#' December PDP landscape lists (S contracts, plan IDs below 800), with
#' whether CMS terminated the plan for the next January (`exit_type`) and
#' the plan's enrollment in the state in December and in September of
#' `dec_year`. Built by [make_pdp_exits()]. Load it with
#' `maexits_data("pdp_exits")`; filter it by `states`.
#'
#' @section Counting:
#' Each plan-state appears once, so sums count every enrollee once. The
#' share of PDP enrollment in terminated plans is
#' `sum(sep_enrollment[exit_type == "terminated"]) / sum(sep_enrollment)`
#' (or with `dec_enrollment` for the years it covers). December and
#' September give shares within 0.05 points of each other (2018-2025).
#'
#' @section What counts as an exit:
#' PDPs are offered region-wide, and CMS's crosswalk has no service-area
#' reductions for individual PDPs: a plan leaving a region is terminated
#' or consolidated into another plan. `exit_type` is `terminated` when
#' every crosswalk link of the plan is a termination and no plan it maps to
#' is offered in the state, and for the contracts CMS terminated outside the
#' crosswalk (`cms_terminated`; `MAEXITS_CMS_TERMINATED_CONTRACTS` in
#' R/config.R): Clear Spring Health (S6946), terminated by CMS for 2025
#' while the 2025 crosswalk lists 30 of its 33 plans as renewing (333,633
#' of its 336,213 September 2024 enrollees in the table). Consolidations
#' are `none`. CMS reassigns low-income subsidy enrollees of terminated
#' PDPs to another plan.
#'
#' @section Universe:
#' Enrollment is counted in the states where the December landscape offers
#' the plan; PDP enrollees living elsewhere and employer plans are left
#' out. Those enrollees are almost all in CMS-suppressed cells: 5-9% of CPSC
#' PDP enrollment in 2018-2025 and under 5% in 2026 with each suppressed
#' cell counted as 10, about 1% with each counted as 1.
#'
#' @eval .rd_format("pdp_exits")
#' @seealso [maexits_data()], [exits], [exit_sept]
#' @name pdp_exits
#' @docType data
#' @keywords datasets
NULL


#' Plan characteristics and supplemental benefits by contract year
#'
#' One row per contract year, contract, plan and segment for the
#' individual-market MA and SNP plans in the CMS landscape files, CY2018 on:
#' organization and parent, plan type, premiums, Part D deductible and
#' benefit type, in-network out-of-pocket maximum, star ratings and SNP
#' details, plus the Plan Benefit Package (PBP) benefits: whether the plan
#' reduces enrollees' Part B premium (the "giveback") and by how much, and
#' whether it covers dental, eye exams and eyewear, hearing exams and
#' hearing aids, an over-the-counter (OTC) allowance (with its annual
#' maximum), meals, transportation, worldwide emergency care, fitness,
#' in-home support, and food and produce. Built by [make_plan_info()]. Load
#' it with `maexits_data("plan_info")`; join it to other tables on `year`
#' (= `dec_year` for December plans), `contract_id`, `plan_id` and
#' `segment_id`. Money amounts are monthly dollars except deductibles and
#' the out-of-pocket maximum (annual).
#'
#' @section Sources:
#' Plan details come from the landscape files, the Part D Plan and Premium
#' reports (CY2018-CY2024) and CPSC Contract Info. Benefits come from the
#' PBP Benefits files, one registered quarterly vintage a year
#' (`MAEXITS_PBP_FILES`); a benefit flag is TRUE when the plan's bid answers
#' "Yes" to covering it (for dental from CY2025, when any preventive (16b)
#' or comprehensive (16c) dental service is a mandatory supplemental
#' benefit). The OTC maximum is annualized from the plan's period; plans
#' with no plan-level maximum have `NA`. The few plans without a PBP record
#' (under 0.2% a year) have `NA` benefit columns.
#'
#' @eval .rd_format("plan_info")
#' @seealso [maexits_data()], [displacement], [exits]
#' @name plan_info
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
