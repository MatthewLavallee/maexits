# ============================================================
# catalog.R — The variable key: one table describing every dataset and
# variable, used by maexits_catalog() and to generate the ?county_panel,
# ?plan_county, ?landscape, ?enrollment and ?cms_enrollment help pages, so
# the two never disagree.
#
# The `counting` column says whether a variable's values repeat across
# rows. plan_county is a table of crosswalk links: a prior plan-county with
# several links (a split, or a move into another contract) repeats its
# December enrollment on each, and a successor plan-county reached by
# several links (a consolidation) repeats its January enrollment. Sums of
# those columns are sums over links; the _once and _split columns count
# each plan-county once.
# ============================================================


.COUNT_DEC <- paste(
  "Repeats on every crosswalk link of the same prior plan-county (see `prev_links`).",
  "For totals that count each plan-county once, sum `dec_enrollment_once`",
  "or `dec_enrollment_split` (shared across its links).")
.COUNT_JAN <- paste(
  "Repeats on every link to the same successor plan-county (see `curr_links`).",
  "For totals that count each plan-county once, sum `jan_enrollment_once`",
  "or `jan_enrollment_split`.")
.COUNT_DEC_LOW <- paste(
  "Repeats like dec_enrollment; for totals that count each prior plan-county",
  "once, sum it over rows with `dec_first` TRUE.")
.COUNT_JAN_LOW <- paste(
  "Repeats like jan_enrollment; for totals that count each successor",
  "plan-county once, sum it over rows with `jan_first` TRUE.")
.COUNT_LINK_FLAG <- paste(
  "Link-level: one prior plan-county can have both forced and unforced links",
  "(e.g. a Terminated/Non-renewed Contract row next to consolidation rows into",
  "another contract). `forced_county` says whether the plan-county's enrollees",
  "were forced out.")
.COUNT_PANEL_SUM <- paste(
  "Sums over crosswalk links, so a plan-county with several links enters",
  "once per link. The matching `_once` column counts each plan-county once.")
.COUNT_PANEL_RATE <- paste(
  "Built from sums over crosswalk links (see `total_dec_enrollment`); the",
  "matching `_once` column counts each plan-county once.")


.COUNT_PLAN_REPEAT <- paste(
  "Plan-level: repeats on every county row of the plan and dec_year, so do not",
  "sum it across rows; summarise one row per plan instead.")
.COUNT_CONTRACT_REPEAT <- "Contract-level: repeats on every row of the contract and dec_year."
.COUNT_COUNTY_REPEAT <- paste(
  "County-level: repeats on every plan row of the county and dec_year, so do not",
  "sum it across rows; take one row per county.")


#' The variable key (internal table)
#' @return data.table with dataset, variable, description, counting.
#' @keywords internal
.catalog_table <- function() {
  v <- function(ds, var, desc, counting = "") {
    data.table(dataset = ds, variable = var, description = desc, counting = counting)
  }
  geo <- function(ds) rbind(
    v(ds, "county_name", "County name, spelled as in CMS enrollment files."),
    v(ds, "state_name", "State or territory name."),
    v(ds, "fips", "County FIPS code (NA where the county name has no CMS FIPS match)."))
  plan <- function(ds) rbind(
    v(ds, "contract_id", "CMS contract ID (H/R = MA, S = standalone drug plan, E = employer)."),
    v(ds, "plan_id", "Plan ID within the contract (integer)."),
    v(ds, "plan_key", "Contract and zero-padded plan, e.g. \"H1234-001\"."))

  # Plan characteristics shared by plan_details and displacement
  details <- function(ds) rbind(
    v(ds, "plan_name", "Plan name (CPSC Contract Info; landscape if missing)."),
    v(ds, "org_marketing_name", "Organization marketing name (CPSC Contract Info)."),
    v(ds, "org_legal_name", "Organization legal name (CPSC Contract Info)."),
    v(ds, "parent_org", "Parent organization (CPSC Contract Info, December of the contract year, else January)."),
    v(ds, "org_type", "CMS organization type: Local CCP, Regional CCP, PFFS, MSA, 1876 Cost, HCPP - 1833 Cost, Demo, National PACE."),
    v(ds, "plan_type_group", "Plan type with one vocabulary across years (CPSC Contract Info): HMO/HMOPOS, Local PPO, Regional PPO, PFFS, MSA, 1876 Cost."),
    v(ds, "contract_effective_date", "Date the contract took effect."),
    v(ds, "dsnp_integration", "D-SNP integration status: CO (coordination-only), HIDE or FIDE; from CY2023 (NA before, and for other plans)."),
    v(ds, "dsnp_aip", "D-SNP is an applicable integrated plan (AIP); from CY2023."),
    v(ds, "csnp_conditions", "C-SNP condition(s), separated by \"; \". CMS's wording differs from CY2025 and again in CY2026."),
    v(ds, "snp_institutional_type", "I-SNP type; from CY2025. CMS changed the labels in CY2026 (e.g. Institutional Only became Facility-based Institutional (FI-SNP))."),
    v(ds, "zero_dollar_dsnp", "D-SNP with zero-dollar Medicare cost sharing; from CY2025."),
    v(ds, "sanctioned", "Plan listed among CMS-sanctioned plans in the landscape."),
    v(ds, "part_d", "Plan offers Part D drug coverage."),
    v(ds, "premium_total", "Monthly premium, Part C plus Part D (dollars). For plans without Part D, the Part C premium. MSA plans: NA to CY2024 (blank in the landscape), 0 from CY2025."),
    v(ds, "premium_part_c", "Monthly Part C premium (dollars). CY2018-2024 from the Part D Plan and Premium report (plans with Part D; NA for the few Part D plans missing from it) or the consolidated premium (plans without Part D); from CY2025 the landscape."),
    v(ds, "premium_part_d_basic", "Monthly Part D basic premium (dollars); NA without Part D."),
    v(ds, "premium_part_d_supp", "Monthly Part D supplemental premium (dollars); NA without Part D."),
    v(ds, "premium_part_d_total", "Monthly Part D total premium (dollars); NA without Part D."),
    v(ds, "premium_part_d_lis", "Monthly Part D premium owed by an enrollee with the full low-income subsidy (dollars)."),
    v(ds, "part_d_deductible", "Annual Part D deductible (dollars); NA without Part D."),
    v(ds, "drug_benefit_type", "Part D benefit type: Defined Standard, Actuarially Equivalent, Basic Alternative or Enhanced Alternative."),
    v(ds, "gap_coverage", "Additional coverage in the Part D coverage gap; to CY2024 (the gap ended in 2025)."),
    v(ds, "moop_in_network", "In-network maximum out-of-pocket amount (dollars), as CMS reports it (a few plans report 0). NA for PFFS and MSA plans, for Cost plans without one, and for every SNP before CY2025, whose landscape files have no MOOP column."),
    v(ds, "star_overall", "Overall star rating (1-5, in halves). NA when not rated; see star_status. Blank for every plan in the registered CY2026 landscape file."),
    v(ds, "star_status", "Whether the plan is rated: rated, not enough data, plan too new, not applicable."),
    v(ds, "star_part_c", "Part C summary star rating; CY2024 for plans with Part D (from the Part D report), all plans from CY2025."),
    v(ds, "star_part_d", "Part D summary star rating; CY2024 for plans with Part D, all plans from CY2025."))

  rbind(
    # --- county_panel ---------------------------------------------------
    geo("county_panel"),
    v("county_panel", "dec_year", "December year of the transition (2025 = December 2025 to January 2026)."),
    v("county_panel", "total_dec_enrollment", "December MA enrollment in the county.", .COUNT_PANEL_SUM),
    v("county_panel", "total_jan_enrollment", "January MA enrollment in the county.", .COUNT_PANEL_SUM),
    v("county_panel", "displaced_enrollment", "December enrollment on forced links (terminated, or a service-area reduction that dropped the county).", .COUNT_PANEL_SUM),
    v("county_panel", "incumbent_dec", "December enrollment on continuing (incumbent) links.", .COUNT_PANEL_SUM),
    v("county_panel", "incumbent_jan", "January enrollment on continuing (incumbent) links.", .COUNT_PANEL_SUM),
    v("county_panel", "new_entrant_jan", "January enrollment of plans new to the county.", .COUNT_PANEL_SUM),
    v("county_panel", "n_plans_dec", "Distinct plans with a row in the county-year (all roles)."),
    v("county_panel", "n_exiting_plans", "Distinct plans with a forced link in the county.",
      paste("Link-level: includes plans whose only forced link is a Terminated/Non-renewed",
            "Contract row next to a move into another contract that serves the county.",
            "`n_exiting_plans_county` counts plans whose enrollees in the county were forced out.")),
    v("county_panel", "penetration_2018", "2018 MA penetration rate (share of Medicare eligibles in MA)."),
    v("county_panel", "eligibles_2018", "2018 Medicare eligibles in the county."),
    v("county_panel", "benchmark", "County MA benchmark for dec_year: monthly Parts A and B rate at 0 percent bonus (NBER ratebook)."),
    v("county_panel", "exit_rate", "displaced_enrollment / total_dec_enrollment.", .COUNT_PANEL_RATE),
    v("county_panel", "enrollment_change", "(total_jan_enrollment - total_dec_enrollment) / total_dec_enrollment.", .COUNT_PANEL_RATE),
    v("county_panel", "net_enrollment_change", "total_jan_enrollment - total_dec_enrollment.",
      paste("Built from sums over crosswalk links (see `total_dec_enrollment`);",
            "`total_jan_enrollment_once` minus `total_dec_enrollment_once` counts each plan-county once.")),
    v("county_panel", "incumbent_growth", "incumbent_jan - incumbent_dec.", .COUNT_PANEL_RATE),
    v("county_panel", "exit_group", "Exit exposure from exit_rate: No exits; Low (under 5 percent); Medium (5-15 percent); High (over 15 percent).", .COUNT_PANEL_RATE),
    v("county_panel", "pen_quartile", "Quartile of 2018 penetration across counties."),
    v("county_panel", "total_dec_enrollment_once", "December MA enrollment, each plan-county counted once."),
    v("county_panel", "total_dec_enrollment_once_low", "As total_dec_enrollment_once, counting each CMS-suppressed cell as 1 instead of 10 (lower bound)."),
    v("county_panel", "total_jan_enrollment_once", "January MA enrollment, each plan-county counted once."),
    v("county_panel", "total_jan_enrollment_once_low", "As total_jan_enrollment_once, suppressed cells as 1 (lower bound)."),
    v("county_panel", "displaced_enrollment_once", "December enrollment of plan-counties whose enrollees were forced out (forced_county), each counted once."),
    v("county_panel", "displaced_enrollment_once_low", "As displaced_enrollment_once, suppressed cells as 1 (lower bound)."),
    v("county_panel", "incumbent_dec_once", "December enrollment of plan-counties not forced out, each counted once."),
    v("county_panel", "incumbent_jan_once", "January enrollment of successor plan-counties that a December plan in the county continues into (curr_is_incumbent), each counted once."),
    v("county_panel", "new_entrant_jan_once", "All other January enrollment, each plan-county counted once: new plans, counties a plan adds, and the few successors that report January enrollment in a county where the January landscape does not list them."),
    v("county_panel", "n_exiting_plans_county", "Distinct plans whose plan-county enrollees were forced out (forced_county)."),
    v("county_panel", "exit_rate_once", "displaced_enrollment_once / total_dec_enrollment_once."),
    v("county_panel", "enrollment_change_once", "(total_jan_enrollment_once - total_dec_enrollment_once) / total_dec_enrollment_once."),
    v("county_panel", "incumbent_growth_once", "incumbent_jan_once - incumbent_dec_once."),
    v("county_panel", "exit_group_once", "Exit exposure from exit_rate_once, same bins as exit_group."),
    v("county_panel", "snp_dec_enrollment_once", "December enrollment in special needs plans (SNPs), each plan-county once."),
    v("county_panel", "snp_displaced_enrollment_once", "SNP December enrollment forced out (forced_county), each plan-county once."),

    # --- plan_county ----------------------------------------------------
    v("plan_county", "dec_year", "December year of the transition."),
    plan("plan_county"),
    v("plan_county", "segment_id", "Plan segment (0 if unsegmented)."),
    geo("plan_county"),
    v("plan_county", "status", "CMS crosswalk status of this link (Renewal Plan, Consolidated Renewal Plan, Renewal Plan with SAR or SAE, Terminated Plan, Terminated/Non-renewed Contract, New Plan, Initial Contract)."),
    v("plan_county", "curr_contract_id", "Successor contract in January (NA for terminated links)."),
    v("plan_county", "curr_plan_id", "Successor plan in January (NA for terminated links)."),
    v("plan_county", "curr_plan_key", "Successor contract and zero-padded plan."),
    v("plan_county", "dec_enrollment", "December enrollment of the prior plan in the county: reported count, 10 per CMS-suppressed cell, 0 with no CMS record. NA on January-only rows.", .COUNT_DEC),
    v("plan_county", "dec_enrollment_low", "As dec_enrollment with suppressed cells as 1 (lower bound).", .COUNT_DEC_LOW),
    v("plan_county", "dec_src", "Source of dec_enrollment: reported, suppressed, mixed (both), or no_record."),
    v("plan_county", "prev_links", "Number of rows (links) sharing this row's prior plan-county."),
    v("plan_county", "dec_first", "TRUE on the one row per prior plan-county that carries its December value in dec_enrollment_once."),
    v("plan_county", "dec_enrollment_once", "dec_enrollment on one row per prior plan-county, 0 on its other links: sums count each plan-county once."),
    v("plan_county", "dec_enrollment_split", "dec_enrollment divided equally across the prior plan-county's links: sums count each plan-county once."),
    v("plan_county", "jan_enrollment", "January enrollment of the successor plan in the county (same rules as dec_enrollment); 0 for terminated links and dropped counties.", .COUNT_JAN),
    v("plan_county", "jan_enrollment_low", "As jan_enrollment with suppressed cells as 1 (lower bound).", .COUNT_JAN_LOW),
    v("plan_county", "jan_src", "Source of jan_enrollment: reported, suppressed, mixed, no_record, terminated, or dropped_county."),
    v("plan_county", "curr_links", "Number of rows (links) sharing this row's successor plan-county."),
    v("plan_county", "jan_first", "TRUE on the one row per successor plan-county that carries its January value in jan_enrollment_once."),
    v("plan_county", "jan_enrollment_once", "jan_enrollment on one row per successor plan-county, 0 on its other links."),
    v("plan_county", "jan_enrollment_split", "jan_enrollment divided equally across the successor plan-county's links."),
    v("plan_county", "curr_is_incumbent", "TRUE when a December plan in the county continues into this successor plan-county (a renewal, consolidation, SAR or SAE link whose successor serves the county in January); FALSE otherwise. NA on terminated and dropped-county links, which carry no January enrollment."),
    v("plan_county", "successor_serves", "TRUE when the January landscape lists the successor plan in this county."),
    v("plan_county", "sar_dropped", "Service-area-reduction link whose successor no longer serves the county."),
    v("plan_county", "forced", "Link-level exit flag: a terminated link, or sar_dropped.", .COUNT_LINK_FLAG),
    v("plan_county", "role", "Link-level role: exiting, incumbent, or new_entrant.", .COUNT_LINK_FLAG),
    v("plan_county", "forced_county", "Plan-county exit flag: TRUE when no continuing successor of the prior plan serves the county in January, so its enrollees had to leave the plan. Same value on every link of the plan-county."),
    v("plan_county", "forced_reason", "Why forced_county is TRUE: terminated (every link terminated); service_area_reduction; successor_drops_county (moved to another plan or contract that does not serve the county); renewal_not_in_landscape (same plan renewed but not listed in the county in January; some of these still show January enrollment)."),
    v("plan_county", "multi_status", "TRUE when this plan-segment-county appears on more than one row."),
    v("plan_county", "plan_type", "Plan type as labelled by CMS that year."),
    v("plan_county", "snp", "Special needs plan: Yes or No."),
    v("plan_county", "snp_type", "SNP type: Dual-Eligible, Chronic or Disabling Condition, or Institutional."),
    v("plan_county", "dsnp_integration", "D-SNP integration status (from CY2025): FIDE, HIDE, CO (coordination-only), or Not Applicable; NA before 2025."),
    v("plan_county", "eligibles_2018", "2018 Medicare eligibles in the county."),
    v("plan_county", "enrolled_2018", "2018 MA enrollees in the county."),
    v("plan_county", "penetration_2018", "2018 MA penetration rate."),
    v("plan_county", "benchmark", "County MA benchmark for dec_year."),

    # --- displacement ----------------------------------------------------
    v("displacement", "dec_year", "December year: the December before the January the outcome refers to."),
    plan("displacement"),
    v("displacement", "segment_id", "Plan segment serving the county (each plan-county has one)."),
    geo("displacement"),
    v("displacement", "dec_enrollment", "December enrollment of the plan in the county: reported count, 10 per CMS-suppressed cell, 0 with no CMS record (2,068 plan-counties). Each plan-county appears once, so sums count everyone once."),
    v("displacement", "dec_enrollment_low", "As dec_enrollment with suppressed cells as 1 (lower bound)."),
    v("displacement", "dec_n_suppressed", "Number of CMS-suppressed cells (1-10 enrollees each) in the plan-county."),
    v("displacement", "dec_src", "Source of dec_enrollment: reported, suppressed, mixed, or no_record."),
    v("displacement", "lost_coverage", "TRUE when no plan continuing this plan-county serves the county in January, so its enrollees lost their plan (same as forced_county in plan_county)."),
    v("displacement", "outcome", paste(
      "What happened to the plan-county in January. Continued:",
      "renewed; renewed_sae (renewed, expanded elsewhere); renewed_sar_kept_county (service area",
      "cut elsewhere); consolidated_same_plan_id (absorbed other plans); moved_plan_same_contract;",
      "moved_contract; moved_new_plan. Lost coverage: terminated_contract_listed (plan terminated,",
      "the contract still offers plans in January); terminated_contract_gone (the contract offers",
      "none); new_plan_not_in_county; sar_dropped_county (the plan continues but dropped the",
      "county); sar_successor_not_in_county; moved_plan_not_in_county; moved_contract_not_in_county",
      "(moved to a plan that does not serve the county); renewal_not_listed_in_county (renewed, but",
      "not listed in the county in the January landscape; the least certain category: for about",
      "69% of these enrollees the plan still reports January enrollment in the county).")),
    v("displacement", "outcome_group", "continued or lost_coverage."),
    v("displacement", "forced_reason", "Coarser reason for lost coverage: terminated, service_area_reduction, successor_drops_county, renewal_not_in_landscape; NA when coverage continued."),
    v("displacement", "xwalk_statuses", "The CMS crosswalk statuses of the plan-county's links, joined with \" + \". From the 2024 crosswalk on, CMS labels every termination \"Terminated/Non-renewed Contract\", whether or not the contract leaves."),
    v("displacement", "n_links", "Number of crosswalk links of the plan-county (several when CMS splits a plan or moves it into another contract)."),
    v("displacement", "n_successors", "Number of distinct January plans the crosswalk maps the plan-county to."),
    v("displacement", "successor_contract_id", "Contract of the January plan that continues the plan-county; NA when coverage was lost. When CMS splits the plan across several plans (n_successors > 1), the one with the most January enrollment in the county."),
    v("displacement", "successor_plan_id", "Plan of the January plan that continues the plan-county; NA when coverage was lost."),
    v("displacement", "successor_plan_key", "Contract and zero-padded plan of the successor."),
    v("displacement", "successor_segment_id", "Segment of the successor plan in the county in the January landscape (join January plan details on it)."),
    v("displacement", "plan_exits", "TRUE when the plan lost coverage in every county it served in December.", .COUNT_PLAN_REPEAT),
    v("displacement", "plan_n_counties", "Number of counties the plan served in December.", .COUNT_PLAN_REPEAT),
    v("displacement", "plan_n_counties_lost", "Number of the plan's counties with lost coverage.", .COUNT_PLAN_REPEAT),
    v("displacement", "plan_dec_enrollment", "The plan's December enrollment across all its counties.", .COUNT_PLAN_REPEAT),
    v("displacement", "plan_share_lost", "Share of the plan's December enrollment that lost coverage.", .COUNT_PLAN_REPEAT),
    v("displacement", "contract_exits", "TRUE when the contract offers no individual MA or SNP plan anywhere in the January landscape.", .COUNT_CONTRACT_REPEAT),
    v("displacement", "contract_all_lost", "TRUE when every December plan-county of the contract lost coverage.", .COUNT_CONTRACT_REPEAT),
    v("displacement", "contract_exits_county", "TRUE when the contract offers no plan in this county in January."),
    v("displacement", "parent_exits_county", "TRUE when no contract of the same parent organization offers a plan in this county in January; NA when the parent is unknown. Parents are matched on their December and January names, so renames still match."),
    v("displacement", "parent_nonsnp_in_county_jan", "TRUE when the same parent offers a non-SNP, non-Cost plan in the county in January."),
    v("displacement", "n_plans_jan", "Plans (non-SNP, non-Cost) offered in the county the next January: the alternatives.", .COUNT_COUNTY_REPEAT),
    v("displacement", "n_plans_jan_other_parent", "As n_plans_jan, excluding plans of the same parent organization; NA when the parent is unknown."),
    v("displacement", "n_parents_jan", "Parent organizations offering the n_plans_jan plans (a contract with no Contract Info parent counts as its own).", .COUNT_COUNTY_REPEAT),
    v("displacement", "n_same_snp_type_jan", "SNP rows: SNPs of the same type (dual-eligible, chronic condition, institutional) offered in the county next January; NA for other plans."),
    v("displacement", "n_plans_jan_all", "Every MA and SNP plan (including Cost plans) in the county next January.", .COUNT_COUNTY_REPEAT),
    v("displacement", "n_plans_dec_county", "Plans (non-SNP, non-Cost) offered in the county in December.", .COUNT_COUNTY_REPEAT),
    v("displacement", "n_parents_dec_county", "Parent organizations offering the n_plans_dec_county plans.", .COUNT_COUNTY_REPEAT),
    v("displacement", "county_dec_enrollment", "December enrollment of all plans in the county (equals county_panel total_dec_enrollment_once).", .COUNT_COUNTY_REPEAT),
    v("displacement", "county_lost_enrollment", "December enrollment in the county that lost coverage (equals county_panel displaced_enrollment_once).", .COUNT_COUNTY_REPEAT),
    v("displacement", "county_lost_share", "county_lost_enrollment / county_dec_enrollment.", .COUNT_COUNTY_REPEAT),
    v("displacement", "penetration_2018", "2018 MA penetration rate in the county.", .COUNT_COUNTY_REPEAT),
    v("displacement", "eligibles_2018", "2018 Medicare eligibles in the county.", .COUNT_COUNTY_REPEAT),
    v("displacement", "enrolled_2018", "2018 MA enrollees in the county.", .COUNT_COUNTY_REPEAT),
    v("displacement", "benchmark", "County MA benchmark for dec_year.", .COUNT_COUNTY_REPEAT),
    v("displacement", "plan_type", "Plan type as labelled by CMS in the December landscape."),
    v("displacement", "snp", "Special needs plan: Yes or No."),
    v("displacement", "snp_type", "SNP type: Dual-Eligible, Chronic or Disabling Condition, or Institutional."),
    details("displacement"),

    # --- exits -------------------------------------------------------------
    v("exits", "dec_year", "December year of the transition (2026 = December 2026 to January 2027)."),
    plan("exits"),
    v("exits", "segment_id", "Plan segment in the county (the lowest, if the plan has several there)."),
    geo("exits"),
    v("exits", "plan_type", "Plan type (landscape)."),
    v("exits", "snp", "Special needs plan: Yes or No."),
    v("exits", "snp_type", "SNP type: Dual-Eligible, Chronic or Disabling Condition, or Institutional; NA for other plans."),
    v("exits", "exit_type", paste(
      "What CMS's crosswalk does to the plan in this county for the next January:",
      "terminated (every crosswalk link of the plan-county is a termination),",
      "service_area_reduction (the plan renews with a service-area reduction and neither it",
      "nor any other plan it maps to serves the county), or none. none includes plan-counties",
      "moved to a plan that does not serve the county or renewed without listing in the county,",
      "which displacement counts as having lost coverage.")),
    v("exits", "xwalk_statuses", "The CMS crosswalk statuses of the plan-county's links, joined with \" + \"."),
    v("exits", "dec_enrollment", "December enrollment of the plan in the county: reported count, 10 per CMS-suppressed cell, 0 with no CMS record. NA for the newest dec_year until December enrollment is out. Each plan-county appears once, so sums count everyone once."),
    v("exits", "dec_enrollment_low", "As dec_enrollment with suppressed cells as 1 (lower bound)."),
    v("exits", "dec_src", "Source of dec_enrollment: reported, suppressed, mixed, or no_record."),
    v("exits", "sep_enrollment", "September enrollment of the plan in the county (same rules as dec_enrollment), every dec_year. Use it to compare the newest transition with earlier ones before its December enrollment is out."),
    v("exits", "sep_enrollment_low", "As sep_enrollment with suppressed cells as 1 (lower bound)."),
    v("exits", "sep_src", "Source of sep_enrollment: reported, suppressed, mixed, or no_record."),

    # --- plan_details ----------------------------------------------------
    v("plan_details", "year", "Contract (plan) year."),
    plan("plan_details"),
    v("plan_details", "segment_id", "Plan segment (0 if unsegmented)."),
    v("plan_details", "plan_type", "Plan type as labelled by CMS that year."),
    v("plan_details", "snp", "Special needs plan: Yes or No."),
    v("plan_details", "snp_type", "SNP type."),
    details("plan_details"),

    # --- landscape -------------------------------------------------------
    v("landscape", "year", "Contract (plan) year."),
    plan("landscape"),
    v("landscape", "segment_id", "Plan segment (0 if unsegmented)."),
    geo("landscape"),
    v("landscape", "plan_type", "Plan type as labelled by CMS that year."),
    v("landscape", "snp", "Special needs plan: Yes or No."),
    v("landscape", "snp_type", "SNP type."),
    v("landscape", "dsnp_integration", "D-SNP integration status (from CY2025): FIDE, HIDE, CO, or Not Applicable."),

    # --- enrollment -------------------------------------------------------
    v("enrollment", "month", "Month of the CMS file, \"YYYY-MM\" (December 2018-2025, January 2018-2026)."),
    v("enrollment", "year", "Calendar year of the month."),
    plan("enrollment"),
    geo("enrollment"),
    v("enrollment", "county_enrollment", "Sum of the reported CMS counts for the plan and county."),
    v("enrollment", "n_suppressed", "Number of CMS-suppressed cells for the plan and county (each 1-10 enrollees; not included in county_enrollment)."),

    # --- cms_enrollment ------------------------------------------------------
    v("cms_enrollment", "month", "Month requested, \"YYYY-MM\"."),
    plan("cms_enrollment"),
    v("cms_enrollment", "state", "State postal abbreviation."),
    v("cms_enrollment", "state_name", "State or territory name."),
    v("cms_enrollment", "county", "County name as spelled by CMS."),
    v("cms_enrollment", "fips", "County FIPS code."),
    v("cms_enrollment", "ssa_code", "County SSA code (5 digits)."),
    v("cms_enrollment", "enrollment", "Enrollment (NA when CMS suppresses a count below 11)."),
    v("cms_enrollment", "suppressed", "TRUE when CMS suppressed the count (fewer than 11 enrollees)."),
    v("cms_enrollment", "organization_type", "CMS contract organization type."),
    v("cms_enrollment", "plan_type", "Plan type (HMO/HMOPOS, Local PPO, Regional PPO, PFFS, Medicare Prescription Drug Plan, ...)."),
    v("cms_enrollment", "offers_part_d", "Plan offers Part D: Yes or No."),
    v("cms_enrollment", "snp_plan", "Special needs plan: Yes or No."),
    v("cms_enrollment", "eghp", "Employer group health plan: Yes or No."),
    v("cms_enrollment", "organization_name", "Legal organization name."),
    v("cms_enrollment", "organization_marketing_name", "Marketing name."),
    v("cms_enrollment", "plan_name", "Plan name."),
    v("cms_enrollment", "parent_organization", "Parent organization."),
    v("cms_enrollment", "contract_effective_date", "Contract effective date.")
  )
}


#' List Datasets and Variables
#'
#' The variable key: every dataset the package can load, a description of
#' each variable, and a `counting` note for variables whose values repeat
#' across rows (see the "Counting rows" section of [plan_county]). Datasets
#' "county_panel", "plan_county", "displacement", "plan_details",
#' "landscape" and "enrollment" are loaded with [maexits_data()];
#' "cms_enrollment" with [cms_enrollment()]. The same information is in the
#' help pages `?county_panel`, `?plan_county`, `?displacement`,
#' `?plan_details`, `?landscape`, `?enrollment` and `?cms_enrollment`.
#'
#' @param dataset Optional dataset name to show only its variables.
#' @return A data.table with columns dataset, variable, description, counting.
#' @examples
#' maexits_catalog()
#' maexits_catalog("plan_county")[counting != ""]
#' @export
maexits_catalog <- function(dataset = NULL) {
  out <- .catalog_table()
  if (!is.null(dataset)) {
    wanted <- dataset
    if (!wanted %in% out$dataset) {
      .api_stop("unknown dataset '%s'; choose one of %s", wanted,
                paste(unique(out$dataset), collapse = ", "))
    }
    out <- out[out$dataset == wanted]
  }
  out[]
}


#' Roxygen @format block for a dataset, generated from the variable key
#' @keywords internal
.rd_format <- function(ds) {
  x <- .catalog_table()
  x <- x[x$dataset == ds]
  items <- sprintf("* `%s`: %s%s", x$variable, x$description,
                   ifelse(x$counting == "", "", paste0(" *Counting:* ", x$counting)))
  c("@format A data.table with these columns:", "", items)
}
