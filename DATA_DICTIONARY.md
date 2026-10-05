# Data Dictionary

The package loads these tables with `maexits_data()`. Each has a help page listing every column (`?county_panel`, `?plan_county`, `?displacement`, `?exits`, `?plan_details`, `?landscape`, `?enrollment`), and `maexits_catalog()` returns the same list as a table. This file gives more detail on how each column is built. `plan_county` is `analytictable_augmented.csv` with `plan_key` and `curr_plan_key` added.

## Rows are crosswalk links

`analytictable_augmented.csv` has one row per **crosswalk link** and county, not one row per plan-county. The CMS Plan Crosswalk can list a December plan more than once:

- a **split**: one December plan is mapped to several January plans;
- a **move into another contract**: a `Terminated/Non-renewed Contract` row for the old contract next to `Consolidated Renewal Plan` rows into the new one;
- a **consolidation**: several December plans are mapped to one January plan.

Enrollment is attached per plan-county, so a December plan-county with several links carries its December enrollment on each of them (`prev_links` > 1). A January plan-county reached by several links carries its January enrollment on each of them (`curr_links` > 1). A sum of `dec_enrollment` or `jan_enrollment` over rows is therefore a sum over links. For plan-county measures, use the counting columns:

| To get | Use |
|---|---|
| December or January totals, each plan-county once | `sum(dec_enrollment_once)`, `sum(jan_enrollment_once)`. Each value sits on one row (`dec_first` / `jan_first`); the other links carry 0. |
| Enrollment shared across links (for link-level breakdowns that still add up) | `dec_enrollment_split`, `jan_enrollment_split`: the value divided equally across the plan-county's links. |
| Whether a plan-county's enrollees were forced out | `forced_county` (the same on every link of the plan-county), with `forced_reason`. The link-level `forced` also flags the companion termination row of a move into another contract, even when the new contract serves the county. |
| Lower bounds for CMS-suppressed counts | `dec_enrollment_low`, `jan_enrollment_low` (suppressed cells counted as 1 instead of 10); sum them over rows with `dec_first` / `jan_first`. |
| County totals | the `_once` columns of `county_panel.csv`. |

The `_once` December total equals CMS's December enrollment for the same plans and counties. The link-level columns support link-level analyses (e.g. enrollment by crosswalk status).

## Plan universe

Individual-market Medicare Advantage plans and special needs plans (SNPs) listed in the CMS landscape files, on both the December and January side of each transition. Standalone drug plans (PDP) and Medicare-Medicaid Plans (MMP) are dropped from the landscape. Employer group (800-series) plans are not in the landscape. County names are harmonized to the CMS enrollment (CPSC) spelling (`MAEXITS_COUNTY_NAME_MAP` in `R/config.R`). A "(Partial)" county label is dropped when the plan also lists the whole county; otherwise the suffix is removed.

## Enrollment values

CMS enrollment files report a count for each plan-county-cell and suppress counts of 1-10 (shown as `*`). Each plan-county's enrollment is the sum of its reported counts plus 10 for each suppressed cell. The `_low` columns count suppressed cells as 1 instead. A plan-county with no CMS record gets 0. `dec_src` and `jan_src` record which case applies.

## analytictable_augmented.csv (`plan_county`)

Built by `make_analytic()` → `add_fips_and_penetration()` → `add_benchmark()` → `augment_analytic()` in `R/data_build.R` and `R/data_augment.R`. Covers `dec_year` 2018-2025 (crosswalk years 2019-2026). A transition runs from December of `dec_year` to January of `dec_year + 1`.

Rows come in three kinds:
- **December rows**: a December landscape plan-county joined to each of its crosswalk links.
- **New-plan rows**: crosswalk `New Plan` / `Initial Contract` plans in the counties of their January landscape service area.
- **January-only rows**: counties in a continuing successor's January service area that no December row or new-plan row reaches. These are service-area expansions, counties a consolidation or renewal successor picks up, and successors whose predecessor is outside the December universe.

New-plan and January-only rows have no December values (`dec_src` NA) and use the January plan's IDs in `contract_id` / `plan_id`.

| Column | Type | Definition | Source / derivation |
|---|---|---|---|
| `fips` | integer | 5-digit county FIPS code. | `add_fips()`: county and state name matched to the CPSC file in `MAEXITS_FIPS_LOOKUP_FILE`. NA when the name has no match. |
| `dec_year` | integer | December year of the transition. | `crosswalk year − 1`. |
| `county_name`, `state_name` | character | County and state, in CPSC spelling. | Landscape, harmonized (see Plan universe). |
| `contract_id`, `plan_id` | character, integer | December plan (crosswalk previous side). On new-plan and January-only rows, the January plan. | Landscape joined to crosswalk `PREVIOUS_CONTRACT_ID` / `PREVIOUS_PLAN_ID`. |
| `segment_id` | integer | Plan segment (0 if unsegmented). | Landscape. |
| `status` | character | Crosswalk status of this link. | Crosswalk `DESCRIPTION` (before 2022) or `STATUS`. Each status has a class in `MAEXITS_XWALK_STATUS_CLASS`: continuing (`Renewal Plan`, `Consolidated Renewal Plan`), service-area reduction (`Renewal Plan with SAR`), service-area expansion (`Renewal Plan with SAE`), terminated (`Terminated Plan`, `Terminated/Non-renewed Contract`), new (`New Plan`, `Initial Contract`). |
| `curr_contract_id`, `curr_plan_id` | character, integer | The January successor plan of this link. NA on terminated links. | Crosswalk `CURRENT_CONTRACT_ID` / `CURRENT_PLAN_ID`. |
| `dec_enrollment` | numeric | December enrollment of the December plan in the county. NA on new-plan and January-only rows. | December CPSC file, aggregated to plan-county (see Enrollment values). **Repeats across links**; see above. |
| `dec_enrollment_low` | numeric | As `dec_enrollment`, suppressed cells counted as 1. | |
| `dec_src` | character | `reported`, `suppressed` (all cells suppressed), `mixed`, or `no_record`. | |
| `jan_enrollment` | numeric | January enrollment of the successor plan in the county. 0 on terminated links (`jan_src` = `terminated`) and on service-area-reduction links that dropped the county (`dropped_county`). | January CPSC file, joined on the successor IDs. **Repeats across links**; see above. |
| `jan_enrollment_low` | numeric | As `jan_enrollment`, suppressed cells counted as 1. | |
| `jan_src` | character | `reported`, `suppressed`, `mixed`, `no_record`, `terminated`, or `dropped_county`. | |
| `plan_type` | character | Plan type as labelled by CMS that year. | December landscape (January landscape on new-plan and January-only rows). |
| `snp`, `snp_type` | character | Special needs plan (`Yes`/`No`) and SNP type. | Landscape. |
| `dsnp_integration` | character | D-SNP integration status: `FIDE`, `HIDE`, `CO` (coordination-only) or `Not Applicable`. NA before CY2025. | Landscape column "Dual Eligible SNP (D-SNP) Integration Status" (CY2025 on). |
| `multi_status` | logical | TRUE when this plan-segment-county appears on more than one row. | `make_analytic()`. |
| `eligibles_2018`, `enrolled_2018`, `penetration_2018` | numeric | 2018 Medicare eligibles, MA enrollees and MA penetration (proportion) in the county. | CMS State/County Penetration file (December 2018), merged by `fips`. |
| `benchmark` | numeric | County MA benchmark for `dec_year`: monthly Parts A and B rate at 0% bonus. | NBER ratebook `countyrate{dec_year}.csv`, SSA code mapped to FIPS. |
| `successor_serves` | logical | TRUE when the January landscape lists the successor plan in this county. | `augment_analytic()`. |
| `sar_dropped` | logical | Service-area-reduction link whose successor does not serve the county in January. | `augment_analytic()`. |
| `forced` | logical | Link-level exit flag: a terminated link, or `sar_dropped`. | `augment_analytic()`. |
| `forced_county` | logical | Plan-county exit flag: TRUE when no continuing successor of the December plan (a renewal, consolidation, SAR or SAE link, or a New Plan link that carries a previous plan ID) serves the county in January. Same value on every link of the plan-county; NA on new-plan and January-only rows. | `augment_analytic()`. |
| `forced_reason` | character | Why `forced_county` is TRUE: `terminated` (every link is a termination), `service_area_reduction`, `successor_drops_county` (moved to a different plan or contract that does not serve the county), `renewal_not_in_landscape` (the same plan renewed but is not listed in the county in the January landscape; some of these still report January enrollment). | `augment_analytic()`. |
| `role` | character | Link-level role: `exiting` (`forced`), `incumbent` (continuing class), `new_entrant` (new class, or a January-only row). | `augment_analytic()`. |
| `prev_links` | integer | Number of links sharing this row's December plan-county. | |
| `dec_first` | logical | TRUE on one link per December plan-county. | |
| `dec_enrollment_once` | numeric | `dec_enrollment` on the `dec_first` link, 0 on the others. | |
| `dec_enrollment_split` | numeric | `dec_enrollment / prev_links`. | |
| `curr_links` | integer | Number of links sharing this row's successor plan-county (rows that carry January enrollment). | |
| `jan_first` | logical | TRUE on one link per successor plan-county. | |
| `jan_enrollment_once` | numeric | `jan_enrollment` on the `jan_first` link, 0 on the other links to the same successor plan-county. | |
| `jan_enrollment_split` | numeric | `jan_enrollment / curr_links`. | |
| `curr_is_incumbent` | logical | TRUE when a December plan in the county continues into this successor plan-county (some link to it is continuing and its successor serves the county). NA on terminated and dropped-county links. | |

## displacement.csv (`displacement`)

One row per December plan × county (`dec_year`, `contract_id`, `plan_id`, `county_name`, `state_name`) for every December plan-county in `plan_county`, built by `make_displacement()`. It answers "how many people lost their plan, and how": each plan-county appears once, so `sum(dec_enrollment)` counts every December enrollee once, and `sum(dec_enrollment[lost_coverage])` is the enrollment that lost coverage. Every column is described in `?displacement` and `maexits_catalog("displacement")`.

**Outcome.** The crosswalk links of each December plan-county are ranked, and the first link whose January plan serves the county sets the outcome. If none serves the county, `lost_coverage` is TRUE and the outcome gives the reason:

| outcome | lost coverage | meaning |
|---|---|---|
| `renewed` | no | The same plan continues in the county |
| `renewed_sae` | no | The same plan continues and expanded its service area elsewhere |
| `renewed_sar_kept_county` | no | The same plan cut its service area elsewhere but kept this county |
| `consolidated_same_plan_id` | no | The plan kept its ID and absorbed other plans |
| `moved_plan_same_contract` | no | Enrollees moved to another plan of the same contract that serves the county |
| `moved_contract` | no | Enrollees moved to a plan of another contract that serves the county |
| `moved_new_plan` | no | Enrollees moved to a new plan that serves the county |
| `terminated_contract_listed` | yes | The plan was terminated; the contract still offers plans in January |
| `terminated_contract_gone` | yes | The plan was terminated; the contract offers no plan in January |
| `new_plan_not_in_county` | yes | Mapped only to new plans that do not serve the county |
| `sar_dropped_county` | yes | The plan continues but dropped this county |
| `sar_successor_not_in_county` | yes | A service-area-reduction link to another plan that does not serve the county |
| `moved_plan_not_in_county` | yes | Moved to another plan of the same contract that does not serve the county |
| `moved_contract_not_in_county` | yes | Moved to another contract's plan that does not serve the county |
| `renewal_not_listed_in_county` | yes | Renewed, but the January landscape does not list the plan in the county; in most of these the plan still reports January enrollment there |

`lost_coverage` equals `forced_county` in `plan_county`, and the county totals equal `county_panel`'s `_once` columns. Terminations are split by whether the contract is in the January landscape, because from the 2024 crosswalk on CMS labels every termination "Terminated/Non-renewed Contract"; the crosswalk statuses are kept in `xwalk_statuses`.

**Scope and alternatives.** `plan_exits` (the plan left every county), `contract_exits` (the contract offers no plan in January), `contract_exits_county`, `parent_exits_county` (no plan of the same parent organization in the county in January; parents are matched on their December and January names from CPSC Contract Info), and counts of the county's plans next January (`n_plans_jan`: non-SNP, non-Cost plans; `n_plans_jan_other_parent`; `n_parents_jan`; `n_same_snp_type_jan` for SNPs). Plan-, contract- and county-level columns repeat on every row of their plan or county.

## exits.csv (`exits`)

One row per December plan × county (`dec_year`, `contract_id`, `plan_id`, `county_name`, `state_name`): the same plan-counties as `displacement` for the years it covers, plus the newest `dec_year` (`MAEXITS_EXITS_YEARS`) from its September table. Built by `make_exits()`. It answers "what share of MA enrollees were in a plan CMS terminated or cut from their county": `exit_type` is `terminated`, `service_area_reduction` or `none`, from CMS's crosswalk labels checked against the January landscape, and each plan-county appears once, so `sum(dec_enrollment[exit_type != "none"]) / sum(dec_enrollment)` is the share.

| Column | Built from |
|---|---|
| `exit_type` | `forced_reason` of the plan-county (in `plan_county`, or the September table for the newest year): `service_area_reduction` as there; `terminated` when every crosswalk link is a termination (a plan mapped to a New Plan that does not serve the county is `none`); the other reasons (moved or consolidated into a plan that does not serve the county, renewal not listed in the county) and plan-counties that kept coverage are `none` |
| `dec_enrollment`, `dec_enrollment_low`, `dec_src` | December enrollment, as in `displacement`; NA for a `dec_year` whose December file is not in the build yet |
| `sep_enrollment`, `sep_enrollment_low`, `sep_src` | September enrollment of `dec_year` (`raw/monthly enrollment/`, `MAEXITS_EXITS_MONTH`), built as in `run_preliminary()` for every year in `MAEXITS_EXITS_YEARS` |

September is there so the newest transition can be compared with earlier ones in October, two months before its December file: in 2018–2025 the September share is 0.01–0.28 points above the December one. The build checks that the two months give the same plan-counties and exit types.

## plan_details.csv (`plan_details`)

One row per contract year × contract × plan × segment, CY2018 on, built by `make_plan_details()` from the landscape files, the Part D Plan and Premium reports (CY2018–2024, `MAEXITS_PARTD_REPORT_FILES` in `R/config.R`) and CPSC Contract Info (December of the contract year, else January). `displacement` carries the same columns for each December plan. Coverage by year:

| Columns | Years | Notes |
|---|---|---|
| `parent_org`, organization names, `org_type`, `plan_type_group`, `part_d` | all | CPSC Contract Info |
| `premium_total`, `premium_part_c`, Part D premiums, `part_d_deductible`, `drug_benefit_type` | all | Monthly dollars (deductible annual). Without Part D, the Part C premium is the consolidated premium |
| `moop_in_network` | all | NA for PFFS, MSA and Cost plans, and for every SNP before CY2025 (the SNP landscape files have no MOOP column) |
| `star_overall`, `star_status` | CY2018–2025 | Blank in the registered CY2026 landscape file |
| `star_part_c`, `star_part_d` | CY2024 on | |
| `dsnp_integration`, `dsnp_aip` | CY2023 on | |
| `snp_institutional_type`, `zero_dollar_dsnp` | CY2025 on | |
| `gap_coverage` | to CY2024 | |

Supplemental benefits (dental, vision, hearing, over-the-counter), the Part B premium reduction and copays are not in these files; they are in the CMS Plan Benefit Package data.

## county_panel.csv

One row per county and `dec_year`, built by `make_county_panel()` from the augmented table. Rows with blank county or state are dropped first. The columns without `_once` sum over links (see above); the `_once` columns count each plan-county once and use `forced_county`.

| Column | Type | Definition |
|---|---|---|
| `county_name`, `state_name`, `dec_year`, `fips` | | Keys. |
| `total_dec_enrollment`, `total_jan_enrollment` | numeric | Sum of `dec_enrollment` / `jan_enrollment` over links. |
| `displaced_enrollment` | numeric | Sum of `dec_enrollment` over `forced` links. |
| `incumbent_dec`, `incumbent_jan`, `new_entrant_jan` | numeric | Sums over links by `role`. |
| `n_plans_dec` | integer | Distinct plans with a row in the county-year (all roles). |
| `n_exiting_plans` | integer | Distinct plans with a `forced` link. |
| `penetration_2018`, `eligibles_2018`, `benchmark` | numeric | County covariates (see above). |
| `exit_rate` | numeric | `displaced_enrollment / total_dec_enrollment`; NA when the denominator is 0. |
| `enrollment_change` | numeric | `(total_jan_enrollment − total_dec_enrollment) / total_dec_enrollment`. |
| `net_enrollment_change` | numeric | `total_jan_enrollment − total_dec_enrollment`. |
| `incumbent_growth` | numeric | `incumbent_jan − incumbent_dec`. |
| `exit_group` | factor | `exit_rate` binned: `No exits` (NA or 0), `Low (<5%)` (≤ 0.05), `Medium (5-15%)` (≤ 0.15), `High (>15%)`. |
| `pen_quartile` | factor | Quartile of 2018 penetration, with breaks from the distinct county values. |
| `total_dec_enrollment_once`, `total_jan_enrollment_once` | numeric | Sum of `dec_enrollment_once` / `jan_enrollment_once`. |
| `total_dec_enrollment_once_low`, `total_jan_enrollment_once_low` | numeric | Lower bounds (suppressed cells as 1). |
| `displaced_enrollment_once`, `displaced_enrollment_once_low` | numeric | December enrollment of plan-counties with `forced_county`, each counted once. |
| `incumbent_dec_once` | numeric | December enrollment of plan-counties not forced out. |
| `incumbent_jan_once` | numeric | January enrollment of successor plan-counties with `curr_is_incumbent`. |
| `new_entrant_jan_once` | numeric | All other January enrollment, each plan-county once: new plans, counties a plan adds, and the few successors that report January enrollment in a county where the January landscape does not list them. |
| `n_exiting_plans_county` | integer | Distinct plans with `forced_county`. |
| `snp_dec_enrollment_once`, `snp_displaced_enrollment_once` | numeric | SNP December enrollment, and the part of it forced out. |
| `exit_rate_once`, `enrollment_change_once`, `incumbent_growth_once`, `exit_group_once` | | As the columns without `_once`, from the `_once` totals. |

## Caveats

- **Suppressed counts.** Suppression is far more common in small, rural and low-penetration counties. Counting each suppressed cell as 10 is an upper bound; the `_low` columns give the lower bound.
- **Net flows, not individual transitions.** Both tables are built from plan-county enrollment counts. Enrollment changes are net county-level changes; they cannot identify where individual enrollees went.
- **Exit types differ.** `forced_reason` separates full terminations from service-area reductions and moves into successors that do not serve the county. These differ in scale and in the notice enrollees receive.
- **`renewal_not_in_landscape`.** A plan that renewed but is not listed in the county in the January landscape is counted as forced out, though some such plan-counties still report January enrollment (it is counted in `new_entrant_jan_once`). Exclude this reason for a stricter exit definition.
- **`benchmark` is the concurrent-year rate** (`dec_year` rate), not the following year's. `penetration_2018` and `eligibles_2018` are fixed 2018 values. FIPS-dependent columns are NA where the county name has no FIPS match.
