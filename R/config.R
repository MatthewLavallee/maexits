# ============================================================
# config.R — Single source of truth for the years and files the
# pipeline covers.
#
# EACH ANNUAL CYCLE (full runbook: PIPELINE.md "Annual update"):
#   1. When the CY N landscape is released (~Oct 1), register its exact
#      file in MAEXITS_LANDSCAPE_FILES.
#   2. When the N crosswalk is released (~Oct 1), register its exact file
#      in MAEXITS_XWALK_FILES. If it uses a new status wording, add it to
#      MAEXITS_XWALK_STATUS_CLASS with the matching class.
#      (run_preliminary() can run from here on.) With September N-1 CPSC
#      in raw/monthly enrollment/, add N to MAEXITS_EXITS_YEARS and run
#      run_data_pipeline(): the exits table gains dec_year N-1.
#   3. Once the December N-1 and January N CPSC files are in raw/ (~Jan),
#      add N to MAEXITS_XWALK_YEARS, then run check_inputs() and
#      run_data_pipeline().
# Format changes CMS makes along the way (new column names, a new status
# class) may also need code changes; the checks say where.
#
# Coverage is deliberately NOT inferred from whatever is on disk.
# Between the crosswalk release (~Oct 1) and the January CPSC release
# (~Jan) the new crosswalk exists but its enrollment does not; inferring
# coverage would build the new year on imputed values. check_inputs()
# stops with a list of what is missing instead, and enrollment files for
# years beyond MAEXITS_XWALK_YEARS are not read.
# ============================================================


# Crosswalk year N covers the December (N-1) -> January (N) transition.
MAEXITS_XWALK_YEARS <- 2019:2026

# Exact CMS Part C&D Plan Crosswalk filenames under raw/plan crosswalk/.
# CMS filenames are not predictable: the 2025 crosswalk was published as
# PlanCrosswalk2024_10012024.txt. Never rename CMS files; register them.
MAEXITS_XWALK_FILES <- c(
  "2019" = "PlanCrosswalk2019_10012018.txt",
  "2020" = "PlanCrosswalk2020_10022019.txt",
  "2021" = "PlanCrosswalk2021_10222020.txt",
  "2022" = "PlanCrosswalk2022_10042021.txt",
  "2023" = "PlanCrosswalk2023_10032022.txt",
  "2024" = "PlanCrosswalk2024_09282023.txt",
  "2025" = "PlanCrosswalk2024_10012024.txt",  # CMS labelled this file "2024"
  "2026" = "PlanCrosswalk2026_10012025.txt",
  "2027" = "PlanCrosswalk2027_10012026.txt"
)

# Landscape years are read from this year through the last January year.
MAEXITS_LANDSCAPE_FIRST_YEAR <- 2016L

# CY2016-CY2023 are read from their folders (separate MA and SNP csvs) and
# CY2024 from its fixed set of MA/SNP/sanctioned files; see data_build.R.
# From CY2025 CMS publishes one combined file per year; register the exact
# file (relative to raw/landscape/). CMS reissues landscape files during the
# year, so the registered vintage is part of the result.
MAEXITS_LANDSCAPE_FILES <- c(
  "2025" = "CY2025/CY2025_Landscape_202506.1.csv",
  # Frozen vintage: the committed dec_year 2025 tables use 202509.
  # Do not replace it with CMS's later 202609 reissue.
  "2026" = "CY2026/CY2026_Landscape_202509.csv",
  # CMS's first CY2027 posting, cy2027-landscape-202609-1.zip (readme dated
  # 2026-09-30, data last updated 2026-09-22).
  "2027" = "CY2027/CY2027_Landscape_202609.csv"
)

# Crosswalk years of the exits table (make_exits()). It runs one year ahead
# of MAEXITS_XWALK_YEARS on September enrollment: add N here once the N
# crosswalk and landscape are registered and September N-1 CPSC is in
# raw/monthly enrollment/ (CMS posts it in mid-September, before the
# crosswalk), so the newest transition can be compared with earlier years
# months before its December file is out.
MAEXITS_EXITS_YEARS <- 2019:2027

# Month whose CPSC enrollment stands in for December in the exits table:
# September N-1 for crosswalk year N.
MAEXITS_EXITS_MONTH <- 9L

# Plan details (make_plan_details()) are built from this contract year on:
# the first December the pipeline covers.
MAEXITS_PLAN_DETAIL_FIRST_YEAR <- 2018L

# Landscape files whose star rating columns are blank for every plan (the
# CY2026 202509 and CY2027 202609 vintages, posted before CMS's October star
# ratings); make_plan_details() skips their star coverage check.
MAEXITS_LANDSCAPE_BLANK_STARS <- c(2026L, 2027L)

# CMS "Plan and Premium Information for Medicare Plans Offering Part D"
# reports, relative to raw/landscape/: Part C and Part D premiums for MA
# plans that offer Part D, CY2018-CY2024 (CY2025+ premiums are in the
# combined landscape file). CY2018-CY2023 are zips, of which the
# 508_AlabamatoMontana, 508_NebraskatoWyoming and 508_Sanctioned csvs are
# read; CY2024 is a main and a sanctioned csv. Register a new year's exact
# file here only if CMS stops putting premiums in the combined landscape.
MAEXITS_PARTD_REPORT_FILES <- list(
  "2018" = "CY2018/2018 Plan and Premium Information for Medicare Plans Offering Part D Coverage (v9 22 17).zip",
  "2019" = "CY2019/2019 Plan and Premium Information for Medicare Plans Offering Part D Coverage (v10 12 18).zip",
  "2020" = "CY2020/2020 Plan and Premium Information for Medicare Plans Offering Part D Coverage (v9 03 19).zip",
  "2021" = "CY2021/2021 Plan and Premium Information for Medicare Plans Offering Part D Coverage (v9 08 20).zip",
  "2022" = "CY2022/2022 Plan and Premium Information for Medicare Plans Offering Part D Coverage (v10 26 21).zip",
  "2023" = "CY2023/2023 Plan and Premium Information for Medicare Plans Offering Part D Coverage (v 10 14 2022).zip",
  "2024" = c("CY2024/csv version/CY2024_Plan_Premium_Report_20240723.csv",
             "CY2024/csv version/sanctioned plans/CY2024_Plan_Premium_Report_sanctioned_20240628.csv")
)

# Every crosswalk status CMS has used (2019-2026) and its class. All status
# logic (forced exits, SAR and SAE handling, roles) works from the class,
# so a reworded status only needs a line here. A status not listed stops
# the pipeline; read the new crosswalk's readme before adding one. A status
# that fits none of the six classes needs a code change.
MAEXITS_XWALK_STATUS_CLASS <- c(
  "Renewal Plan"                    = "continuing",
  "Consolidated Renewal Plan"       = "continuing",
  "Renewal Plan with SAR"           = "service_area_reduction",
  "Renewal Plan with SAE"           = "service_area_expansion",
  "Terminated Plan"                 = "terminated",
  "Terminated/Non-renewed Contract" = "terminated",
  "New Plan"                        = "new",
  "Initial Contract"                = "new"
)

# Statuses (all in the "continuing" class) that merge other plans into a
# surviving plan. Used only to label outcomes in make_displacement().
MAEXITS_XWALK_CONSOLIDATION_STATUSES <- "Consolidated Renewal Plan"

# Non-numeric plan-ID placeholders CMS uses in crosswalk ID columns.
MAEXITS_XWALK_PLACEHOLDER_IDS <- c("NEW", "DEL", "TERMINATED")

# Allowed "Contract Category Type" values in CY2025+ combined landscapes.
MAEXITS_CONTRACT_CATEGORIES <- c("MA", "MA-PD", "SNP", "Cost", "MMP", "PDP")

# Contract categories kept out of the plan universe. Standalone drug plans
# (PDP) are not MA; Medicare-Medicaid Plans (MMP) appear only in the CY2025+
# combined files, so they are dropped to keep every year's universe the same.
# Employer group (800-series) plans are not in the landscape at all, so the
# universe is individual-market MA plus SNPs on both sides of each transition.
MAEXITS_EXCLUDED_CATEGORIES <- c("PDP", "MMP")

# Landscape county names that CMS enrollment (CPSC) files spell differently.
# make_landscape() rewrites them to the CPSC spelling so joins on county name
# match; make_analytic() stops if a landscape county has no CPSC counterpart,
# naming the entries to add here. "(Partial)" county labels are handled in
# code: the suffix is dropped, or the row is dropped when the same plan also
# lists the whole county.
MAEXITS_COUNTY_NAME_MAP <- data.frame(
  state_name = c("Alaska", "Alaska", "Alaska", "Alaska", "Alaska",
                 "Indiana", "Louisiana", "South Dakota"),
  from = c("Kusilvak", "Hoonah-Angoon", "Skagway", "Petersburg", "Wrangell",
           "DeKalb", "LaSalle", "Oglala Lakota"),
  to = c("Wade Hampton", "Skagway-Hoonah-Angoon", "Skagway-Hoonah-Angoon",
         "Wrangell-Petersburg", "Wrangell-Petersburg",
         "De Kalb", "La Salle", "Shannon"),
  stringsAsFactors = FALSE)

# State labels used by older landscape files
MAEXITS_STATE_NAME_MAP <- c("Washington D.C." = "District of Columbia")

# CPSC file used as the county -> FIPS and SSA -> FIPS lookup
# (relative to raw/).
MAEXITS_FIPS_LOOKUP_FILE <- file.path(
  "january enrollment", "CPSC_Enrollment_2025_01",
  "CPSC_Enrollment_Info_2025_01.csv")

# GitHub release that holds the processed datasets maexits_data() downloads.
# Publish a new release (prepare_data_release()) whenever the derived
# tables change, and point this at it.
MAEXITS_DATA_REPO <- "MatthewLavallee/maexits"
MAEXITS_DATA_RELEASE <- "data-2018-2025"  # dec_year 2018-2025

# Where CMS publishes Monthly Enrollment by Contract/Plan/State/County
# (monthly-enrollment-cpsc-<month>-<year>.zip; this pattern starts with
# December 2019).
MAEXITS_CMS_ZIP_URL <- "https://www.cms.gov/files/zip"
MAEXITS_CMS_FIRST_MONTH <- "2019-12"

# Where CMS and NBER announce the files each cycle needs. check_cms_releases()
# reads these pages and sends HEAD requests to the file links they list; it
# never downloads a data file. The crosswalk and CPSC lists have RSS feeds
# (taxonomy terms 31946 and 31911); the landscape page does not.
MAEXITS_WATCH_SOURCES <- list(
  cms = "https://www.cms.gov",
  crosswalk_rss = "https://www.cms.gov/rss/31946",
  crosswalk_index = paste0(
    "https://www.cms.gov/data-research/statistics-trends-and-reports/",
    "medicare-advantagepart-d-contract-and-enrollment-data/plan-crosswalks"),
  landscape_page = "https://www.cms.gov/medicare/coverage/prescription-drug-coverage",
  cpsc_rss = "https://www.cms.gov/rss/31911",
  nber_ratebook = "https://data.nber.org/ratebook"
)

# MA penetration baseline (relative to raw/).
MAEXITS_PENETRATION_FILE <- file.path(
  "penetration", "State_County_Penetration_MA_2018_12",
  "State_County_Penetration_MA_2018_12.csv")


#' Crosswalk statuses belonging to one or more classes
#'
#' @param cls Character vector of classes from
#'   \code{MAEXITS_XWALK_STATUS_CLASS} ("continuing",
#'   "service_area_reduction", "service_area_expansion", "terminated", "new").
#' @return Character vector of status strings.
#' @keywords internal
.statuses <- function(cls) {
  names(MAEXITS_XWALK_STATUS_CLASS)[MAEXITS_XWALK_STATUS_CLASS %in% cls]
}


#' Landscape years needed for a set of crosswalk years
#'
#' @param xwalk_years Integer vector of crosswalk years.
#' @return Integer vector from \code{MAEXITS_LANDSCAPE_FIRST_YEAR} through
#'   the last January year.
#' @keywords internal
.landscape_years <- function(xwalk_years = MAEXITS_XWALK_YEARS) {
  MAEXITS_LANDSCAPE_FIRST_YEAR:max(as.integer(xwalk_years))
}


#' Contract years with plan details for a set of crosswalk years
#' @keywords internal
.plan_detail_years <- function(xwalk_years = MAEXITS_XWALK_YEARS) {
  MAEXITS_PLAN_DETAIL_FIRST_YEAR:max(as.integer(xwalk_years))
}
