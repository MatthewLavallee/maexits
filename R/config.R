# ============================================================
# config.R — Single source of truth for the years and files the
# pipeline covers.
#
# EACH ANNUAL CYCLE (full runbook: README "Annual update"):
#   1. When the CY N landscape is released (~Oct 1), register its exact
#      file in MAEXITS_LANDSCAPE_FILES.
#   2. When the N crosswalk is released (~Oct 1), register its exact file
#      in MAEXITS_XWALK_FILES. If it uses a new status wording, add it to
#      MAEXITS_XWALK_STATUS_CLASS with the matching class.
#      (run_preliminary() can run from here on.)
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
  "2026" = "PlanCrosswalk2026_10012025.txt"
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
  "2026" = "CY2026/CY2026_Landscape_202509.csv"
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
