# ============================================================
# data_build.R — Raw Data to Analytic Table (Pipeline Steps 1-3)
#
# Functions in this file read raw CMS data files and produce
# the core analytic table. They should be called in order:
#   1. make_enrollment()  → december_enrollment.csv, january_enrollment.csv
#   2. make_landscape()   → landscape.csv
#   3. make_analytic()    → analytictable.csv
#
# The years covered and the exact input files are set in R/config.R;
# fail-loud checks live in R/validate.R. See data_augment.R for steps
# 5-11 (FIPS, penetration, benchmark, augmentation, county panel, runner).
# ============================================================


# --- Internal Helpers -----------------------------------------------

#' Load a data.table from CSV if the input is NULL
#'
#' @param dt A data.table or NULL.
#' @param path Relative path (from project root) to the CSV file.
#' @return A copy of the data.table (never modifies caller's object).
#' @keywords internal
.load_if_null <- function(dt, path) {
  if (is.null(dt)) {
    dt <- fread(here(path))
  }
  dt <- copy(dt)

  # Restore factor columns lost during CSV round-trip
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


#' Resolve the file path for a CMS plan crosswalk file
#'
#' Looks the year up in \code{MAEXITS_XWALK_FILES} (R/config.R). CMS
#' filenames are not predictable (the 2025 crosswalk is named
#' PlanCrosswalk2024_10012024.txt), so each year's exact file is
#' registered rather than pattern-matched.
#'
#' @param xwalk_year Integer. The crosswalk year.
#' @return Character. A single file path to the crosswalk file.
#' @keywords internal
.resolve_crosswalk_path <- function(xwalk_year) {
  key <- as.character(xwalk_year)
  if (!key %in% names(MAEXITS_XWALK_FILES)) {
    stop(sprintf(paste0(
      "Crosswalk year %s is not registered. Download the %s Part C&D Plan ",
      "Crosswalk from CMS into raw/plan crosswalk/ and add its exact filename ",
      "to MAEXITS_XWALK_FILES in R/config.R."), key, key), call. = FALSE)
  }
  path <- here("raw", "plan crosswalk", MAEXITS_XWALK_FILES[[key]])
  if (!file.exists(path)) {
    stop(sprintf("Crosswalk %s is registered as '%s' but %s does not exist.",
                 key, MAEXITS_XWALK_FILES[[key]], path), call. = FALSE)
  }
  path
}


#' Parse crosswalk plan IDs, allowing only the known placeholders
#'
#' Numeric IDs become numbers; the placeholders in
#' \code{MAEXITS_XWALK_PLACEHOLDER_IDS} ("NEW", "DEL", "TERMINATED") and
#' blanks become NA. Any other value stops the pipeline.
#' @keywords internal
.parse_plan_id <- function(x, xwalk_year) {
  x <- trimws(as.character(x))
  num <- !is.na(x) & grepl("^[0-9]+$", x)
  odd <- !num & !is.na(x) & x != "" & !x %in% MAEXITS_XWALK_PLACEHOLDER_IDS
  .vcheck(!any(odd), "crosswalk %d: unexpected plan ID value(s): %s",
          as.integer(xwalk_year), paste(head(unique(x[odd]), 10), collapse = ", "))
  out <- rep(NA_real_, length(x))
  out[num] <- as.numeric(x[num])
  out
}


#' Read and validate one CMS plan crosswalk
#'
#' The status column is detected by name (DESCRIPTION before 2022, STATUS
#' after). Required columns and the status vocabulary are checked against
#' \code{MAEXITS_XWALK_STATUS_CLASS}.
#'
#' @param xwalk_year Integer. The crosswalk year.
#' @param path Crosswalk file; defaults to the registered file for
#'   \code{xwalk_year}.
#' @return data.table with prev_contract, prev_plan, curr_contract,
#'   curr_plan, status. New plans have \code{prev_plan = NA}.
#' @keywords internal
.read_crosswalk <- function(xwalk_year, path = .resolve_crosswalk_path(xwalk_year)) {
  dfx <- fread(path, colClasses = "character")

  status_col <- intersect(c("STATUS", "DESCRIPTION"), names(dfx))
  .vcheck(length(status_col) == 1,
          "crosswalk %d (%s): expected exactly one of STATUS/DESCRIPTION, found %s",
          as.integer(xwalk_year), basename(path),
          if (length(status_col)) paste(status_col, collapse = "+") else "neither")
  need <- c("PREVIOUS_CONTRACT_ID", "PREVIOUS_PLAN_ID",
            "CURRENT_CONTRACT_ID", "CURRENT_PLAN_ID")
  miss <- setdiff(need, names(dfx))
  .vcheck(length(miss) == 0, "crosswalk %d (%s) is missing column(s): %s",
          as.integer(xwalk_year), basename(path), paste(miss, collapse = ", "))

  unknown <- setdiff(unique(dfx[[status_col]]), names(MAEXITS_XWALK_STATUS_CLASS))
  .vcheck(length(unknown) == 0, paste0(
    "crosswalk %d has status value(s) not in MAEXITS_XWALK_STATUS_CLASS: %s. ",
    "Read the crosswalk readme, then add each to MAEXITS_XWALK_STATUS_CLASS in ",
    "R/config.R with its class; forced exits, SAR/SAE handling and roles follow ",
    "the class. A status that fits none of the classes needs a code change."),
    as.integer(xwalk_year), paste(sprintf("'%s'", unknown), collapse = ", "))

  dfx[, .(
    prev_contract = PREVIOUS_CONTRACT_ID,
    prev_plan = .parse_plan_id(PREVIOUS_PLAN_ID, xwalk_year),
    curr_contract = CURRENT_CONTRACT_ID,
    curr_plan = .parse_plan_id(CURRENT_PLAN_ID, xwalk_year),
    status = get(status_col)
  )]
}


# --- Step 1: Landscape Helpers (Internal) ---------------------------

#' Read and harmonize CMS landscape files for a single year (2016-2023)
#'
#' Reads separate MA and SNP CSV files from the raw landscape directory,
#' standardizes column names across years, and returns a combined data.table.
#' Each file is checked for the expected columns before binding, so a file
#' whose header row was misread fails loudly instead of being dropped.
#'
#' @param cy_year Character. Year folder name (e.g., "CY2021").
#' @return data.table with columns: state_name, county_name, contract_id,
#'   plan_id, segment_id, plan_type, snp, snp_type, year.
#' @keywords internal
.get_landscape_2016to2023 <- function(cy_year) {
  paths <- list.files(here("raw", "landscape", cy_year),
                      full.names = TRUE, pattern = "csv", recursive = TRUE)
  snp_paths <- paths[grep(paths, pattern = "SNP")]
  ma_paths <- paths[grep(paths, pattern = "MA")]
  .vcheck(length(ma_paths) > 0 && length(snp_paths) > 0,
          "landscape %s: expected MA and SNP csv files under raw/landscape/%s",
          cy_year, cy_year)
  snp_skip <- ifelse(cy_year == "CY2023", 6, 4)

  ma_cols <- c("State", "County", "Contract ID", "Plan ID", "Segment ID",
               "Type of Medicare Health Plan")
  read_checked <- function(p, skip, need) {
    d <- fread(p, skip = skip)
    miss <- setdiff(need, names(d))
    .vcheck(length(miss) == 0, paste0(
      "landscape file %s is missing column(s) %s after skipping %d line(s); ",
      "the preamble length may have changed"), p, paste(miss, collapse = ", "), skip)
    d
  }
  dfsnp <- rbindlist(lapply(snp_paths, read_checked, skip = snp_skip,
                            need = c(ma_cols, "Special Needs Plan Type")), fill = TRUE)
  dfma <- rbindlist(lapply(ma_paths, read_checked, skip = 5, need = ma_cols),
                    fill = TRUE)
  df <- rbind(
    dfma[, .(
      state_name = State,
      county_name = County,
      contract_id = `Contract ID`,
      plan_id = `Plan ID`,
      segment_id = `Segment ID`,
      plan_type = `Type of Medicare Health Plan`,
      snp = "No",
      snp_type = NA,
      dsnp_integration = NA_character_
    )][!is.na(county_name)],
    dfsnp[, .(
      state_name = State,
      county_name = County,
      contract_id = `Contract ID`,
      plan_id = `Plan ID`,
      segment_id = `Segment ID`,
      plan_type = `Type of Medicare Health Plan`,
      snp = "Yes",
      snp_type = `Special Needs Plan Type`,
      dsnp_integration = NA_character_
    )][!is.na(county_name)]
  )
  df[, year := cy_year]
  df
}


#' Paths of the four CY2024 landscape files
#' @keywords internal
.landscape_2024_paths <- function() {
  base <- here("raw", "landscape", "CY2024", "csv version")
  c(ma = file.path(base, "CY2024_Landscape_MA_20240723.csv"),
    ma_sanctioned = file.path(base, "sanctioned plans",
                              "CY2024_Landscape_MA_sanctioned_20240628.csv"),
    snp = file.path(base, "CY2024_Landscape_SNP_20240710.csv"),
    snp_sanctioned = file.path(base, "sanctioned plans",
                               "CY2024_Landscape_SNP_sanctioned_20240628.csv"))
}


#' Read the CY2024 landscape (separate MA + SNP files, plus sanctioned plans)
#' @keywords internal
.get_landscape_2024 <- function() {
  p <- .landscape_2024_paths()
  miss <- p[!file.exists(p)]
  .vcheck(length(miss) == 0, "CY2024 landscape file(s) not found: %s",
          paste(miss, collapse = ", "))
  dfma <- rbind(
    fread(p[["ma"]])[, .(contract_id = `Contract ID`, plan_id = `Plan ID`,
      segment_id = `Segment ID`, state_name = State, county_name = County,
      plan_type = `Type of Medicare Health Plan`, snp = "No", snp_type = NA)],
    fread(p[["ma_sanctioned"]])[, .(contract_id = `Contract ID`, plan_id = `Plan ID`,
      segment_id = `Segment ID`, state_name = State, county_name = County,
      plan_type = `Type of Medicare Health Plan`, snp = "No", snp_type = NA)]
  )
  dfsnp <- rbind(
    fread(p[["snp"]])[, .(contract_id = `Contract ID`, plan_id = `Plan ID`,
      segment_id = `Segment ID`, state_name = State, county_name = County,
      plan_type = `Type of Medicare Health Plan`, snp = "Yes",
      snp_type = `Special Needs Plan Type`)],
    fread(p[["snp_sanctioned"]])[, .(contract_id = `Contract ID`, plan_id = `Plan ID`,
      segment_id = `Segment ID`, state_name = State, county_name = County,
      plan_type = `Type of Medicare Health Plan`, snp = "Yes",
      snp_type = `Special Needs Plan Type`)]
  )
  df24 <- rbind(dfma, dfsnp)
  df24[, dsnp_integration := NA_character_]
  df24[, year := "CY2024"]
  df24
}


# Column aliases for CY2025+ combined landscape files. Each output column
# must match exactly one of its aliases (CMS renamed "State Name" to
# "State Territory Name" in CY2026).
#' D-SNP integration status as CMS's short code
#'
#' The CY2023-CY2026 files give CO, HIDE or FIDE; from CY2027 CMS spells
#' them out ("Coordination Only (CO)"). Returns the short code, keeps "Not
#' Applicable", turns blanks into NA, and stops on any other value.
#' @param x Character vector of statuses.
#' @param what File label for the error message.
#' @keywords internal
.dsnp_code <- function(x, what) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  code <- sub("^.*\\(\\s*(CO|HIDE|FIDE)\\s*\\)$", "\\1", x)
  bad <- setdiff(unique(code[!is.na(code)]), c("CO", "HIDE", "FIDE", "Not Applicable"))
  .vcheck(length(bad) == 0, paste0(
    "%s has unknown D-SNP integration status(es) %s; map them in .dsnp_code() ",
    "(R/data_build.R)"), what, paste(sprintf("'%s'", bad), collapse = ", "))
  code
}


.LANDSCAPE_COLUMNS <- list(
  contract_id = "Contract ID",
  plan_id     = "Plan ID",
  segment_id  = "Segment ID",
  state_name  = c("State Name", "State Territory Name"),
  county_name = "County Name",
  plan_type   = "Plan Type",
  snp         = "Special Needs Plan (SNP) Indicator",
  snp_type    = "SNP Type"
)


#' Path of the registered CY2025+ combined landscape file
#' @keywords internal
.landscape_file <- function(cy) {
  key <- as.character(cy)
  .vcheck(key %in% names(MAEXITS_LANDSCAPE_FILES), paste0(
    "landscape CY%s is not registered. Download the CY%s landscape source ",
    "files from CMS into raw/landscape/CY%s/ and add the csv's exact path ",
    "to MAEXITS_LANDSCAPE_FILES in R/config.R."), key, key, key)
  path <- here("raw", "landscape", MAEXITS_LANDSCAPE_FILES[[key]])
  .vcheck(file.exists(path), "landscape CY%s is registered as '%s' but %s does not exist",
          key, MAEXITS_LANDSCAPE_FILES[[key]], path)
  path
}


#' Read one CY2025+ combined landscape file
#'
#' @param cy Integer. Contract year.
#' @param path File to read; defaults to the file registered in
#'   \code{MAEXITS_LANDSCAPE_FILES}.
#' @param allowed Allowed "Contract Category Type" values.
#' @return data.table with the standard landscape columns and
#'   \code{year = "CY<cy>"}.
#' @keywords internal
.get_landscape_combined <- function(cy, path = .landscape_file(cy),
                                    allowed = MAEXITS_CONTRACT_CATEGORIES) {
  key <- as.character(cy)
  d <- fread(path)
  if ("Contract Year" %in% names(d)) {
    years_in_file <- unique(as.character(d[["Contract Year"]]))
    .vcheck(identical(years_in_file, key), paste0(
      "landscape CY%s is registered as '%s', but its Contract Year column says %s; ",
      "register the CY%s file in MAEXITS_LANDSCAPE_FILES"),
      key, basename(path), paste(years_in_file, collapse = ","), key)
  }
  src <- vapply(names(.LANDSCAPE_COLUMNS), function(nm) {
    hit <- intersect(.LANDSCAPE_COLUMNS[[nm]], names(d))
    .vcheck(length(hit) == 1,
            "landscape CY%s (%s): expected exactly one of [%s] for %s, found %d",
            key, basename(path), paste(.LANDSCAPE_COLUMNS[[nm]], collapse = " | "),
            nm, length(hit))
    hit
  }, character(1))

  .vcheck("Contract Category Type" %in% names(d),
          "landscape CY%s (%s) has no 'Contract Category Type' column", key, basename(path))
  unknown <- setdiff(unique(d[["Contract Category Type"]]), allowed)
  .vcheck(length(unknown) == 0, paste0(
    "landscape CY%s has unknown Contract Category Type value(s): %s. Decide ",
    "whether they belong in the MA universe, then update ",
    "MAEXITS_CONTRACT_CATEGORIES (and the PDP filter in make_landscape())."),
    key, paste(sprintf("'%s'", unknown), collapse = ", "))

  # Standalone drug plans and MMPs are outside the plan universe
  d <- d[!`Contract Category Type` %in% MAEXITS_EXCLUDED_CATEGORIES]
  out <- d[, unname(src), with = FALSE]
  setnames(out, names(src))
  dsnp_col <- "Dual Eligible SNP (D-SNP) Integration Status"
  out[, dsnp_integration := if (dsnp_col %in% names(d))
    .dsnp_code(d[[dsnp_col]], sprintf("landscape CY%s", key)) else NA_character_]
  out[, year := paste0("CY", key)]
  out
}


#' Rewrite landscape county and state names to the CMS enrollment spelling
#'
#' Applies \code{MAEXITS_STATE_NAME_MAP} and \code{MAEXITS_COUNTY_NAME_MAP}
#' (R/config.R) so that landscape rows join to CPSC enrollment by name, and
#' resolves "(Partial)" county labels: the row is dropped when the same plan
#' also lists the whole county that year, otherwise the suffix is removed.
#'
#' @param dfl Landscape data.table with state_name and county_name.
#' @return The landscape with harmonized names.
#' @keywords internal
.harmonize_county_names <- function(dfl) {
  for (from in names(MAEXITS_STATE_NAME_MAP)) {
    dfl[state_name == from, state_name := MAEXITS_STATE_NAME_MAP[[from]]]
  }
  map <- MAEXITS_COUNTY_NAME_MAP
  idx <- match(paste(dfl$state_name, dfl$county_name, sep = "|"),
               paste(map$state_name, map$from, sep = "|"))
  hit <- !is.na(idx)
  dfl[hit, county_name := map$to[idx[hit]]]

  part <- grepl(" (Partial)", dfl$county_name, fixed = TRUE)
  if (any(part)) {
    dfl[, base_county := sub(" (Partial)", "", county_name, fixed = TRUE)]
    whole <- unique(dfl[!part, .(state_name, base_county = county_name,
                                 contract_id, plan_id, year)])
    dfl[, has_whole := FALSE]
    dfl[whole, has_whole := TRUE,
        on = .(state_name, base_county, contract_id, plan_id, year)]
    dfl <- dfl[!(part & has_whole)]
    dfl[, county_name := base_county]
    dfl[, c("base_county", "has_whole") := NULL]
  }
  dfl
}


# --- Step 1: Enrollment ---------------------------------------------

#' Process CPSC Enrollment Files
#'
#' Reads raw CMS CPSC enrollment CSV files from a monthly folder,
#' standardizes column names and state abbreviations, and aggregates
#' to the county-plan level. Each file must be named
#' CPSC_Enrollment_Info_YYYY_MM.csv with the folder's month, and each year
#' may appear only once.
#'
#' @param month_folder Character. Folder name under \code{raw/}
#'   (e.g., \code{"december enrollment"} or \code{"january enrollment"}).
#' @param save Logical. If TRUE (default), writes the aggregated
#'   enrollment CSV to \code{trunk/derived/}.
#' @param max_year Integer or NULL. Files for later years are skipped (with
#'   a message), so staging next cycle's CPSC file does not change the
#'   tables until its year is added to \code{MAEXITS_XWALK_YEARS}.
#'   \code{run_data_pipeline()} sets it from the configured years.
#' @return Invisibly returns the aggregated data.table with columns:
#'   contract_id, plan_id, county_name, state_name, year, county_enrollment
#'   (sum of reported counts) and n_suppressed (number of CMS-suppressed
#'   cells, each 1-10 enrollees).
#' @export
make_enrollment <- function(month_folder, save = TRUE, max_year = NULL) {
  files <- list.files(here("raw", month_folder), recursive = TRUE,
                      pattern = "CPSC_Enrollment_Info", full.names = TRUE)
  file_years <- as.integer(.check_enrollment_files(files, month_folder))
  if (!is.null(max_year) && any(file_years > max_year)) {
    message("make_enrollment: skipping ", paste(basename(files[file_years > max_year]),
            collapse = ", "), " (after ", max_year, "; not yet in MAEXITS_XWALK_YEARS)")
    files <- files[file_years <= max_year]
  }
  dfr2 <- .aggregate_cpsc(files, paste0("raw/", month_folder))

  if (save) {
    out_name <- gsub(" ", "_", month_folder)
    fwrite(dfr2, here("trunk", "derived", paste0(out_name, ".csv")))
  }

  invisible(dfr2)
}


#' Read CPSC enrollment files and aggregate to contract x plan x county
#'
#' Shared by \code{make_enrollment()} and \code{run_preliminary()}. The
#' year is taken from each file name.
#'
#' @param files Character vector of CPSC_Enrollment_Info csv paths.
#' @param label Description used in error messages.
#' @return data.table with contract_id, plan_id, county_name, state_name,
#'   year, county_enrollment (sum of reported counts), n_suppressed (number
#'   of CMS-suppressed cells, each 1-10 enrollees).
#' @keywords internal
.aggregate_cpsc <- function(files, label) {
  dflist <- lapply(files, fread)
  names(dflist) <- files
  headers <- unique(lapply(dflist, names))
  if (length(headers) > 1) {
    first <- headers[[1]]
    odd <- files[!vapply(dflist, function(d) identical(names(d), first), logical(1))]
    .vcheck(FALSE, paste0(
      "CPSC files in %s do not share one header; files are combined by column ",
      "position, so a changed layout must be handled first. Expected [%s]; ",
      "differs in: %s"), label, paste(first, collapse = ", "),
      paste(sprintf("%s [%s]", basename(odd),
                    vapply(dflist[odd], function(d) paste(names(d), collapse = ", "), "")),
            collapse = "; "))
  }
  need <- c("Contract Number", "Plan ID", "State", "SSA State County Code",
            "County", "Enrollment")
  miss <- setdiff(need, headers[[1]])
  .vcheck(length(miss) == 0, "CPSC files in %s are missing column(s): %s",
          label, paste(miss, collapse = ", "))

  df <- rbindlist(dflist, idcol = "path")

  # Enrollment is numeric except CMS's "*" suppression marker (<11 enrollees)
  df[, Enrollment := as.character(Enrollment)]
  bad <- df[!is.na(Enrollment) & Enrollment != "*" & Enrollment != "" &
              !grepl("^[0-9]+$", Enrollment), unique(Enrollment)]
  .vcheck(length(bad) == 0, "unexpected Enrollment value(s) in %s: %s",
          label, paste(head(bad, 10), collapse = ", "))

  dfr <- df[, .(
    contract_id = `Contract Number`,
    plan_id = `Plan ID`,
    state_abb = State,
    county_code = `SSA State County Code`,
    county_name = County,
    county_enrollment = as.numeric(fifelse(Enrollment %in% c("*", ""),
                                           NA_character_, Enrollment)),
    suppressed = Enrollment == "*",
    year = sub(".*_(\\d{4})_.*", "\\1", path)
  )]

  dfr[, state_name := .state_name_from_abb(state_abb)]

  # county_enrollment sums the reported counts; n_suppressed counts CMS "*"
  # cells (1-10 enrollees each). Keys with only suppressed cells are kept
  # for individual-market MA plans, the only ones the analytic tables use;
  # drug-only (S) and employer (800-series) plans list nearly every county
  # with a suppressed count, which would multiply the file size many times.
  dfr[, .(county_enrollment = sum(county_enrollment, na.rm = TRUE),
          n_suppressed = sum(suppressed)),
      .(contract_id, plan_id, county_name, state_name, year)
  ][county_enrollment != 0 |
      (n_suppressed > 0 & !startsWith(contract_id, "S") & plan_id < 800)]
}


# --- Step 2: Landscape -----------------------------------------------

#' Create Consolidated Medicare Advantage Landscape File
#'
#' Compiles and harmonizes Medicare Advantage (MA) landscape data from the
#' CMS Landscape files for \code{years}. CY2016-CY2023 are read from their
#' folders, CY2024 from its fixed file set, and CY2025+ from the files
#' registered in \code{MAEXITS_LANDSCAPE_FILES} (R/config.R). The result
#' includes plan identifiers, geographic service areas, plan types, and SNP
#' designations in a consistent cross-year structure.
#'
#' @param save Logical. If TRUE (default), writes the combined dataset
#'   to \code{trunk/derived/landscape.csv}.
#' @param years Integer vector of contract years. Defaults to
#'   \code{MAEXITS_LANDSCAPE_FIRST_YEAR} through the last crosswalk year.
#' @return Invisibly returns the combined data.table with columns:
#'   state_name, county_name, contract_id, plan_id, segment_id,
#'   plan_type, snp, snp_type, year.
#' @export
make_landscape <- function(save = TRUE, years = .landscape_years()) {
  years <- sort(as.integer(years))
  parts <- lapply(years, function(cy) {
    if (cy <= 2023L) {
      .get_landscape_2016to2023(paste0("CY", cy))
    } else if (cy == 2024L) {
      .get_landscape_2024()
    } else {
      .get_landscape_combined(cy)
    }
  })

  # Combine all years
  dfl <- rbindlist(parts, use.names = TRUE)
  setcolorder(dfl, c("state_name", "county_name", "contract_id", "plan_id",
                     "segment_id", "plan_type", "snp", "snp_type", "year",
                     "dsnp_integration"))
  dfl[, year := as.numeric(gsub("CY", "", year))]
  dfl <- dfl[county_name != ""]
  dfl <- dfl[!plan_type %in% c("PDP", "Medicare-Medicaid Plan")]
  dfl <- .harmonize_county_names(dfl)
  dfl <- unique(dfl)

  # Every year must be present (historical minimum: 43,631 non-PDP rows)
  counts <- dfl[, .N, by = year]
  for (cy in years) {
    .vcheck(isTRUE(counts[year == cy, N] > 10000),
            "landscape CY%d has %d non-PDP rows after cleaning (expected tens of thousands)",
            cy, if (length(counts[year == cy, N])) counts[year == cy, N] else 0L)
  }

  if (save) {
    fwrite(dfl, here("trunk", "derived", "landscape.csv"))
  }

  invisible(dfl)
}


# --- Step 3: Analytic Table ------------------------------------------

#' Attach CPSC enrollment to plan-county rows
#'
#' Adds `<side>_enrollment`, `<side>_enrollment_low` and `<side>_src`.
#' Reported counts are used as is; each CMS-suppressed cell counts as 10 in
#' `<side>_enrollment` and 1 in `<side>_enrollment_low`; a plan-county with no
#' CPSC record gets 0.
#'
#' @param dt Rows to attach to.
#' @param enr Enrollment table from make_enrollment() for one month.
#' @param by Contract and plan columns of `dt` to match on.
#' @param side "dec" or "jan".
#' @return `dt` with the three columns added.
#' @keywords internal
.attach_enrollment <- function(dt, enr, by, side) {
  .vcheck("n_suppressed" %in% names(enr),
          "enrollment table has no n_suppressed column; rebuild it with make_enrollment()")
  e <- enr[, .(.k_contract = contract_id, .k_plan = as.numeric(plan_id),
               county_name, state_name, .rep = county_enrollment, .nsup = n_suppressed)]
  dt <- merge(dt, e, by.x = c(by, "county_name", "state_name"),
              by.y = c(".k_contract", ".k_plan", "county_name", "state_name"),
              all.x = TRUE, sort = FALSE)
  found <- !is.na(dt$.rep)
  dt[, (paste0(side, "_enrollment")) := fifelse(found, .rep + 10 * .nsup, 0)]
  dt[, (paste0(side, "_enrollment_low")) := fifelse(found, .rep + .nsup, 0)]
  dt[, (paste0(side, "_src")) := fcase(!found, "no_record",
                                       .nsup == 0, "reported",
                                       .rep == 0, "suppressed",
                                       default = "mixed")]
  dt[, c(".rep", ".nsup") := NULL]
  dt
}

#' Build Core Analytic Table
#'
#' Links plan crosswalks, landscape data, and enrollment files across
#' December-to-January transitions for each crosswalk year in
#' \code{xwalk_years}. Produces a plan-county-year level dataset with
#' December enrollment, January enrollment, and plan transition status.
#'
#' @param landscape data.table or NULL. If NULL, reads from
#'   \code{trunk/derived/landscape.csv}.
#' @param dec_enrollment data.table or NULL. If NULL, reads from
#'   \code{trunk/derived/december_enrollment.csv}.
#' @param jan_enrollment data.table or NULL. If NULL, reads from
#'   \code{trunk/derived/january_enrollment.csv}.
#' @param save Logical. If TRUE (default), writes the analytic table
#'   to \code{trunk/derived/analytictable.csv}.
#' @param xwalk_years Integer vector of crosswalk years (default
#'   \code{MAEXITS_XWALK_YEARS}). Crosswalk year N covers December N-1 to
#'   January N.
#' @param preliminary Logical. If TRUE, no January data is used:
#'   \code{jan_enrollment} is NA and the January-only rows (SAE expansion
#'   counties, new plans) are omitted. Exit measures need only the
#'   pre-exit month. Used by \code{run_preliminary()}.
#' @return Invisibly returns the analytic data.table with columns:
#'   dec_year, contract_id, plan_id, segment_id, county_name, state_name,
#'   dec_enrollment, jan_enrollment, status, multi_status.
#' @export
make_analytic <- function(landscape = NULL, dec_enrollment = NULL,
                          jan_enrollment = NULL, save = TRUE,
                          xwalk_years = MAEXITS_XWALK_YEARS,
                          preliminary = FALSE) {
  .vcheck(!(preliminary && save), paste0(
    "make_analytic(preliminary = TRUE) must not overwrite trunk/derived/",
    "analytictable.csv; use save = FALSE (run_preliminary() does)"))
  .vcheck(!anyDuplicated(xwalk_years), "duplicated crosswalk year(s): %s",
          paste(xwalk_years[duplicated(xwalk_years)], collapse = ", "))
  dfl <- .load_if_null(landscape, "trunk/derived/landscape.csv")
  dfe_dec <- .load_if_null(dec_enrollment, "trunk/derived/december_enrollment.csv")
  dfe_dec[, year := as.character(year)]
  dfe_jan <- NULL
  if (!preliminary) {
    dfe_jan <- .load_if_null(jan_enrollment, "trunk/derived/january_enrollment.csv")
    dfe_jan[, year := as.character(year)]
  }

  # Stop before any imputation if a needed year is absent
  .check_year_coverage(dfl, dfe_dec, dfe_jan, xwalk_years)

  # Every landscape county must match a CMS enrollment county by name
  .check_county_names(dfl, dfe_dec, dfe_jan, xwalk_years, known = .fips_lookup())

  term_statuses <- .statuses("terminated")
  out_cols <- c("dec_year", "contract_id", "plan_id", "segment_id", "county_name",
                "state_name", "status", "curr_contract_id", "curr_plan_id",
                "dec_enrollment", "dec_enrollment_low", "dec_src",
                "jan_enrollment", "jan_enrollment_low", "jan_src",
                "plan_type", "snp", "snp_type", "dsnp_integration")

  dfh <- list()

  # Crosswalk year N: PREVIOUS = December year N-1, CURRENT = January year N
  for (xwalk_year in as.integer(xwalk_years)) {
    dec_year <- xwalk_year - 1L
    jan_year <- xwalk_year

    # Read crosswalk
    dfx <- .read_crosswalk(xwalk_year)
    .check_xwalk_content(dfx, dfl, xwalk_year)
    .check_xwalk_new_rows(dfx, xwalk_year)
    .check_new_plans_landscape(dfx, dfl, xwalk_year)

    enr_dec <- dfe_dec[year == as.character(dec_year)]
    enr_jan <- if (!preliminary) dfe_jan[year == as.character(jan_year)]

    # December plan-counties, one row per crosswalk link. A prior plan with
    # several crosswalk rows (a split, or a move into another contract)
    # repeats its December enrollment on each row; augment_analytic() adds
    # columns for counting each plan-county once.
    dfm <- merge(dfl[year == dec_year], dfx,
                 by.x = c("contract_id", "plan_id"),
                 by.y = c("prev_contract", "prev_plan"),
                 allow.cartesian = TRUE)
    dfm <- .attach_enrollment(dfm, enr_dec, c("contract_id", "plan_id"), "dec")
    .check_imputation(dfm$dec_src, "December", xwalk_year)
    .check_dec_new_plans(dfl, enr_dec, dec_year)

    if (preliminary) {
      dfm[, c("jan_enrollment", "jan_enrollment_low") := NA_real_]
      dfm[, jan_src := NA_character_]
    } else {
      # January enrollment of the successor plan in the same county
      dfm <- .attach_enrollment(dfm, enr_jan, c("curr_contract", "curr_plan"), "jan")
      .check_imputation(dfm[!status %in% term_statuses, jan_src], "January", xwalk_year)
      # A terminated plan has no January enrollment
      dfm[status %in% term_statuses,
          `:=`(jan_enrollment = 0, jan_enrollment_low = 0, jan_src = "terminated")]
    }
    dfm[status %in% term_statuses, `:=`(curr_contract = NA_character_, curr_plan = NA_real_)]
    setnames(dfm, c("curr_contract", "curr_plan"), c("curr_contract_id", "curr_plan_id"))
    dfm[, dec_year := dec_year]
    dfm_out <- dfm[, out_cols, with = FALSE]

    if (preliminary) {
      # SAE expansion counties and new plans only carry January enrollment
      dfh[[length(dfh) + 1]] <- dfm_out
      next
    }

    dfl_jan <- dfl[year == jan_year]

    # New plans (placeholder PREVIOUS_PLAN_ID such as "NEW" parses to NA),
    # in the counties of their January service area. Plans outside the
    # landscape universe (standalone drug plans, employer plans) drop out.
    dfnew <- dfx[is.na(prev_plan)]
    .check_new_plans(dfnew, enr_jan, xwalk_year)
    newp <- merge(unique(dfnew[, .(curr_contract, curr_plan, status)]), dfl_jan,
                  by.x = c("curr_contract", "curr_plan"),
                  by.y = c("contract_id", "plan_id"))
    newp <- .attach_enrollment(newp, enr_jan, c("curr_contract", "curr_plan"), "jan")
    message(paste0("Crosswalk ", xwalk_year, ": ",
                   uniqueN(newp[, .(curr_contract, curr_plan)]),
                   " new plans in the January landscape"))

    # January plan-counties of continuing plans that no December row
    # reaches: service-area expansions, counties a consolidation or renewal
    # successor picks up, and successors whose predecessor is outside the
    # December universe (e.g., a Medicare-Medicaid Plan that became a D-SNP).
    # Each successor is labelled with its crosswalk status (expansion first).
    cont <- unique(dfx[!is.na(prev_plan) & !is.na(curr_plan) & !status %in% term_statuses,
                       .(curr_contract, curr_plan, status)])
    cont[, prio := match(MAEXITS_XWALK_STATUS_CLASS[status],
                         c("service_area_expansion", "service_area_reduction", "continuing"))]
    cont <- cont[order(prio)][, .SD[1], by = .(curr_contract, curr_plan)][, prio := NULL]
    reached <- unique(rbind(
      dfm[!is.na(curr_contract_id), .(curr_contract = curr_contract_id,
                                      curr_plan = curr_plan_id, county_name, state_name)],
      newp[, .(curr_contract, curr_plan, county_name, state_name)]))
    expansion <- merge(cont, dfl_jan, by.x = c("curr_contract", "curr_plan"),
                       by.y = c("contract_id", "plan_id"))
    expansion <- expansion[!reached, on = .(curr_contract, curr_plan, county_name, state_name)]
    expansion <- .attach_enrollment(expansion, enr_jan, c("curr_contract", "curr_plan"), "jan")
    dfexp_out <- expansion[, .(
      dec_year = dec_year, contract_id = curr_contract, plan_id = curr_plan,
      segment_id, county_name, state_name, status,
      curr_contract_id = curr_contract, curr_plan_id = curr_plan,
      dec_enrollment = NA_real_, dec_enrollment_low = NA_real_, dec_src = NA_character_,
      jan_enrollment, jan_enrollment_low, jan_src,
      plan_type, snp, snp_type, dsnp_integration)]
    message(paste0("Crosswalk ", xwalk_year, ": ", nrow(dfexp_out),
                   " January-only county-plan rows added for continuing plans"))

    dfnew_out <- newp[, .(
      dec_year = dec_year, contract_id = curr_contract, plan_id = curr_plan,
      segment_id, county_name, state_name, status,
      curr_contract_id = curr_contract, curr_plan_id = curr_plan,
      dec_enrollment = NA_real_, dec_enrollment_low = NA_real_, dec_src = NA_character_,
      jan_enrollment, jan_enrollment_low, jan_src,
      plan_type, snp, snp_type, dsnp_integration)]

    dfh[[length(dfh) + 1]] <- rbind(dfm_out, dfexp_out, dfnew_out, use.names = TRUE)
  }

  df <- rbindlist(dfh, use.names = TRUE)
  df[, `:=`(dec_year = as.integer(dec_year), plan_id = as.integer(plan_id),
            curr_plan_id = as.integer(curr_plan_id))]

  # Flag plan-segment-counties with more than one crosswalk link
  df[, multi_status := .N > 1,
     by = .(dec_year, contract_id, plan_id, segment_id, county_name, state_name)]
  message(paste0(sum(df$multi_status),
    " rows on plan-segment-counties with more than one crosswalk link (multi_status; ",
    uniqueN(df[multi_status == TRUE, paste(dec_year, contract_id, plan_id)]),
    " plan-years)"))

  if (save) {
    fwrite(df, here("trunk", "derived", "analytictable.csv"))
  }

  invisible(df)
}
