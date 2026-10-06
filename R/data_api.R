# ============================================================
# data_api.R — Load the processed datasets and any month of CMS enrollment
#
#   maexits_catalog()   which datasets and variables exist
#   maexits_data()      the pipeline's processed tables, downloaded once
#                       from the package's GitHub data release, then cached
#   cms_enrollment()    CMS Monthly Enrollment by Contract/Plan/State/County
#                       for any month from December 2019, downloaded once
#                       from CMS, then cached
#
# All three filter by plan, year or month, state, county and variable.
# Downloads are cached in maexits_cache_dir(); set
# options(maexits.cache_dir = ...) to use another folder.
# ============================================================


# Datasets in the GitHub data release
.DATASETS <- list(
  county_panel = list(
    file = "county_panel.rds", year_col = "dec_year", plans = FALSE, months = FALSE,
    keys = c("county_name", "state_name", "fips", "dec_year"),
    title = "County x transition year (December dec_year -> January dec_year + 1)"),
  plan_county = list(
    file = "plan_county.rds", year_col = "dec_year", plans = TRUE, months = FALSE,
    keys = c("dec_year", "contract_id", "plan_id", "plan_key", "segment_id",
             "county_name", "state_name", "fips"),
    title = "Plan x county x transition year: crosswalk status, enrollment, forced exits"),
  landscape = list(
    file = "landscape.rds", year_col = "year", plans = TRUE, months = FALSE,
    keys = c("year", "contract_id", "plan_id", "plan_key", "segment_id",
             "county_name", "state_name", "fips"),
    title = "Plan service areas by contract year (CY2016 on)"),
  enrollment = list(
    file = "enrollment.rds", year_col = "year", plans = TRUE, months = TRUE,
    keys = c("month", "contract_id", "plan_id", "plan_key", "county_name",
             "state_name", "fips"),
    title = "December and January plan x county enrollment as used by the pipeline"),
  displacement = list(
    file = "displacement.rds", year_col = "dec_year", plans = TRUE, months = FALSE,
    keys = c("dec_year", "contract_id", "plan_id", "plan_key", "segment_id",
             "county_name", "state_name", "fips"),
    title = "December plan x county: outcome in January, lost coverage, plan details"),
  exits = list(
    file = "exits.rds", year_col = "dec_year", plans = TRUE, months = FALSE,
    keys = c("dec_year", "contract_id", "plan_id", "plan_key", "segment_id",
             "county_name", "state_name", "fips"),
    title = "December plan x county: terminated or service-area reduction, December and September enrollment"),
  pdp_exits = list(
    file = "pdp_exits.rds", year_col = "dec_year", plans = TRUE, months = FALSE, geo = "state",
    keys = c("dec_year", "contract_id", "plan_id", "plan_key", "state", "state_name"),
    title = "December standalone Part D plan x state: terminated, December and September enrollment"),
  plan_details = list(
    file = "plan_details.rds", year_col = "year", plans = TRUE, months = FALSE, geo = FALSE,
    keys = c("year", "contract_id", "plan_id", "plan_key", "segment_id"),
    title = "Plan x segment x contract year: organization, premiums, MOOP, stars, SNP details")
)

.CMS_KEYS <- c("month", "contract_id", "plan_id", "plan_key", "state", "county", "fips")

.maexits_session <- new.env(parent = emptyenv())


.api_stop <- function(...) stop(sprintf(...), call. = FALSE)


# The variable key and maexits_catalog() are in catalog.R.


# --- Processed datasets (GitHub data release) ------------------------------

#' Load a Processed Dataset
#'
#' Loads one of the pipeline's processed tables and keeps only the plans,
#' years or months, states, counties and variables asked for. The table is
#' downloaded once from the package's GitHub data release (a few MB to
#' about 30 MB) and cached in [maexits_cache_dir()].
#'
#' Datasets (each has a help page listing every variable, e.g.
#' `?plan_county`; [maexits_catalog()] has the same list as a table):
#' * `"county_panel"` ([county_panel]): county x transition year. Exit rate,
#'   displaced enrollment, December and January enrollment, 2018
#'   penetration, benchmark.
#' * `"plan_county"` ([plan_county]): plan x county x transition year, one
#'   row per crosswalk link. Crosswalk status, December and January
#'   enrollment, forced-exit flags, role.
#' * `"displacement"` ([displacement]): one row per December plan x county,
#'   with its December enrollment, what happened to it in January
#'   (`outcome`, `lost_coverage`), how far the exit reached, the county's
#'   plans next January, and the plan's details. Built for counting who
#'   lost their plan; each plan-county appears once.
#' * `"exits"` ([exits]): one row per December plan x county, with whether
#'   CMS terminated the plan or cut the county from its service area the
#'   next January, and its December and September enrollment. September
#'   covers the newest transition before its December file is out.
#' * `"pdp_exits"` ([pdp_exits]): one row per December standalone Part D
#'   plan (PDP) x state, with whether CMS terminated it the next January,
#'   and its December and September enrollment. Filter it by `states`, not
#'   `counties`.
#' * `"plan_details"` ([plan_details]): plan x segment x contract year:
#'   organization and parent, premiums, deductible, out-of-pocket maximum,
#'   star ratings, SNP details.
#' * `"landscape"` ([landscape]): plan service areas by contract year, with
#'   plan type, SNP type and D-SNP integration status.
#' * `"enrollment"` ([enrollment]): the December and January plan x county
#'   enrollment the pipeline uses (December 2018-2025, January 2018-2026).
#'   For other months use [cms_enrollment()].
#'
#' A transition is indexed by its December year: `dec_year` 2025 is the
#' December 2025 to January 2026 transition.
#'
#' Some plan_county enrollment values repeat across rows, because a plan
#' can have several crosswalk links; the `counting` column of
#' [maexits_catalog()] flags them and names the matching columns that
#' count each plan-county once (see the "Counting rows" section of
#' [plan_county]).
#'
#' @param dataset One of `"county_panel"`, `"plan_county"`, `"displacement"`,
#'   `"exits"`, `"pdp_exits"`, `"plan_details"`, `"landscape"`, `"enrollment"`.
#' @param plans Plans to keep: `"H1234-001"` for one plan, `"H1234"` for
#'   every plan in a contract. Not available for the county panel.
#' @param years Years to keep: `dec_year` for `county_panel`,
#'   `plan_county`, `displacement`, `exits` and `pdp_exits`, contract year for `plan_details` and
#'   `landscape`, calendar year for `enrollment`.
#' @param months `enrollment` only: months as `"YYYY-MM"` strings or Dates.
#' @param states State names or postal abbreviations (`"MD"`, `"Maryland"`).
#' @param counties County FIPS codes (`24005` or `"24005"`) or county names
#'   (exact match, not case-sensitive; combine with `states` to
#'   disambiguate).
#' @param variables Variables to keep in addition to the identifying
#'   columns; `NULL` keeps all.
#' @param release Data release to load (default: the release this version
#'   of the package was built for).
#' @param refresh If `TRUE`, download again even if a cached copy exists.
#' @return A data.table. The first time in a session that a dataset is
#'   returned with columns whose values repeat across rows, a message names
#'   them; `options(maexits.counting_note = FALSE)` turns it off.
#' @examples
#' \dontrun{
#' # Exit rates for Maryland counties, 2024 and 2025 transitions
#' maexits_data("county_panel", years = 2024:2025, states = "MD",
#'              variables = c("exit_rate", "displaced_enrollment"))
#'
#' # Every plan in two contracts, with their exit flags
#' maexits_data("plan_county", plans = c("H2001", "H3447-020"), years = 2025,
#'              variables = c("status", "forced_county", "dec_enrollment_once"))
#'
#' # December 2025 enrollment for one county
#' maexits_data("enrollment", months = "2025-12", counties = 24005)
#' }
#' @export
maexits_data <- function(dataset = c("county_panel", "plan_county", "displacement", "exits",
                                     "pdp_exits", "plan_details", "landscape", "enrollment"),
                         plans = NULL, years = NULL, months = NULL, states = NULL,
                         counties = NULL, variables = NULL,
                         release = MAEXITS_DATA_RELEASE, refresh = FALSE) {
  dataset <- match.arg(dataset)
  spec <- .DATASETS[[dataset]]
  if (!is.null(plans) && !spec$plans) {
    .api_stop("%s is county-level and has no plans; use plan_county for plan-level data", dataset)
  }
  if (!is.null(months) && !spec$months) {
    .api_stop("months applies to the enrollment dataset and cms_enrollment(); use years for %s", dataset)
  }
  if (identical(spec$geo, "state") && !is.null(counties)) {
    .api_stop("%s is by plan and state (PDPs are offered state-wide); filter it by states", dataset)
  }
  if (isFALSE(spec$geo) && (!is.null(states) || !is.null(counties))) {
    .api_stop("%s has no counties (one row per plan segment); filter displacement by state or county instead",
              dataset)
  }
  dt <- .load_release_table(dataset, release, refresh)
  dt <- .apply_filters(dt, plans = plans, years = years, year_col = spec$year_col,
                       months = months, states = states, counties = counties,
                       county_col = "county_name")
  dt <- .select_variables(dt, variables, spec$keys, dataset)
  .note_counting(dataset, names(dt))
  dt[]
}


# Once per session and dataset, name the returned columns whose values
# repeat across rows (the `counting` column of the variable key).
.note_counting <- function(dataset, cols) {
  key <- .catalog_table()
  want <- dataset   # "dataset" inside key[...] would mean the column
  key <- key[key$dataset == want & key$counting != "" & key$variable %in% cols]
  seen <- .maexits_session$counting_noted
  if (!nrow(key) || dataset %in% seen || isFALSE(getOption("maexits.counting_note", TRUE))) {
    return(invisible())
  }
  .maexits_session$counting_noted <- c(seen, dataset)
  message(sprintf(paste0(
    "Note: some columns (%s) repeat across rows or are built from sums over ",
    "them. See ?%s or maexits_catalog(\"%s\")$counting for how to add them ",
    "up. options(maexits.counting_note = FALSE) turns this note off."),
    paste(key$variable, collapse = ", "), dataset, dataset))
}


#' Folder Where Downloads Are Cached
#'
#' Defaults to the user cache folder for the package
#' (`tools::R_user_dir("maexitsv2", "cache")`); set
#' `options(maexits.cache_dir = "path")` to use another.
#'
#' @return The folder path.
#' @export
maexits_cache_dir <- function() {
  getOption("maexits.cache_dir", tools::R_user_dir("maexitsv2", "cache"))
}


#' Delete Cached Downloads
#'
#' @param what `"all"`, `"releases"` (processed datasets) or `"cms"` (CMS
#'   monthly enrollment).
#' @return Invisibly, the folders removed.
#' @export
maexits_clear_cache <- function(what = c("all", "releases", "cms")) {
  what <- match.arg(what)
  dirs <- file.path(maexits_cache_dir(),
                    switch(what, all = c("releases", "cms"), releases = "releases", cms = "cms"))
  unlink(dirs, recursive = TRUE)
  rm(list = ls(.maexits_session), envir = .maexits_session)
  invisible(dirs)
}


.release_url <- function(release) {
  getOption("maexits.release_url",
            sprintf("https://github.com/%s/releases/download/%s", MAEXITS_DATA_REPO, release))
}


.load_release_table <- function(dataset, release, refresh) {
  key <- paste(release, dataset)
  if (!refresh && !is.null(.maexits_session[[key]])) return(.maexits_session[[key]])
  path <- .release_file(dataset, release, refresh)
  dt <- setDT(readRDS(path))
  .maexits_session[[key]] <- dt
  dt
}


.release_manifest <- function(release, refresh) {
  dest <- file.path(maexits_cache_dir(), "releases", release, "manifest.csv")
  if (refresh || !file.exists(dest)) {
    .download(paste0(.release_url(release), "/manifest.csv"), dest)
  }
  utils::read.csv(dest, stringsAsFactors = FALSE)
}


.release_file <- function(dataset, release, refresh) {
  manifest <- .release_manifest(release, refresh)
  # A release can gain datasets after its manifest was cached: check it again
  if (!refresh && !dataset %in% manifest$dataset) manifest <- .release_manifest(release, TRUE)
  row <- manifest[manifest$dataset == dataset, , drop = FALSE]
  if (nrow(row) != 1) .api_stop("data release %s has no %s dataset", release, dataset)
  dest <- file.path(maexits_cache_dir(), "releases", release, row$file)
  if (refresh || !file.exists(dest)) {
    message(sprintf("Downloading %s (%.1f MB) from data release %s ...",
                    row$file, row$bytes / 1e6, release))
    .download(paste0(.release_url(release), "/", row$file), dest)
    got <- unname(tools::md5sum(dest))
    if (!identical(got, row$md5)) {
      unlink(dest)
      .api_stop("download of %s is corrupt (md5 %s, expected %s); try again", row$file, got, row$md5)
    }
  }
  dest
}


.download <- function(url, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  part <- paste0(dest, ".part")
  old <- options(timeout = max(1800, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  status <- tryCatch(utils::download.file(url, part, mode = "wb", quiet = TRUE),
                     error = function(e) conditionMessage(e),
                     warning = function(w) conditionMessage(w))
  if (!identical(status, 0L) || !file.exists(part)) {
    unlink(part)
    .api_stop("could not download %s%s", url,
              if (is.character(status)) paste0(" (", status, ")") else "")
  }
  file.rename(part, dest)
  invisible(dest)
}


# --- CMS monthly enrollment -------------------------------------------------

#' Load CMS Monthly Enrollment for Any Month
#'
#' Loads CMS "Monthly Enrollment by Contract/Plan/State/County" (CPSC) for
#' the requested months, joined to the month's contract information (plan
#' type, organization, parent organization, SNP and employer flags). Each
#' month is downloaded once from CMS (about 30-40 MB) and cached in
#' [maexits_cache_dir()]. The CMS file covers every plan type, including
#' standalone drug plans; use `ma_only = TRUE` to drop them.
#'
#' CMS publishes months from December 2019 on under a standard address. For
#' an earlier month, download its zip from the CMS page and pass it as
#' `zipfile`.
#'
#' @param months Months as `"YYYY-MM"` strings or Dates, e.g.
#'   `c("2025-09", "2025-12")` or `seq(as.Date("2025-01-01"), by = "month",
#'   length.out = 12)`.
#' @param plans Plans to keep: `"H1234-001"` or `"H1234"` (whole contract).
#' @param states State names or postal abbreviations.
#' @param counties County FIPS codes or county names.
#' @param variables Variables to keep in addition to month, plan and
#'   county identifiers; `NULL` keeps all. See
#'   `maexits_catalog("cms_enrollment")`.
#' @param ma_only If `TRUE`, drop standalone Part D plans (plan types
#'   "Medicare Prescription Drug Plan" and "Employer/Union Only Direct
#'   Contract PDP").
#' @param zipfile Optional path to a CMS CPSC zip for a single month, used
#'   instead of downloading.
#' @param refresh If `TRUE`, download again even if cached.
#' @return A data.table with one row per month, plan and county.
#' @examples
#' \dontrun{
#' # September 2026 MA enrollment in Maryland, with plan type and parent org
#' cms_enrollment("2026-09", states = "MD", ma_only = TRUE,
#'                variables = c("enrollment", "plan_type", "parent_organization"))
#'
#' # Monthly enrollment in one contract through 2025
#' cms_enrollment(seq(as.Date("2025-01-01"), by = "month", length.out = 12),
#'                plans = "H2001", variables = "enrollment")
#' }
#' @export
cms_enrollment <- function(months, plans = NULL, states = NULL, counties = NULL,
                           variables = NULL, ma_only = FALSE, zipfile = NULL,
                           refresh = FALSE) {
  months <- .normalize_months(months)
  if (!is.null(zipfile) && length(months) != 1) {
    .api_stop("zipfile holds one month; request a single month with it")
  }
  pdp <- c("Medicare Prescription Drug Plan", "Employer/Union Only Direct Contract PDP")
  parts <- lapply(months, function(m) {
    x <- .cms_month(m, zipfile, refresh)
    e <- .apply_filters(x$enrollment, plans = plans, states = states,
                        counties = counties, county_col = "county")
    e <- merge(e, x$contracts, by = c("contract_id", "plan_id"), all.x = TRUE, sort = FALSE)
    if (ma_only) e <- e[!plan_type %in% pdp]
    e
  })
  dt <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  front <- c(.CMS_KEYS, "state_name", "ssa_code", "enrollment", "suppressed")
  setcolorder(dt, c(intersect(front, names(dt)), setdiff(names(dt), front)))
  dt <- .select_variables(dt, variables, .CMS_KEYS, "cms_enrollment")
  dt[]
}


.normalize_months <- function(months) {
  if (inherits(months, "Date")) return(unique(format(months, "%Y-%m")))
  m <- trimws(as.character(months))
  ok <- grepl("^[0-9]{4}-[0-9]{2}(-[0-9]{2})?$", m) &
    suppressWarnings(as.integer(substr(m, 6, 7))) %in% 1:12
  if (!all(ok)) {
    .api_stop("months must look like \"2025-09\" (or be Dates); got: %s",
              paste(months[!ok], collapse = ", "))
  }
  unique(substr(m, 1, 7))
}


.cms_zip_url <- function(month) {
  sprintf("%s/monthly-enrollment-cpsc-%s-%s.zip",
          getOption("maexits.cms_url", MAEXITS_CMS_ZIP_URL),
          tolower(month.name[as.integer(substr(month, 6, 7))]), substr(month, 1, 4))
}


.cms_month <- function(month, zipfile, refresh) {
  key <- paste("cms", month)
  dest <- file.path(maexits_cache_dir(), "cms", sprintf("cpsc_%s.rds", sub("-", "_", month)))
  if (is.null(zipfile) && !refresh) {
    if (!is.null(.maexits_session[[key]])) return(.maexits_session[[key]])
    if (file.exists(dest)) {
      x <- lapply(readRDS(dest), setDT)
      .maexits_session[[key]] <- x
      return(x)
    }
  }
  zip <- zipfile
  if (is.null(zip)) {
    if (month < MAEXITS_CMS_FIRST_MONTH) {
      .api_stop(paste0(
        "CMS publishes months before %s under older file names. Download the ",
        "%s zip from the CMS \"Monthly Enrollment by Contract/Plan/State/County\" ",
        "page and pass it as zipfile ="), MAEXITS_CMS_FIRST_MONTH, month)
    }
    zip <- tempfile(fileext = ".zip")
    on.exit(unlink(zip), add = TRUE)
    message("Downloading CMS enrollment for ", month, " ...")
    .download(.cms_zip_url(month), zip)
  }
  x <- .parse_cpsc_zip(zip, month)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  saveRDS(lapply(x, as.data.frame), dest)
  .maexits_session[[key]] <- x
  x
}


# Convert CMS column labels to snake_case ("Offers Part D" -> offers_part_d)
.snake <- function(x) gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(x)))


.parse_cpsc_zip <- function(zip, month) {
  ym <- sub("-", "_", month)
  files <- utils::unzip(zip, list = TRUE)$Name
  enr_file <- grep(sprintf("CPSC_Enrollment_Info_%s\\.csv$", ym), files, value = TRUE)
  con_file <- grep(sprintf("CPSC_Contract_Info_%s\\.csv$", ym), files, value = TRUE)
  if (length(enr_file) != 1 || length(con_file) != 1) {
    .api_stop("%s does not contain CPSC_Enrollment_Info_%s.csv and CPSC_Contract_Info_%s.csv; is it the %s file?",
              basename(zip), ym, ym, month)
  }
  exdir <- tempfile("cpsc_")
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  utils::unzip(zip, files = c(enr_file, con_file), exdir = exdir)

  e <- fread(file.path(exdir, enr_file), colClasses = "character")
  need <- c("Contract Number", "Plan ID", "SSA State County Code",
            "FIPS State County Code", "State", "County", "Enrollment")
  miss <- setdiff(need, names(e))
  if (length(miss)) .api_stop("%s is missing column(s): %s", enr_file, paste(miss, collapse = ", "))
  odd <- e[!Enrollment %in% c("*", "") & !grepl("^[0-9]+$", Enrollment), unique(Enrollment)]
  if (length(odd)) .api_stop("%s has unexpected Enrollment value(s): %s", enr_file,
                             paste(head(odd, 5), collapse = ", "))
  blank_na <- function(x) fifelse(x == "", NA_character_, x)
  enr <- e[, .(
    month = month,
    contract_id = `Contract Number`,
    plan_id = as.integer(blank_na(`Plan ID`)),
    state = State,
    county = County,
    fips = as.integer(blank_na(`FIPS State County Code`)),
    ssa_code = .pad0(blank_na(`SSA State County Code`), 5),
    enrollment = as.numeric(fifelse(Enrollment %in% c("*", ""), NA_character_, Enrollment)),
    suppressed = Enrollment == "*"
  )]
  enr[, plan_key := fifelse(is.na(plan_id), NA_character_,
                            sprintf("%s-%03d", contract_id, plan_id))]
  enr[, state_name := .state_name_from_abb(state)]

  con <- fread(file.path(exdir, con_file), colClasses = "character")
  setnames(con, .snake(names(con)))
  if (!all(c("contract_id", "plan_id") %in% names(con))) {
    .api_stop("%s has no Contract ID / Plan ID columns", con_file)
  }
  con[, plan_id := as.integer(blank_na(plan_id))]
  con <- unique(con, by = c("contract_id", "plan_id"))
  list(enrollment = enr, contracts = con)
}


# --- Filters -----------------------------------------------------------------

.parse_plans <- function(plans) {
  p <- toupper(trimws(as.character(plans)))
  ok <- grepl("^[A-Z][0-9]{4}(-[0-9]{1,3})?$", p)
  if (!all(ok)) {
    .api_stop("plans must look like \"H1234-001\" (one plan) or \"H1234\" (every plan in a contract); got: %s",
              paste(plans[!ok], collapse = ", "))
  }
  has_plan <- grepl("-", p)
  list(whole = p[!has_plan],
       single = paste(sub("-.*$", "", p[has_plan]), as.integer(sub("^.*-", "", p[has_plan]))))
}


.state_filter_names <- function(states) {
  s <- trimws(as.character(states))
  abb <- nchar(s) == 2
  names <- s
  names[abb] <- .state_name_from_abb(toupper(s[abb]))
  bad <- abb & is.na(names)
  if (any(bad)) .api_stop("unknown state abbreviation(s): %s", paste(s[bad], collapse = ", "))
  # CMS landscape files label DC "Washington D.C." in some years
  out <- tolower(names)
  if ("district of columbia" %in% out) out <- c(out, "washington d.c.")
  if (any(c("virgin islands", "u.s. virgin islands") %in% out)) {
    out <- unique(c(out, "virgin islands", "u.s. virgin islands"))
  }
  out
}


.apply_filters <- function(dt, plans = NULL, years = NULL, year_col = NULL, months = NULL,
                           states = NULL, counties = NULL, county_col = "county_name") {
  keep <- rep(TRUE, nrow(dt))
  filtered <- FALSE
  if (!is.null(plans)) {
    p <- .parse_plans(plans)
    keep <- keep & (dt$contract_id %in% p$whole |
                      paste(dt$contract_id, dt$plan_id) %in% p$single)
    filtered <- TRUE
  }
  if (!is.null(years)) {
    keep <- keep & dt[[year_col]] %in% as.integer(years)
    filtered <- TRUE
  }
  if (!is.null(months)) {
    keep <- keep & dt$month %in% .normalize_months(months)
    filtered <- TRUE
  }
  if (!is.null(states)) {
    keep <- keep & tolower(dt$state_name) %in% .state_filter_names(states)
    filtered <- TRUE
  }
  if (!is.null(counties)) {
    cty <- trimws(as.character(counties))
    is_fips <- grepl("^[0-9]{4,5}$", cty)
    keep <- keep & (dt$fips %in% as.integer(cty[is_fips]) |
                      tolower(dt[[county_col]]) %in% tolower(cty[!is_fips]))
    filtered <- TRUE
  }
  if (filtered) dt[keep] else copy(dt)
}


.select_variables <- function(dt, variables, keys, dataset) {
  if (is.null(variables)) return(dt)
  keys <- intersect(keys, names(dt))
  unknown <- setdiff(variables, names(dt))
  if (length(unknown)) {
    .api_stop("%s has no variable(s) %s. Available: %s", dataset,
              paste(unknown, collapse = ", "),
              paste(setdiff(names(dt), keys), collapse = ", "))
  }
  dt[, unique(c(keys, variables)), with = FALSE]
}
