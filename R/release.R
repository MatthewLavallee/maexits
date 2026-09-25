# ============================================================
# release.R — Package the derived tables for a GitHub data release
#
# Maintainer tool, run in a clone of the repository after
# run_data_pipeline(). It writes the files maexits_data() downloads, plus a
# manifest with row counts and md5 checksums. Upload them to a GitHub
# release and point MAEXITS_DATA_RELEASE (R/config.R) at it, e.g.:
#
#   prepare_data_release("/tmp/release")
#   # gh release create <release-name> /tmp/release/* --title "..." --notes "..."
# ============================================================


#' Prepare the Files for a GitHub Data Release
#'
#' Maintainer tool. Reads the derived tables in `derived_dir` and writes, to
#' `out_dir`, the compressed files that [maexits_data()] downloads
#' (`county_panel.rds`, `plan_county.rds`, `landscape.rds`,
#' `enrollment.rds`) and a `manifest.csv` with row counts and md5
#' checksums. It adds a `plan_key` column ("H1234-001") to the plan-level
#' tables and a `fips` column to the landscape and enrollment tables. Values
#' are otherwise unchanged. Needs `raw/` for the FIPS lookup.
#'
#' Upload every file in `out_dir` to a GitHub release named `release`, and
#' set `MAEXITS_DATA_RELEASE` in R/config.R to that name.
#'
#' @param out_dir Folder to write the release files to.
#' @param derived_dir Folder with the derived CSVs (default `trunk/derived`).
#' @param release Release name recorded in the manifest.
#' @return Invisibly, the manifest as a data.frame.
#' @export
prepare_data_release <- function(out_dir, derived_dir = here("trunk", "derived"),
                                 release = MAEXITS_DATA_RELEASE) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  src <- function(f) file.path(derived_dir, f)
  lookup <- .fips_lookup()

  add_plan_key <- function(dt) {
    dt[, plan_key := fifelse(is.na(plan_id), NA_character_,
                             sprintf("%s-%03d", contract_id, as.integer(plan_id)))]
    setcolorder(dt, "plan_key", after = "plan_id")
  }
  attach_fips <- function(dt) {
    dt[, fips := NA_integer_]
    dt[lookup, fips := i.fips, on = .(county_name, state_name)]
  }

  tables <- list()
  tables$county_panel <- .load_if_null(fread(src("county_panel.csv")), "")
  plan_county <- add_plan_key(fread(src("analytictable_augmented.csv")))
  plan_county[, curr_plan_key := fifelse(is.na(curr_plan_id), NA_character_,
                                         sprintf("%s-%03d", curr_contract_id, as.integer(curr_plan_id)))]
  setcolorder(plan_county, "curr_plan_key", after = "curr_plan_id")
  tables$plan_county <- plan_county

  landscape <- fread(src("landscape.csv"))
  attach_fips(landscape)
  tables$landscape <- add_plan_key(landscape)

  dec <- fread(src("december_enrollment.csv"))[, month := sprintf("%d-12", year)]
  jan <- fread(src("january_enrollment.csv"))[, month := sprintf("%d-01", year)]
  enrollment <- rbind(dec, jan)
  setcolorder(enrollment, c("month", "year"))
  attach_fips(enrollment)
  tables$enrollment <- add_plan_key(enrollment)

  commit <- tryCatch(system2("git", c("-C", shQuote(here()), "rev-parse", "--short", "HEAD"),
                             stdout = TRUE, stderr = FALSE), error = function(e) NA_character_)
  if (!length(commit)) commit <- NA_character_

  manifest <- do.call(rbind, lapply(names(tables), function(nm) {
    file <- .DATASETS[[nm]]$file
    path <- file.path(out_dir, file)
    saveRDS(as.data.frame(tables[[nm]]), path, compress = "xz")
    data.frame(dataset = nm, file = file, rows = nrow(tables[[nm]]),
               columns = ncol(tables[[nm]]), bytes = file.size(path),
               md5 = unname(tools::md5sum(path)), release = release,
               built_on = format(Sys.Date()), pipeline_commit = commit[1],
               stringsAsFactors = FALSE)
  }))
  utils::write.csv(manifest, file.path(out_dir, "manifest.csv"), row.names = FALSE)
  message("Wrote ", nrow(manifest), " datasets and manifest.csv to ", out_dir)
  invisible(manifest)
}
