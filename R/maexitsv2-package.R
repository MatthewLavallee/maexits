#' maexitsv2: Medicare Advantage Plan Exits and Forced Disenrollment
#'
#' @section Loading data:
#' * [maexits_catalog()] lists the datasets and their variables, with a
#'   `counting` note on variables whose values repeat across rows. Each
#'   dataset also has a help page: [displacement], [exits], [pdp_exits], [plan_details],
#'   [county_panel], [plan_county], [landscape], [enrollment],
#'   [cms_enrollment_data].
#' * [maexits_data()] loads the pipeline's processed tables (who lost their
#'   plan each year, who was in a plan CMS terminated or cut from the
#'   county, standalone Part D plan terminations, plan details, county
#'   panel, plan-by-county exits,
#'   landscape, December/January enrollment),
#'   downloading them once from the package's GitHub data release.
#' * [cms_enrollment()] loads CMS Monthly Enrollment by
#'   Contract/Plan/State/County for any month from December 2019,
#'   downloading it once from CMS.
#'
#' All loaders filter by plan, year or month, state, county and variable,
#' and cache downloads in [maexits_cache_dir()].
#'
#' @section Building data:
#' Building from the raw CMS and NBER files needs a clone of the
#' repository with `raw/` in place: see [check_inputs()],
#' [run_data_pipeline()] and [run_preliminary()], and the "Annual update"
#' runbook in PIPELINE.md.
#'
#' @section Knowing when new files are out:
#' [check_cms_releases()] checks whether next cycle's crosswalk, landscape,
#' CPSC enrollment files and ratebook are out; [schedule_release_check()]
#' runs that check daily with a desktop notification until they are, and
#' [release_check_status()] shows what it has found.
#'
#' @keywords internal
#' @import data.table
#' @importFrom here here
#' @importFrom stats cor median quantile
#' @importFrom tools md5sum R_user_dir
#' @importFrom utils download.file head tail unzip
"_PACKAGE"
