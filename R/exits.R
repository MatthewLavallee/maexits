# ============================================================
# exits.R — Who was in a plan CMS terminated or cut from their county
#
# One row per December plan x county (dec_year): the exit CMS's crosswalk
# gives the plan-county for the next January (terminated, service-area
# reduction, or none) and its enrollment in December and in September.
# September covers every registered crosswalk year, so the latest
# transition, whose December enrollment comes out in mid-December, can be
# compared with earlier years on the same footing.
# ============================================================


.EXIT_TYPES <- c("terminated", "service_area_reduction", "none")


#' One row per prior plan-county of an augmented analytic table
#' @return data.table keyed by dec_year, contract, plan, county and state
#'   with exit_type, xwalk_statuses and the December-side enrollment
#'   (enrollment, enrollment_low, src).
#' @keywords internal
.exit_rows <- function(a) {
  a <- as.data.table(a)[!is.na(dec_src)]
  a[, `:=`(dec_year = as.integer(dec_year), plan_id = as.integer(plan_id),
           segment_id = as.integer(segment_id))]
  a[, .term := unname(MAEXITS_XWALK_STATUS_CLASS[status]) %in% "terminated"]
  out <- a[, .(
    segment_id = min(segment_id), fips = fips[1],
    plan_type = plan_type[1], snp = snp[1], snp_type = snp_type[1],
    # CMS's labels only: a termination needs every link to be one (a plan
    # mapped to a New Plan that does not serve the county is not)
    exit_type = fcase(forced_reason[1] %in% "terminated" & all(.term), "terminated",
                      forced_reason[1] %in% "service_area_reduction", "service_area_reduction",
                      default = "none"),
    xwalk_statuses = paste(sort(unique(status)), collapse = " + "),
    enrollment = dec_enrollment[1], enrollment_low = dec_enrollment_low[1], src = dec_src[1]
  ), by = .(dec_year, contract_id, plan_id, county_name, state_name)]
  out[!snp %in% "Yes", snp_type := NA_character_]
  out
}


#' Augmented analytic table built on September enrollment
#'
#' For each crosswalk year N, the September N-1 CPSC file in
#' `raw/monthly enrollment/` stands in for December N-1, as in
#' [run_preliminary()].
#' @param xwalk_years Crosswalk years to build.
#' @param month Month that stands in for December.
#' @keywords internal
.proxy_augmented <- function(xwalk_years = MAEXITS_EXITS_YEARS, month = MAEXITS_EXITS_MONTH) {
  n_all <- as.integer(xwalk_years)
  landscape <- make_landscape(save = FALSE, years = (min(n_all) - 2L):max(n_all))
  rbindlist(lapply(n_all, function(n) {
    f <- .cpsc_files("monthly enrollment", n - 1L, month)
    .vcheck(length(f) == 1L, "raw/monthly enrollment needs exactly one CPSC_Enrollment_Info_%d_%02d.csv (found %d)",
            n - 1L, as.integer(month), length(f))
    suppressMessages({
      proxy <- .aggregate_cpsc(f, basename(f))
      a <- make_analytic(landscape, proxy, save = FALSE, xwalk_years = n, preliminary = TRUE)
      a <- add_fips_and_penetration(a, save = FALSE)
      a[, benchmark := NA_real_]
      augment_analytic(a, save = FALSE, verbose = FALSE, landscape = landscape, xwalk_years = n)
    })
  }), fill = TRUE)
}


#' Build the Exits Table
#'
#' One row per December plan and county for each transition (`dec_year`):
#' whether CMS's crosswalk terminated the plan or cut the county from its
#' service area for the next January, with the plan-county's enrollment in
#' December and in September of `dec_year`.
#'
#' `exit_type` uses CMS's crosswalk labels, checked against the January
#' landscape:
#' * `terminated`: every crosswalk link of the plan-county is a termination.
#' * `service_area_reduction`: the plan renews with a service-area reduction
#'   ("Renewal Plan with SAR"), and neither it nor any other plan the
#'   crosswalk maps it to serves the county in January.
#' * `none`: every other plan-county. This includes the plan-counties that
#'   [displacement] counts as having lost coverage because CMS moved the
#'   plan to one that does not serve the county, or renewed it without
#'   listing it in the county in January.
#'
#' The September columns cover `MAEXITS_EXITS_YEARS`, which runs one year
#' past `MAEXITS_XWALK_YEARS` once the newest crosswalk is out; the December
#' columns cover `MAEXITS_XWALK_YEARS`. The plan-counties and their
#' `exit_type` depend only on the crosswalk and landscape files, so they
#' agree for the two months (checked).
#'
#' @param at Augmented analytic table on December enrollment
#'   ([augment_analytic()] output); NULL reads
#'   `trunk/derived/analytictable_augmented.csv`.
#' @param proxy_at Augmented analytic table on September enrollment for
#'   every year; NULL builds it from `raw/monthly enrollment/`.
#' @param save If TRUE, write `trunk/derived/exits.csv`.
#' @return The exits table (invisibly).
#' @export
make_exits <- function(at = NULL, proxy_at = NULL, save = TRUE) {
  if (is.null(at)) {
    at <- fread(here("trunk", "derived", "analytictable_augmented.csv"), na.strings = c("", "NA"))
  }
  if (is.null(proxy_at)) proxy_at <- .proxy_augmented()
  key <- c("dec_year", "contract_id", "plan_id", "county_name", "state_name")
  dec <- .exit_rows(at)
  sep <- .exit_rows(proxy_at)
  setnames(dec, c("enrollment", "enrollment_low", "src"),
           c("dec_enrollment", "dec_enrollment_low", "dec_src"))
  setnames(sep, c("enrollment", "enrollment_low", "src"),
           c("sep_enrollment", "sep_enrollment_low", "sep_src"))

  # Every December year needs its September rows, and the two months must
  # give the same plan-counties and exit types
  missing <- setdiff(dec$dec_year, sep$dec_year)
  .vcheck(length(missing) == 0, "exits: no September enrollment for dec_year %s",
          paste(missing, collapse = ", "))
  same <- c(key, "segment_id", "exit_type", "xwalk_statuses")
  cmp <- merge(dec[, same, with = FALSE], sep[dec_year %in% dec$dec_year, same, with = FALSE],
               by = key, all = TRUE, suffixes = c(".dec", ".sep"))
  same_val <- function(x, y) (is.na(x) & is.na(y)) | (x == y) %in% TRUE
  bad <- cmp[is.na(exit_type.dec) | is.na(exit_type.sep) | !same_val(exit_type.dec, exit_type.sep) |
               !same_val(segment_id.dec, segment_id.sep) |
               !same_val(xwalk_statuses.dec, xwalk_statuses.sep)]
  .vcheck(nrow(bad) == 0, paste0(
    "exits: December and September give different plan-counties or exit types for %d ",
    "plan-counties (e.g. %s)"), nrow(bad),
    paste(head(bad[, paste(dec_year, contract_id, plan_id, county_name, state_name)], 3), collapse = "; "))

  out <- merge(sep, dec[, c(key, "dec_enrollment", "dec_enrollment_low", "dec_src"), with = FALSE],
               by = key, all.x = TRUE)
  for (v in c("sep_enrollment", "sep_enrollment_low", "dec_enrollment", "dec_enrollment_low")) {
    x <- out[[v]]
    .vcheck(all(is.na(x) | x == round(x)), "exits: %s has non-integer values", v)
    set(out, j = v, value = as.integer(round(x)))
  }
  setcolorder(out, c(key[1:3], "segment_id", key[4:5], "fips", "plan_type", "snp", "snp_type",
                     "exit_type", "xwalk_statuses", "dec_enrollment", "dec_enrollment_low",
                     "dec_src", "sep_enrollment", "sep_enrollment_low", "sep_src"))
  setorderv(out, key)
  .vcheck(!anyDuplicated(out, by = key), "exits has duplicate plan-county rows")
  .vcheck(all(out$exit_type %in% .EXIT_TYPES), "exits: unexpected exit_type values")

  yrs <- range(out$dec_year)
  message(sprintf("Exits: %d plan-counties, dec_year %d-%d (December through %s)",
                  nrow(out), yrs[1], yrs[2],
                  if (nrow(dec)) max(dec$dec_year) else "none"))
  if (save) fwrite(out, here("trunk", "derived", "exits.csv"))
  invisible(out)
}
