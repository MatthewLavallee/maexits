# ============================================================
# medicare_enrollment.R — County x month Medicare enrollment: Original
# Medicare (Traditional Medicare) and Medicare Advantage
#
#   make_medicare_enrollment()   county x month table from the CMS
#                                Medicare Monthly Enrollment file
#
# The CPSC files count enrollment in MA and Part D plans only. This table
# adds the rest of the county's Medicare population each month: everyone
# enrolled in Medicare, the Original Medicare and MA-and-other split, and
# Part D, dual and age groups, from CMS's Medicare Monthly Enrollment
# file (data.cms.gov). It gives MA penetration and Original Medicare
# enrollment around each January transition.
# ============================================================


# Columns kept from the CMS file, and their names in the table
.MEDICARE_ENROLLMENT_COLS <- c(
  TOT_BENES = "total_benes",
  ORGNL_MDCR_BENES = "orig_medicare_benes",
  MA_AND_OTH_BENES = "ma_other_benes",
  AGED_TOT_BENES = "aged_benes",
  DSBLD_TOT_BENES = "disabled_benes",
  DUAL_TOT_BENES = "dual_benes",
  FULL_DUAL_TOT_BENES = "full_dual_benes",
  PRSCRPTN_DRUG_TOT_BENES = "part_d_benes",
  PRSCRPTN_DRUG_PDP_BENES = "pdp_benes",
  PRSCRPTN_DRUG_MAPD_BENES = "mapd_benes"
)


#' Path of the registered Medicare Monthly Enrollment file
#' @keywords internal
.medicare_enrollment_file <- function() {
  here("raw", MAEXITS_MEDICARE_ENROLLMENT_FILE)
}


#' Build the County x Month Medicare Enrollment Table
#'
#' Reads CMS's Medicare Monthly Enrollment file (one row per month and
#' county, state or nation) and keeps the county rows from
#' `MAEXITS_MEDICARE_FIRST_YEAR` on: everyone enrolled in Medicare
#' (`total_benes`), split into Original Medicare (`orig_medicare_benes`)
#' and Medicare Advantage and other health plans (`ma_other_benes`), with
#' Part D, dual-eligible and age counts. CMS suppresses small counts
#' (shown as `*`); they are `NA`. Annual rows and the "Unknown" county rows
#' are dropped.
#'
#' @param file Path to the CMS CSV (default: the file registered as
#'   `MAEXITS_MEDICARE_ENROLLMENT_FILE` under raw/).
#' @param first_year First calendar year to keep (default
#'   `MAEXITS_MEDICARE_FIRST_YEAR`).
#' @param save Logical. If TRUE (default), writes
#'   trunk/derived/medicare_enrollment.csv.
#' @return Invisibly, a data.table with one row per county and month.
#' @export
make_medicare_enrollment <- function(file = .medicare_enrollment_file(),
                                     first_year = MAEXITS_MEDICARE_FIRST_YEAR,
                                     save = TRUE) {
  .vcheck(file.exists(file), "Medicare Monthly Enrollment file not found: %s", file)
  keep <- c("YEAR", "MONTH", "BENE_GEO_LVL", "BENE_STATE_ABRVTN", "BENE_STATE_DESC",
            "BENE_COUNTY_DESC", "BENE_FIPS_CD", names(.MEDICARE_ENROLLMENT_COLS))
  raw <- fread(file, select = keep, colClasses = "character", encoding = "UTF-8")
  .vcheck(all(keep %in% names(raw)), "Medicare Monthly Enrollment file lacks column(s): %s",
          paste(setdiff(keep, names(raw)), collapse = ", "))
  me <- raw[BENE_GEO_LVL == "County" & MONTH %in% month.name &
              as.integer(YEAR) >= first_year & BENE_COUNTY_DESC != "Unknown"]
  .vcheck(nrow(me) > 0, "no county-month rows from %d on in %s", first_year, file)

  num <- function(x) {
    x <- trimws(x)
    bad <- !(x %in% c("*", "") | grepl("^[0-9]+(\\.0+)?$", x))
    .vcheck(!any(bad), "unexpected value(s) in Medicare Monthly Enrollment: %s",
            paste(head(unique(x[bad]), 5), collapse = ", "))
    out <- suppressWarnings(as.numeric(x))
    as.integer(round(out))
  }
  out <- me[, .(
    month = sprintf("%s-%02d", YEAR, match(MONTH, month.name)),
    year = as.integer(YEAR),
    state = BENE_STATE_ABRVTN,
    state_name = BENE_STATE_DESC,
    county_name = BENE_COUNTY_DESC,
    fips = as.integer(BENE_FIPS_CD)
  )]
  for (src in names(.MEDICARE_ENROLLMENT_COLS)) {
    set(out, j = .MEDICARE_ENROLLMENT_COLS[[src]], value = num(me[[src]]))
  }
  out[, ma_share := fifelse(total_benes > 0, ma_other_benes / total_benes, NA_real_)]
  setorder(out, fips, month)

  .vcheck(!anyDuplicated(out, by = c("fips", "month")),
          "Medicare Monthly Enrollment has duplicated county-months")
  both <- out[!is.na(total_benes) & !is.na(orig_medicare_benes) & !is.na(ma_other_benes)]
  gap <- both[, max(abs(total_benes - orig_medicare_benes - ma_other_benes))]
  .vcheck(gap <= 1, "Original Medicare + MA and other differs from total by up to %s", gap)
  months <- sort(unique(out$month))
  expect <- format(seq(as.Date(paste0(months[1], "-01")), as.Date(paste0(tail(months, 1), "-01")),
                       by = "month"), "%Y-%m")
  .vcheck(identical(months, expect), "Medicare Monthly Enrollment skips month(s): %s",
          paste(setdiff(expect, months), collapse = ", "))

  if (save) fwrite(out, here("trunk", "derived", "medicare_enrollment.csv"))
  message("Medicare enrollment: ", nrow(out), " county-months, ", uniqueN(out$fips),
          " counties, ", months[1], " to ", tail(months, 1))
  invisible(out)
}
