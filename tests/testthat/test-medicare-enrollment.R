# make_medicare_enrollment(): county x month Original Medicare and MA
# enrollment from CMS's Medicare Monthly Enrollment file.

me_fixture <- function(path, rows) {
  cols <- c("YEAR", "MONTH", "BENE_GEO_LVL", "BENE_STATE_ABRVTN", "BENE_STATE_DESC",
            "BENE_COUNTY_DESC", "BENE_FIPS_CD", names(.MEDICARE_ENROLLMENT_COLS))
  dt <- rbindlist(lapply(rows, function(r) {
    x <- as.list(setNames(rep("0", length(cols)), cols))
    x[names(r)] <- r
    as.data.table(x)
  }))
  fwrite(dt, path)
  path
}
row <- function(year, month, geo, county, fips, tot, om, ma, state = "VT") {
  list(YEAR = year, MONTH = month, BENE_GEO_LVL = geo, BENE_STATE_ABRVTN = state,
       BENE_STATE_DESC = "Vermont", BENE_COUNTY_DESC = county, BENE_FIPS_CD = fips,
       TOT_BENES = tot, ORGNL_MDCR_BENES = om, MA_AND_OTH_BENES = ma)
}

test_that("county months are kept, suppressed counts are NA, other rows are dropped", {
  f <- me_fixture(tempfile(fileext = ".csv"), list(
    row("2025", "December", "County", "Addison County", "50001", "10050.0", "6630.0", "3420.0"),
    row("2026", "January", "County", "Addison County", "50001", "10100.0", "8000.0", "2100.0"),
    row("2025", "December", "County", "Essex County", "50009", "*", "*", "*"),
    row("2026", "January", "County", "Essex County", "50009", "2049.0", "1394.0", "655.0"),
    row("2025", "Year", "County", "Addison County", "50001", "10050.0", "6630.0", "3420.0"),
    row("2025", "December", "State", "Total", "50", "150000.0", "100000.0", "50000.0"),
    row("2025", "December", "County", "Unknown", "50999", "*", "*", "*"),
    row("2017", "December", "County", "Addison County", "50001", "9000.0", "7000.0", "2000.0")
  ))
  x <- suppressMessages(make_medicare_enrollment(f, first_year = 2018L, save = FALSE))
  expect_equal(nrow(x), 4L)
  expect_setequal(unique(x$month), c("2025-12", "2026-01"))
  a <- x[fips == 50001L & month == "2026-01"]
  expect_identical(a$orig_medicare_benes, 8000L)
  expect_identical(a$ma_other_benes, 2100L)
  expect_equal(a$ma_share, 2100 / 10100)
  expect_true(is.na(x[fips == 50009L & month == "2025-12", total_benes]))
  expect_true(is.na(x[fips == 50009L & month == "2025-12", ma_share]))
})

test_that("a skipped month or an inconsistent total stops the build", {
  gap <- me_fixture(tempfile(fileext = ".csv"), list(
    row("2025", "November", "County", "Addison County", "50001", "100", "60", "40"),
    row("2026", "January", "County", "Addison County", "50001", "100", "60", "40")))
  expect_error(make_medicare_enrollment(gap, first_year = 2018L, save = FALSE), "skips month")
  bad <- me_fixture(tempfile(fileext = ".csv"), list(
    row("2025", "December", "County", "Addison County", "50001", "100", "60", "30")))
  expect_error(make_medicare_enrollment(bad, first_year = 2018L, save = FALSE), "differs from total")
  odd <- me_fixture(tempfile(fileext = ".csv"), list(
    row("2025", "December", "County", "Addison County", "50001", "1,000", "600", "400")))
  expect_error(make_medicare_enrollment(odd, first_year = 2018L, save = FALSE), "unexpected value")
})

test_that("the registered file builds and Original Medicare + MA adds up", {
  skip_if_not(file.exists(.medicare_enrollment_file()), "Medicare Monthly Enrollment file not in raw/")
  x <- suppressMessages(make_medicare_enrollment(save = FALSE))
  expect_identical(min(x$month), sprintf("%d-01", MAEXITS_MEDICARE_FIRST_YEAR))
  expect_gt(uniqueN(x$fips), 3100L)
  ok <- x[!is.na(total_benes) & !is.na(orig_medicare_benes) & !is.na(ma_other_benes)]
  expect_lte(ok[, max(abs(total_benes - orig_medicare_benes - ma_other_benes))], 1)
})
