# make_pbp_benefits(): Part B giveback and supplemental benefits from CMS's
# Plan Benefit Package files.

test_that("the registered PBP files build with plausible benefit shares", {
  skip_if_not(all(file.exists(here("raw", "pbp", MAEXITS_PBP_FILES))), "PBP files not in raw/pbp/")
  x <- suppressMessages(make_pbp_benefits(years = c(2024L, 2025L), save = FALSE))
  expect_false(anyDuplicated(x, by = c("year", "contract_id", "plan_id", "segment_id")) > 0)
  ma <- x[grepl("^[HR]", contract_id) & plan_id < 800]
  s <- ma[, lapply(.SD, mean), by = year, .SDcols = c("dental_preventive", "dental_comprehensive",
                                                       "eyewear", "hearing_aids", "otc", "fitness")]
  # the CY2025 dental restructuring should not move coverage by much
  expect_lt(abs(diff(s$dental_preventive)), 0.05)
  expect_lt(abs(diff(s$dental_comprehensive)), 0.05)
  expect_true(all(unlist(s[, -1]) > 0.5))
  expect_true(all(is.na(ma[partb_giveback == FALSE, partb_giveback_amt])))
  expect_true(all(ma[otc == FALSE, is.na(otc_annual_amt)]))
  pdp <- x[grepl("^S", contract_id)]
  expect_false(any(unlist(pdp[, .(dental_preventive, eyewear, hearing_aids, otc)])))
})

test_that("unregistered years stop the build", {
  expect_error(make_pbp_benefits(years = 2010L, save = FALSE), "not registered")
})
