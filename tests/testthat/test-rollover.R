# Failure modes the pipeline must catch rather than silently producing wrong
# or missing numbers. Each must stop loudly with an actionable message.

test_that("config registers a crosswalk file for every configured year", {
  expect_true(all(as.character(MAEXITS_XWALK_YEARS) %in% names(MAEXITS_XWALK_FILES)))
  expect_equal(.landscape_years(2019:2027), 2016:2027)
})

test_that("an unregistered crosswalk year names the year and the fix", {
  expect_error(.resolve_crosswalk_path(2099),
               "Crosswalk year 2099 is not registered.*MAEXITS_XWALK_FILES")
})

test_that("check_inputs lists every missing next-cycle input at once", {
  skip_if_not(has_raw(), "raw/ not present")
  expect_silent(suppressMessages(check_inputs()))
  err <- tryCatch(check_inputs(2099), error = conditionMessage)
  expect_match(err, "crosswalk 2099 is not registered")
  expect_match(err, "landscape CY2099")
  expect_match(err, "CPSC_Enrollment_Info_2098_12.csv not found")
  expect_match(err, "CPSC_Enrollment_Info_2099_01.csv not found")
  expect_match(err, "countyrate2098.csv")
})

test_that("CPSC file names are checked for month, duplicates and format", {
  jan <- "january enrollment"
  ok <- c("a/CPSC_Enrollment_Info_2025_01.csv", "b/CPSC_Enrollment_Info_2026_01.csv")
  expect_equal(.check_enrollment_files(ok, jan), c("2025", "2026"))
  expect_error(.check_enrollment_files(c(ok, "c/CPSC_Enrollment_Info_2026_01.csv"), jan),
               "more than one CPSC file for year\\(s\\) 2026")
  expect_error(.check_enrollment_files("CPSC_Enrollment_Info_2027_02.csv", jan),
               "wrong month")
  expect_error(.check_enrollment_files("CPSC_Enrollment_Info_2026_11.csv",
                                       "december enrollment"), "wrong month")
  expect_error(.check_enrollment_files("CPSC_Enrollment_Info_2026.csv", jan),
               "unrecognised CPSC file name")
  expect_error(.check_enrollment_files(character(0), jan), "no CPSC_Enrollment_Info files")
})

test_that("a missing year is caught before any 10-imputation", {
  dfl <- data.table(year = c(2025, 2026), contract_id = "H0001", plan_id = 1L)
  dec <- data.table(year = "2025", county_enrollment = 100)
  jan <- data.table(year = "2026", county_enrollment = 101)
  expect_silent(.check_year_coverage(dfl, dec, jan, 2026L))
  expect_error(.check_year_coverage(dfl, dec, jan[0], 2026L), "no January 2026 enrollment")
  expect_error(.check_year_coverage(dfl, dec[0], jan, 2026L), "no December 2025 enrollment")
  expect_error(.check_year_coverage(dfl[year == 2025], dec, jan, 2026L),
               "landscape has no rows for CY2026")
  expect_error(.check_year_coverage(dfl, dec, data.table(year = "2026", county_enrollment = 50),
                                    2026L), "differs from December 2025 total")
  # Preliminary runs have no January data
  expect_silent(.check_year_coverage(dfl, dec, NULL, 2026L))
})

test_that("mass imputation stops the run", {
  expect_silent(.check_imputation(c("reported", "mixed", "no_record"), "December", 2027))
  expect_error(.check_imputation(c("reported", "suppressed", "no_record"), "January", 2027),
               "67% of January enrollment values are CMS-suppressed or have no CPSC record")
})

test_that("an unknown crosswalk status stops instead of becoming role 'other'", {
  path <- fake_crosswalk(c("Renewal Plan", "Renewal Plan with Merger"), c("001", "002"))
  expect_error(.read_crosswalk(2027, path),
               "status value\\(s\\) not in MAEXITS_XWALK_STATUS_CLASS: 'Renewal Plan with Merger'")
})

test_that("crosswalk parsing handles placeholders and rejects other IDs", {
  x <- .read_crosswalk(2027, fake_crosswalk())
  expect_equal(x$prev_plan, c(1, NA))
  expect_equal(x$status, c("Renewal Plan", "New Plan"))
  x2 <- .read_crosswalk(2021, fake_crosswalk(status_col = "DESCRIPTION"))
  expect_equal(x2$status, c("Renewal Plan", "New Plan"))
  expect_error(.read_crosswalk(2027, fake_crosswalk(prev_plan = c("001", "N/A"))),
               "unexpected plan ID value\\(s\\): N/A")
})

test_that("a crosswalk missing its status column stops", {
  path <- fake_crosswalk(status_col = "OUTCOME")
  expect_error(.read_crosswalk(2027, path), "expected exactly one of STATUS/DESCRIPTION")
})

test_that("a crosswalk registered for the wrong year is detected", {
  skip_if_not(has_raw() && has_derived(), "raw/ or trunk/derived not present")
  dfl <- fread(here("trunk", "derived", "landscape.csv"))
  x26 <- .read_crosswalk(2026)
  expect_silent(.check_xwalk_content(x26, dfl, 2026L))
  # The 2026 file presented as the 2027 crosswalk matches CY2025 better than CY2026
  expect_error(.check_xwalk_content(x26, dfl, 2027L), "does not look like the 2027 crosswalk")
})

test_that("a missing January landscape stops augment instead of dropping every SAR county", {
  at <- data.table(dec_year = 2026, status = "Renewal Plan with SAR",
                   sar_dropped = TRUE, role = "exiting")
  expect_error(.check_augmented(at, ls_years = 2016:2026, xwalk_years = 2027L),
               "landscape has no January year\\(s\\) 2027")
  expect_error(.check_augmented(at, ls_years = 2016:2027, xwalk_years = 2027L),
               "SAR dropped share out of range in dec_year 2026 \\(1.00\\)")
  expect_error(.check_augmented(at, ls_years = 2016:2027, xwalk_years = 2026L),
               "both must use the same years")
  other <- data.table(dec_year = 2026, status = "Renewal Plan with Merger",
                      sar_dropped = FALSE, role = "other")
  expect_error(.check_augmented(other, 2016:2027, 2027L), "role 'other'")
})

test_that("new MA plans absent from January enrollment stop the run", {
  dfnew <- data.table(curr_contract = c("H0001", "H0002"), curr_plan = c(1, 2))
  jan <- data.table(contract_id = c("H0001", "H0002"), plan_id = c(1L, 2L))
  expect_silent(.check_new_plans(dfnew, jan, 2027))
  expect_error(.check_new_plans(dfnew, jan[0], 2027),
               "only 0% of 2 new MA plans have January enrollment")
})

test_that("benchmark coverage is checked per year", {
  at <- data.table(dec_year = c(rep(2025, 100), rep(2026, 100)),
                   benchmark = c(rep(900, 100), rep(NA, 100)))
  expect_error(.check_benchmark(at), "dec_year 2026 \\(100.0%\\)")
  expect_silent(.check_benchmark(at[dec_year == 2025]))
})

test_that("zero-padding is platform independent", {
  expect_equal(.pad0(c("1000", "01000", "5210", "", NA, "XX123"), 5),
               c("01000", "01000", "05210", "00000", NA, "XX123"))
})

test_that("CY2025+ landscapes: column aliases, renames and unknown categories", {
  fake_landscape <- function(state_col = "State Territory Name", category = "MA") {
    d <- data.table(`Contract Category Type` = category, `County Name` = "Kent",
                    `Contract ID` = "H0001", `Plan ID` = 1L, `Segment ID` = 0L,
                    `Plan Type` = "HMO", `Special Needs Plan (SNP) Indicator` = "No",
                    `SNP Type` = "")
    d[, (state_col) := "Delaware"]
    path <- tempfile(fileext = ".csv"); fwrite(d, path); path
  }
  x <- .get_landscape_combined(2027, fake_landscape())
  expect_equal(x$state_name, "Delaware")
  expect_equal(x$year, "CY2027")
  expect_equal(.get_landscape_combined(2027, fake_landscape("State Name"))$state_name, "Delaware")
  expect_error(.get_landscape_combined(2027, fake_landscape("State/Territory")),
               "expected exactly one of \\[State Name \\| State Territory Name\\] for state_name")
  expect_error(.get_landscape_combined(2027, fake_landscape(category = "Mystery")),
               "unknown Contract Category Type value\\(s\\): 'Mystery'")
  expect_error(.landscape_file(2099), "landscape CY2099 is not registered")
})

test_that("preliminary runs refuse to write over the final tables", {
  skip_if_not(has_raw(), "raw/ not present")
  proxy <- file.path("january enrollment", "CPSC_Enrollment_2025_01",
                     "CPSC_Enrollment_Info_2025_01.csv")
  expect_error(run_preliminary(2026, proxy, out_dir = here("trunk", "derived")),
               "out_dir must not be trunk/derived")
  expect_error(run_preliminary(2025, proxy, save = FALSE),
               "proxy month must be from 2024")
})


# --- Stale, mislabelled and reworded inputs -------------------------------

test_that("status logic is driven by class, not hardcoded strings", {
  skip_if_not(file.exists(here("R", "data_build.R")), "R/ sources not available")
  code <- unlist(lapply(c("data_build.R", "data_augment.R", "validate.R",
                          "analysis.R", "preliminary.R"),
                        function(f) readLines(here("R", f))))
  code <- code[!grepl("^\\s*#", code)]
  lit <- grep(paste0('"(Renewal Plan( with SA[RE])?|Consolidated Renewal Plan|Terminated Plan|',
                     'Terminated/Non-renewed Contract|New Plan|Initial Contract)"'),
              code, value = TRUE)
  expect_length(lit, 0)
  expect_setequal(.statuses("service_area_reduction"), "Renewal Plan with SAR")
  expect_setequal(.statuses("terminated"),
                  c("Terminated Plan", "Terminated/Non-renewed Contract"))
})

test_that("a landscape registered under the wrong year is caught", {
  d <- data.table(`Contract Year` = 2026L, `Contract Category Type` = "MA",
                  `State Name` = "Delaware", `County Name` = "Kent",
                  `Contract ID` = "H0001", `Plan ID` = 1L, `Segment ID` = 0L,
                  `Plan Type` = "HMO", `Special Needs Plan (SNP) Indicator` = "No",
                  `SNP Type` = "")
  path <- tempfile(fileext = ".csv"); fwrite(d, path)
  expect_equal(nrow(.get_landscape_combined(2026, path)), 1L)
  expect_error(.get_landscape_combined(2027, path), "Contract Year column says 2026")
  d2 <- copy(d)[, `Contract Category Type` := "PDP"]
  path2 <- tempfile(fileext = ".csv"); fwrite(d2, path2)
  expect_equal(nrow(.get_landscape_combined(2026, path2)), 0L)  # PDP dropped by category
})

test_that("crosswalks without NEW placeholders or with odd new rows stop", {
  new_rows <- data.table(prev_contract = NA_character_, prev_plan = NA_real_,
                         curr_contract = sprintf("H%04d", 1:400), curr_plan = 1,
                         status = "New Plan")
  expect_silent(.check_xwalk_new_rows(new_rows, 2027))
  expect_error(.check_xwalk_new_rows(new_rows[1:10], 2027),
               "only 10 new MA plans have a NEW placeholder")
  numeric_prev <- copy(new_rows)[1:30, prev_plan := 5]
  expect_error(.check_xwalk_new_rows(numeric_prev, 2027),
               "30 New Plan/Initial Contract rows have a numeric PREVIOUS_PLAN_ID")
  dfl <- data.table(year = 2027, contract_id = sprintf("H%04d", 1:400), plan_id = 1L)
  expect_silent(.check_new_plans_landscape(new_rows, dfl, 2027))
  expect_error(.check_new_plans_landscape(new_rows, dfl[1:100], 2027),
               "only 25% of crosswalk 2027's new MA plans appear in landscape CY2027")
})

test_that("stale or copied CPSC files are caught", {
  dec <- data.table(year = c("2024", "2025"), county_enrollment = c(100, 103))
  expect_silent(.check_totals_vs_prior(dec, 2025L, "December"))
  expect_error(.check_totals_vs_prior(data.table(year = c("2024", "2025"),
                                                 county_enrollment = c(100, 100)),
                                      2025L, "December"), "looks like a copy")
  expect_error(.check_totals_vs_prior(data.table(year = c("2024", "2025"),
                                                 county_enrollment = c(100, 130)),
                                      2025L, "December"), "differs from December 2024 by 30.0%")
  dfl <- data.table(year = c(2024, 2025, 2025), contract_id = c("H0001", "H0001", "H0002"),
                    plan_id = 1L)
  expect_silent(.check_dec_new_plans(dfl, data.table(contract_id = "H0002", plan_id = 1L), 2025))
  expect_error(.check_dec_new_plans(dfl, data.table(contract_id = "H0001", plan_id = 1L), 2025),
               "Is CPSC_Enrollment_Info_2025_12.csv really December 2025")
})

test_that("duplicated crosswalk years stop", {
  expect_error(check_inputs(c(2025, 2025)), "duplicated crosswalk year\\(s\\): 2025")
  expect_error(make_analytic(save = FALSE, xwalk_years = c(2026, 2026)),
               "duplicated crosswalk year")
})

test_that("preliminary analytic tables cannot be saved over the final table", {
  expect_error(make_analytic(save = TRUE, preliminary = TRUE),
               "must not overwrite trunk/derived/analytictable.csv")
})

test_that("enrollment files beyond the configured years are skipped", {
  skip_if_not(has_raw(), "raw/ not present")
  expect_message(d <- make_enrollment("december enrollment", save = FALSE, max_year = 2024L),
                 "skipping CPSC_Enrollment_Info_2025_12.csv")
  expect_equal(max(as.integer(d$year)), 2024L)
})

test_that("FIPS coverage is checked per year", {
  at <- data.table(dec_year = rep(2026, 100), fips = c(rep(1001L, 98), NA, NA),
                   county_name = "Nowhere", state_name = "Alabama")
  expect_error(.check_fips(at), "dec_year 2026 \\(2.0%\\).*Nowhere, Alabama")
  expect_silent(.check_fips(at[!is.na(fips)]))
})

test_that(".write_derived replaces the whole set and leaves no staging folders", {
  out <- file.path(tempfile("wd_"), "derived")
  .write_derived(list(a = data.table(x = 1), b = data.table(y = 2)), out)
  suppressMessages(.write_derived(list(a = data.table(x = 10), b = data.table(y = 20)), out))
  expect_equal(fread(file.path(out, "a.csv"))$x, 10)
  expect_equal(fread(file.path(out, "b.csv"))$y, 20)
  expect_length(list.files(dirname(out), pattern = "^derived_(staging|backup)_"), 0)
})

test_that("preliminary out_dir guard resolves indirect paths", {
  skip_if_not(has_raw(), "raw/ not present")
  proxy <- file.path("january enrollment", "CPSC_Enrollment_2025_01",
                     "CPSC_Enrollment_Info_2025_01.csv")
  sneaky <- file.path(here("trunk", "derived"), "..", "derived")
  expect_error(run_preliminary(2026, proxy, out_dir = sneaky),
               "out_dir must not be trunk/derived")
})
