# The exits table: CMS's crosswalk exit for each December plan-county, with
# December and September enrollment.

# Augmented analytic table for one transition; `scale` changes enrollment
# only, as a different enrollment month would
exits_aug <- function(dec_year = 2025L, scale = 1, h0011_status = NULL) {
  row <- function(contract, county, st, curr_contract, curr_plan, dec) {
    data.table(dec_year = dec_year, contract_id = contract, plan_id = 1L, segment_id = 0L,
               county_name = county, state_name = "Maryland", status = st,
               curr_contract_id = curr_contract, curr_plan_id = curr_plan,
               dec_enrollment = dec * scale, dec_enrollment_low = dec * scale, dec_src = "reported",
               jan_enrollment = NA_real_, jan_enrollment_low = NA_real_, jan_src = "reported",
               plan_type = "HMO", snp = "No")
  }
  at <- rbind(
    row("H0011", "Frederick", "Renewal Plan", "H0011", 1L, 50),
    row("H0015", "Frederick", "Terminated/Non-renewed Contract", NA_character_, NA_integer_, 20),
    row("H0017", "Frederick", "Renewal Plan with SAR", "H0017", 1L, 25),
    row("H0017", "Carroll", "Renewal Plan with SAR", "H0017", 1L, 30),
    row("H0018", "Frederick", "Consolidated Renewal Plan", "H0018", 2L, 35),
    row("H0016", "Frederick", "New Plan", "H0016", 5L, 15))
  if (!is.null(h0011_status)) at[contract_id == "H0011", status := h0011_status]
  at[, `:=`(snp_type = "Not Applicable", dsnp_integration = NA_character_, multi_status = FALSE,
            fips = fifelse(county_name == "Frederick", 24021L, 24013L), eligibles_2018 = 1000,
            enrolled_2018 = 400, benchmark = NA_real_, penetration_2018 = 0.3)]
  # January: H0017 keeps Carroll only; H0018-2 serves Carroll only
  jan <- data.table(contract_id = c("H0011", "H0017", "H0018"), plan_id = c(1L, 1L, 2L),
                    county_name = c("Frederick", "Carroll", "Carroll"))
  ls <- rbind(unique(at[, .(contract_id, plan_id, county_name, year = dec_year)]),
              jan[, .(contract_id, plan_id, county_name, year = dec_year + 1L)])
  ls[, `:=`(state_name = "Maryland", segment_id = 0L, plan_type = "HMO", snp = "No",
            snp_type = NA_character_)]
  augment_analytic(at, save = FALSE, verbose = FALSE, landscape = ls, xwalk_years = dec_year + 1L)
}

test_that("exit_type follows CMS's labels, and both months are attached", {
  dec <- exits_aug(2025L)
  sep <- rbind(exits_aug(2025L, scale = 2), exits_aug(2026L, scale = 3), fill = TRUE)
  ex <- suppressMessages(make_exits(dec, proxy_at = sep, save = FALSE))
  expect_equal(nrow(ex), 12L)
  expect_false(anyDuplicated(ex, by = c("dec_year", "contract_id", "plan_id", "county_name")) > 0)
  e <- ex[dec_year == 2025L]
  type <- function(c, cty = "Frederick") e[contract_id == c & county_name == cty, exit_type]
  expect_equal(type("H0011"), "none")
  expect_equal(type("H0015"), "terminated")
  expect_equal(type("H0017"), "service_area_reduction")
  expect_equal(type("H0017", "Carroll"), "none")             # the SAR kept this county
  expect_equal(type("H0018"), "none")                        # consolidated away: not a CMS exit label
  expect_equal(type("H0016"), "none")                        # mapped to a New Plan elsewhere: not a termination
  expect_equal(e[contract_id == "H0016", xwalk_statuses], "New Plan")
  expect_equal(e[contract_id == "H0015", xwalk_statuses], "Terminated/Non-renewed Contract")
  expect_equal(e$sep_enrollment, 2L * e$dec_enrollment)
  expect_true(is.integer(ex$sep_enrollment) && is.integer(ex$dec_enrollment))
  expect_true(all(is.na(ex[dec_year == 2026L, dec_enrollment])))   # December not out yet
  expect_equal(ex[dec_year == 2026L & exit_type != "none", sum(sep_enrollment)], 3L * (20L + 25L))
  expect_true(all(is.na(ex$snp_type)))                       # non-SNPs carry no SNP type
})

test_that("December and September must agree on the plan-counties and exit types", {
  dec <- exits_aug(2025L)
  expect_error(suppressMessages(make_exits(dec, proxy_at = exits_aug(2024L), save = FALSE)),
               "no September enrollment for dec_year 2025")
  expect_error(suppressMessages(make_exits(dec, proxy_at = exits_aug(2025L, h0011_status = "Terminated/Non-renewed Contract"),
                                           save = FALSE)),
               "different plan-counties or exit types for 1 plan-counties")
})

test_that("the exits years cover the crosswalk years and only registered crosswalks", {
  expect_true(all(MAEXITS_XWALK_YEARS %in% MAEXITS_EXITS_YEARS))
  expect_true(all(as.character(MAEXITS_EXITS_YEARS) %in% names(MAEXITS_XWALK_FILES)))
  expect_true(all(as.character(MAEXITS_EXITS_YEARS) %in% names(MAEXITS_LANDSCAPE_FILES) |
                    MAEXITS_EXITS_YEARS <= 2024L))
  skip_if_not(has_raw(), "raw/ not present")
  err <- tryCatch(check_inputs(2019:2026, exits_years = c(2020:2027, 2099)), error = conditionMessage)
  expect_match(err, "must include every crosswalk year; missing 2019")
  expect_match(err, "crosswalk 2099 \\(for the exits table\\) is not registered")
})

test_that("the built exits table matches the December counts of displacement", {
  skip_if_not(file.exists(here::here("trunk", "derived", "exits.csv")) &&
                file.exists(here::here("trunk", "derived", "displacement.csv")),
              "built tables not found")
  ex <- fread(here::here("trunk", "derived", "exits.csv"))
  ds <- fread(here::here("trunk", "derived", "displacement.csv"),
              select = c("dec_year", "dec_enrollment", "forced_reason", "outcome"))
  a <- ex[!is.na(dec_enrollment), .(total = sum(dec_enrollment),
                                    terminated = sum(dec_enrollment[exit_type == "terminated"]),
                                    sar = sum(dec_enrollment[exit_type == "service_area_reduction"])),
          keyby = dec_year]
  # displacement's terminations less new_plan_not_in_county (a New Plan link, not a termination)
  b <- ds[, .(total = sum(dec_enrollment),
              terminated = sum(dec_enrollment[outcome %in% c("terminated_contract_listed", "terminated_contract_gone")]),
              sar = sum(dec_enrollment[forced_reason %in% "service_area_reduction"])), keyby = dec_year]
  expect_equal(a, b)
  # Recorded totals (2025-26: 2,140,073 terminated and 716,371 SAR of 29,693,500 in December)
  expect_equal(unlist(a[dec_year == 2025L, .(total, terminated, sar)]),
               c(total = 29693500L, terminated = 2140073L, sar = 716371L))
})
