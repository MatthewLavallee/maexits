# The PDP exits table: terminations of standalone Part D plans by state.

pdp_fixture <- function() {
  uni <- data.table(contract_id = c("S0001", "S0002", "S0003", "S0003", "S0004", "S0005", "S0006"),
                    plan_id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L),
                    state = c("MD", "MD", "MD", "VA", "MD", "MD", "MD"))
  nxt <- data.table(contract_id = c("S0002", "S0003", "S0009", "S0005"), plan_id = c(1L, 1L, 2L, 1L),
                    state = c("MD", "VA", "MD", "MD"))
  xw <- data.table(
    prev_contract = c("S0001", "S0002", "S0003", "S0004", "S0005", "S0006"),
    prev_plan = c(1L, 1L, 1L, 1L, 1L, 1L),
    curr_contract = c("TERMINATED", "S0002", "S0003", "S0009", "S0005", "S0006"),
    curr_plan = c(NA, 1L, 1L, 2L, 1L, 1L),
    status = c("Terminated/Non-renewed Contract", "Renewal Plan", "Renewal Plan",
               "Consolidated Renewal Plan", "Renewal Plan", "Renewal Plan"))
  list(uni = uni, nxt = nxt, xw = xw)
}

test_that("PDP exit types follow the crosswalk, the next landscape and CMS terminations", {
  f <- pdp_fixture()
  r <- .pdp_exit_rows(f$uni, f$nxt, f$xw, cms_terminated = "S0006")
  type <- function(c, s = "MD") r[contract_id == c & state == s, exit_type]
  expect_equal(type("S0001"), "terminated")
  expect_equal(type("S0002"), "none")                        # renewed in the state
  expect_equal(type("S0003", "VA"), "none")
  expect_equal(type("S0003", "MD"), "none")                  # renewal not offered in MD: not a CMS exit label
  expect_equal(type("S0004"), "none")                        # consolidated into another PDP
  expect_equal(type("S0006"), "terminated")                  # CMS terminated it outside the crosswalk
  expect_true(r[contract_id == "S0006", cms_terminated])
  expect_false(any(r[contract_id != "S0006", cms_terminated]))
  expect_equal(r[contract_id == "S0004", xwalk_statuses], "Consolidated Renewal Plan")
  # A registered CMS termination whose plans are still offered is a registration mistake
  expect_error(.pdp_exit_rows(f$uni, f$nxt, f$xw, cms_terminated = "S0005"), "still offered next January")
})

test_that("PDP enrollment sums by state with suppressed cells as 10 or 1", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c('"Contract Number","Plan ID","SSA State County Code","FIPS State County Code","State","County","Enrollment"',
               '"S0001","001","21000","24001","MD","Allegany","25"',
               '"S0001","001","21010","24003","MD","Anne Arundel","*"',
               '"S0001","001","49000","51001","VA","Accomack","*"',
               '"S0001","801","21000","24001","MD","Allegany","50"',
               '"H0001","001","21000","24001","MD","Allegany","70"'), path)
  e <- .pdp_enrollment(path)
  expect_equal(nrow(e), 2L)                                  # employer and MA rows dropped
  rows <- data.table(contract_id = "S0001", plan_id = 1L, state = c("MD", "VA", "DE"))
  m <- .attach_pdp_enrollment(rows, e, "sep")
  expect_equal(m$sep_enrollment, c(35L, 10L, 0L))
  expect_equal(m$sep_enrollment_low, c(26L, 1L, 0L))
  expect_equal(m$sep_src, c("mixed", "suppressed", "no_record"))
})

test_that("the built PDP exits table matches the recorded totals", {
  skip_if_not(file.exists(here::here("trunk", "derived", "pdp_exits.csv")), "built tables not found")
  p <- fread(here::here("trunk", "derived", "pdp_exits.csv"))
  expect_false(anyDuplicated(p, by = c("dec_year", "contract_id", "plan_id", "state")) > 0)
  s <- p[, .(total = sum(sep_enrollment), terminated = sum(sep_enrollment[exit_type == "terminated"])),
         keyby = dec_year]
  # Recorded September totals: 2024 includes Clear Spring Health (333,633)
  expect_equal(s[dec_year == 2024L, terminated], 583961L)
  expect_equal(s[dec_year == 2026L, c(total, terminated)], c(18866246L, 1169219L))
  expect_equal(p[cms_terminated == TRUE, unique(contract_id)], "S6946")
  expect_true(all(is.na(p[dec_year == max(dec_year), dec_enrollment])))
})

test_that("state filters accept both spellings of the Virgin Islands", {
  expect_true(all(c("virgin islands", "u.s. virgin islands") %in% .state_filter_names("Virgin Islands")))
  expect_true("u.s. virgin islands" %in% .state_filter_names("VI"))
})
