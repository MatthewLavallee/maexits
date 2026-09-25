# Counting across crosswalk links. plan_county rows are links: a prior
# plan-county with several links repeats its December enrollment, and a
# successor plan-county reached by several links repeats its January
# enrollment. These tests build a small transition with every link pattern
# and check the plan-county flags, the _once/_split columns and the panel.

counting_fixture <- function() {
  row <- function(contract, plan, county, status, curr_contract, curr_plan,
                  dec, dec_low, dec_src, jan, jan_low, jan_src, snp = "No") {
    data.table(dec_year = 2025L, contract_id = contract, plan_id = plan,
               segment_id = 0L, county_name = county, state_name = "Delaware",
               status = status, curr_contract_id = curr_contract,
               curr_plan_id = curr_plan, dec_enrollment = dec,
               dec_enrollment_low = dec_low, dec_src = dec_src,
               jan_enrollment = jan, jan_enrollment_low = jan_low,
               jan_src = jan_src, plan_type = "HMO", snp = snp)
  }
  na_i <- NA_integer_; na_c <- NA_character_; na_r <- NA_real_
  at <- rbind(
    # 1. split: one prior plan-county, two successors that both serve Kent
    row("H0001", 1L, "Kent", "Consolidated Renewal Plan", "H0001", 2L, 100, 100, "reported", 50, 50, "reported"),
    row("H0001", 1L, "Kent", "Consolidated Renewal Plan", "H0001", 3L, 100, 100, "reported", 70, 70, "reported"),
    # 2. move into another contract that serves Kent (companion termination row)
    row("H0002", 1L, "Kent", "Terminated/Non-renewed Contract", na_c, na_i, 200, 200, "reported", 0, 0, "terminated", snp = "Yes"),
    row("H0002", 1L, "Kent", "Consolidated Renewal Plan", "H0009", 1L, 200, 200, "reported", 180, 180, "reported", snp = "Yes"),
    # 3. consolidation: two prior plans into one successor
    row("H0003", 1L, "Kent", "Renewal Plan", "H0003", 1L, 40, 40, "reported", 95, 95, "reported"),
    row("H0003", 2L, "Kent", "Consolidated Renewal Plan", "H0003", 1L, 60, 60, "reported", 95, 95, "reported"),
    # 4. move into another contract that does not serve Kent
    row("H0004", 1L, "Kent", "Terminated/Non-renewed Contract", na_c, na_i, 30, 30, "reported", 0, 0, "terminated"),
    row("H0004", 1L, "Kent", "Consolidated Renewal Plan", "H0008", 1L, 30, 30, "reported", 0, 0, "no_record"),
    # 5. service-area reduction: drops Kent, keeps Sussex and New Castle
    row("H0005", 1L, "Kent", "Renewal Plan with SAR", "H0005", 1L, 20, 20, "reported", 3, 3, "reported"),
    row("H0005", 1L, "Sussex", "Renewal Plan with SAR", "H0005", 1L, 25, 25, "reported", 24, 24, "reported"),
    row("H0005", 1L, "New Castle", "Renewal Plan with SAR", "H0005", 1L, 10, 1, "suppressed", 10, 1, "suppressed"),
    # 6. terminated plan (suppressed December count)
    row("H0006", 1L, "Kent", "Terminated Plan", na_c, na_i, 10, 1, "suppressed", 0, 0, "terminated", snp = "Yes"),
    # 7. renewed, but not listed in Kent in the January landscape
    row("H0007", 1L, "Kent", "Renewal Plan", "H0007", 1L, 15, 15, "reported", 12, 12, "reported"),
    # 8. new plan (January-only row)
    row("H0010", 1L, "Kent", "New Plan", "H0010", 1L, na_r, na_r, na_c, 12, 12, "reported")
  )
  at[, `:=`(snp_type = "", dsnp_integration = NA_character_, multi_status = FALSE,
            fips = fifelse(county_name == "Kent", 10001L,
                           fifelse(county_name == "Sussex", 10005L, 10003L)),
            eligibles_2018 = 1000, enrolled_2018 = 400, benchmark = 900,
            penetration_2018 = fifelse(county_name == "Kent", 0.2,
                                       fifelse(county_name == "Sussex", 0.4, 0.6)))]
  jan_plans <- data.table(
    contract_id = c("H0001", "H0001", "H0009", "H0003", "H0005", "H0005", "H0010", "H0008"),
    plan_id = c(2L, 3L, 1L, 1L, 1L, 1L, 1L, 1L),
    county_name = c("Kent", "Kent", "Kent", "Kent", "Sussex", "New Castle", "Kent", "Sussex"))
  landscape <- rbind(
    unique(at[!is.na(dec_src), .(contract_id, plan_id, county_name, year = 2025L)]),
    jan_plans[, .(contract_id, plan_id, county_name, year = 2026L)])
  landscape[, `:=`(state_name = "Delaware", segment_id = 0L)]
  list(at = at, landscape = landscape)
}

augmented_fixture <- function() {
  f <- counting_fixture()
  augment_analytic(copy(f$at), save = FALSE, verbose = FALSE,
                   landscape = f$landscape, xwalk_years = 2026L)
}

pc <- function(at, contract, plan, county = "Kent") {
  at[contract_id == contract & plan_id == plan & county_name == county]
}


test_that("plan-county exit flags follow the successors, not the link status", {
  at <- augmented_fixture()
  flag <- function(contract, plan, county = "Kent") unique(pc(at, contract, plan, county)$forced_county)
  reason <- function(contract, plan, county = "Kent") unique(pc(at, contract, plan, county)$forced_reason)

  expect_false(flag("H0001", 1L))                       # split, successors serve Kent
  expect_false(flag("H0002", 1L))                       # moved to a contract serving Kent
  expect_true(any(pc(at, "H0002", 1L)$forced))          # ...though its termination row is a forced link
  expect_false(flag("H0003", 1L)); expect_false(flag("H0003", 2L))
  expect_true(flag("H0004", 1L))
  expect_equal(reason("H0004", 1L), "successor_drops_county")
  expect_true(flag("H0005", 1L))
  expect_equal(reason("H0005", 1L), "service_area_reduction")
  expect_false(flag("H0005", 1L, "Sussex"))
  expect_true(flag("H0006", 1L))
  expect_equal(reason("H0006", 1L), "terminated")
  expect_true(flag("H0007", 1L))
  expect_equal(reason("H0007", 1L), "renewal_not_in_landscape")
  expect_true(is.na(flag("H0010", 1L)))                 # January-only row
  expect_equal(pc(at, "H0010", 1L)$role, "new_entrant")
})

test_that("a service-area reduction zeroes January only in the dropped county", {
  at <- augmented_fixture()
  kent <- pc(at, "H0005", 1L)
  expect_true(kent$sar_dropped)
  expect_equal(kent$jan_enrollment, 0)
  expect_equal(kent$jan_src, "dropped_county")
  sussex <- pc(at, "H0005", 1L, "Sussex")
  expect_false(sussex$sar_dropped)
  expect_equal(sussex$jan_enrollment, 24)
})

test_that("_once and _split count each plan-county once", {
  at <- augmented_fixture()
  split <- pc(at, "H0001", 1L)
  expect_equal(split$prev_links, c(2L, 2L))
  expect_equal(sum(split$dec_enrollment), 200)          # repeated on both links
  expect_equal(sum(split$dec_enrollment_once), 100)
  expect_equal(sum(split$dec_enrollment_split), 100)
  expect_equal(sum(split$dec_first), 1L)

  cons <- at[curr_contract_id == "H0003" & curr_plan_id == 1L]
  expect_equal(cons$curr_links, c(2L, 2L))
  expect_equal(sum(cons$jan_enrollment), 190)           # repeated on both links
  expect_equal(sum(cons$jan_enrollment_once), 95)
  expect_equal(sum(cons$jan_enrollment_split), 95)
  expect_true(all(cons$curr_is_incumbent))

  # December once-total equals the plan-county total, whatever the links
  dec_pc <- unique(at[!is.na(dec_src), .(contract_id, plan_id, county_name, dec_enrollment)])
  expect_equal(sum(at$dec_enrollment_once, na.rm = TRUE), sum(dec_pc$dec_enrollment))
  expect_equal(sum(at$dec_enrollment_split, na.rm = TRUE), sum(dec_pc$dec_enrollment))
  # January-only rows carry no December value
  expect_true(is.na(pc(at, "H0010", 1L)$dec_enrollment_once))
})

test_that("the county panel's _once columns add up plan-counties once", {
  at <- augmented_fixture()
  panel <- make_county_panel(at, save = FALSE)
  kent <- panel[county_name == "Kent"]

  # December: 100 + 200 + 40 + 60 + 30 + 20 + 10 + 15
  expect_equal(kent$total_dec_enrollment_once, 475)
  expect_equal(kent$total_dec_enrollment_once_low, 466)  # suppressed 10 counted as 1
  expect_equal(kent$total_dec_enrollment, 805)           # link sum repeats H0001, H0002, H0004
  # Forced out: H0004 (30), H0005 in Kent (20), H0006 (10), H0007 (15)
  expect_equal(kent$displaced_enrollment_once, 75)
  expect_equal(kent$displaced_enrollment_once_low, 66)
  expect_equal(kent$exit_rate_once, 75 / 475)
  expect_equal(kent$n_exiting_plans_county, 4L)
  # January: 50 + 70 + 180 + 95 + 12 (H0007, not in the landscape) + 12 (new plan)
  expect_equal(kent$total_jan_enrollment_once, 419)
  expect_equal(kent$incumbent_jan_once, 50 + 70 + 180 + 95)
  expect_equal(kent$new_entrant_jan_once, 12 + 12)
  expect_equal(kent$incumbent_dec_once, 400)
  expect_equal(kent$enrollment_change_once, (419 - 475) / 475)
  # SNPs: H0002 (moved, not forced) and H0006 (terminated)
  expect_equal(kent$snp_dec_enrollment_once, 200 + 10)
  expect_equal(kent$snp_displaced_enrollment_once, 10)
})


# --- The variable key ---------------------------------------------------------

test_that("the variable key flags every repeated column and names its once-counted match", {
  key <- maexits_catalog("plan_county")
  flagged <- key[counting != "", variable]
  expect_true(all(c("dec_enrollment", "dec_enrollment_low", "jan_enrollment",
                    "jan_enrollment_low", "forced", "role") %in% flagged))
  expect_false(any(grepl("_once|_split|_links|_first$|^forced_", flagged)))
  # Every column a counting note names is itself a documented column
  named <- unique(unlist(regmatches(key$counting, gregexpr("`[a-z_]+`", key$counting))))
  expect_true(all(gsub("`", "", named) %in% key$variable))

  # Each flagged panel column has a _once counterpart or names one
  panel_key <- maexits_catalog("county_panel")
  flagged <- panel_key[counting != ""]
  has_once <- paste0(flagged$variable, "_once") %in% panel_key$variable
  names_alt <- grepl("`[a-z_]+_(once|county)`", flagged$counting)
  expect_true(all(has_once | names_alt))
})

test_that("help pages list only their own dataset's variables", {
  fmt <- .rd_format("landscape")
  expect_equal(length(fmt) - 2L, nrow(maexits_catalog("landscape")))
  expect_false(any(grepl("dec_enrollment", fmt)))
  expect_true(any(grepl("\\*Counting:\\*", .rd_format("plan_county"))))
})
