# Shared fixtures: a small December 2025 -> January 2026 transition with
# every crosswalk link pattern (split, move into another contract,
# consolidation, service-area reduction, termination, renewal not listed in
# the January landscape, new plan). Used by test-counting.R and
# test-displacement.R.

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
