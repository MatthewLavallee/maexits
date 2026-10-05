# The displacement table: one row per December plan x county with its
# January outcome, scope flags, alternatives and plan details. Built here
# from the shared fixture (helper-fixtures.R), whose plan-counties cover
# every crosswalk pattern.

displacement_fixture <- function() {
  f <- counting_fixture()
  aug <- augment_analytic(copy(f$at), save = FALSE, verbose = FALSE,
                          landscape = f$landscape, xwalk_years = 2026L)
  ls <- copy(f$landscape)[, `:=`(plan_type = "HMO", snp = "No", snp_type = NA_character_)]
  parents <- data.table(
    dec_year = 2025L,
    contract_id = c("H0001", "H0001", "H0002", "H0009", "H0003", "H0003", "H0004", "H0008",
                    "H0005", "H0005", "H0006", "H0010"),
    parent_org = c("Alpha Health", "Alpha Health", "Beta Corp", "Beta Corp.", "Gamma", "Gamma",
                   "Delta Inc", "Delta, Inc.", "Eps Old Name", "Eps New Name", "Alpha Health", "Zeta"),
    month = c("dec", "jan", "dec", "jan", "dec", "jan", "dec", "jan", "dec", "jan", "dec", "jan"))
  dec_plans <- unique(aug[!is.na(dec_src), .(contract_id, plan_id)])
  details <- dec_plans[, .(year = 2025L, contract_id, plan_id, segment_id = 0L,
                           parent_org = "someone", premium_total = 10 * plan_id,
                           moop_in_network = 5000)]
  panel <- make_county_panel(copy(aug), save = FALSE)
  suppressMessages(make_displacement(aug, landscape = ls, plan_details = details,
                                     parents = parents, panel = panel, save = FALSE))
}

row_of <- function(ds, contract, plan, county = "Kent") {
  ds[contract_id == contract & plan_id == plan & county_name == county]
}


test_that("each December plan-county appears once, with the right outcome", {
  ds <- displacement_fixture()
  expect_equal(nrow(ds), 10L)                       # January-only rows are not December plans
  expect_false(anyDuplicated(ds[, .(dec_year, contract_id, plan_id, county_name, state_name)]) > 0)
  out <- function(c, p, cty = "Kent") as.character(row_of(ds, c, p, cty)$outcome)
  expect_equal(out("H0001", 1L), "moved_plan_same_contract")      # split into two plans of H0001
  expect_equal(out("H0002", 1L), "moved_contract")                # moved into H0009, serves Kent
  expect_equal(out("H0003", 1L), "renewed")
  expect_equal(out("H0003", 2L), "moved_plan_same_contract")      # consolidated into H0003-001
  expect_equal(out("H0004", 1L), "moved_contract_not_in_county")  # H0008 does not serve Kent
  expect_equal(out("H0005", 1L), "sar_dropped_county")
  expect_equal(out("H0005", 1L, "Sussex"), "renewed_sar_kept_county")
  expect_equal(out("H0006", 1L), "terminated_contract_gone")      # H0006 is not in January
  expect_equal(out("H0007", 1L), "renewal_not_listed_in_county")
  expect_true(all(ds$lost_coverage == (ds$outcome_group == "lost_coverage")))
  expect_equal(row_of(ds, "H0002", 1L)$successor_contract_id, "H0009")
  expect_true(is.na(row_of(ds, "H0006", 1L)$successor_contract_id))
})

test_that("enrollment counts each plan-county once and matches the county panel", {
  ds <- displacement_fixture()
  kent <- ds[county_name == "Kent"]
  expect_equal(sum(kent$dec_enrollment), 475)                     # 100 + 200 + 40 + 60 + 30 + 20 + 10 + 15
  expect_equal(sum(kent$dec_enrollment[kent$lost_coverage]), 75)  # 30 + 20 + 10 + 15
  expect_equal(unique(kent$county_lost_share), 75 / 475)
  expect_equal(row_of(ds, "H0006", 1L)$dec_n_suppressed, 1L)
  expect_equal(sum(kent$dec_enrollment_low[kent$lost_coverage]), 66)
})

test_that("scope flags: plan, contract and parent", {
  ds <- displacement_fixture()
  expect_false(row_of(ds, "H0005", 1L)$plan_exits)                # still serves Sussex and New Castle
  expect_equal(row_of(ds, "H0005", 1L)$plan_share_lost, 20 / 55)
  expect_true(row_of(ds, "H0006", 1L)$plan_exits)
  expect_true(row_of(ds, "H0006", 1L)$contract_exits)
  expect_false(row_of(ds, "H0005", 1L)$contract_exits)
  expect_true(row_of(ds, "H0005", 1L)$contract_exits_county)
  # Alpha Health (H0006's parent) still offers H0001 plans in Kent
  expect_false(row_of(ds, "H0006", 1L)$parent_exits_county)
  expect_true(row_of(ds, "H0006", 1L)$parent_nonsnp_in_county_jan)
  # Delta's January contract H0008 does not serve Kent; renamed Eps does not either
  expect_true(row_of(ds, "H0004", 1L)$parent_exits_county)
  expect_true(row_of(ds, "H0005", 1L)$parent_exits_county)
  expect_true(is.na(row_of(ds, "H0007", 1L)$parent_exits_county))  # parent unknown
})

test_that("alternatives next January are counted per county", {
  ds <- displacement_fixture()
  k <- row_of(ds, "H0006", 1L)
  expect_equal(k$n_plans_jan, 5L)             # H0001-2, H0001-3, H0009-1, H0003-1, H0010-1
  expect_equal(k$n_plans_jan_other_parent, 3L)
  expect_equal(k$n_parents_jan, 4L)           # Alpha, Beta (renamed), Gamma, Zeta
  expect_equal(k$n_same_snp_type_jan, 0L)    # an SNP; no SNP serves Kent next January
  expect_true(is.na(row_of(ds, "H0003", 1L)$n_same_snp_type_jan))   # not an SNP
})

test_that("plan details are attached by year, contract, plan and segment", {
  ds <- displacement_fixture()
  expect_equal(row_of(ds, "H0003", 2L)$premium_total, 20)
  expect_equal(unique(ds$moop_in_network), 5000)
})

# The remaining outcome levels, plus a split with a tie, in Maryland
outcome_fixture <- function() {
  row <- function(contract, plan, county, status, curr_contract, curr_plan, dec, jan, jan_src = "reported") {
    data.table(dec_year = 2025L, contract_id = contract, plan_id = plan, segment_id = 0L,
               county_name = county, state_name = "Maryland", status = status,
               curr_contract_id = curr_contract, curr_plan_id = curr_plan,
               dec_enrollment = dec, dec_enrollment_low = dec, dec_src = "reported",
               jan_enrollment = jan, jan_enrollment_low = jan, jan_src = jan_src,
               plan_type = "HMO", snp = "No")
  }
  na_c <- NA_character_; na_i <- NA_integer_
  at <- rbind(
    row("H0011", 1L, "Frederick", "Renewal Plan with SAE", "H0011", 1L, 50, 52),
    row("H0012", 1L, "Frederick", "Consolidated Renewal Plan", "H0012", 1L, 40, 41),
    row("H0013", 1L, "Frederick", "New Plan", "H0014", 1L, 30, 29),
    row("H0015", 1L, "Frederick", "Terminated Plan", na_c, na_i, 20, 0, "terminated"),
    row("H0016", 1L, "Frederick", "New Plan", "H0016", 5L, 15, 0, "no_record"),
    row("H0017", 1L, "Frederick", "Renewal Plan with SAR", "H0017", 9L, 25, 0, "no_record"),
    row("H0018", 1L, "Frederick", "Consolidated Renewal Plan", "H0018", 2L, 35, 0, "no_record"),
    row("H0019", 1L, "Frederick", "Renewal Plan with SAR", "H0019", 1L, 60, 58),
    row("H0019", 1L, "Carroll", "Renewal Plan with SAR", "H0019", 1L, 45, 44),
    row("H0020", 1L, "Frederick", "Consolidated Renewal Plan", "H0020", 2L, 70, 10),
    row("H0020", 1L, "Frederick", "Consolidated Renewal Plan", "H0020", 3L, 70, 60))
  at[, `:=`(snp_type = NA_character_, dsnp_integration = NA_character_, multi_status = FALSE,
            fips = fifelse(county_name == "Frederick", 24021L, 24013L), eligibles_2018 = 1000,
            enrolled_2018 = 400, benchmark = 900,
            penetration_2018 = fifelse(county_name == "Frederick", 0.3, 0.5))]
  jan <- data.table(contract_id = c("H0011", "H0012", "H0014", "H0015", "H0016", "H0017", "H0018",
                                    "H0019", "H0019", "H0020", "H0020"),
                    plan_id = c(1L, 1L, 1L, 2L, 5L, 9L, 2L, 1L, 1L, 2L, 3L),
                    county_name = c("Frederick", "Frederick", "Frederick", "Carroll", "Carroll", "Carroll",
                                    "Carroll", "Frederick", "Carroll", "Frederick", "Frederick"))
  ls <- rbind(unique(at[, .(contract_id, plan_id, county_name, year = 2025L)]),
              jan[, .(contract_id, plan_id, county_name, year = 2026L)])
  ls[, `:=`(state_name = "Maryland", segment_id = 0L, plan_type = "HMO", snp = "No", snp_type = NA_character_)]
  aug <- augment_analytic(copy(at), save = FALSE, verbose = FALSE, landscape = ls, xwalk_years = 2026L)
  details <- unique(at[, .(year = 2025L, contract_id, plan_id, segment_id = 0L, parent_org = "p")])
  parents <- data.table(dec_year = integer(), contract_id = character(), parent_org = character(),
                        month = character())
  suppressMessages(make_displacement(aug, landscape = ls, plan_details = details, parents = parents,
                                     panel = make_county_panel(copy(aug), save = FALSE), save = FALSE))
}

test_that("every outcome level is produced by one of the fixtures", {
  ds <- outcome_fixture()
  out <- function(c, cty = "Frederick") as.character(ds[contract_id == c & county_name == cty, outcome])
  expect_equal(out("H0011"), "renewed_sae")
  expect_equal(out("H0012"), "consolidated_same_plan_id")
  expect_equal(out("H0013"), "moved_new_plan")
  expect_equal(out("H0015"), "terminated_contract_listed")     # H0015 still offers a plan in Carroll
  expect_equal(out("H0016"), "new_plan_not_in_county")
  expect_equal(out("H0017"), "sar_successor_not_in_county")
  expect_equal(out("H0018"), "moved_plan_not_in_county")
  expect_equal(out("H0019", "Carroll"), "renewed_sar_kept_county")
  levels_seen <- union(as.character(ds$outcome), as.character(displacement_fixture()$outcome))
  expect_setequal(levels_seen, .DISPLACEMENT_OUTCOMES)
})

test_that("a split keeps the successor with the most January enrollment", {
  ds <- outcome_fixture()
  r <- ds[contract_id == "H0020"]
  expect_equal(as.character(r$outcome), "moved_plan_same_contract")
  expect_equal(r$successor_plan_id, 3L)
  expect_equal(r$n_successors, 2L)
  expect_equal(r$successor_segment_id, 0L)
  expect_equal(r$n_parents_jan, 5L)   # H0011, H0012, H0014, H0019, H0020: unknown parents count per contract
})

test_that("the county reconciliation catches a dropped county or changed bounds", {
  f <- counting_fixture()
  aug <- augment_analytic(copy(f$at), save = FALSE, verbose = FALSE, landscape = f$landscape,
                          xwalk_years = 2026L)
  panel <- suppressMessages(make_county_panel(copy(aug), save = FALSE))
  ds <- displacement_fixture()
  expect_silent(.check_displacement(ds, panel))
  expect_error(.check_displacement(ds[county_name != "Sussex"], panel), "differ from county_panel")
  low <- copy(ds)[lost_coverage == TRUE, dec_enrollment_low := 0]
  expect_error(.check_displacement(low, panel), "differ from county_panel")
})

test_that("CMS money, star and header formats parse the same way every year", {
  expect_equal(.parse_money(c("$-", "($7.20)", "$(21.70)", "-$9.99", " $6,700 ", "28.1",
                              "$49.000", "N/A", "Not Applicable", "")),
               c(0, -7.2, -21.7, -9.99, 6700, 28.1, 49, NA, NA, NA))
  expect_equal(.parse_star(c("4 Stars", "3.5 Stars", "4", "4.0", "4.5",
                             "Not enough data available", "Plan too new to be measured", "")),
               c(4, 3.5, 4, 4, 4.5, NA, NA, NA))
  expect_equal(.star_status(c("4.5", "Not Enough Data Available", "Plan too new to be measured",
                              "Not Applicable", "")),
               c("rated", "not enough data", "plan too new", "not applicable", NA))
  expect_equal(.norm_header(c("Monthly Consolidated Premium\n(Includes\nPart C + D)",
                              "Overall            Star Rating", "Part C Premium2",
                              "In-network MOOP Amount **", "Drug Benefit Type", "Drug Benefit Type")),
               c("Monthly Consolidated Premium (Includes Part C + D)", "Overall Star Rating",
                 "Part C Premium", "In-network MOOP Amount", "Drug Benefit Type",
                 "Drug Benefit Type [2]"))
  expect_equal(.norm_header(c("Overal Star Rating", "2024 Overall Star Rating", "Part C Premium 2")),
               c("Overall Star Rating", "Overall Star Rating [2]", "Part C Premium"))
  expect_equal(.norm_header(c("A", "A", "A")), c("A", "A [2]", "A [3]"))
})

test_that("the consolidation statuses are continuing statuses", {
  expect_true(all(MAEXITS_XWALK_CONSOLIDATION_STATUSES %in% .statuses("continuing")))
})


# --- Built tables (when present) --------------------------------------------------

test_that("the built displacement table reconciles and matches the recorded outcome totals", {
  skip_if_not(file.exists(here("trunk", "derived", "displacement.csv")), "displacement.csv not built")
  ds <- fread(here("trunk", "derived", "displacement.csv"), na.strings = c("", "NA"),
              select = c("dec_year", "outcome", "lost_coverage", "dec_enrollment"))
  panel <- fread(here("trunk", "derived", "county_panel.csv"),
                 select = c("dec_year", "total_dec_enrollment_once", "displaced_enrollment_once"))
  byy <- ds[, .(dec = sum(dec_enrollment), lost = sum(dec_enrollment[lost_coverage])), keyby = dec_year]
  p <- panel[, .(dec = sum(total_dec_enrollment_once), lost = sum(displaced_enrollment_once)), keyby = dec_year]
  expect_equal(byy$lost[byy$dec_year %in% p$dec_year], p$lost)
  lost <- ds[lost_coverage == TRUE, .(enr = sum(dec_enrollment)), keyby = .(outcome, dec_year)]
  snap <- function(o) lost[outcome == o][order(dec_year), enr]
  skip_if_not(identical(sort(unique(ds$dec_year)), 2018:2025), "snapshot recorded for dec_year 2018-2025")
  expect_equal(nrow(ds), 764177L)
  expect_equal(byy$lost, c(529048, 252962, 153419, 99894, 343848, 377253, 2053458, 2936310))
  expect_equal(snap("terminated_contract_listed"),
               c(73860, 126410, 62209, 29138, 203377, 208118, 1064666, 1759449))
  expect_equal(snap("sar_dropped_county"),
               c(383858, 74704, 34523, 38505, 39868, 47916, 525739, 653230))
})

test_that("built plan details are one row per plan-segment with parsed values", {
  skip_if_not(file.exists(here("trunk", "derived", "plan_details.csv")), "plan_details.csv not built")
  pd <- fread(here("trunk", "derived", "plan_details.csv"), na.strings = c("", "NA"))
  expect_false(anyDuplicated(pd[, .(year, contract_id, plan_id, segment_id)]) > 0)
  expect_true(all(is.na(pd[snp == "Yes" & year <= 2024, moop_in_network])))
  expect_gt(mean(!is.na(pd$premium_total)), 0.99)
  expect_gt(mean(!is.na(pd$parent_org)), 0.98)
  # Reported $0 MOOPs are kept; PFFS and MSA plans have none
  expect_equal(pd[year == 2024 & contract_id == "H8634" & plan_id == 14, unique(moop_in_network)], 0)
  expect_true(all(is.na(pd[org_type %in% c("PFFS", "MSA"), moop_in_network])))
  # A Part D plan missing from the Part D report keeps an NA Part C premium
  expect_true(is.na(pd[year == 2023 & contract_id == "H3305" & plan_id == 33, premium_part_c][1]))
})

test_that("D-SNP integration labels keep CMS's short codes in every year", {
  expect_equal(.dsnp_code(c("Coordination Only (CO)", "Highly Integrated (HIDE)", "Fully Integrated (FIDE)",
                            "CO", " FIDE ", "Not Applicable", "", NA), "test"),
               c("CO", "HIDE", "FIDE", "CO", "FIDE", "Not Applicable", NA, NA))
  expect_error(.dsnp_code("Partially Integrated (PIDE)", "landscape CY2099"), "unknown D-SNP integration")
})
