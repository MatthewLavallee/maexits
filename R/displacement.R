# ============================================================
# displacement.R — Who lost their plan each December-to-January, and how
#
# make_displacement() builds one row per December plan x county
# (dec_year, contract, plan, county): its December enrollment, what
# happened to it in January (outcome, lost_coverage), whether the whole
# plan, contract or parent organization left, how many plans the county
# has next January, and the plan's details from make_plan_details().
#
# It is built from the augmented analytic table (plan_county), whose rows
# are crosswalk links; here each plan-county is one row, so enrollment
# sums count everyone once. lost_coverage is identical to forced_county.
# ============================================================


# Outcome levels: continuing (the plan-county's enrollees keep a plan that
# serves the county) and lost coverage (no successor serves the county)
.DISPLACEMENT_OUTCOMES <- c(
  "renewed", "renewed_sae", "renewed_sar_kept_county", "consolidated_same_plan_id",
  "moved_plan_same_contract", "moved_contract", "moved_new_plan",
  "terminated_contract_listed", "terminated_contract_gone", "new_plan_not_in_county",
  "sar_dropped_county", "sar_successor_not_in_county", "moved_plan_not_in_county",
  "moved_contract_not_in_county", "renewal_not_listed_in_county")
.DISPLACEMENT_LOST <- .DISPLACEMENT_OUTCOMES[8:15]


# Parent names compared case- and punctuation-blind; non-ASCII characters
# (including malformed ones in some CMS files) become spaces
.norm_org <- function(x) {
  x <- iconv(x, "UTF-8", "ASCII", sub = " ")
  gsub("\\s+", " ", trimws(gsub("[^A-Z0-9 ]", " ", toupper(x), useBytes = TRUE)))
}


#' Parent organizations of each contract around each transition
#'
#' From CPSC Contract Info: the December (dec_year) and January
#' (dec_year + 1) parent of every contract. A contract's parent set holds
#' both, so a rename between the two months (e.g. Aetna Inc to CVS Health
#' Corporation in 2019) still matches.
#' @return data.table(dec_year, contract_id, parent_org, month) with month
#'   "dec" or "jan".
#' @keywords internal
.contract_parents <- function(dec_years) {
  rbindlist(lapply(as.integer(dec_years), function(y) {
    rbindlist(list(
      if (!is.null(ci <- .contract_info(y, 12L))) unique(ci[, .(dec_year = y, contract_id, parent_org, month = "dec")]),
      if (!is.null(ci <- .contract_info(y + 1L, 1L))) unique(ci[, .(dec_year = y, contract_id, parent_org, month = "jan")])
    ))
  }))[!is.na(parent_org)]
}


#' Build the Displacement Dataset
#'
#' One row per December plan x county (`dec_year`, contract, plan, county)
#' with its December enrollment and what happened to it the next January:
#' the `outcome`, whether its enrollees lost their plan (`lost_coverage`),
#' whether the whole plan, contract or parent organization left, how many
#' plans the county offers next January, county totals, and the plan's
#' details (premiums, out-of-pocket maximum, star ratings, SNP details,
#' organization). Each plan-county appears once, so sums of
#' `dec_enrollment` count everyone once. See [displacement] for every
#' column.
#'
#' @param at Augmented analytic table (`augment_analytic()` output); NULL
#'   reads `trunk/derived/analytictable_augmented.csv`.
#' @param landscape Landscape table; NULL reads `trunk/derived/landscape.csv`.
#' @param plan_details Plan details (`make_plan_details()`); NULL reads
#'   `trunk/derived/plan_details.csv`.
#' @param parents Contract parents (`.contract_parents()` format); NULL
#'   reads CPSC Contract Info from raw/.
#' @param panel County panel for the reconciliation checks; NULL reads
#'   `trunk/derived/county_panel.csv` if it exists.
#' @param save If TRUE, write `trunk/derived/displacement.csv`.
#' @return data.table.
#' @export
make_displacement <- function(at = NULL, landscape = NULL, plan_details = NULL,
                              parents = NULL, panel = NULL, save = TRUE) {
  rd <- function(x, f) {
    if (!is.null(x)) return(copy(as.data.table(x)))
    fread(here("trunk", "derived", f), na.strings = c("", "NA"))
  }
  at <- rd(at, "analytictable_augmented.csv")
  landscape <- rd(landscape, "landscape.csv")
  plan_details <- rd(plan_details, "plan_details.csv")
  if (is.null(panel) && file.exists(here("trunk", "derived", "county_panel.csv"))) {
    panel <- rd(NULL, "county_panel.csv")
  }
  landscape[, `:=`(year = as.integer(year), plan_id = as.integer(plan_id))]
  key <- c("dec_year", "contract_id", "plan_id", "county_name", "state_name")

  # --- December links -------------------------------------------------------
  d <- at[!is.na(dec_src)]
  d[, `:=`(dec_year = as.integer(dec_year), plan_id = as.integer(plan_id),
           curr_plan_id = as.integer(curr_plan_id))]
  cont <- c("continuing", "service_area_reduction", "service_area_expansion")
  d[, .cls := unname(MAEXITS_XWALK_STATUS_CLASS[status])]
  d[, .same_contract := !is.na(curr_contract_id) & curr_contract_id == contract_id]
  d[, .same_plan := .same_contract & !is.na(curr_plan_id) & curr_plan_id == plan_id]
  # A link keeps enrollees covered when its successor serves the county
  # (as served_link in augment_analytic())
  d[, .served := .cls %in% c(cont, "new") & successor_serves %in% TRUE]
  d[, .rank := fifelse(!.served, NA_integer_, fcase(
    .same_plan & .cls == "continuing" & !status %in% MAEXITS_XWALK_CONSOLIDATION_STATUSES, 1L,
    .same_plan & .cls == "service_area_expansion", 2L,
    .same_plan & .cls == "service_area_reduction", 3L,
    .same_plan & .cls == "continuing", 4L,
    .same_contract & .cls %in% cont, 5L,
    .cls %in% cont, 6L,
    default = 7L))]
  # Among equally ranked links (a split), the successor with the most
  # January enrollment in the county, then the lowest plan ID
  d[, .jan := fcoalesce(as.numeric(jan_enrollment), -1)]
  setorderv(d, c(key, ".rank", ".jan", "curr_contract_id", "curr_plan_id"),
            order = c(rep(1L, length(key)), 1L, -1L, 1L, 1L), na.last = TRUE)

  pc <- d[, .(
    segment_id = segment_id[1], fips = fips[1],
    dec_enrollment = dec_enrollment[1], dec_enrollment_low = dec_enrollment_low[1],
    dec_src = dec_src[1],
    forced_county = forced_county[1], forced_reason = forced_reason[1],
    best_rank = .rank[1],
    successor_contract_id = if (.served[1]) curr_contract_id[1] else NA_character_,
    successor_plan_id = if (.served[1]) curr_plan_id[1] else NA_integer_,
    xwalk_statuses = paste(sort(unique(status)), collapse = " + "),
    n_links = .N,
    n_successors = uniqueN(paste(curr_contract_id, curr_plan_id)[!is.na(curr_plan_id)]),
    .any_new = any(.cls == "new"),
    .any_sar_same = any(.cls == "service_area_reduction" & .same_plan),
    .any_cont_other_contract = any(.cls %in% cont & !.same_contract),
    plan_type = plan_type[1], snp = snp[1], snp_type = snp_type[1],
    penetration_2018 = penetration_2018[1], eligibles_2018 = eligibles_2018[1],
    enrolled_2018 = enrolled_2018[1], benchmark = benchmark[1]
  ), by = key]
  .vcheck(all(pc$forced_county == is.na(pc$best_rank)),
          "displacement: forced_county disagrees with the successor links for %d plan-counties",
          sum(pc$forced_county != is.na(pc$best_rank)))
  pc[, jan_year := dec_year + 1L]
  jseg <- unique(landscape[, .(jan_year = year, successor_contract_id = contract_id,
                               successor_plan_id = plan_id, county_name, state_name,
                               successor_segment_id = as.integer(segment_id))],
                 by = c("jan_year", "successor_contract_id", "successor_plan_id", "county_name", "state_name"))
  pc <- jseg[pc, on = .(jan_year, successor_contract_id, successor_plan_id, county_name, state_name)]

  # --- Outcome ----------------------------------------------------------------
  jan_contracts <- unique(landscape[, .(jan_year = year, contract_id)])
  pc[, contract_exits := TRUE]
  pc[jan_contracts, contract_exits := FALSE, on = .(jan_year, contract_id)]
  pc[, outcome := fcase(
    best_rank == 1L, "renewed",
    best_rank == 2L, "renewed_sae",
    best_rank == 3L, "renewed_sar_kept_county",
    best_rank == 4L, "consolidated_same_plan_id",
    best_rank == 5L, "moved_plan_same_contract",
    best_rank == 6L, "moved_contract",
    best_rank == 7L, "moved_new_plan",
    forced_reason == "terminated" & .any_new, "new_plan_not_in_county",
    forced_reason == "terminated" & !contract_exits, "terminated_contract_listed",
    forced_reason == "terminated", "terminated_contract_gone",
    forced_reason == "service_area_reduction" & .any_sar_same, "sar_dropped_county",
    forced_reason == "service_area_reduction", "sar_successor_not_in_county",
    forced_reason == "successor_drops_county" & .any_cont_other_contract, "moved_contract_not_in_county",
    forced_reason == "successor_drops_county", "moved_plan_not_in_county",
    forced_reason == "renewal_not_in_landscape", "renewal_not_listed_in_county",
    default = NA_character_)]
  .vcheck(!anyNA(pc$outcome), "displacement: %d plan-counties have no outcome (forced_reason %s)",
          sum(is.na(pc$outcome)), paste(unique(pc[is.na(outcome), forced_reason]), collapse = ", "))
  pc[, outcome := factor(outcome, levels = .DISPLACEMENT_OUTCOMES)]
  pc[, lost_coverage := forced_county]
  pc[, outcome_group := fifelse(lost_coverage, "lost_coverage", "continued")]
  pc[, c("best_rank", ".any_new", ".any_sar_same", ".any_cont_other_contract", "forced_county") := NULL]

  # --- How far the exit reaches ------------------------------------------------
  pc[, `:=`(plan_n_counties = .N, plan_n_counties_lost = sum(lost_coverage),
            plan_dec_enrollment = sum(dec_enrollment),
            plan_lost_enrollment = sum(dec_enrollment[lost_coverage]),
            plan_exits = all(lost_coverage)), by = .(dec_year, contract_id, plan_id)]
  pc[, plan_share_lost := fifelse(plan_dec_enrollment > 0, plan_lost_enrollment / plan_dec_enrollment,
                                  NA_real_)]
  pc[, plan_lost_enrollment := NULL]
  pc[, contract_all_lost := all(lost_coverage), by = .(dec_year, contract_id)]
  jan_cc <- unique(landscape[, .(jan_year = year, contract_id, county_name, state_name)])
  pc[, contract_exits_county := TRUE]
  pc[jan_cc, contract_exits_county := FALSE, on = .(jan_year, contract_id, county_name, state_name)]

  # --- Parent organizations and alternatives next January ---------------------
  if (is.null(parents)) parents <- .contract_parents(unique(pc$dec_year))
  parents <- as.data.table(parents)[, .(dec_year = as.integer(dec_year), contract_id,
                                        parent_key = .norm_org(parent_org), month)]
  pset <- unique(parents[nzchar(parent_key), .(dec_year, contract_id, parent_key)])
  # One parent per contract for counting organizations: the side's own month
  # first; a contract with no Contract Info parent counts as its own
  # organization
  primary <- function(first) {
    parents[nzchar(parent_key)][order(month != first)][, .SD[1], by = .(dec_year, contract_id)][
      , .(dec_year, contract_id, parent_key)]
  }

  lsp <- function(side) {
    yr <- if (side == "jan") pc[, unique(jan_year)] else pc[, unique(dec_year)]
    l <- unique(landscape[year %in% yr, .(year, contract_id, plan_id, county_name, state_name,
                                          snp, snp_type, plan_type)])
    l[, dec_year := if (side == "jan") year - 1L else year]
    l[, `:=`(.ma = !(snp %in% "Yes") & !grepl("^Cost", plan_type), .cp = paste(contract_id, plan_id))]
    l
  }
  jl <- lsp("jan")
  dl <- lsp("dec")

  ccount <- function(l, prefix, prim) {
    out <- l[, .(n_all = uniqueN(.cp), n_ma = uniqueN(.cp[.ma])), by = .(dec_year, county_name, state_name)]
    np <- prim[l[.ma == TRUE], on = .(dec_year, contract_id)]
    np[is.na(parent_key), parent_key := paste("CONTRACT", contract_id)]
    np <- np[, .(n_par = uniqueN(parent_key)), by = .(dec_year, county_name, state_name)]
    out <- np[out, on = .(dec_year, county_name, state_name)]
    out[is.na(n_par), n_par := 0L]
    setnames(out, c("n_all", "n_ma", "n_par"), paste0(prefix, c("_all", "", "_parents")))
    out
  }
  jcount <- ccount(jl, "n_plans_jan", primary("jan"))
  dcount <- ccount(dl, "n_plans_dec_county", primary("dec"))
  pc <- jcount[pc, on = .(dec_year, county_name, state_name)]
  pc <- dcount[pc, on = .(dec_year, county_name, state_name)]
  for (v in c("n_plans_jan", "n_plans_jan_all", "n_plans_jan_parents", "n_plans_dec_county",
              "n_plans_dec_county_all", "n_plans_dec_county_parents")) {
    set(pc, which(is.na(pc[[v]])), v, 0L)
  }
  setnames(pc, c("n_plans_jan_parents", "n_plans_dec_county_parents"),
           c("n_parents_jan", "n_parents_dec_county"))
  pc[, n_plans_dec_county_all := NULL]

  # Same SNP type next January (SNP rows only)
  sj <- jl[snp %in% "Yes", .(n_same_snp_type_jan = uniqueN(.cp)),
           by = .(dec_year, county_name, state_name, snp_type)]
  pc <- sj[pc, on = .(dec_year, county_name, state_name, snp_type)]
  pc[snp %in% "Yes" & is.na(n_same_snp_type_jan), n_same_snp_type_jan := 0L]
  pc[!snp %in% "Yes", n_same_snp_type_jan := NA_integer_]

  # Does the same parent offer a plan in the county next January?
  pc[, .row := .I]
  rp <- pset[pc[, .(.row, dec_year, contract_id, county_name, state_name)],
             on = .(dec_year, contract_id), nomatch = 0L, allow.cartesian = TRUE]
  jp <- pset[jl, on = .(dec_year, contract_id), nomatch = 0L, allow.cartesian = TRUE]
  any_par <- unique(jp[, .(dec_year, county_name, state_name, parent_key)])
  same_any <- any_par[rp, on = .(dec_year, county_name, state_name, parent_key), nomatch = 0L]
  ma_par <- unique(jp[.ma == TRUE, .(dec_year, county_name, state_name, parent_key, .cp)])
  same_ma <- ma_par[rp, on = .(dec_year, county_name, state_name, parent_key), nomatch = 0L,
                    allow.cartesian = TRUE][, .(n_same = uniqueN(.cp)), by = .row]
  known <- unique(rp$.row)
  pc[, parent_exits_county := fifelse(.row %in% known, !.row %in% same_any$.row, NA)]
  pc[, parent_nonsnp_in_county_jan := fifelse(.row %in% known, .row %in% same_ma$.row, NA)]
  pc[same_ma, n_same := i.n_same, on = ".row"]
  pc[, n_plans_jan_other_parent := fifelse(.row %in% known, n_plans_jan - fcoalesce(n_same, 0L), NA_integer_)]
  pc[, c(".row", "n_same", "jan_year") := NULL]

  # --- County totals -------------------------------------------------------------
  pc[, `:=`(county_dec_enrollment = sum(dec_enrollment),
            county_lost_enrollment = sum(dec_enrollment[lost_coverage])),
     by = .(dec_year, county_name, state_name)]
  pc[, county_lost_share := fifelse(county_dec_enrollment > 0,
                                    county_lost_enrollment / county_dec_enrollment, NA_real_)]
  pc[, dec_n_suppressed := as.integer(round((dec_enrollment - dec_enrollment_low) / 9))]
  pc[!snp %in% "Yes", snp_type := NA_character_]

  # --- Plan details ------------------------------------------------------------
  det <- copy(as.data.table(plan_details))
  det[, `:=`(year = as.integer(year), plan_id = as.integer(plan_id), segment_id = as.integer(segment_id))]
  det[, intersect(c("plan_type", "snp", "snp_type"), names(det)) := NULL]
  setnames(det, "year", "dec_year")
  pc <- merge(pc, det, by = c("dec_year", "contract_id", "plan_id", "segment_id"),
              all.x = TRUE, sort = FALSE)

  front <- c(key, "segment_id", "fips", "dec_enrollment", "dec_enrollment_low", "dec_n_suppressed",
             "dec_src", "lost_coverage", "outcome", "outcome_group", "forced_reason",
             "xwalk_statuses", "n_links", "n_successors", "successor_contract_id", "successor_plan_id",
             "successor_segment_id",
             "plan_exits", "plan_n_counties", "plan_n_counties_lost", "plan_dec_enrollment",
             "plan_share_lost", "contract_exits", "contract_all_lost", "contract_exits_county",
             "parent_exits_county", "parent_nonsnp_in_county_jan", "n_plans_jan",
             "n_plans_jan_other_parent", "n_parents_jan", "n_same_snp_type_jan", "n_plans_jan_all",
             "n_plans_dec_county", "n_parents_dec_county", "county_dec_enrollment",
             "county_lost_enrollment", "county_lost_share", "penetration_2018", "eligibles_2018",
             "enrolled_2018", "benchmark", "plan_type", "snp", "snp_type")
  setcolorder(pc, c(front, setdiff(names(pc), front)))
  setorderv(pc, key)

  .check_displacement(pc, panel)
  message(sprintf("Displacement: %d plan-counties, %d with lost coverage (dec_year %s)",
                  nrow(pc), sum(pc$lost_coverage), paste(range(pc$dec_year), collapse = "-")))
  if (save) fwrite(pc, here("trunk", "derived", "displacement.csv"))
  invisible(pc)
}


#' Checks on the displacement table
#'
#' One row per plan-county; December and lost enrollment equal the county
#' panel's once-counted totals county by county; enrolled plan-counties
#' have plan details.
#' @keywords internal
.check_displacement <- function(ds, panel = NULL) {
  key <- c("dec_year", "contract_id", "plan_id", "county_name", "state_name")
  .vcheck(!anyDuplicated(ds, by = key), "displacement has duplicate plan-county rows")
  .vcheck(all(ds$lost_coverage == (ds$outcome %in% .DISPLACEMENT_LOST)),
          "displacement: lost_coverage and outcome disagree")
  need <- c("total_dec_enrollment_once", "displaced_enrollment_once", "displaced_enrollment_once_low",
            "snp_displaced_enrollment_once", "n_exiting_plans_county")
  if (!is.null(panel) && all(need %in% names(panel))) {
    cty <- ds[county_name != "" & state_name != "", .(
      dec = sum(dec_enrollment), lost = sum(dec_enrollment[lost_coverage]),
      lost_low = sum(dec_enrollment_low[lost_coverage]),
      snp_lost = sum(dec_enrollment[lost_coverage & snp %in% "Yes"]),
      n_exit = uniqueN(paste(contract_id, plan_id)[lost_coverage])),
      by = .(dec_year, county_name, state_name)]
    p <- as.data.table(panel)[, .(dec_year = as.integer(dec_year), county_name, state_name,
                                  p_dec = total_dec_enrollment_once, p_lost = displaced_enrollment_once,
                                  p_lost_low = displaced_enrollment_once_low,
                                  p_snp_lost = snp_displaced_enrollment_once,
                                  p_n_exit = n_exiting_plans_county)]
    # A county-year on one side only must hold no December enrollment
    cmp <- merge(cty, p, by = c("dec_year", "county_name", "state_name"), all = TRUE)
    for (v in setdiff(names(cmp), c("dec_year", "county_name", "state_name"))) {
      set(cmp, which(is.na(cmp[[v]])), v, 0)
    }
    bad <- cmp[abs(dec - p_dec) > 1e-6 | abs(lost - p_lost) > 1e-6 | abs(lost_low - p_lost_low) > 1e-6 |
                 abs(snp_lost - p_snp_lost) > 1e-6 | n_exit != p_n_exit]
    .vcheck(nrow(bad) == 0, paste0(
      "displacement: enrollment or exiting plans differ from county_panel in %d county-years ",
      "(e.g. %s)"), nrow(bad), paste(head(bad[, paste(county_name, state_name, dec_year)], 3), collapse = "; "))
  }
  if ("parent_org" %in% names(ds)) {
    enr <- ds[dec_enrollment > 0]
    share <- enr[, sum(dec_enrollment[!is.na(parent_org)]) / sum(dec_enrollment)]
    .vcheck(share >= 0.999, "displacement: plan details found for only %.2f%% of December enrollment",
            100 * share)
  }
  invisible(TRUE)
}
