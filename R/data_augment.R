# ============================================================
# data_augment.R — Augmentation and Panel Construction (Steps 5-10)
#
# Functions in this file take the analytic table produced by
# data_build.R and add FIPS codes, penetration data, county
# benchmarks, forced disenrollment flags, and collapse to the
# county-year panel.
#
# Pipeline order:
#   5. add_fips()                  → adds fips column
#   6. add_penetration()           → adds penetration_2018
#   7. add_fips_and_penetration()  → convenience wrapper (5 + 6)
#   8. add_benchmark()             → adds MA county benchmark
#   9. augment_analytic()          → analytictable_augmented.csv
#  10. make_county_panel()         → county_panel.csv
#  11. run_data_pipeline()         → runs everything (steps 1-10)
# ============================================================


# --- Step 5: Add FIPS Codes ------------------------------------------

#' Add FIPS Codes to the Analytic Table
#'
#' Builds a county-to-FIPS lookup from the CPSC file set in
#' \code{MAEXITS_FIPS_LOOKUP_FILE} (R/config.R; January 2025) and merges
#' FIPS codes onto the analytic table by county and state name. Stops if more than 0.5% of a year's rows get no FIPS code.
#'
#' @param at data.table or NULL. If NULL, reads from
#'   \code{trunk/derived/analytictable.csv}.
#' @param save Logical. If TRUE, writes updated table to
#'   \code{trunk/derived/analytictable.csv}. Default FALSE (typically
#'   called via \code{add_fips_and_penetration()}).
#' @return The input data.table with a \code{fips} column added.
#' @export
add_fips <- function(at = NULL, save = FALSE) {
  at <- .load_if_null(at, "trunk/derived/analytictable.csv")
  if ("fips" %in% names(at)) at[, fips := NULL]  # safe to re-run

  fips_lookup <- .fips_lookup()

  # Merge FIPS onto analytic table (county names use the CPSC spelling; see
  # MAEXITS_COUNTY_NAME_MAP)
  at <- merge(at, fips_lookup, by = c("county_name", "state_name"), all.x = TRUE)
  message("FIPS merge rate: ", round(sum(!is.na(at$fips)) / nrow(at) * 100, 1), "%")
  .check_fips(at)

  if (save) {
    fwrite(at, here("trunk", "derived", "analytictable.csv"))
  }

  invisible(at)
}


#' County-to-FIPS lookup used by add_fips()
#'
#' Built from the CPSC file set in \code{MAEXITS_FIPS_LOOKUP_FILE}; one
#' FIPS code per county and state (the first when CMS lists several), using
#' CPSC county spellings.
#'
#' @return data.table with county_name, state_name, fips.
#' @keywords internal
.fips_lookup <- function() {
  cpsc <- fread(here("raw", MAEXITS_FIPS_LOOKUP_FILE))
  fips_raw <- unique(cpsc[, .(`FIPS State County Code`, `SSA State County Code`,
                               State, County)])
  setnames(fips_raw, c("fips", "ssa", "state_abb", "county_name"))
  fips_raw[, state_name := .state_name_from_abb(state_abb)]

  fips_raw <- fips_raw[!is.na(fips) & fips != 0]
  fips_lookup <- unique(fips_raw[, .(county_name, state_name, fips)])

  # Deduplicate (keep first FIPS per county if multiple)
  dups <- fips_lookup[, .N, by = .(county_name, state_name)][N > 1]
  if (nrow(dups) > 0) {
    message("Counties with multiple FIPS codes: ", nrow(dups))
    fips_lookup <- fips_lookup[, .SD[1], by = .(county_name, state_name)]
  }

  fips_lookup
}


#' Full state or territory name from a postal abbreviation
#' @keywords internal
.state_name_from_abb <- function(abb) {
  out <- datasets::state.name[match(abb, datasets::state.abb)]
  fcase(
    abb == "DC", "District of Columbia",
    abb == "AS", "American Samoa",
    abb == "GU", "Guam",
    abb == "MP", "Northern Mariana Islands",
    abb == "PR", "Puerto Rico",
    abb == "VI", "U.S. Virgin Islands",
    default = out
  )
}


# --- Step 6: Add Penetration Data ------------------------------------

#' Add 2018 MA Penetration Data to the Analytic Table
#'
#' Reads the CMS State/County MA Penetration file for 2018 and merges
#' penetration rates onto the analytic table via FIPS codes.
#'
#' @param at data.table or NULL. Must have a \code{fips} column (i.e.,
#'   \code{add_fips()} must be called first). If NULL, reads from
#'   \code{trunk/derived/analytictable.csv}.
#' @param save Logical. If TRUE, writes updated table to
#'   \code{trunk/derived/analytictable.csv}. Default FALSE.
#' @return The input data.table with columns \code{eligibles_2018},
#'   \code{enrolled_2018}, and \code{penetration_2018} added.
#' @export
add_penetration <- function(at = NULL, save = FALSE) {
  at <- .load_if_null(at, "trunk/derived/analytictable.csv")

  # Validate that FIPS column exists
 if (!"fips" %in% names(at)) {
    stop("'fips' column not found. Run add_fips() before add_penetration().")
  }

  # Safe to re-run
  old <- intersect(c("eligibles_2018", "enrolled_2018", "penetration_2018"), names(at))
  if (length(old)) at[, (old) := NULL]

  pen <- fread(here("raw", MAEXITS_PENETRATION_FILE))

  # CMS marks suppressed/undefined values with "*" (Enrolled) and "."
  # (Penetration); those become NA, anything else unexpected still warns.
  to_num <- function(x, strip) {
    x <- gsub(strip, "", as.character(x))
    as.numeric(fifelse(x %in% c("*", ".", ""), NA_character_, x))
  }
  pen[, fips := as.integer(FIPS)]
  pen[, eligibles := to_num(Eligibles, ",")]
  pen[, enrolled := to_num(Enrolled, ",")]
  pen[, penetration := to_num(Penetration, "%") / 100]

  pen_clean <- pen[!is.na(fips) & fips > 0,
                   .(fips, eligibles_2018 = eligibles,
                     enrolled_2018 = enrolled,
                     penetration_2018 = penetration)]

  at <- merge(at, pen_clean, by = "fips", all.x = TRUE)
  message("Penetration merge rate: ",
          round(sum(!is.na(at$penetration_2018)) / nrow(at) * 100, 1), "%")

  if (save) {
    fwrite(at, here("trunk", "derived", "analytictable.csv"))
  }

  invisible(at)
}


# --- Step 7: Convenience Wrapper -------------------------------------

#' Add FIPS Codes and 2018 Penetration Data
#'
#' Convenience wrapper that calls \code{add_fips()} then
#' \code{add_penetration()} and saves the result.
#'
#' @param at data.table or NULL. If NULL, reads from
#'   \code{trunk/derived/analytictable.csv}.
#' @param save Logical. If TRUE (default), writes the augmented table
#'   to \code{trunk/derived/analytictable.csv}.
#' @return Invisibly returns the data.table with fips and penetration columns.
#' @export
add_fips_and_penetration <- function(at = NULL, save = TRUE) {
  at <- add_fips(at, save = FALSE)
  at <- add_penetration(at, save = FALSE)
  if (save) {
    fwrite(at, here("trunk", "derived", "analytictable.csv"))
  }
  invisible(at)
}


# --- Step 8: Add MA County Benchmark Rates ----------------------------

#' Add MA County Benchmark Rates
#'
#' Merges NBER Medicare Advantage Ratebook county-level benchmark rates
#' (Parts A&B, 0% bonus) onto the analytic table by FIPS code and year.
#' Uses the CPSC enrollment file as an SSA-to-FIPS crosswalk.
#'
#' The benchmark for year Y is the monthly per-enrollee capitation rate
#' paid during calendar year Y. We merge on dec_year = ratebook year,
#' so dec_year 2023 gets the 2023 rate (the concurrent rate, not the
#' rate announced for the following year's exit decisions).
#'
#' @param at data.table or NULL. If NULL, reads from
#'   \code{trunk/derived/analytictable.csv}.
#' @param save Logical. If TRUE, writes updated table to
#'   \code{trunk/derived/analytictable.csv}. Default FALSE.
#' @param verbose Logical. If TRUE (default), prints merge diagnostics.
#' @return The input data.table with a \code{benchmark} column added.
#' @export
add_benchmark <- function(at = NULL, save = FALSE, verbose = TRUE) {
  at <- .load_if_null(at, "trunk/derived/analytictable.csv")
  at <- copy(at)

  # Idempotency guard
  if ("benchmark" %in% names(at)) at[, benchmark := NULL]

  # Validate FIPS column exists
  if (!"fips" %in% names(at)) {
    stop("'fips' column not found. Run add_fips() before add_benchmark().")
  }

  # 1. Build SSA-FIPS crosswalk from CPSC file
  cpsc <- fread(here("raw", MAEXITS_FIPS_LOOKUP_FILE),
                colClasses = list(character = c("SSA State County Code",
                                                "FIPS State County Code")))
  ssa_fips <- unique(cpsc[`FIPS State County Code` != "",
                          .(`SSA State County Code`, `FIPS State County Code`)])
  setnames(ssa_fips, c("ssa_code", "fips"))
  # Normalize SSA codes to 5-digit zero-padded
  ssa_fips[, ssa_code := .pad0(ssa_code, 5)]
  # Cast FIPS to integer to match analytic table type
  ssa_fips[, fips := as.integer(fips)]
  # Note: some FIPS map to multiple SSA (LA 06037 -> 05200 + 05210,
  # plus territories). Deduplication happens after ratebook merge.

  # 2. Read the ratebook for every dec_year in the table (the benchmark for
  # dec_year Y is the countyrateY.csv rate)
  years <- sort(unique(as.integer(at$dec_year)))
  rb_list <- lapply(years, function(yr) {
    path <- here("raw", "ratebook", paste0("countyrate", yr, ".csv"))
    .vcheck(file.exists(path), paste0(
      "raw/ratebook/countyrate%d.csv not found (needed for dec_year %d). ",
      "Download it from https://www.nber.org/research/data/medicare-advantage-ratebook-data"),
      yr, yr)
    first_line <- readLines(path, n = 1, warn = FALSE)
    if (grepl("ssa_code", first_line) && grepl("parts_0_bonus", first_line)) {
      # NBER layout
      rb <- fread(path, colClasses = list(character = "ssa_code"),
                  select = c("ssa_code", "parts_0_bonus"))
      setnames(rb, "parts_0_bonus", "benchmark")
    } else {
      # CMS layout (as in 2024): 3 metadata rows, then a "0% ... Bonus" column
      rb <- fread(path, skip = 3, colClasses = list(character = 1))
      setnames(rb, 1, "ssa_code")
      bonus_col <- grep("0%.*Bonus", names(rb), value = TRUE)
      .vcheck(length(bonus_col) == 1, paste0(
        "countyrate%d.csv has an unrecognised layout: no ssa_code/parts_0_bonus ",
        "header and no single '0%% ... Bonus' column after 3 metadata rows"), yr)
      .vcheck(grepl(as.character(yr), bonus_col, fixed = TRUE),
              "countyrate%d.csv is a CMS ratebook for a different year (column '%s')",
              yr, bonus_col)
      rb <- rb[, .(ssa_code, benchmark = get(bonus_col))]
    }
    # Normalize SSA codes to 5-digit zero-padded
    rb[, ssa_code := .pad0(ssa_code, 5)]
    # Filter territory placeholders and blank rows
    rb <- rb[!grepl("X", ssa_code) & ssa_code != "" & !is.na(ssa_code)]
    # Parse benchmark: remove commas, convert to numeric
    rb[, benchmark := as.numeric(gsub(",", "", benchmark))]
    rb[, year := yr]
    rb
  })
  ratebook <- rbindlist(rb_list)
  # A copied or mislabelled file repeats the prior year's rates
  # (historically 0% of county rates are equal year to year)
  for (yr in intersect(years, years + 1L)) {
    m <- merge(ratebook[year == yr - 1L], ratebook[year == yr], by = "ssa_code")
    same <- mean(m$benchmark.x == m$benchmark.y, na.rm = TRUE)
    .vcheck(same < 0.5, "countyrate%d.csv: %.0f%% of county rates equal countyrate%d.csv; is it the right year?",
            yr, 100 * same, yr - 1L)
  }

  # 3. Merge SSA-FIPS crosswalk onto ratebook
  benchmark_fips <- merge(ratebook, ssa_fips, by = "ssa_code", all.x = FALSE)
  # Deduplicate: one benchmark per fips-year (LA has 2 SSA codes with
  # identical benchmarks; territories may differ — take first)
  benchmark_fips <- benchmark_fips[, .(benchmark = benchmark[1]),
                                    by = .(fips, year)]

  # 4. Merge onto analytic table
  n_before <- nrow(at)
  at <- merge(at, benchmark_fips[, .(fips, year, benchmark)],
              by.x = c("fips", "dec_year"), by.y = c("fips", "year"),
              all.x = TRUE)
  stopifnot(nrow(at) == n_before)

  # 5. Report merge rate
  merge_rate <- 100 * sum(!is.na(at$benchmark)) / nrow(at)
  if (verbose) {
    cat(sprintf("Benchmark merge rate: %.1f%% (%d/%d rows)\n",
                merge_rate, sum(!is.na(at$benchmark)), nrow(at)))
    cat(sprintf("Missing: %d rows\n", sum(is.na(at$benchmark))))
  }
  .check_benchmark(at)

  if (save) {
    fwrite(at, here("trunk", "derived", "analytictable.csv"))
    if (verbose) cat("Saved analytictable.csv with benchmark column\n")
  }

  invisible(at)
}


# --- Step 9: Augment with Displacement Flags -------------------------

#' Augment Analytic Table with Forced Disenrollment Flags
#'
#' Identifies SAR (Service Area Reduction) counties where plans
#' actually dropped the county, flags forced disenrollment (terminated
#' plans + SAR-dropped counties), and classifies plan roles (exiting,
#' incumbent, new_entrant).
#'
#' @param at data.table or NULL. If NULL, reads from
#'   \code{trunk/derived/analytictable.csv}.
#' @param save Logical. If TRUE (default), writes the augmented table
#'   to \code{trunk/derived/analytictable_augmented.csv}.
#' @param verbose Logical. If TRUE (default), prints diagnostic tables
#'   showing forced disenrollment counts and growth in affected counties.
#' @param landscape data.table or NULL. The landscape used to build
#'   \code{at}; if NULL, reads \code{trunk/derived/landscape.csv}. Pass it
#'   explicitly when running with \code{save = FALSE}, so a stale file on
#'   disk cannot be used.
#' @param xwalk_years Integer vector of crosswalk years; must match the
#'   years \code{at} was built with (default \code{MAEXITS_XWALK_YEARS}).
#' @return Invisibly returns the augmented data.table with columns:
#'   sar_dropped, forced, role.
#' @export
augment_analytic <- function(at = NULL, save = TRUE, verbose = TRUE,
                             landscape = NULL,
                             xwalk_years = MAEXITS_XWALK_YEARS) {
  at <- .load_if_null(at, "trunk/derived/analytictable.csv")
  ls_data <- if (is.null(landscape)) {
    fread(here("trunk", "derived", "landscape.csv"))
  } else {
    copy(landscape)
  }

  .vcheck(all(c("curr_contract_id", "curr_plan_id", "dec_src", "jan_src") %in% names(at)),
          "the analytic table has no successor IDs or enrollment sources; rebuild it with make_analytic()")
  cls <- unname(MAEXITS_XWALK_STATUS_CLASS[at$status])
  continuing <- c("continuing", "service_area_reduction", "service_area_expansion")

  # Does this row's successor plan serve the county in January?
  ls_key <- unique(ls_data[, .(contract_id, plan_id = as.integer(plan_id),
                               county_name, state_name, year = as.integer(year))])
  at[, jan_year := as.integer(dec_year) + 1L]
  at[, successor_serves := FALSE]
  at[ls_key, successor_serves := TRUE,
     on = .(curr_contract_id = contract_id, curr_plan_id = plan_id,
            county_name, state_name, jan_year = year)]
  at[, jan_year := NULL]

  # A service-area-reduction row whose successor no longer serves the county
  at[, sar_dropped := cls %in% "service_area_reduction" & !successor_serves]
  at[sar_dropped == TRUE,
     `:=`(jan_enrollment = 0, jan_enrollment_low = 0, jan_src = "dropped_county")]

  # Row-level exit flag: a terminated plan, or a SAR row that dropped the county
  at[, forced := cls %in% "terminated" | sar_dropped]

  # Plan-county exit flag: the prior plan-county's enrollees are forced out
  # unless some continuing successor (renewal, consolidation, SAR or SAE)
  # serves the county in January. A "Terminated/Non-renewed Contract" row
  # listed alongside consolidation rows into another contract is then not an
  # exit where the new contract serves the county.
  is_dec <- !is.na(at$dec_src)
  # A link to a plan serving the county keeps enrollees covered. CMS
  # occasionally lists a New Plan with a previous plan ID; that link counts too.
  at[, served_link := cls %in% c(continuing, "new") & successor_serves]
  prev_key <- c("dec_year", "contract_id", "plan_id", "county_name", "state_name")
  at[is_dec, forced_county := !any(served_link), by = prev_key]

  # Classify roles from each status's class (MAEXITS_XWALK_STATUS_CLASS)
  # January-only rows (new plans, counties a continuing plan adds) are new
  # entrants in the county
  at[, role := fcase(
    forced == TRUE, "exiting",
    is.na(dec_enrollment) & cls %in% continuing, "new_entrant",
    cls %in% continuing, "incumbent",
    cls %in% "new", "new_entrant",
    default = "other"
  )]

  # Why a prior plan-county counts as forced: every link terminated; a
  # service-area reduction dropped the county; the plan moved to a different
  # plan or contract that does not serve the county; or the same plan renewed
  # but is not listed in the county in the January landscape.
  at[, .cls := cls]
  at[is_dec, forced_reason := if (!forced_county[1]) NA_character_ else fcase(
    !any(.cls %in% continuing), "terminated",
    any(.cls %in% "service_area_reduction"), "service_area_reduction",
    any(.cls %in% continuing & (curr_contract_id != contract_id | curr_plan_id != plan_id)),
      "successor_drops_county",
    default = "renewal_not_in_landscape"), by = prev_key]
  at[, .cls := NULL]

  # Links. Rows are crosswalk links, so December enrollment repeats on every
  # row of a prior plan-county (a split, or a move into another contract),
  # and January enrollment repeats on every row leading to the same
  # successor plan-county (a consolidation). The _once columns carry each
  # value on one row and 0 on the others; _split shares it across the rows.
  at[is_dec, prev_links := .N, by = prev_key]
  at[is_dec, dec_first := seq_len(.N) == 1L, by = prev_key]
  at[, dec_enrollment_once := fifelse(dec_first %in% TRUE, dec_enrollment,
                                      fifelse(is_dec, 0, NA_real_))]
  at[, dec_enrollment_split := dec_enrollment / prev_links]

  carries_jan <- at$jan_src %in% c("reported", "suppressed", "mixed", "no_record")
  curr_key <- c("dec_year", "curr_contract_id", "curr_plan_id", "county_name", "state_name")
  at[carries_jan, curr_links := .N, by = curr_key]
  at[carries_jan, jan_first := seq_len(.N) == 1L, by = curr_key]
  at[carries_jan, curr_is_incumbent := any(!is.na(dec_src) & served_link), by = curr_key]
  at[, jan_enrollment_once := fifelse(jan_first %in% TRUE, jan_enrollment,
                                      fifelse(carries_jan, 0, jan_enrollment))]
  at[, jan_enrollment_split := fifelse(carries_jan, jan_enrollment / curr_links,
                                       jan_enrollment)]
  at[, served_link := NULL]

  if (verbose) {
    odd <- at[is_dec & cls %in% c("continuing", "service_area_expansion") & !successor_serves, .N]
    message("Renewal/consolidation/SAE rows whose successor does not serve the county in January: ", odd)
    message("December rows repeated across links: ", at[prev_links > 1, .N],
            "; January rows repeated across links: ", at[curr_links > 1, .N])
  }

  .check_augmented(at, unique(ls_data$year), xwalk_years)

  # Part 1: Forced disenrollment by year
  if (verbose) {
    message("\n================================================================")
    message("  PART 1: FORCED DISENROLLMENT BY YEAR")
    message("================================================================\n")

    # Plan-county level: each forced plan-county once, by reason
    forced_detail <- at[forced_county %in% TRUE & dec_first %in% TRUE,
      .(displaced = sum(dec_enrollment_once, na.rm = TRUE),
        n_plans = uniqueN(paste(contract_id, plan_id)),
        n_county_obs = .N),
      by = .(dec_year, source = forced_reason)][order(dec_year, source)]

    displaced_wide <- dcast(forced_detail, dec_year ~ source,
                            value.var = "displaced", fill = 0)
    # Sum whichever sources are present (a year may have only one)
    src_cols <- setdiff(names(displaced_wide), "dec_year")
    displaced_wide[, total_displaced := rowSums(.SD), .SDcols = src_cols]
    print(displaced_wide)

    message("\nPlan counts:")
    print(dcast(forced_detail, dec_year ~ source,
                value.var = "n_plans", fill = 0))
  }

  # Part 2: Growth in affected counties
  if (verbose) {
    message("\n================================================================")
    message("  PART 2: GROWTH IN AFFECTED COUNTIES")
    message("  (Counties that experienced forced disenrollment)")
    message("================================================================\n")

    affected <- unique(at[forced_county %in% TRUE, .(dec_year, county_name, state_name)])
    message("Affected counties per year:")
    print(affected[, .(n_counties = .N), by = dec_year][order(dec_year)])

    at_aff <- at[affected, on = .(dec_year, county_name, state_name),
                 nomatch = NULL]

    comp <- at_aff[, .(
      displaced = sum(dec_enrollment_once[forced_county %in% TRUE], na.rm = TRUE),
      incumbent_growth = sum(jan_enrollment_once[curr_is_incumbent %in% TRUE], na.rm = TRUE) -
        sum(dec_enrollment_once[forced_county %in% FALSE], na.rm = TRUE),
      new_entrant_jan = sum(jan_enrollment_once[curr_is_incumbent %in% FALSE], na.rm = TRUE)),
      by = dec_year]
    comp[, total_growth := incumbent_growth + new_entrant_jan]
    comp[, pct_incumbent := round(incumbent_growth / total_growth * 100, 1)]
    comp[, pct_new_entrant := round(new_entrant_jan / total_growth * 100, 1)]
    comp[, absorption := round(total_growth / displaced * 100, 1)]

    message("\n================================================================")
    message("  SUMMARY: INCUMBENT vs NEW ENTRANT GROWTH")
    message("  (In counties that experienced forced disenrollment)")
    message("================================================================\n")
    print(comp[order(dec_year)])

    message("\nColumn definitions:")
    message("  displaced:        Dec enrollment of forced plan-counties (forced_county)")
    message("  incumbent_growth: Jan enrollment of continuing successors minus Dec")
    message("                    enrollment of plan-counties not forced out")
    message("  new_entrant_jan:  Jan enrollment of plans new to those counties")
    message("  (every plan-county counted once: *_enrollment_once columns)")
    message("  absorption:       Total growth as % of displaced (>100% = net MA growth)")
  }

  if (save) {
    fwrite(at, here("trunk", "derived", "analytictable_augmented.csv"))
    message("\nSaved augmented table to trunk/derived/analytictable_augmented.csv")
    message("Added columns: sar_dropped, forced, forced_county, forced_reason, role, ",
            "and the counting columns (prev_links, dec_enrollment_once, ...)")
  }

  invisible(at)
}


# --- Step 10: County-Year Panel ----------------------------------------

#' Build County-Year Panel
#'
#' Collapses the augmented analytic table to the county x year level,
#' computing exit rates, enrollment changes, and classifying counties
#' by exit exposure and 2018 MA penetration quartile.
#'
#' @param at data.table or NULL. If NULL, reads from
#'   \code{trunk/derived/analytictable_augmented.csv}.
#' @param save Logical. If TRUE (default), writes the panel to
#'   \code{trunk/derived/county_panel.csv}.
#' @return Invisibly returns the county-year panel data.table.
#' @export
make_county_panel <- function(at = NULL, save = TRUE) {
  at <- .load_if_null(at, "trunk/derived/analytictable_augmented.csv")

  # Drop rows with blank county/state (expansion-only artifacts)
  at <- at[county_name != "" & state_name != ""]

  # Collapse to county x dec_year level
  panel <- at[, .(
    fips = fips[1],
    total_dec_enrollment = sum(dec_enrollment, na.rm = TRUE),
    total_jan_enrollment = sum(jan_enrollment, na.rm = TRUE),
    displaced_enrollment = sum(fifelse(forced == TRUE, dec_enrollment, 0),
                               na.rm = TRUE),
    incumbent_dec = sum(fifelse(role == "incumbent", dec_enrollment, 0),
                        na.rm = TRUE),
    incumbent_jan = sum(fifelse(role == "incumbent", jan_enrollment, 0),
                        na.rm = TRUE),
    new_entrant_jan = sum(fifelse(role == "new_entrant", jan_enrollment, 0),
                          na.rm = TRUE),
    n_plans_dec = uniqueN(paste(contract_id, plan_id)),
    n_exiting_plans = uniqueN(paste(contract_id, plan_id)[forced == TRUE]),
    penetration_2018 = penetration_2018[1],
    eligibles_2018 = eligibles_2018[1],
    benchmark = benchmark[1],
    # Each plan-county counted once (see ?plan_county)
    total_dec_enrollment_once = sum(dec_enrollment_once, na.rm = TRUE),
    total_dec_enrollment_once_low = sum(dec_enrollment_low[dec_first %in% TRUE], na.rm = TRUE),
    total_jan_enrollment_once = sum(jan_enrollment_once, na.rm = TRUE),
    total_jan_enrollment_once_low = sum(jan_enrollment_low[jan_first %in% TRUE], na.rm = TRUE),
    displaced_enrollment_once = sum(dec_enrollment_once[forced_county %in% TRUE], na.rm = TRUE),
    displaced_enrollment_once_low = sum(dec_enrollment_low[dec_first %in% TRUE &
                                                             forced_county %in% TRUE], na.rm = TRUE),
    incumbent_dec_once = sum(dec_enrollment_once[forced_county %in% FALSE], na.rm = TRUE),
    incumbent_jan_once = sum(jan_enrollment_once[curr_is_incumbent %in% TRUE], na.rm = TRUE),
    new_entrant_jan_once = sum(jan_enrollment_once[curr_is_incumbent %in% FALSE], na.rm = TRUE),
    n_exiting_plans_county = uniqueN(paste(contract_id, plan_id)[forced_county %in% TRUE]),
    snp_dec_enrollment_once = sum(dec_enrollment_once[snp %in% "Yes"], na.rm = TRUE),
    snp_displaced_enrollment_once = sum(dec_enrollment_once[snp %in% "Yes" &
                                                              forced_county %in% TRUE], na.rm = TRUE)
  ), by = .(county_name, state_name, dec_year)]

  # Compute derived variables (NA when dec enrollment is 0 to avoid Inf)
  panel[, exit_rate := fifelse(total_dec_enrollment > 0,
    displaced_enrollment / total_dec_enrollment, NA_real_)]
  panel[, enrollment_change := fifelse(total_dec_enrollment > 0,
    (total_jan_enrollment - total_dec_enrollment) / total_dec_enrollment,
    NA_real_)]
  panel[, net_enrollment_change := total_jan_enrollment - total_dec_enrollment]
  panel[, incumbent_growth := incumbent_jan - incumbent_dec]
  panel[, exit_rate_once := fifelse(total_dec_enrollment_once > 0,
    displaced_enrollment_once / total_dec_enrollment_once, NA_real_)]
  panel[, enrollment_change_once := fifelse(total_dec_enrollment_once > 0,
    (total_jan_enrollment_once - total_dec_enrollment_once) / total_dec_enrollment_once,
    NA_real_)]
  panel[, incumbent_growth_once := incumbent_jan_once - incumbent_dec_once]

  # Bin exit exposure
  panel[, exit_group := fcase(
    is.na(exit_rate), "No exits",
    exit_rate == 0, "No exits",
    exit_rate > 0 & exit_rate <= 0.05, "Low (<5%)",
    exit_rate > 0.05 & exit_rate <= 0.15, "Medium (5-15%)",
    exit_rate > 0.15, "High (>15%)"
  )]
  panel[, exit_group := factor(exit_group,
    levels = c("No exits", "Low (<5%)", "Medium (5-15%)", "High (>15%)"))]
  panel[, exit_group_once := factor(fcase(
    is.na(exit_rate_once) | exit_rate_once == 0, "No exits",
    exit_rate_once <= 0.05, "Low (<5%)",
    exit_rate_once <= 0.15, "Medium (5-15%)",
    default = "High (>15%)"),
    levels = c("No exits", "Low (<5%)", "Medium (5-15%)", "High (>15%)"))]

  # Penetration quartiles based on 2018 penetration
  pen_breaks <- quantile(
    panel[!is.na(penetration_2018), unique(penetration_2018)],
    probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  panel[, pen_quartile := cut(penetration_2018, breaks = pen_breaks,
    labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"),
    include.lowest = TRUE)]

  if (save) {
    fwrite(panel, here("trunk", "derived", "county_panel.csv"))
  }

  message("County panel: ", nrow(panel), " rows, ",
          uniqueN(panel[, paste(county_name, state_name)]), " counties, ",
          uniqueN(panel$dec_year), " years")

  invisible(panel)
}


# --- Step 11: Full Pipeline Runner ------------------------------------

#' Run the Complete Data Pipeline
#'
#' Runs all data construction steps in order, passing in-memory
#' data.tables between steps. Inputs are checked first
#' (\code{check_inputs()}), every step runs its validation checks, and
#' nothing is written until the whole run has succeeded; the eight derived
#' tables are then written to a staging folder and moved into
#' \code{out_dir} together, so a failed run never leaves a mix of old and
#' new files.
#'
#' @param save Logical. If TRUE (default), writes the derived tables.
#' @param verbose Logical. If TRUE (default), diagnostic output is
#'   printed during augmentation.
#' @param xwalk_years Integer vector of crosswalk years (default
#'   \code{MAEXITS_XWALK_YEARS} from R/config.R).
#' @param out_dir Folder for the derived tables (default
#'   \code{trunk/derived}). Point it elsewhere for a trial run.
#' @return Invisibly returns the final county-year panel data.table.
#' @export
run_data_pipeline <- function(save = TRUE, verbose = TRUE,
                              xwalk_years = MAEXITS_XWALK_YEARS,
                              out_dir = here("trunk", "derived")) {
  message("=== Step 0: Checking inputs ===")
  check_inputs(xwalk_years)

  message("\n=== Step 1: Processing enrollment files ===")
  last <- max(as.integer(xwalk_years))
  dec_enroll <- make_enrollment("december enrollment", save = FALSE, max_year = last - 1L)
  jan_enroll <- make_enrollment("january enrollment", save = FALSE, max_year = last)

  message("\n=== Step 2: Building landscape ===")
  landscape <- make_landscape(save = FALSE, years = .landscape_years(xwalk_years))

  message("\n=== Step 3: Building analytic table ===")
  at <- make_analytic(landscape, dec_enroll, jan_enroll, save = FALSE,
                      xwalk_years = xwalk_years)

  message("\n=== Step 4: Adding FIPS and penetration ===")
  at_fp <- add_fips_and_penetration(at, save = FALSE)

  message("\n=== Step 5: Adding county benchmarks ===")
  at_bench <- add_benchmark(at_fp, save = FALSE, verbose = verbose)

  message("\n=== Step 6: Augmenting with displacement flags ===")
  at_aug <- augment_analytic(at_bench, save = FALSE, verbose = verbose,
                             landscape = landscape, xwalk_years = xwalk_years)

  message("\n=== Step 7: Building county panel ===")
  panel <- make_county_panel(at_aug, save = FALSE)

  message("\n=== Step 8: Building plan details ===")
  details <- make_plan_details(years = .plan_detail_years(xwalk_years), save = FALSE)

  message("\n=== Step 9: Building displacement table ===")
  displacement <- make_displacement(at_aug, landscape = landscape, plan_details = details,
                                    panel = panel, save = FALSE)

  if (save) {
    # analytictable.csv holds the table after FIPS and penetration (no
    # benchmark), as in the step-by-step path.
    .write_derived(list(
      december_enrollment = dec_enroll,
      january_enrollment = jan_enroll,
      landscape = landscape,
      analytictable = at_fp,
      analytictable_augmented = at_aug,
      county_panel = panel,
      plan_details = details,
      displacement = displacement
    ), out_dir)
  }

  message("\n=== Pipeline complete ===")
  invisible(panel)
}


#' Write derived tables together
#'
#' Writes every table to a staging folder next to \code{out_dir}, moves the
#' existing tables aside, then moves the new ones in. If any move fails,
#' the previous tables are restored, so \code{out_dir} holds either the
#' complete old set or the complete new set.
#' @keywords internal
.write_derived <- function(tables, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  parent <- dirname(normalizePath(out_dir))
  stage <- tempfile("derived_staging_", tmpdir = parent)
  backup <- tempfile("derived_backup_", tmpdir = parent)
  dir.create(stage)
  dir.create(backup)
  on.exit(unlink(c(stage, backup), recursive = TRUE), add = TRUE)
  for (nm in names(tables)) {
    fwrite(tables[[nm]], file.path(stage, paste0(nm, ".csv")))
  }

  files <- paste0(names(tables), ".csv")
  target <- file.path(out_dir, files)
  moved_old <- character(0)
  moved_new <- character(0)
  restore <- function() {
    for (f in moved_new) file.rename(file.path(out_dir, f), file.path(stage, f))
    for (f in moved_old) file.rename(file.path(backup, f), file.path(out_dir, f))
  }
  for (i in seq_along(files)) {
    if (file.exists(target[i])) {
      if (!file.rename(target[i], file.path(backup, files[i]))) {
        restore()
        stop("[validate] could not move the existing ", files[i], " aside in ",
             out_dir, "; nothing was changed", call. = FALSE)
      }
      moved_old <- c(moved_old, files[i])
    }
  }
  for (i in seq_along(files)) {
    if (!file.rename(file.path(stage, files[i]), target[i])) {
      restore()
      stop("[validate] could not move the new ", files[i], " into ", out_dir,
           "; the previous tables were restored", call. = FALSE)
    }
    moved_new <- c(moved_new, files[i])
  }
  message("Wrote ", length(tables), " tables to ", out_dir)
  invisible(TRUE)
}


#' Left-pad codes with zeros to a fixed width
#'
#' Pads with zeros identically on every platform.
#' @keywords internal
.pad0 <- function(x, width) {
  x <- as.character(x)
  n <- nchar(x)
  ifelse(is.na(x) | n >= width, x,
         paste0(strrep("0", pmax(width - n, 0L)), x))
}
