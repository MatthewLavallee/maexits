# ============================================================
# pdp_exits.R — Standalone Part D plans (PDPs) CMS terminated for the next
# January
#
# One row per December PDP x state (dec_year): whether CMS terminated the
# plan (or, in principle, cut the state from its service area) for the
# next January, and its enrollment in December and in September. PDPs are
# offered region-wide, so the unit is the state, not the county; CMS's
# crosswalk has no service-area reductions for individual PDPs, and leaving
# a region shows up as a termination or a consolidation.
# ============================================================


# Landscape state names to postal codes (CPSC files use the codes)
.PDP_STATE_ABB <- c(
  stats::setNames(datasets::state.abb, datasets::state.name),
  "District of Columbia" = "DC", "Washington D.C." = "DC", "American Samoa" = "AS",
  "Guam" = "GU", "Northern Mariana Islands" = "MP", "Puerto Rico" = "PR",
  "U.S. Virgin Islands" = "VI", "Virgin Islands" = "VI")


#' PDP landscape for one contract year
#'
#' @return data.table with one row per individual-market PDP (S contract,
#'   plan ID below 800) and state: contract_id, plan_id, state (postal
#'   code), plan_name, org_name.
#' @keywords internal
.pdp_landscape <- function(cy) {
  key <- as.character(cy)
  if (key %in% names(MAEXITS_PDP_LANDSCAPE_FILES)) {
    files <- here("raw", "landscape", MAEXITS_PDP_LANDSCAPE_FILES[[key]])
    .vcheck(all(file.exists(files)), "PDP landscape CY%s not found: %s", key,
            paste(files[!file.exists(files)], collapse = ", "))
    if (all(grepl("\\.zip$", files))) {
      ex <- file.path(tempdir(), paste0("maexits_pdp_landscape_", key))
      unlink(ex, recursive = TRUE)
      dir.create(ex)
      for (z in files) utils::unzip(z, exdir = ex)
      files <- list.files(ex, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
    }
    # CMS ships some sanctioned-plan files with a header and no plans
    lists_plans <- vapply(files, function(f)
      any(grepl(",S[0-9]{4},", readLines(f, warn = FALSE), useBytes = TRUE)), TRUE)
    parts <- Filter(Negate(is.null), lapply(files[lists_plans], .read_detail_csv))
    .vcheck(length(parts) > 0, "PDP landscape CY%s: no file with a header row", key)
    d <- rbindlist(lapply(parts, function(x) {
      .need(x, "State", "State")
      .need(x, "Plan ID", "Plan ID")
      data.table(state_name = .pick(x, "State"), contract_id = .pick(x, "Contract ID"),
                 plan_id = .pick(x, "Plan ID"), plan_name = .fix_utf8(.pick(x, "Plan Name")),
                 org_name = .fix_utf8(.pick(x, c("Company Name", "Organization Name",
                                                 "Organization Marketing Name"))))
    }))
  } else {
    x <- fread(.landscape_file(cy), colClasses = "character", encoding = "UTF-8")
    x <- x[`Contract Category Type` %in% "PDP"]
    .vcheck(nrow(x) > 0, "landscape CY%s has no PDP rows", key)
    d <- data.table(state_name = .pick(x, c("State Territory Name", "State Name")),
                    contract_id = .pick(x, "Contract ID"), plan_id = .pick(x, "Plan ID"),
                    plan_name = .pick(x, "Plan Name"),
                    org_name = .pick(x, "Organization Marketing Name"))
  }
  d <- d[!is.na(contract_id) & nzchar(contract_id)]
  bad_id <- d[!grepl("^[0-9]+$", plan_id), unique(plan_id)]
  .vcheck(length(bad_id) == 0, "PDP landscape CY%s: non-numeric plan ID(s) %s", key,
          paste(head(bad_id), collapse = ", "))
  d[, plan_id := as.integer(plan_id)]
  d[, state := unname(.PDP_STATE_ABB[state_name])]
  bad_st <- d[is.na(state), unique(state_name)]
  .vcheck(length(bad_st) == 0, "PDP landscape CY%s: unknown state name(s) %s; add them to .PDP_STATE_ABB",
          key, paste(bad_st, collapse = ", "))
  d <- d[startsWith(contract_id, "S") & plan_id < 800L]
  .vcheck(nrow(d) > 0, "PDP landscape CY%s has no individual-market PDPs", key)
  d[, .(plan_name = plan_name[1], org_name = org_name[1]), by = .(contract_id, plan_id, state)]
}


#' PDP enrollment by plan and state from one CPSC file
#' @return data.table with contract_id, plan_id, state, reported (sum of
#'   reported counts) and n_suppressed (cells shown as "*", 1-10 each).
#' @keywords internal
.pdp_enrollment <- function(path) {
  d <- fread(path, colClasses = "character", na.strings = NULL,
             select = c("Contract Number", "Plan ID", "State", "Enrollment"))
  d <- d[startsWith(`Contract Number`, "S")]
  d[, plan_id := suppressWarnings(as.integer(`Plan ID`))]
  .vcheck(!anyNA(d$plan_id), "%s: non-numeric PDP plan ID(s)", basename(path))
  d <- d[plan_id < 800L]
  e <- trimws(d$Enrollment)
  sup <- e == "*"
  .vcheck(all(sup | grepl("^[0-9]+$", e)), "%s: unexpected enrollment value(s) %s", basename(path),
          paste(head(unique(e[!sup & !grepl("^[0-9]+$", e)])), collapse = ", "))
  e[sup] <- "0"
  d[, `:=`(.n = as.numeric(e), .s = sup)]
  d[, .(reported = sum(.n), n_suppressed = sum(.s)),
    by = .(contract_id = `Contract Number`, plan_id, state = trimws(State))]
}


#' Exit type of each PDP plan-state for one crosswalk year
#'
#' @param uni Plan-states of December (contract_id, plan_id, state).
#' @param offered_next Plan-states offered the next January.
#' @param xw Crosswalk ([.read_crosswalk()] output).
#' @param cms_terminated Contracts CMS terminated outside the crosswalk.
#' @return data.table with contract_id, plan_id, state, exit_type,
#'   xwalk_statuses and cms_terminated.
#' @keywords internal
.pdp_exit_rows <- function(uni, offered_next, xw, cms_terminated = character()) {
  links <- xw[startsWith(prev_contract, "S") & !is.na(prev_plan) & prev_plan < 800L,
              .(contract_id = prev_contract, plan_id = as.integer(prev_plan),
                curr_contract, curr_plan = as.integer(curr_plan), status)]
  links[, .cls := unname(MAEXITS_XWALK_STATUS_CLASS[status])]
  pl <- merge(unique(uni[, .(contract_id, plan_id, state)]), links,
              by = c("contract_id", "plan_id"), all.x = TRUE, allow.cartesian = TRUE)
  off <- unique(offered_next[, .(curr_contract = contract_id, curr_plan = plan_id, state)])
  off[, .off := TRUE]
  pl <- off[pl, on = .(curr_contract, curr_plan, state)]
  # A link keeps the plan-state covered when its successor is offered in the
  # state next January (as for MA plan-counties)
  pl[, .serves := .cls %in% c("continuing", "service_area_reduction", "service_area_expansion", "new") &
       .off %in% TRUE]
  out <- pl[, .(
    n_links = sum(!is.na(status)), served = any(.serves),
    all_term = all(.cls %in% "terminated"), any_sar = any(.cls %in% "service_area_reduction"),
    xwalk_statuses = if (all(is.na(status))) NA_character_ else
      paste(sort(unique(status[!is.na(status)])), collapse = " + ")
  ), by = .(contract_id, plan_id, state)]
  out[, cms_terminated := contract_id %in% cms_terminated]
  kept <- out[cms_terminated & served, unique(contract_id)]
  .vcheck(length(kept) == 0, paste0(
    "MAEXITS_CMS_TERMINATED_CONTRACTS lists %s, but its plans are still offered next January; ",
    "check the registration in R/config.R"), paste(kept, collapse = ", "))
  out[, exit_type := fcase(
    cms_terminated, "terminated",
    n_links > 0 & !served & all_term, "terminated",
    n_links > 0 & !served & any_sar, "service_area_reduction",
    default = "none")]
  out[, c("contract_id", "plan_id", "state", "exit_type", "xwalk_statuses", "cms_terminated"), with = FALSE]
}


#' Attach one month's PDP enrollment to plan-state rows
#' @keywords internal
.attach_pdp_enrollment <- function(rows, enr, prefix) {
  m <- enr[rows, on = .(contract_id, plan_id, state)]
  m[is.na(reported), `:=`(reported = 0, n_suppressed = 0L)]
  m[, `:=`(enrollment = as.integer(reported + 10L * n_suppressed),
           enrollment_low = as.integer(reported + n_suppressed),
           src = fcase(reported == 0 & n_suppressed == 0, "no_record",
                       n_suppressed == 0, "reported",
                       reported == 0, "suppressed",
                       default = "mixed"))]
  m[, c("reported", "n_suppressed") := NULL]
  setnames(m, c("enrollment", "enrollment_low", "src"), paste0(prefix, c("_enrollment", "_enrollment_low", "_src")))
  m
}


#' Build the PDP Exits Table
#'
#' One row per December standalone Part D plan (PDP) and state for each
#' transition (`dec_year`): whether CMS terminated the plan for the next
#' January, with the plan-state's enrollment in December and in September of
#' `dec_year`. Individual-market PDPs only (S contracts, plan IDs below 800),
#' listed for the state in the December PDP landscape.
#'
#' `exit_type` is `terminated` when every crosswalk link of the plan is a
#' termination and no plan it maps to is offered in the state next January,
#' or when the plan's contract is in `MAEXITS_CMS_TERMINATED_CONTRACTS`
#' (CMS ended it outside the crosswalk; `cms_terminated` is TRUE).
#' `service_area_reduction` follows the MA rule, but CMS's crosswalk has no
#' service-area reductions for individual PDPs, so it does not occur.
#' Everything else is `none`, including plans consolidated into another PDP.
#'
#' Enrollment is CPSC enrollment of the plan in the state (10 per suppressed
#' cell; `_low` columns 1). Enrollees living in a state where the plan is not
#' offered are left out, as for MA.
#'
#' @param exits_years Crosswalk years (default `MAEXITS_EXITS_YEARS`); the
#'   September columns cover them all.
#' @param xwalk_years Crosswalk years with December enrollment (default
#'   `MAEXITS_XWALK_YEARS`).
#' @param save If TRUE, write `trunk/derived/pdp_exits.csv`.
#' @return The PDP exits table (invisibly).
#' @export
make_pdp_exits <- function(exits_years = MAEXITS_EXITS_YEARS, xwalk_years = MAEXITS_XWALK_YEARS,
                           save = TRUE) {
  ns <- as.integer(exits_years)
  cys <- (min(ns) - 1L):max(ns)
  land <- stats::setNames(lapply(cys, .pdp_landscape), cys)
  one_file <- function(folder, year, month) {
    f <- .cpsc_files(folder, year, month)
    .vcheck(length(f) == 1L, "raw/%s needs exactly one CPSC_Enrollment_Info_%d_%02d.csv (found %d)",
            folder, as.integer(year), as.integer(month), length(f))
    f
  }
  out <- rbindlist(lapply(ns, function(n) {
    y <- n - 1L
    uni <- land[[as.character(y)]]
    rows <- .pdp_exit_rows(uni, land[[as.character(n)]], .read_crosswalk(n),
                           MAEXITS_CMS_TERMINATED_CONTRACTS[[as.character(n)]] %||% character())
    rows <- uni[rows, on = .(contract_id, plan_id, state)]
    rows <- .attach_pdp_enrollment(rows, .pdp_enrollment(one_file("monthly enrollment", y, MAEXITS_EXITS_MONTH)), "sep")
    if (n %in% as.integer(xwalk_years)) {
      rows <- .attach_pdp_enrollment(rows, .pdp_enrollment(one_file("december enrollment", y, 12L)), "dec")
    } else {
      rows[, `:=`(dec_enrollment = NA_integer_, dec_enrollment_low = NA_integer_, dec_src = NA_character_)]
    }
    rows[, dec_year := y]
  }))
  out[, state_name := .state_name_from_abb(state)]
  setcolorder(out, c("dec_year", "contract_id", "plan_id", "state", "state_name", "plan_name", "org_name",
                     "exit_type", "xwalk_statuses", "cms_terminated", "dec_enrollment",
                     "dec_enrollment_low", "dec_src", "sep_enrollment", "sep_enrollment_low", "sep_src"))
  setorderv(out, c("dec_year", "contract_id", "plan_id", "state"))
  .vcheck(!anyDuplicated(out, by = c("dec_year", "contract_id", "plan_id", "state")),
          "pdp_exits has duplicate plan-state rows")
  .vcheck(!anyNA(out$state_name), "pdp_exits: state code(s) without a name: %s",
          paste(unique(out[is.na(state_name), state]), collapse = ", "))
  no_link <- out[is.na(xwalk_statuses), uniqueN(paste(contract_id, plan_id))]
  if (no_link) message("PDP exits: ", no_link, " plan(s) have no crosswalk row (exit_type none)")
  message(sprintf("PDP exits: %d plan-states, dec_year %d-%d, %d terminated",
                  nrow(out), min(out$dec_year), max(out$dec_year), sum(out$exit_type == "terminated")))
  if (save) fwrite(out, here("trunk", "derived", "pdp_exits.csv"))
  invisible(out)
}
