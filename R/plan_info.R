# ============================================================
# plan_info.R — One table of plan characteristics by contract year
#
#   make_plan_info()   plan_details joined with pbp_benefits
#
# plan_details (organization, premiums, deductible, MOOP, star ratings, SNP
# details, from the landscape, Part D reports and CPSC contract info) and
# pbp_benefits (Part B giveback and supplemental benefits, from the Plan
# Benefit Package files) share the key year x contract x plan x segment.
# plan_info keeps every plan_details row (individual MA plans and SNPs in the
# landscape) and adds the benefit columns; the PBP-only plans (standalone
# drug plans, employer plans) drop out.
# ============================================================


#' Build the Plan Information Table
#'
#' One row per contract year, contract, plan and segment of
#' [make_plan_details()] (individual MA plans and SNPs in the landscape), with
#' the Part B giveback and supplemental benefit columns of
#' [make_pbp_benefits()] for the same plan-segment-year.
#'
#' @param plan_details Output of [make_plan_details()], or NULL to read
#'   trunk/derived/plan_details.csv.
#' @param pbp_benefits Output of [make_pbp_benefits()], or NULL to read
#'   trunk/derived/pbp_benefits.csv.
#' @param save Logical. If TRUE (default), writes trunk/derived/plan_info.csv.
#' @return Invisibly, a data.table with one row per plan-segment and year.
#' @export
make_plan_info <- function(plan_details = NULL, pbp_benefits = NULL, save = TRUE) {
  rd <- function(x, f) if (!is.null(x)) copy(as.data.table(x)) else fread(here("trunk", "derived", f), na.strings = c("", "NA"))
  pd <- rd(plan_details, "plan_details.csv")
  pb <- rd(pbp_benefits, "pbp_benefits.csv")
  key <- c("year", "contract_id", "plan_id", "segment_id")
  for (x in list(pd, pb)) x[, `:=`(year = as.integer(year), plan_id = as.integer(plan_id), segment_id = as.integer(segment_id))]
  .vcheck(!anyDuplicated(pd, by = key), "plan_details has duplicate year-contract-plan-segment rows")
  .vcheck(!anyDuplicated(pb, by = key), "pbp_benefits has duplicate year-contract-plan-segment rows")
  clash <- setdiff(intersect(names(pd), names(pb)), key)
  .vcheck(length(clash) == 0, "plan_details and pbp_benefits share column(s) %s", paste(clash, collapse = ", "))

  out <- merge(pd, pb, by = key, all.x = TRUE, sort = FALSE)
  setcolorder(out, names(pd))
  setorderv(out, key)
  # Every plan_details plan should have a Plan Benefit Package (historically all but a handful)
  cover <- out[, .(share = mean(!is.na(partb_giveback))), by = year]
  .vcheck(all(cover$share > 0.98), "plan_info: PBP benefits found for under 98%% of plans in CY%s",
          paste(cover[share <= 0.98, sprintf("%d (%.1f%%)", year, 100 * share)], collapse = ", "))
  if (save) fwrite(out, here("trunk", "derived", "plan_info.csv"))
  message("Plan info: ", nrow(out), " plan-segments, CY", min(out$year), "-CY", max(out$year),
          sprintf(" (PBP benefits for %.1f%%)", 100 * mean(!is.na(out$partb_giveback))))
  invisible(out)
}
