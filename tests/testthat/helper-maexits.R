# Load the pipeline code when tests are run with testthat::test_dir()
# (devtools::test() loads it through the package instead).
if (!exists("check_inputs", mode = "function")) {
  library(data.table)
  library(here)
  for (f in list.files(here("R"), pattern = "[.]R$", full.names = TRUE)) source(f)
}

has_raw <- function() dir.exists(here("raw", "plan crosswalk"))
has_derived <- function() file.exists(here("trunk", "derived", "landscape.csv"))

# Write a small crosswalk file with the given statuses and return its path
fake_crosswalk <- function(status = c("Renewal Plan", "New Plan"),
                           prev_plan = c("001", "NEW"),
                           status_col = "STATUS") {
  dt <- data.table(PREVIOUS_CONTRACT_ID = "H0001", PREVIOUS_PLAN_ID = prev_plan,
                   CURRENT_CONTRACT_ID = "H0001",
                   CURRENT_PLAN_ID = sprintf("%03d", seq_along(status)))
  dt[, (status_col) := status]
  path <- tempfile(fileext = ".txt")
  fwrite(dt, path, sep = "\t")
  path
}
