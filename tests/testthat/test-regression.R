# Full rebuild from raw/ must reproduce the derived tables recorded in
# provenance/derived_md5_data-2018-2025.txt. Takes ~5 minutes, so it only
# runs when MAEXITS_FULL_REBUILD=true.
#
# The build is pinned to the recorded crosswalk years (2019-2026), and
# make_enrollment() skips CPSC files for later years, so this test stays
# valid after next cycle's files are staged and MAEXITS_XWALK_YEARS grows.
# Only a deliberate change to the derived tables needs a new provenance file.

test_that("rebuild from raw reproduces the recorded derived tables", {
  skip_if_not(Sys.getenv("MAEXITS_FULL_REBUILD") == "true",
              "set MAEXITS_FULL_REBUILD=true to run the ~5 minute full rebuild")
  skip_if_not(has_raw(), "raw/ not present")

  out <- file.path(tempdir(), "maexits_rebuild")
  unlink(out, recursive = TRUE)
  withCallingHandlers(
    suppressMessages(run_data_pipeline(save = TRUE, verbose = FALSE,
                                       xwalk_years = 2019:2026, out_dir = out)),
    warning = function(w) stop("rebuild raised a warning: ", conditionMessage(w)))

  rec <- read.table(here("provenance", "derived_md5_data-2018-2025.txt"),
                    comment.char = "#", col.names = c("md5", "path"))
  rec$file <- basename(rec$path)
  got <- tools::md5sum(file.path(out, rec$file))
  expect_equal(unname(got), rec$md5, label = paste(rec$file, collapse = ", "))
})
