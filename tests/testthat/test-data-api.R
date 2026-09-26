# Data-loading API, tested offline against a small fake data release and a
# fake CMS zip served from a temporary folder.

local_fake_release <- function(env = parent.frame()) {
  rel <- withr::local_tempdir(.local_envir = env)
  cache <- withr::local_tempdir(.local_envir = env)
  panel <- data.frame(
    county_name = c("Baltimore", "Kent", "Kent"), state_name = c("Maryland", "Maryland", "Delaware"),
    dec_year = c(2025L, 2025L, 2024L), fips = c(24005L, 24029L, 10001L),
    total_dec_enrollment = c(100, 50, 80), displaced_enrollment = c(10, 0, 8),
    exit_rate = c(0.1, 0, 0.1))
  plan <- data.frame(
    dec_year = 2025L, contract_id = c("H0001", "H0001", "H0002"), plan_id = c(1L, 2L, 1L),
    plan_key = c("H0001-001", "H0001-002", "H0002-001"), segment_id = 0L,
    county_name = "Baltimore", state_name = "Maryland", fips = 24005L,
    status = c("Renewal Plan", "Terminated Plan", "New Plan"), forced = c(FALSE, TRUE, FALSE))
  files <- list(county_panel = panel, plan_county = plan)
  man <- do.call(rbind, lapply(names(files), function(nm) {
    f <- file.path(rel, paste0(nm, ".rds")); saveRDS(files[[nm]], f)
    data.frame(dataset = nm, file = basename(f), rows = nrow(files[[nm]]),
               columns = ncol(files[[nm]]), bytes = file.size(f),
               md5 = unname(tools::md5sum(f)), stringsAsFactors = FALSE)
  }))
  utils::write.csv(man, file.path(rel, "manifest.csv"), row.names = FALSE)
  withr::local_options(list(maexits.release_url = paste0("file://", rel),
                            maexits.cache_dir = cache), .local_envir = env)
  clear_session()
  withr::defer(clear_session(), envir = env)
  invisible(rel)
}

clear_session <- function() rm(list = ls(.maexits_session), envir = .maexits_session)

test_that("maexits_data downloads once, caches, and filters", {
  local_fake_release()
  expect_message(p <- maexits_data("county_panel"), "Downloading county_panel.rds")
  expect_equal(nrow(p), 3L)
  expect_true(file.exists(file.path(maexits_cache_dir(), "releases",
                                    MAEXITS_DATA_RELEASE, "county_panel.rds")))
  expect_equal(nrow(maexits_data("county_panel", states = "MD")), 2L)
  expect_equal(nrow(maexits_data("county_panel", states = c("Delaware"), years = 2024)), 1L)
  expect_equal(maexits_data("county_panel", counties = "24005")$county_name, "Baltimore")
  expect_equal(nrow(maexits_data("county_panel", counties = "kent")), 2L)
  v <- maexits_data("county_panel", variables = "exit_rate")
  expect_equal(names(v), c("county_name", "state_name", "fips", "dec_year", "exit_rate"))
})

test_that("repeated-value columns get a one-time note pointing to the key", {
  local_fake_release()
  suppressMessages(maexits_data("plan_county"))  # download into the cache
  clear_session()
  expect_message(maexits_data("plan_county"), "\\(forced\\).*\\?plan_county")
  expect_no_message(maexits_data("plan_county"))
  clear_session()
  expect_no_message(maexits_data("plan_county", variables = "status"))
  # Notes come from the requested dataset's own variable key
  clear_session()
  expect_no_message(maexitsv2:::.note_counting("displacement", c("dec_year", "dec_enrollment")))
  expect_message(maexitsv2:::.note_counting("displacement", "plan_exits"), "\\(plan_exits\\)")
  clear_session()
  withr::local_options(maexits.counting_note = FALSE)
  expect_no_message(maexits_data("plan_county"))
})

test_that("plan filters take whole contracts or single plans", {
  local_fake_release()
  expect_equal(nrow(maexits_data("plan_county", plans = "H0001")), 2L)
  expect_equal(maexits_data("plan_county", plans = c("H0001-002", "h0002-1"))$plan_key,
               c("H0001-002", "H0002-001"))
  expect_error(maexits_data("plan_county", plans = "H01"), "must look like")
  expect_error(maexits_data("county_panel", plans = "H0001"), "county-level")
})

test_that("results are copies: changing one does not change the next", {
  local_fake_release()
  a <- maexits_data("county_panel")
  a[, exit_rate := 99]
  expect_false(any(maexits_data("county_panel")$exit_rate == 99))
})

test_that("bad requests give actionable errors", {
  local_fake_release()
  expect_error(maexits_data("county_panel", variables = "nope"), "has no variable\\(s\\) nope")
  expect_error(maexits_data("county_panel", months = "2025-12"), "months applies to the enrollment")
  expect_error(maexits_data("county_panel", states = "ZZ"), "unknown state abbreviation")
  expect_error(maexits_data("landscape"), "has no landscape dataset")
  expect_error(maexits_catalog("nope"), "unknown dataset")
  expect_error(maexits_data("plan_details", states = "MD"), "has no counties")
})

test_that("a dataset added to a release after its manifest was cached is found", {
  rel <- local_fake_release()
  suppressMessages(maexits_data("county_panel"))       # caches the manifest
  saveRDS(data.frame(year = 2026L, plan_key = "H0001-001"), file.path(rel, "plan_details.rds"))
  f <- file.path(rel, "plan_details.rds")
  man <- utils::read.csv(file.path(rel, "manifest.csv"), stringsAsFactors = FALSE)
  man <- rbind(man, data.frame(dataset = "plan_details", file = basename(f), rows = 1L, columns = 2L,
                               bytes = file.size(f), md5 = unname(tools::md5sum(f))))
  utils::write.csv(man, file.path(rel, "manifest.csv"), row.names = FALSE)
  expect_equal(suppressMessages(maexits_data("plan_details"))$plan_key, "H0001-001")
  expect_error(maexits_data("landscape"), "has no landscape dataset")
})

test_that("a corrupt download is rejected", {
  rel <- local_fake_release()
  man <- utils::read.csv(file.path(rel, "manifest.csv"), stringsAsFactors = FALSE)
  man$md5[man$dataset == "county_panel"] <- "00000000000000000000000000000000"
  utils::write.csv(man, file.path(rel, "manifest.csv"), row.names = FALSE)
  expect_error(suppressMessages(maexits_data("county_panel")), "is corrupt")
  expect_false(file.exists(file.path(maexits_cache_dir(), "releases",
                                     MAEXITS_DATA_RELEASE, "county_panel.rds")))
})

test_that("catalog covers every dataset", {
  cat <- maexits_catalog()
  expect_setequal(unique(cat$dataset),
                  c(names(.DATASETS), "cms_enrollment"))
  expect_equal(names(cat), c("dataset", "variable", "description", "counting"))
  expect_equal(unique(maexits_catalog("landscape")$dataset), "landscape")
})

test_that("months, plans and CMS URLs are parsed consistently", {
  expect_equal(.normalize_months(c("2025-09", "2025-09-15")), "2025-09")
  expect_equal(.normalize_months(as.Date(c("2025-01-31", "2025-02-01"))), c("2025-01", "2025-02"))
  expect_error(.normalize_months("2025-13"), "must look like")
  expect_equal(.cms_zip_url("2026-09"),
               "https://www.cms.gov/files/zip/monthly-enrollment-cpsc-september-2026.zip")
  p <- .parse_plans(c("H1234", "h1234-7"))
  expect_equal(p$whole, "H1234")
  expect_equal(p$single, "H1234 7")
})

test_that("cms_enrollment parses a CMS zip, joins contract info, and filters", {
  skip_if(Sys.which("zip") == "", "zip utility not available")
  dir <- withr::local_tempdir()
  cache <- withr::local_tempdir()
  inner <- file.path(dir, "CPSC_Enrollment_2025_09")
  dir.create(inner)
  fwrite(data.table(`Contract Number` = c("H0001", "H0001", "S0001"),
                    `Plan ID` = c("1", "1", "5"),
                    `SSA State County Code` = c("21020", "08000", "21020"),
                    `FIPS State County Code` = c("24005", "10001", "24005"),
                    State = c("MD", "DE", "MD"), County = c("Baltimore", "Kent", "Baltimore"),
                    Enrollment = c("120", "*", "40")),
         file.path(inner, "CPSC_Enrollment_Info_2025_09.csv"))
  fwrite(data.table(`Contract ID` = c("H0001", "S0001"), `Plan ID` = c("001", "005"),
                    `Plan Type` = c("HMO/HMOPOS", "Medicare Prescription Drug Plan"),
                    `Parent Organization` = c("Acme", "Acme")),
         file.path(inner, "CPSC_Contract_Info_2025_09.csv"))
  zipfile <- file.path(dir, "monthly-enrollment-cpsc-september-2025.zip")
  withr::with_dir(dir, utils::zip(zipfile, "CPSC_Enrollment_2025_09", flags = "-rq"))

  withr::local_options(list(maexits.cms_url = paste0("file://", dir), maexits.cache_dir = cache))
  clear_session()
  withr::defer(clear_session())

  x <- suppressMessages(cms_enrollment("2025-09"))
  expect_equal(nrow(x), 3L)
  expect_equal(x[state == "DE", suppressed], TRUE)
  expect_true(is.na(x[state == "DE", enrollment]))
  expect_equal(x[contract_id == "H0001" & state == "MD", plan_type], "HMO/HMOPOS")
  expect_equal(x[contract_id == "H0001" & state == "MD", ssa_code], "21020")
  expect_equal(nrow(cms_enrollment("2025-09", ma_only = TRUE)), 2L)
  y <- cms_enrollment("2025-09", states = "MD", plans = "H0001", variables = "enrollment")
  expect_equal(y$enrollment, 120)
  expect_equal(names(y), c(.CMS_KEYS, "enrollment"))
  expect_true(file.exists(file.path(cache, "cms", "cpsc_2025_09.rds")))
  expect_error(cms_enrollment("2018-06"), "older file names")
})

test_that("release files mark text as UTF-8 and refuse invalid bytes", {
  mas <- "Medicare y Mucho M\xc3\xa1s"                     # UTF-8 bytes, unmarked
  dt <- data.table(plan_name = c(mas, "Plan A", NA), outcome = factor(c(mas, "b", "b")), n = 1:3)
  .mark_utf8(dt)
  expect_equal(Encoding(dt$plan_name[1]), "UTF-8")
  expect_equal(Encoding(levels(dt$outcome)[1]), "UTF-8")
  expect_equal(charToRaw(dt$plan_name[1]), charToRaw(mas))  # bytes unchanged
  expect_true(is.na(dt$plan_name[3]))
  expect_error(.mark_utf8(data.table(x = "M\xe1s")), "not valid UTF-8")
})
