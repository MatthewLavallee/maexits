# Release checker, tested offline: every CMS/NBER page and HEAD response is
# served by a fake (options maexits.http_get / maexits.http_head), built to
# match the structure of the real pages on 2026-09-25, including the odd
# cases found then (the 2020 "Plan C&D" title, the 2021 path, ".zip-0"
# hrefs, and CPSC addresses that redirect to a different report).

src <- MAEXITS_WATCH_SOURCES
cms <- function(path) paste0(src$cms, path)

rss_item <- function(title, link, period) {
  sprintf(paste0("<item><title>%s</title><pubDate>Tue, 04 Nov 2025 12:55:57 -0500</pubDate>",
                 "<link>%s</link><guid isPermaLink=\"false\">x</guid><description><![CDATA[",
                 "<p>report_period: %s</p><p>title: %s</p>]]></description></item>"),
          title, link, period, title)
}
xwalk_link <- function(y) paste0(src$crosswalk_index, "/", y, "-part-cd-plan-crosswalk")
xwalk_rss <- function(extra = character()) {
  paste0("<?xml version=\"1.0\"?><rss version=\"2.0\"><channel><title>Plan Crosswalks</title>",
         paste(c(extra,
                 rss_item("2026 Part C&#x26;D Plan Crosswalk", xwalk_link(2026), "2026"),
                 rss_item("2021 Part C&#x26;D Plan Crosswalk (updated as of 10/22/2020)",
                          cms("/research-statistics-data-and-systemsstatistics-trends-and-reportsmcradvpartdenroldataplan-crosswalks/2021-part-cd-plan-crosswalk"),
                          "2021"),
                 rss_item("2020 Plan C&#x26;D Plan Crosswalk", cms("/x/2020-part-candd-plan-crosswalk"), "2020")),
               collapse = ""),
         "</channel></rss>")
}
item_page <- function(title, links) {
  paste0("<html><head><title>", title, " | CMS</title></head><body>",
         "<header><a href=\"/files/document/agent/broker-help-desks.pdf\">Help desks</a></header>",
         "<div class=\"field field--name-field-downloads field--type-entity-reference\">",
         "<h2 class=\"field__label\">Downloads</h2><ul class=\"field__items\">",
         if (length(links)) paste0("<li class=\"field__item\"><a href=\"", links,
                                   "\" hreflang=\"en\">file</a></li>", collapse = ""),
         "</ul></div><a href=\"/files/zip/unrelated-footer.zip\">Other</a></body></html>")
}
landscape_page <- function(extra = "") {
  paste0("<a href=\"/files/document/header.pdf\">header</a>",
         "<div class=\"field field--name-field-downloads\"><ul class=\"field__items\">", extra,
         "<li><a href=\"/files/zip/cy2026-landscape-202609.zip\">CY2026 Landscape (202609) (ZIP)</a></li>",
         "<li><a href=\"/files/document/cy2025-landscape-format-memo.pdf\">CY2025 Landscape Format Memo (20240919) (PDF)</a></li>",
         "<li><a href=\"/files/zip/cy2006-cy2025-landscape-files.zip\">CY2006-CY2025 Landscape Files (ZIP)</a></li>",
         "</ul></div>")
}
cpsc_rss <- function(months, links = NULL) {
  links <- links %||% paste0(src$crosswalk_index, "/cpsc-", months)
  paste0("<rss><channel>",
         paste(rss_item(sprintf("Monthly Enrollment by CPSC %s %s", substr(months, 1, 4), substr(months, 6, 7)),
                        links, months), collapse = ""),
         "</channel></rss>")
}
zip_head <- function(size = 8e5, filename = NA_character_) {
  list(status = 200L, type = "application/zip", size = size, modified = "Tue, 04 Nov 2025 17:52:03 GMT",
       filename = filename, location = NA_character_)
}
csv_head <- list(status = 200L, type = "text/csv", size = NA_real_, modified = "Wed, 16 Apr 2025 14:17:33 GMT",
                 filename = NA_character_, location = NA_character_)
redirect_head <- function(to) {
  list(status = 301L, type = "text/html", size = 550, modified = NA_character_, filename = NA_character_,
       location = to)
}
down <- list(status = NA_integer_, error = "Could not resolve host: www.cms.gov")

# Serve fake pages and HEAD responses; anything else is a 404. Notifications
# are collected in the returned environment.
local_cms <- function(get = list(), head = list(), env = parent.frame()) {
  out <- new.env()
  out$notes <- character()
  out$calls <- character()
  norm <- function(u) sub("\\?cb=.*$", "", u)
  withr::local_options(list(
    maexits.http_get = function(url) {
      out$calls <- c(out$calls, paste("GET", norm(url)))
      b <- get[[norm(url)]]
      if (is.null(b)) list(status = 404L, error = "HTTP status was '404 Not Found'")
      else if (is.list(b)) b else list(status = 200L, body = b)
    },
    maexits.http_head = function(url) {
      out$calls <- c(out$calls, paste("HEAD", norm(url)))
      head[[norm(url)]] %||% list(status = 404L, type = "text/html", size = 30467,
                                  modified = NA_character_, filename = NA_character_,
                                  location = NA_character_)
    },
    maexits.watch_dir = withr::local_tempdir(.local_envir = env),
    maexits.launch_agents_dir = withr::local_tempdir(.local_envir = env),
    maexits.uid = "501",
    maexits.launchctl = function(args) {
      out$calls <- c(out$calls, paste("launchctl", paste(args, collapse = " ")))
      list(status = 0L, output = character())
    },
    maexits.notify = TRUE,
    maexits.retry_wait = 0,
    maexits.notifier = function(title, text) { out$notes <- c(out$notes, text); TRUE }
  ), .local_envir = env)
  out
}

target <- function(what, edition, overdue = as.Date("2099-01-01")) {
  data.table(target = what, edition = as.character(edition), label = paste(what, edition),
             overdue = overdue, next_step = "Do the next thing.")
}
check1 <- function(what, edition, ...) {
  check_cms_releases(target(what, edition, ...), remember = FALSE, quiet = TRUE)
}
check_notify <- function(t) suppressMessages(check_cms_releases(t, notify = TRUE, quiet = TRUE))
run_job <- function(...) paste(suppressMessages(capture.output(.release_watch_job(...))), collapse = "\n")
on_mac <- Sys.info()[["sysname"]] == "Darwin"


# --- Parsing ----------------------------------------------------------------

test_that("entities, links and the Downloads list are parsed like the CMS pages", {
  expect_equal(.decode_entities("2026 Part C&#x26;D &amp; C&#38;D &#8211; x&nbsp;y"),
               "2026 Part C&D & C&D – x y")
  a <- .anchors("<p><a class='x' href='/a.zip'>A <b>zip</b></a> <a href=\"/b\"\n>B&amp;C</a></p>")
  expect_equal(a$href, c("/a.zip", "/b"))
  expect_equal(a$text, c("A zip", "B&C"))
  page <- item_page("x", c("/files/zip/plan-crosswalk-2027.zip-0"))
  block <- .downloads_block(page)
  expect_equal(.anchors(block)$href, "/files/zip/plan-crosswalk-2027.zip-0")
  expect_true(grepl(.zip_href_re, "/files/zip/plan-crosswalk-2027.zip-0", perl = TRUE))
  expect_true(is.na(.downloads_block("<html>no list</html>")))
  items <- .rss_items(xwalk_rss())
  expect_equal(items$period, c("2026", "2021", "2020"))
  expect_equal(items$title[1], "2026 Part C&D Plan Crosswalk")
  expect_equal(.rss_items(cpsc_rss("2026-09"))$period, "2026-09")
})

test_that("crosswalk titles match every past wording, and only their year", {
  titles <- .rss_items(xwalk_rss())$title
  expect_equal(grepl(.xwalk_title_re(2020), titles, perl = TRUE), c(FALSE, FALSE, TRUE))
  expect_equal(grepl(.xwalk_title_re(2021), titles, perl = TRUE), c(FALSE, TRUE, FALSE))
  expect_false(any(grepl(.xwalk_title_re(2027), titles, perl = TRUE)))
})

test_that("landscape links: the year's file, not memos, archives or other years", {
  a <- .anchors(landscape_page(paste0(
    "<li><a href=\"/files/document/cy2027-landscape-format-memo.pdf\">CY2027 Landscape Format Memo (20260915) (PDF)</a></li>",
    "<li><a href=\"/files/zip/cy2027-landscape-2026091.zip\">CY2027 Landscape (202609.1) (ZIP)</a></li>")))
  expect_equal(.landscape_links(a, 2027)$href, "/files/zip/cy2027-landscape-2026091.zip")
  expect_equal(.landscape_links(a, 2026)$href, "/files/zip/cy2026-landscape-202609.zip")
  old <- .anchors("<a href=\"/files/zip/2023-ma-landscape-source-file-v-09-06-2022.zip\">2023 MA Landscape Source File (v 09 06 2022) (ZIP)</a>")
  expect_equal(nrow(.landscape_links(old, 2023)), 1L)
  expect_equal(nrow(.landscape_links(.anchors(landscape_page()), 2027)), 0L)
})


# --- Crosswalk ------------------------------------------------------------------

test_that("crosswalk: not listed and no early file is 'not yet'", {
  local_cms(get = setNames(list(xwalk_rss()), src$crosswalk_rss))
  r <- check1("crosswalk", 2027)
  expect_equal(r$status, "not_yet")
  expect_match(r$detail, "list ends at 2026")
})

test_that("crosswalk: listed with a downloadable zip is 'released'", {
  get <- list(xwalk_rss(rss_item("2027 Part C&#x26;D Plan Crosswalk", xwalk_link(2027), "2027")),
              item_page("2027 Part C&amp;D Plan Crosswalk", "/files/zip/plan-crosswalk-2027.zip-0"))
  names(get) <- c(src$crosswalk_rss, xwalk_link(2027))
  local_cms(get = get, head = setNames(list(zip_head()), cms("/files/zip/plan-crosswalk-2027.zip-0")))
  r <- check1("crosswalk", 2027)
  expect_equal(r$status, "released")
  expect_equal(r$url, cms("/files/zip/plan-crosswalk-2027.zip-0"))
  expect_equal(r$page, xwalk_link(2027))
  expect_equal(r$last_modified, "Tue, 04 Nov 2025 17:52:03 GMT")
})

test_that("crosswalk: listed before its file works is 'listed'; a non-zip is an 'anomaly'", {
  rss <- xwalk_rss(rss_item("2027 Part C&#x26;D Plan Crosswalk", xwalk_link(2027), "2027"))
  local_cms(get = setNames(list(rss, item_page("x", "/files/zip/plan-crosswalk-2027.zip")),
                           c(src$crosswalk_rss, xwalk_link(2027))))
  expect_equal(check1("crosswalk", 2027)$status, "listed")
  local_cms(get = setNames(list(rss, item_page("x", character())), c(src$crosswalk_rss, xwalk_link(2027))))
  expect_match(check1("crosswalk", 2027)$detail, "no download yet")
  local_cms(get = setNames(list(rss, item_page("x", "/files/document/plan-crosswalk-2027.xlsx")),
                           c(src$crosswalk_rss, xwalk_link(2027))))
  expect_equal(check1("crosswalk", 2027)$status, "anomaly")
  local_cms(get = setNames(list(rss, item_page("x", "/files/zip/plan-crosswalk-2027.zip")),
                           c(src$crosswalk_rss, xwalk_link(2027))),
            head = setNames(list(list(status = 200L, type = "application/pdf", size = 30000)),
                            cms("/files/zip/plan-crosswalk-2027.zip")))
  expect_equal(check1("crosswalk", 2027)$status, "anomaly")
  # A web page where the zip should be: listed, not yet confirmed
  local_cms(get = setNames(list(rss, item_page("x", "/files/zip/plan-crosswalk-2027.zip")),
                           c(src$crosswalk_rss, xwalk_link(2027))),
            head = setNames(list(list(status = 200L, type = "text/html", size = 30000)),
                            cms("/files/zip/plan-crosswalk-2027.zip")))
  r <- check1("crosswalk", 2027)
  expect_equal(r$status, "listed")
  expect_match(r$detail, "a web page where the file should be")
})

test_that("crosswalk: a zip at the usual address before it is listed is a 'hint'", {
  local_cms(get = setNames(list(xwalk_rss()), src$crosswalk_rss),
            head = setNames(list(zip_head()), cms("/files/zip/plan-crosswalk-2027.zip")))
  expect_equal(check1("crosswalk", 2027)$status, "hint")
})

test_that("crosswalk: the HTML list is used when the RSS is down or changed", {
  index <- paste0("<table id=\"dynamic_list_items_table\" data-dltid=\"31946\">",
                  "<tr><td><a href=\"/x/2026-part-cd-plan-crosswalk\">2026 Part C&amp;D Plan Crosswalk</a></td></tr>",
                  "<tr><td><a href=\"/x/2027-part-cd-plan-crosswalk\">2027 Part C&amp;D Plan Crosswalk</a></td></tr>",
                  "</table>")
  get <- list(down, index, item_page("x", "/files/zip/plan-crosswalk-2027.zip"))
  names(get) <- c(src$crosswalk_rss, src$crosswalk_index, cms("/x/2027-part-cd-plan-crosswalk"))
  local_cms(get = get, head = setNames(list(zip_head()), cms("/files/zip/plan-crosswalk-2027.zip")))
  expect_equal(check1("crosswalk", 2027)$status, "released")
})

test_that("crosswalk: unreachable is 'unknown'; a list without last year is 'broken'", {
  local_cms(get = setNames(list(down, down), c(src$crosswalk_rss, src$crosswalk_index)))
  expect_equal(check1("crosswalk", 2027)$status, "unknown")
  local_cms(get = setNames(list("<rss><channel></channel></rss>", "<html>redesigned</html>"),
                           c(src$crosswalk_rss, src$crosswalk_index)))
  r <- check1("crosswalk", 2027)
  expect_equal(r$status, "broken")
  expect_match(r$detail, "no 2026 crosswalk")
})


# --- Landscape ------------------------------------------------------------------

test_that("landscape: first appearance, memo only, file not up, redesign, down", {
  cy27 <- "<li><a href=\"/files/zip/cy2027-landscape-202609.zip\">CY2027 Landscape (202609) (ZIP)</a></li>"
  local_cms(get = setNames(list(landscape_page()), src$landscape_page))
  r <- check1("landscape", 2027)
  expect_equal(r$status, "not_yet")
  expect_match(r$detail, "CY2026 Landscape \\(202609\\)")

  local_cms(get = setNames(list(landscape_page(cy27)), src$landscape_page),
            head = setNames(list(zip_head(1.2e7)), cms("/files/zip/cy2027-landscape-202609.zip")))
  r <- check1("landscape", 2027)
  expect_equal(r$status, "released")
  expect_match(r$detail, "CY2027 landscape \\(202609\\) is out")

  local_cms(get = setNames(list(landscape_page(cy27)), src$landscape_page))
  expect_equal(check1("landscape", 2027)$status, "listed")
  local_cms(get = setNames(list(landscape_page(cy27)), src$landscape_page),
            head = setNames(list(zip_head(2e4)), cms("/files/zip/cy2027-landscape-202609.zip")))
  expect_equal(check1("landscape", 2027)$status, "anomaly")

  memo <- "<li><a href=\"/files/document/cy2027-memo.pdf\">CY2027 Landscape Format Memo (20260915) (PDF)</a></li>"
  local_cms(get = setNames(list(landscape_page(memo)), src$landscape_page))
  expect_equal(check1("landscape", 2027)$status, "not_yet")

  local_cms(get = setNames(list("<html>new layout</html>"), src$landscape_page))
  expect_equal(check1("landscape", 2027)$status, "broken")
  local_cms(get = setNames(list(list(status = 503L, error = "HTTP status was '503")), src$landscape_page))
  expect_equal(check1("landscape", 2027)$status, "unknown")
})


# --- CPSC and ratebook ------------------------------------------------------------

test_that("CPSC: the usual address, a redirect to another report, and anomalies", {
  u <- .cms_zip_url("2026-12")
  local_cms(head = setNames(list(zip_head(3.9e7, "cpsc_enrollment_2026_12.zip")), u))
  expect_equal(check1("cpsc_dec", "2026-12")$status, "released")

  # CMS redirects some usual addresses to a different file: never 'released'
  local_cms(get = setNames(list(cpsc_rss(c("2026-09", "2025-12"))), src$cpsc_rss),
            head = setNames(list(redirect_head(cms("/files/zip/monthly-contract-summary-report-december-2026.zip"))), u))
  r <- check1("cpsc_dec", "2026-12")
  expect_equal(r$status, "not_yet")
  expect_match(r$detail, "latest CPSC month: 2026-09")

  # Listed under another address
  item <- paste0(src$crosswalk_index, "/cpsc-2026-12")
  get <- setNames(list(cpsc_rss(c("2026-12", "2025-12")),
                       item_page("x", "/files/zip/monthly-enrollment-cpsc-december-2026.zip-0")),
                  c(src$cpsc_rss, item))
  local_cms(get = get, head = setNames(list(redirect_head("elsewhere"),
                                            zip_head(3.9e7, "cpsc_enrollment_2026_12.zip")),
                                       c(u, paste0(u, "-0"))))
  r <- check1("cpsc_dec", "2026-12")
  expect_equal(r$status, "released")
  expect_equal(r$url, paste0(u, "-0"))

  local_cms(head = setNames(list(zip_head(3.9e7, "cpsc_enrollment_2025_12.zip")), u))
  expect_equal(check1("cpsc_dec", "2026-12")$status, "anomaly")
  local_cms(get = setNames(list(down), src$cpsc_rss))
  expect_equal(check1("cpsc_dec", "2026-12")$status, "unknown")
  local_cms(get = setNames(list(cpsc_rss("2026-09")), src$cpsc_rss))
  expect_equal(check1("cpsc_dec", "2026-12")$status, "broken")
})

test_that("ratebook: NBER file, CMS-only hint, not yet, unreachable", {
  nber <- function(y) sprintf("%s/%d/countyrate%d.csv", src$nber_ratebook, y, y)
  local_cms(head = setNames(list(csv_head), nber(2026)))
  expect_equal(check1("ratebook", 2026)$status, "released")
  local_cms(head = setNames(list(zip_head(), csv_head), c(cms("/files/zip/2027-ma-rate-book.zip"), nber(2026))))
  expect_equal(check1("ratebook", 2027)$status, "hint")
  local_cms(head = setNames(list(csv_head), nber(2026)))
  expect_equal(check1("ratebook", 2027)$status, "not_yet")
  local_cms(head = setNames(list(down), nber(2027)))
  expect_equal(check1("ratebook", 2027)$status, "unknown")
})


# --- Findings are reported once ------------------------------------------------

test_that("each finding is notified once, then shown until acknowledged", {
  env <- local_cms(head = setNames(list(csv_head), sprintf("%s/2026/countyrate2026.csv", src$nber_ratebook)))
  r1 <- check_notify(target("ratebook", 2026))
  expect_true(r1$new)
  expect_length(env$notes, 1)
  expect_match(env$notes, "ratebook 2026 is out")
  r2 <- check_notify(target("ratebook", 2026))
  expect_false(r2$new)
  expect_length(env$notes, 1)
  expect_match(.watch_pending_message(), "ratebook 2026 \\(released\\)")
  expect_output(release_check_status(acknowledge = TRUE), "Marked as seen")
  expect_null(.watch_pending_message())
})

test_that("late files are flagged once; failures once per streak", {
  env <- local_cms(get = setNames(list(landscape_page()), src$landscape_page))
  late <- target("landscape", 2027, overdue = Sys.Date() - 1)
  for (i in 1:2) check_notify(late)
  expect_length(grep("later than in past years", env$notes), 1)

  # Unreachable: after three runs; again after a recovery and a new outage
  env <- local_cms(get = setNames(list(down), src$landscape_page))
  for (i in 1:4) check_notify(target("landscape", 2027))
  expect_length(grep("not been able to reach", env$notes), 1)
  options(maexits.http_get = function(url) list(status = 200L, body = landscape_page()))
  check_notify(target("landscape", 2027))
  options(maexits.http_get = function(url) down)
  for (i in 1:3) check_notify(target("landscape", 2027))
  expect_length(grep("not been able to reach", env$notes), 2)

  # A page that no longer looks as expected: at once
  options(maexits.http_get = function(url) list(status = 200L, body = "<html>new layout</html>"))
  check_notify(target("landscape", 2027))
  expect_match(tail(env$notes, 1), "needs updating")
})


# --- The scheduled job -----------------------------------------------------------

test_that("the job skips recent runs, overlaps and released files, and stops when done", {
  nber26 <- sprintf("%s/2026/countyrate2026.csv", src$nber_ratebook)
  env <- local_cms(get = setNames(list(landscape_page()), src$landscape_page),
                   head = setNames(list(csv_head), nber26))
  dir <- .watch_dir()
  plist <- .launch_agent_path()
  writeLines("<plist/>", plist)
  st <- list(targets = rbind(target("ratebook", 2026), target("landscape", 2027)),
             until = Sys.Date() + 30, finished = FALSE)
  .watch_write(st, dir)

  expect_match(run_job(dir), "countyrate2026.csv is on NBER")
  expect_true(any(grepl("HEAD .*countyrate2026", env$calls)))
  expect_match(run_job(dir), "skipping")                        # under 4 hours since the last run
  dir.create(file.path(dir, "lock"))
  expect_match(run_job(dir, force = TRUE), "Another check is running")
  unlink(file.path(dir, "lock"), recursive = TRUE)

  env$calls <- character()
  expect_match(run_job(dir, force = TRUE), "landscape 2027")
  expect_false(any(grepl("countyrate2026", env$calls)))         # released: not checked again
  expect_true(file.exists(plist))

  # The landscape comes out: everything is released, so the job removes itself
  cy27 <- "<li><a href=\"/files/zip/cy2027-landscape-202609.zip\">CY2027 Landscape (202609) (ZIP)</a></li>"
  options(maexits.http_get = function(url) list(status = 200L, body = landscape_page(cy27)),
          maexits.http_head = function(url) if (grepl("cy2027", url)) zip_head(1.2e7) else csv_head)
  expect_match(run_job(dir, force = TRUE), "Every file is out")
  expect_true(.watch_read(dir)$finished)
  expect_false(file.exists(plist))
  if (on_mac) expect_true(any(grepl("launchctl bootout gui/501/", env$calls)))
  expect_match(tail(env$notes, 1), "daily release check has stopped")
  expect_match(run_job(dir, force = TRUE), "Nothing left to check")
})

test_that("the job stops at its end date and says what is still missing", {
  env <- local_cms(get = setNames(list(landscape_page()), src$landscape_page))
  .watch_write(list(targets = target("landscape", 2027), until = Sys.Date() - 1, finished = FALSE))
  expect_match(run_job(), "reached its end date")
  expect_match(tail(env$notes, 1), "still not out: landscape 2027")
})


# --- Schedule files ---------------------------------------------------------------

test_that("the launchd file is valid and runs the job script with absolute paths", {
  script <- .job_script(c("/lib/a", "/lib/b & c"), "/state/dir", as.Date("2027-03-31"),
                        "/agents/x.plist", "gui/501/x")
  expect_silent(parse(text = script))
  xml <- .plist_xml(c("/R/bin/Rscript", "--vanilla", "/state/b & c/run-check.R"), "17", "45",
                    "/state/dir/check.log", "/state/dir")
  expect_true(any(grepl("<key>RunAtLoad</key><true/>", xml, fixed = TRUE)))
  expect_true(any(grepl("<integer>17</integer>", xml, fixed = TRUE)))
  expect_true(any(grepl("/state/b &amp; c/run-check.R", xml, fixed = TRUE)))
  skip_if(Sys.which("plutil") == "", "plutil not available")
  f <- withr::local_tempfile(fileext = ".plist")
  writeLines(xml, f)
  expect_equal(suppressWarnings(system2("plutil", c("-lint", f), stdout = FALSE, stderr = FALSE)), 0L)
})

test_that("bad schedule arguments stop before anything is written", {
  local_cms()
  expect_error(schedule_release_check(at = "5pm"), "time like")
  expect_error(schedule_release_check(targets = "crosswalks"), "unknown target")
  expect_error(schedule_release_check(until = "2020-01-01"), "in the past")
  expect_false(file.exists(file.path(.watch_dir(), "state.rds")))
})

test_that("unscheduling removes the job and keeps or forgets the findings", {
  env <- local_cms()
  writeLines("<plist/>", .launch_agent_path())
  .watch_write(list(targets = target("landscape", 2027), finished = FALSE))
  expect_message(unschedule_release_check(), "removed")
  expect_false(file.exists(.launch_agent_path()))
  expect_true(.watch_read()$finished)
  expect_message(unschedule_release_check(forget = TRUE), "No daily release check")
  expect_false(dir.exists(.watch_dir()))
})


# --- Live (opt-in) -------------------------------------------------------------------

test_that("live: last cycle's files are all found where the checker looks", {
  skip_if_not(Sys.getenv("MAEXITS_LIVE_TESTS") == "true", "set MAEXITS_LIVE_TESTS=true to query CMS and NBER")
  withr::local_options(maexits.watch_dir = withr::local_tempdir())
  last <- rbind(target("crosswalk", 2026), target("landscape", 2026), target("cpsc_dec", "2025-12"),
                target("cpsc_jan", "2026-01"), target("ratebook", 2025), target("cpsc_dec", "2025-03"))
  r <- check_cms_releases(last, remember = FALSE, quiet = TRUE)
  expect_equal(r$status, rep("released", nrow(last)), label = paste(r$target, r$edition, r$detail))
  expect_equal(r[edition == "2025-03", url], cms("/files/zip/monthly-enrollment-cpsc-march-2025.zip-0"))
})


# --- Cases found in review ---------------------------------------------------------

test_that("released requires the target year in the file name", {
  rss <- xwalk_rss(rss_item("2027 Part C&#x26;D Plan Crosswalk", xwalk_link(2027), "2027"))
  local_cms(get = setNames(list(rss, item_page("x", "/files/zip/plan-crosswalk-2026.zip")),
                           c(src$crosswalk_rss, xwalk_link(2027))),
            head = setNames(list(zip_head()), cms("/files/zip/plan-crosswalk-2026.zip")))
  r <- check1("crosswalk", 2027)
  expect_equal(r$status, "anomaly")
  expect_match(r$detail, "does not look like the 2027 crosswalk")

  relabelled <- "<li><a href=\"/files/zip/cy2026-landscape-202609.zip\">CY2027 Landscape (202609) (ZIP)</a></li>"
  local_cms(get = setNames(list(landscape_page(relabelled)), src$landscape_page),
            head = setNames(list(zip_head(1.3e7)), cms("/files/zip/cy2026-landscape-202609.zip")))
  expect_equal(check1("landscape", 2027)$status, "anomaly")
})

test_that("every candidate zip is tried, and memo or fact-sheet zips never count", {
  links <- paste0("<li><a href=\"/files/zip/cy2027-landscape-format-memo-20260915.zip\">CY2027 Landscape Format Memo (ZIP)</a></li>",
                  "<li><a href=\"/files/zip/cy2027-landscape-state-state-fact-sheets.zip\">CY2027 Landscape State-by-State Fact Sheets (ZIP)</a></li>")
  local_cms(get = setNames(list(landscape_page(links)), src$landscape_page),
            head = setNames(list(zip_head(3e5), zip_head(8e6)),
                            cms(c("/files/zip/cy2027-landscape-format-memo-20260915.zip",
                                  "/files/zip/cy2027-landscape-state-state-fact-sheets.zip"))))
  expect_equal(check1("landscape", 2027)$status, "not_yet")

  readme_first <- item_page("x", c("/files/zip/plan-crosswalk-2027-readme.zip", "/files/zip/plan-crosswalk-2027.zip"))
  rss <- xwalk_rss(rss_item("2027 Part C&#x26;D Plan Crosswalk", xwalk_link(2027), "2027"))
  local_cms(get = setNames(list(rss, readme_first), c(src$crosswalk_rss, xwalk_link(2027))),
            head = setNames(list(zip_head(2e4), zip_head()),
                            cms(c("/files/zip/plan-crosswalk-2027-readme.zip", "/files/zip/plan-crosswalk-2027.zip"))))
  r <- check1("crosswalk", 2027)
  expect_equal(r$status, "released")
  expect_equal(r$url, cms("/files/zip/plan-crosswalk-2027.zip"))
})

test_that("a landscape in a new wording is found, even with last year gone", {
  page <- paste0("<div class=\"field--name-field-downloads\"><ul>",
                 "<li><a href=\"/files/zip/cy2027-ma-landscape-202609.zip\">CY2027 MA Landscape (202609) (ZIP)</a></li>",
                 "<li><a href=\"/files/zip/cy2006-cy2026-landscape-files.zip\">CY2006-CY2026 Landscape Files (ZIP)</a></li>",
                 "</ul></div>")
  local_cms(get = setNames(list(page), src$landscape_page),
            head = setNames(list(zip_head(1.2e7)), cms("/files/zip/cy2027-ma-landscape-202609.zip")))
  r <- check1("landscape", 2027)
  expect_equal(r$status, "released")
  expect_match(r$detail, "new link wording")
})

test_that("an item page without its Downloads list still yields the file", {
  rss <- xwalk_rss(rss_item("2027 Part C&#x26;D Plan Crosswalk", xwalk_link(2027), "2027"))
  page <- paste0("<html><a href=\"/files/zip/unrelated.zip\">x</a>",
                 "<div class=\"new-field\"><a href=\"/files/zip/crosswalk-2027-final.zip\">Plan Crosswalk 2027</a></div></html>")
  local_cms(get = setNames(list(rss, page), c(src$crosswalk_rss, xwalk_link(2027))),
            head = setNames(list(zip_head()), cms("/files/zip/crosswalk-2027-final.zip")))
  expect_equal(check1("crosswalk", 2027)$status, "released")
})

test_that("CPSC: an odd file at the usual address does not hide the listed one", {
  u <- .cms_zip_url("2026-12")
  item <- paste0(src$crosswalk_index, "/cpsc-2026-12")
  get <- setNames(list(cpsc_rss(c("2026-12", "2025-12")),
                       item_page("x", "/files/zip/monthly-enrollment-cpsc-december-2026.zip-0")),
                  c(src$cpsc_rss, item))
  local_cms(get = get, head = setNames(list(zip_head(35000, "monthly_summary_report_2026_12.zip"),
                                            zip_head(3.9e7, "cpsc_enrollment_2026_12_0.zip")),
                                       c(u, paste0(u, "-0"))))
  r <- check1("cpsc_dec", "2026-12")
  expect_equal(r$status, "released")
  expect_equal(r$url, paste0(u, "-0"))
  # Not listed: the odd file is reported
  local_cms(get = setNames(list(cpsc_rss(c("2026-09", "2025-12"))), src$cpsc_rss),
            head = setNames(list(zip_head(35000, "monthly_summary_report_2026_12.zip")), u))
  expect_equal(check1("cpsc_dec", "2026-12")$status, "anomaly")
})

test_that("a web page where a file should be is 'unknown', not a finding", {
  html200 <- list(status = 200L, type = "text/html; charset=utf-8", size = 5000)
  local_cms(head = setNames(list(html200), .cms_zip_url("2026-12")))
  expect_equal(check1("cpsc_dec", "2026-12")$status, "unknown")
  nber <- sprintf("%s/2026/countyrate2026.csv", src$nber_ratebook)
  local_cms(head = setNames(list(html200), nber))
  expect_equal(check1("ratebook", 2026)$status, "unknown")
  local_cms(head = setNames(list(list(status = 200L, type = NA_character_, size = NA_real_)), nber))
  expect_equal(check1("ratebook", 2026)$status, "anomaly")
})

test_that("the job retries when CMS is unreachable, and records one run", {
  env <- local_cms(get = setNames(list(down), src$landscape_page))
  .watch_write(list(targets = target("landscape", 2027), until = Sys.Date() + 30, finished = FALSE))
  out <- run_job()
  expect_equal(sum(grepl("trying again", strsplit(out, "\n")[[1]])), 2L)
  expect_equal(sum(grepl("GET .*prescription-drug-coverage", env$calls)), 3L)
  expect_equal(.watch_read()$streak$n, 1L)
})

test_that("an interactive check does not hold back the daily job", {
  env <- local_cms(get = setNames(list(landscape_page()), src$landscape_page))
  .watch_write(list(targets = target("landscape", 2027), until = Sys.Date() + 30, finished = FALSE))
  check1b <- check_cms_releases(target("landscape", 2027), quiet = TRUE)
  expect_match(run_job(), "not listed yet")
})

test_that("stopping is recorded, rescheduling keeps findings, and the lock is released", {
  env <- local_cms(head = setNames(list(csv_head), sprintf("%s/2026/countyrate2026.csv", src$nber_ratebook)))
  .watch_write(list(targets = target("ratebook", 2026), until = Sys.Date() + 30, finished = FALSE))
  run_job()
  st <- .watch_read()
  expect_true(st$finished)
  expect_true("stopped" %in% st$events$event)
  expect_false(dir.exists(file.path(.watch_dir(), "lock")))
  expect_match(.watch_pending_message(), "Daily release check \\(stopped\\)")
})

test_that("the default end date does not depend on the files chosen", {
  expect_equal(.default_until(), as.Date(sprintf("%d-03-31", max(MAEXITS_XWALK_YEARS) + 1L)))
  expect_gt(.default_until(), max(.watch_targets()$overdue))
})

test_that("the targets are one transition, and registered files are done without a check", {
  n <- max(MAEXITS_XWALK_YEARS) + 1L
  tg <- .watch_targets()
  expect_equal(tg$edition, as.character(c(n, n, sprintf("%d-12", n - 1L), sprintf("%d-01", n), n - 1L)))
  expect_equal(is.na(tg$done[tg$target == "crosswalk"]), !as.character(n) %in% names(MAEXITS_XWALK_FILES))
  expect_match(.target_done("crosswalk", names(MAEXITS_XWALK_FILES)[1]), "registered in R/config.R")
  expect_true(is.na(.target_done("landscape", 2099)))

  env <- local_cms()
  t <- target("crosswalk", 2099, overdue = as.Date("2000-01-01"))[, done := "registered in R/config.R (x.txt)"]
  r <- check_cms_releases(t, remember = FALSE, quiet = TRUE)
  expect_equal(r$status, "done")
  expect_false(r$overdue)
  expect_length(env$calls, 0)                                   # nothing fetched

  # A schedule whose files are all done stops at its first run
  dir <- getOption("maexits.watch_dir")
  .watch_write(list(targets = t, until = Sys.Date() + 30, finished = FALSE))
  expect_match(run_job(dir, force = TRUE), "Every file is out")
})

test_that("the job script removes the schedule when the package is gone", {
  dir <- withr::local_tempdir()
  plist <- file.path(dir, "job.plist")
  writeLines("<plist/>", plist)
  script <- file.path(dir, "run-check.R")
  writeLines(.job_script(file.path(dir, "empty-lib"), dir, Sys.Date() + 30, plist, "gui/0/none"), script)
  rscript <- file.path(R.home("bin"), "Rscript")
  run <- function() suppressWarnings(system2(rscript, c("--vanilla", shQuote(script)),
                                             stdout = TRUE, stderr = TRUE,
                                             env = "R_LIBS_USER=/nonexistent"))
  skip_if(file.exists(file.path(.Library, "maexitsv2")), "maexitsv2 is installed in the system library")
  for (i in 1:2) run()
  expect_true(file.exists(plist))
  out <- run()
  expect_false(file.exists(plist))
  expect_true(any(grepl("no longer installed", out)))
})
