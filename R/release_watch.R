# ============================================================
# release_watch.R — Notice when next cycle's CMS and NBER files come out
#
#   check_cms_releases()       check now whether each file is out
#   schedule_release_check()   run that check once a day (macOS launchd)
#                              with a desktop notification, until every
#                              file is out
#   release_check_status()     what the daily check has seen so far
#   unschedule_release_check() stop the daily check
#
# The files watched follow from R/config.R: the crosswalk and landscape for
# the first year not yet registered, and the December and January CPSC
# months and NBER ratebook for the next transition. A check only reads the
# CMS/NBER pages in MAEXITS_WATCH_SOURCES and sends HEAD requests to the
# file links they list. It never follows redirects (CMS redirects some
# predictable file URLs to different files) and never downloads or
# registers a file.
#
# Each check gives one status per file:
#   released  listed and downloadable, and named for the right year/month
#   hint      a file answers at the usual address but is not listed yet
#   listed    listed, but the file could not be confirmed yet
#   anomaly   a file is there but not as expected (name, type or size)
#   not_yet   not out; the check could see last year's file
#   unknown   CMS/NBER could not be reached (network, 403, 5xx, or a web
#             page where a file should be)
#   broken    the page loaded but last year's file was not found either,
#             so the page has probably changed and the check needs updating
# ============================================================


# base R has %||% only from 4.4.0
`%||%` <- function(x, y) if (is.null(x)) y else x

.WATCH_LABEL <- "com.github.matthewlavallee.maexitsv2.release-check"
.WATCH_TARGETS <- c("crosswalk", "landscape", "cpsc_dec", "cpsc_jan", "ratebook")
.WATCH_NOTIFY <- c("released", "hint", "listed", "anomaly")


# --- Targets ------------------------------------------------------------------

#' Files the next cycle needs, from R/config.R
#' @return data.table with target, edition (year or "YYYY-MM"), label,
#'   overdue (date after which a missing file is flagged) and next_step.
#' @keywords internal
.watch_targets <- function() {
  xw <- max(as.integer(names(MAEXITS_XWALK_FILES))) + 1L
  cy <- max(as.integer(names(MAEXITS_LANDSCAPE_FILES))) + 1L
  n <- max(as.integer(MAEXITS_XWALK_YEARS)) + 1L
  data.table(
    target = .WATCH_TARGETS,
    edition = c(xw, cy, sprintf("%d-12", n - 1L), sprintf("%d-01", n), n - 1L),
    label = c(sprintf("%d Part C&D Plan Crosswalk", xw),
              sprintf("CY%d MA landscape", cy),
              sprintf("December %d CPSC enrollment", n - 1L),
              sprintf("January %d CPSC enrollment", n),
              sprintf("NBER ratebook countyrate%d.csv", n - 1L)),
    # Latest past releases: crosswalk Nov 4 (2025), landscape Oct 1,
    # December CPSC Dec 14, January CPSC Feb 13 (2026)
    overdue = as.Date(c(sprintf("%d-11-20", xw - 1L), sprintf("%d-10-10", cy - 1L),
                        sprintf("%d-12-22", n - 1L), sprintf("%d-02-20", n),
                        sprintf("%d-01-05", n))),
    next_step = c(
      sprintf(paste0("Download it into raw/plan crosswalk/ (keep CMS's file name), register the ",
                     ".txt in MAEXITS_XWALK_FILES (R/config.R), then run_preliminary(%d, ...)."), xw),
      sprintf(paste0("Download it into raw/landscape/CY%d/ and register the CSV in ",
                     "MAEXITS_LANDSCAPE_FILES (R/config.R)."), cy),
      sprintf("Extract it once into raw/december enrollment/CPSC_Enrollment_%d_12/.", n - 1L),
      sprintf(paste0("Extract it into raw/january enrollment/CPSC_Enrollment_%d_01/, add %d to ",
                     "MAEXITS_XWALK_YEARS, then check_inputs() and run_data_pipeline()."), n, n),
      sprintf("Download countyrate%d.csv into raw/ratebook/.", n - 1L))
  )
}


.resolve_targets <- function(targets) {
  if (is.data.frame(targets)) return(as.data.table(targets))
  all <- .watch_targets()
  if (is.null(targets)) return(all)
  bad <- setdiff(targets, all$target)
  if (length(bad)) {
    .api_stop("unknown target(s) %s; choose from %s", paste(bad, collapse = ", "),
              paste(all$target, collapse = ", "))
  }
  all[all$target %in% targets]
}


# Default last day of the daily check: 31 March after the next January file
.default_until <- function() as.Date(sprintf("%d-03-31", max(as.integer(MAEXITS_XWALK_YEARS)) + 1L))


# --- HTTP -----------------------------------------------------------------------
# Both helpers can be replaced for tests with options(maexits.http_head = f,
# maexits.http_get = f), where f(url) returns the same list.

.watch_ua <- function() {
  v <- tryCatch(as.character(utils::packageVersion("maexitsv2")), error = function(e) "dev")
  sprintf("maexitsv2/%s release check (R %s; https://github.com/%s)", v, getRversion(),
          MAEXITS_DATA_REPO)
}


#' HEAD request without following redirects
#' @return list(status, type, size, modified, filename, location); status is
#'   NA when the server could not be reached.
#' @keywords internal
.http_head <- function(url, timeout = 30L) {
  f <- getOption("maexits.http_head")
  if (is.function(f)) return(f(url))
  old <- options(HTTPUserAgent = .watch_ua())
  on.exit(options(old))
  h <- tryCatch(curlGetHeaders(url, redirect = FALSE, timeout = timeout),
                error = function(e) e)
  if (inherits(h, "error")) return(list(status = NA_integer_, error = conditionMessage(h)))
  field <- function(name) {
    v <- grep(paste0("^", name, ":"), h, ignore.case = TRUE, value = TRUE)
    if (length(v)) trimws(sub("^[^:]+:", "", v[length(v)])) else NA_character_
  }
  list(status = as.integer(attr(h, "status")), type = field("content-type"),
       size = suppressWarnings(as.numeric(field("content-length"))),
       modified = field("last-modified"),
       filename = .disposition_name(field("content-disposition")),
       location = field("location"))
}


#' GET a page (following redirects)
#' @return list(status, body); status is NA when the server could not be
#'   reached.
#' @keywords internal
.http_get <- function(url, timeout = 60L) {
  f <- getOption("maexits.http_get")
  if (is.function(f)) return(f(url))
  old <- options(timeout = max(timeout, getOption("timeout", 60)))
  on.exit(options(old), add = TRUE)
  con <- url(url, method = "libcurl",
             headers = c(`User-Agent` = .watch_ua(), `Cache-Control` = "no-cache"))
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  msg <- character()
  body <- withCallingHandlers(
    tryCatch(readLines(con, warn = FALSE, encoding = "UTF-8"),
             error = function(e) { msg <<- c(msg, conditionMessage(e)); NULL }),
    warning = function(w) { msg <<- c(msg, conditionMessage(w)); invokeRestart("muffleWarning") })
  if (!is.null(body)) {
    return(list(status = 200L, body = iconv(paste(body, collapse = "\n"), "UTF-8", "UTF-8", sub = "")))
  }
  code <- regmatches(msg, regexpr("HTTP status was '[0-9]{3}", msg))
  list(status = if (length(code)) as.integer(sub(".*'", "", code[1])) else NA_integer_,
       error = paste(msg, collapse = "; "))
}


.disposition_name <- function(x) {
  if (is.na(x) || !grepl("filename", x, ignore.case = TRUE)) return(NA_character_)
  sub("(?i)^.*filename\\*?=(?:UTF-8'')?\"?([^\";]+)\"?.*$", "\\1", x, perl = TRUE)
}


.is_html <- function(r) grepl("html", r$type %||% "", ignore.case = TRUE)


.http_problem <- function(r) {
  if (is.na(r$status)) sprintf("no response (%s)", r$error %||% "network error")
  else if (r$status == 200L && .is_html(r)) "a web page where the file should be"
  else sprintf("HTTP %d", r$status)
}


.abs_url <- function(href) {
  ifelse(grepl("^https?://", href), href, paste0(MAEXITS_WATCH_SOURCES$cms, href))
}


.cache_buster <- function() format(Sys.time(), "%Y%m%d%H")


# --- Parsing (pure functions, tested offline) -------------------------------

.decode_entities <- function(x) {
  for (e in list(c("&nbsp;", " "), c("&quot;", "\""), c("&apos;", "'"), c("&lt;", "<"),
                 c("&gt;", ">"), c("&ndash;", "\u2013"))) {
    x <- gsub(e[1], e[2], x, fixed = TRUE)
  }
  m <- gregexpr("&#(x[0-9A-Fa-f]+|[0-9]+);", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(s) {
    code <- sub("^&#(.*);$", "\\1", s)
    n <- ifelse(startsWith(code, "x"), strtoi(substring(code, 2), 16L), strtoi(code, 10L))
    vapply(n, function(k) if (is.na(k) || k <= 0) "" else intToUtf8(k), "")
  })
  gsub("&amp;", "&", x, fixed = TRUE)
}


.strip_tags <- function(x) {
  x <- gsub("<!\\[CDATA\\[|\\]\\]>", "", x)
  trimws(gsub("\\s+", " ", .decode_entities(gsub("<[^>]*>", " ", x))))
}


#' Links in an HTML fragment
#' @return data.table(href, text) with entities decoded and tags stripped.
#' @keywords internal
.anchors <- function(html) {
  none <- data.table(href = character(), text = character())
  if (length(html) != 1 || is.na(html) || !nzchar(html)) return(none)
  a <- regmatches(html, gregexpr("(?is)<a\\s[^>]*?href\\s*=\\s*[\"'][^\"']*[\"'][^>]*>.*?</a>",
                                 html, perl = TRUE))[[1]]
  if (!length(a)) return(none)
  data.table(
    href = .decode_entities(trimws(sub("(?is)^<a\\s[^>]*?href\\s*=\\s*[\"']([^\"']*)[\"'].*$",
                                       "\\1", a, perl = TRUE))),
    text = .strip_tags(sub("(?is)^<a[^>]*>(.*)</a>$", "\\1", a, perl = TRUE)))
}


#' The "Downloads" list of a CMS page (NA if the page has none)
#' @keywords internal
.downloads_block <- function(html) {
  i <- regexpr("field--name-field-downloads", html, fixed = TRUE)
  if (i < 0) return(NA_character_)
  rest <- substring(html, i)
  j <- regexpr("</ul>", rest, fixed = TRUE)
  if (j < 0) NA_character_ else substr(rest, 1, j - 1L)
}


#' Items of a CMS RSS feed
#' @return data.table(title, link, period) where period is the item's
#'   report period ("2026" or "2026-09").
#' @keywords internal
.rss_items <- function(xml) {
  items <- regmatches(xml, gregexpr("(?s)<item>.*?</item>", xml, perl = TRUE))[[1]]
  field <- function(tag) {
    m <- regmatches(items, regexec(sprintf("(?s)<%s>(.*?)</%s>", tag, tag), items, perl = TRUE))
    vapply(m, function(v) if (length(v) == 2) .strip_tags(v[2]) else NA_character_, "")
  }
  desc <- field("description")
  data.table(title = field("title"), link = field("link"),
             period = ifelse(grepl("report_period:\\s*[0-9]{4}", desc),
                             sub("^.*report_period:\\s*([0-9]{4}(?:-[0-9]{2})?).*$", "\\1",
                                 desc, perl = TRUE),
                             NA_character_))
}


.zip_href_re <- "(?i)^(?:https?://www\\.cms\\.gov)?/files/zip/[^\"]+\\.zip(?:-\\d+)?$"

# A year standing on its own (so 2026 does not match inside a vintage like 202609)
.year_re <- function(year) sprintf("(?<![0-9])%d(?![0-9])", as.integer(year))

.xwalk_title_re <- function(year) {
  sprintf("(?i)^\\s*%d\\s+(?:Part|Plan)\\s+C\\s*&\\s*D\\s+Plan\\s+Crosswalk", as.integer(year))
}

.landscape_text_re <- function(year) {
  sprintf(paste0("(?i)^(?:CY\\s?%d[\\s_]+Landscape(?![A-Za-z])|",
                 "%d\\s+MA\\s+Landscape\\s+Source\\s+Files?(?![A-Za-z]))"), year, year)
}

.landscape_href_re <- function(year) {
  sprintf(paste0("(?i)/files/zip/(?:cy-?%d-?landscape|%d-ma-landscape-source-files?)",
                 "[a-z0-9._-]*\\.zip(?:-\\d+)?$"), year, year)
}

# Memos, fact sheets and summaries share the wording (and, since CMS builds
# slugs from link text, the address) of the landscape file; the archive
# "CY2006-CY20XX Landscape Files" holds past years
.landscape_exclude_re <- paste0("(?i)memo|format|fact[\\s_-]*sheet|presentation|summary|",
                                "state[\\s_-]*(?:by[\\s_-]*)?state|congressional|readme|",
                                "cy-?20\\d\\d-cy-?20\\d\\d")

.landscape_links <- function(a, year) {
  a[!grepl(.landscape_exclude_re, text, perl = TRUE) &
      !grepl(.landscape_exclude_re, href, perl = TRUE) &
      (grepl(.landscape_text_re(year), text, perl = TRUE) |
         grepl(.landscape_href_re(year), href, perl = TRUE))]
}

# A landscape zip in a new wording: the year on its own and "landscape"
.landscape_loose <- function(a, year) {
  y <- .year_re(year)
  a[grepl(.zip_href_re, href, perl = TRUE) &
      (grepl(y, text, perl = TRUE) | grepl(y, href, perl = TRUE)) &
      (grepl("(?i)landscape", text, perl = TRUE) | grepl("(?i)landscape", href, perl = TRUE)) &
      !grepl(.landscape_exclude_re, text, perl = TRUE) &
      !grepl(.landscape_exclude_re, href, perl = TRUE)]
}


# --- Checks for one file ------------------------------------------------------

.result <- function(status, detail, url = NA_character_, page = NA_character_, head = NULL) {
  list(status = status, detail = detail, url = url, page = page,
       last_modified = head$modified %||% NA_character_, size = head$size %||% NA_real_)
}


#' HEAD a file link and classify it
#' @return list(code, head, url). code is "ok", "unexpected" (wrong type or
#'   too small), "missing" (404/410), "redirect" or "error" (no response,
#'   another status, or a web page where the file should be).
#' @keywords internal
.verify_file <- function(href, min_size, type = "zip") {
  u <- .abs_url(href)
  h <- .http_head(u)
  code <- if (is.na(h$status)) "error"
    else if (h$status == 200L) {
      if (.is_html(h)) "error"
      else if (!grepl(type, h$type %||% "", ignore.case = TRUE) ||
               (!is.na(h$size %||% NA_real_) && h$size < min_size)) "unexpected"
      else "ok"
    }
    else if (h$status %in% c(404L, 410L)) "missing"
    else if (h$status %in% 300:399) "redirect"
    else "error"
  list(code = code, head = h, url = u)
}


# Does the file's address or served name carry the edition's year?
.names_year <- function(v, year) {
  re <- .year_re(year)
  grepl(re, basename(v$url), perl = TRUE) || grepl(re, v$head$filename %||% "", perl = TRUE)
}


#' HEAD candidate file links in order until one checks out
#'
#' A candidate that is a zip of the right size but fails `accept` (e.g. the
#' wrong year) is "wrong_file". Returns the first "ok", else the most
#' telling failure; NULL if there were no candidates.
#' @keywords internal
.verify_any <- function(hrefs, min_size, accept = function(v) TRUE, max_n = 3L) {
  tried <- list()
  for (h in utils::head(unique(hrefs), max_n)) {
    v <- .verify_file(h, min_size)
    if (v$code == "ok" && !accept(v)) v$code <- "wrong_file"
    if (v$code == "ok") return(v)
    tried[[length(tried) + 1L]] <- v
  }
  if (!length(tried)) return(NULL)
  codes <- vapply(tried, function(v) v$code, "")
  for (code in c("wrong_file", "unexpected", "missing", "redirect", "error")) {
    i <- match(code, codes)
    if (!is.na(i)) return(tried[[i]])
  }
}


.describe_file <- function(v) {
  h <- v$head
  sprintf("%s, %s, file name %s",
          if (is.na(h$type %||% NA)) "no content type" else h$type,
          if (is.na(h$size %||% NA)) "size unknown" else sprintf("%.1f MB", h$size / 1e6),
          h$filename %||% NA_character_)
}


# Status for a verified candidate
.outcome <- function(v, what, page, released_detail = sprintf("%s is out", what)) {
  switch(v$code,
    ok = .result("released", released_detail, v$url, page, v$head),
    wrong_file = .result("anomaly", sprintf("a file is listed, but it does not look like %s (%s, %s); check it by hand",
                                            what, basename(v$url), .describe_file(v)), v$url, page, v$head),
    unexpected = .result("anomaly", sprintf("a file is listed, but not as expected (%s); check it by hand",
                                            .describe_file(v)), v$url, page, v$head),
    .result("listed", sprintf("listed, but the file is not downloadable yet (%s)",
                              .http_problem(v$head)), v$url, page, v$head))
}


# Zip links on an item page: its Downloads list, or, if that list is missing
# or has no zip (CMS renamed the field), zips anywhere on the page whose
# address matches `fallback_re`
.item_zips <- function(body, fallback_re) {
  files <- .anchors(.downloads_block(body))
  zips <- files[grepl(.zip_href_re, href, perl = TRUE)]
  if (!nrow(zips)) {
    whole <- .anchors(body)
    zips <- whole[grepl(.zip_href_re, href, perl = TRUE) & grepl(fallback_re, href, perl = TRUE)]
  }
  list(zips = zips, other = files[!grepl(.zip_href_re, href, perl = TRUE)])
}


# A listed item: confirm the file on its page
.confirm_item <- function(page_url, what, min_size, accept, fallback_re, extra = character()) {
  item <- .http_get(page_url)
  if (!identical(item$status, 200L)) {
    return(.result("listed", sprintf("listed, but its page gave %s", .http_problem(item)),
                   page = page_url))
  }
  f <- .item_zips(item$body, fallback_re)
  v <- .verify_any(c(f$zips$href, extra), min_size, accept)
  if (!is.null(v) && v$code == "ok") return(.outcome(v, what, page_url))
  if (!nrow(f$zips) && nrow(f$other)) {
    return(.result("anomaly", sprintf("listed with a download that is not a zip (%s); check it by hand",
                                      f$other$href[1]), page = page_url))
  }
  if (!nrow(f$zips)) return(.result("listed", "listed, but its page has no download yet", page = page_url))
  .outcome(v, what, page_url)
}


.check_crosswalk_release <- function(year) {
  year <- as.integer(year)
  src <- MAEXITS_WATCH_SOURCES
  what <- sprintf("the %d crosswalk", year)
  listed <- NA_character_
  canary <- FALSE
  reached <- FALSE
  latest <- NA_integer_
  problems <- character()

  rss <- .http_get(src$crosswalk_rss)
  if (identical(rss$status, 200L)) {
    reached <- TRUE
    items <- .rss_items(rss$body)
    per_year <- function(y) items[period %in% as.character(y) |
                                    grepl(.xwalk_title_re(y), title, perl = TRUE)]
    canary <- nrow(per_year(year - 1L)) > 0
    hit <- per_year(year)
    if (nrow(hit)) listed <- hit$link[1]
    yrs <- suppressWarnings(as.integer(items$period))
    if (any(!is.na(yrs))) latest <- max(yrs, na.rm = TRUE)
  } else {
    problems <- c(problems, sprintf("RSS %s", .http_problem(rss)))
  }

  # RSS unreachable or changed: use the HTML list
  if (is.na(listed) && !canary) {
    idx <- .http_get(paste0(src$crosswalk_index, "?cb=", .cache_buster()))
    if (identical(idx$status, 200L)) {
      reached <- TRUE
      html <- idx$body
      t0 <- regexpr("dynamic_list_items_table", html, fixed = TRUE)
      if (t0 > 0) {
        html <- substring(html, t0)
        t1 <- regexpr("</table>", html, fixed = TRUE)
        if (t1 > 0) html <- substr(html, 1, t1)
      }
      a <- .anchors(html)
      canary <- any(grepl(.xwalk_title_re(year - 1L), a$text, perl = TRUE))
      hit <- a[grepl(.xwalk_title_re(year), text, perl = TRUE)]
      if (nrow(hit)) listed <- .abs_url(hit$href[1])
    } else {
      problems <- c(problems, sprintf("list page %s", .http_problem(idx)))
    }
  }

  usual <- sprintf("/files/zip/plan-crosswalk-%d.zip", year)
  right_year <- function(v) .names_year(v, year)
  if (!is.na(listed)) {
    return(.confirm_item(listed, what, 1e5, right_year, .year_re(year), extra = usual))
  }

  # Not listed: has the file gone up ahead of its listing?
  early <- .verify_any(usual, 1e5, right_year)
  if (early$code == "ok") {
    return(.result("hint", sprintf("a %d crosswalk zip is up but not listed yet", year),
                   early$url, src$crosswalk_index, early$head))
  }
  if (!reached) return(.result("unknown", paste(problems, collapse = "; "), page = src$crosswalk_index))
  if (!canary) {
    return(.result("broken", sprintf(paste0("the Plan Crosswalks list loaded but shows no %d crosswalk ",
                                            "either; the page may have changed"), year - 1L),
                   page = src$crosswalk_index))
  }
  .result("not_yet", if (is.na(latest)) "not in the Plan Crosswalks list yet"
                     else sprintf("not listed yet (the list ends at %d)", latest),
          page = src$crosswalk_index)
}


.check_landscape_release <- function(year) {
  year <- as.integer(year)
  src <- MAEXITS_WATCH_SOURCES
  page <- src$landscape_page
  p <- .http_get(paste0(page, "?cb=", .cache_buster()))
  if (!identical(p$status, 200L)) {
    return(.result("unknown", sprintf("landscape page %s", .http_problem(p)), page = page))
  }
  block <- .downloads_block(p$body)
  whole <- .anchors(p$body)
  a <- if (is.na(block)) whole else .anchors(block)
  # Links in the Downloads list, plus exact file links elsewhere on the page;
  # failing those, a landscape zip for the year in a new wording
  hits <- unique(rbind(.landscape_links(a, year),
                       .landscape_links(whole[grepl(.landscape_href_re(year), href, perl = TRUE)], year)))
  loose <- !nrow(hits)
  if (loose) hits <- unique(rbind(.landscape_loose(a, year), .landscape_loose(whole, year)))
  current <- .landscape_links(whole, year - 1L)

  if (nrow(hits)) {
    zips <- hits[grepl(.zip_href_re, href, perl = TRUE)]
    if (!nrow(zips)) {
      return(.result("anomaly", sprintf("CY%d is listed with a link that is not a zip (%s); check it by hand",
                                        year, hits$href[1]), page = page))
    }
    zips <- zips[order(!grepl(.landscape_text_re(year), text, perl = TRUE))]
    v <- .verify_any(zips$href, 5e6, function(v) .names_year(v, year))
    text <- zips$text[match(v$url, .abs_url(zips$href))]
    vintage <- regmatches(text, regexpr("\\(\\d{6}(?:\\.\\d+)?\\)", text, perl = TRUE))
    out <- sprintf("CY%d landscape%s is out%s", year,
                   if (length(vintage)) paste0(" ", vintage) else "",
                   if (loose) sprintf(" (new link wording: \"%s\")", text) else "")
    return(.outcome(v, sprintf("the CY%d landscape", year), page, released_detail = out))
  }
  if (!nrow(current)) {
    return(.result("broken", sprintf(paste0("the landscape page loaded but lists no CY%d or CY%d ",
                                            "landscape; the page may have changed"), year - 1L, year),
                   page = page))
  }
  .result("not_yet", sprintf("not listed yet (the page lists %s)", current$text[1]), page = page)
}


.check_cpsc_release <- function(month) {
  src <- MAEXITS_WATCH_SOURCES
  yyyy <- substr(month, 1, 4)
  mm <- substr(month, 6, 7)
  mname <- month.name[as.integer(mm)]
  what <- sprintf("the %s %s CPSC file", mname, yyyy)
  want <- sprintf("(?i)^cpsc_enrollment_%s_%s(?:_\\d+)?\\.zip$", yyyy, mm)
  accept <- function(v) {
    fname <- v$head$filename %||% NA_character_
    is.na(fname) || grepl(want, fname, perl = TRUE)
  }

  # The usual address. A redirect there leads to a different file (March
  # 2025), and another file may sit at it, so anything but a clean match
  # falls through to the CPSC list.
  v <- .verify_any(.cms_zip_url(month), 1e7, accept)
  if (v$code == "ok") return(.outcome(v, what, NA_character_))
  if (v$code == "error") {
    return(.result("unknown", sprintf("CMS %s", .http_problem(v$head)), page = src$cpsc_rss))
  }
  odd <- if (v$code %in% c("wrong_file", "unexpected")) .outcome(v, what, NA_character_)

  rss <- .http_get(src$cpsc_rss)
  if (!identical(rss$status, 200L)) {
    return(odd %||% .result("unknown", sprintf("CPSC list %s", .http_problem(rss)), page = src$cpsc_rss))
  }
  items <- .rss_items(rss$body)
  label_re <- sprintf("(?i)Monthly Enrollment by CPSC\\s+%s\\s+%s\\b", yyyy, mm)
  hit <- items[period %in% month | grepl(label_re, title, perl = TRUE)]
  if (nrow(hit)) {
    r <- .confirm_item(hit$link[1], what, 1e7, accept,
                       sprintf("(?i)(%s.*%s|%s.*%s)", tolower(mname), yyyy, yyyy, mm))
    return(if (r$status == "released" || is.null(odd)) r else odd)
  }
  if (!is.null(odd)) return(odd)
  prev <- sprintf("%d-%s", as.integer(yyyy) - 1L, mm)
  if (!any(items$period %in% prev)) {
    return(.result("broken", sprintf(paste0("the CPSC list loaded but shows no %s file either; ",
                                            "the feed may have changed"), prev), page = src$cpsc_rss))
  }
  latest <- suppressWarnings(max(items$period[grepl("^\\d{4}-\\d{2}$", items$period)]))
  .result("not_yet", sprintf("not out yet (latest CPSC month: %s)", latest), page = src$cpsc_rss)
}


.check_ratebook_release <- function(year) {
  year <- as.integer(year)
  src <- MAEXITS_WATCH_SOURCES
  nber <- function(y) sprintf("%s/%d/countyrate%d.csv", src$nber_ratebook, y, y)
  page <- "https://www.nber.org/research/data/medicare-advantage-ratebook-data"
  h <- .http_head(nber(year))
  if (is.na(h$status) || (h$status == 200L && .is_html(h))) {
    return(.result("unknown", sprintf("NBER %s", .http_problem(h)), page = page))
  }
  if (h$status == 200L) {
    if (grepl("csv|octet-stream|text/plain", h$type %||% "", ignore.case = TRUE)) {
      return(.result("released", sprintf("countyrate%d.csv is on NBER", year), nber(year), page, h))
    }
    return(.result("anomaly", sprintf("NBER answers for countyrate%d.csv, but with type %s; check it by hand",
                                      year, h$type %||% "none"), nber(year), page, h))
  }
  if (!h$status %in% c(404L, 410L)) return(.result("unknown", sprintf("NBER HTTP %d", h$status), page = page))
  cms <- .verify_file(sprintf("/files/zip/%d-ma-rate-book.zip", year), min_size = 1e5)
  if (cms$code == "ok") {
    return(.result("hint", sprintf(paste0("CMS has posted the %d ratebook, but NBER has not ",
                                          "converted it to countyrate%d.csv yet"), year, year),
                   cms$url, page, cms$head))
  }
  prev <- .http_head(nber(year - 1L))
  if (is.na(prev$status)) return(.result("unknown", sprintf("NBER %s", .http_problem(prev)), page = page))
  if (prev$status != 200L) {
    return(.result("broken", sprintf("NBER has no countyrate%d.csv either; its layout may have changed",
                                     year - 1L), page = page))
  }
  .result("not_yet", "not on NBER or CMS yet", page = page)
}


.check_target <- function(t) {
  r <- tryCatch(switch(t$target,
    crosswalk = .check_crosswalk_release(t$edition),
    landscape = .check_landscape_release(t$edition),
    cpsc_dec = , cpsc_jan = .check_cpsc_release(t$edition),
    ratebook = .check_ratebook_release(t$edition)),
    error = function(e) .result("unknown", sprintf("check failed: %s", conditionMessage(e))))
  as.data.table(c(list(target = t$target, edition = as.character(t$edition), label = t$label), r))
}


# --- Check -------------------------------------------------------------------

.run_checks <- function(tg) {
  res <- rbindlist(lapply(seq_len(nrow(tg)), function(i) .check_target(tg[i])), fill = TRUE)
  res <- merge(res, tg[, .(target, edition = as.character(edition), next_step, overdue_date = overdue)],
               by = c("target", "edition"), sort = FALSE)
  res[, overdue := !status %in% "released" & Sys.Date() > overdue_date]
  res[, overdue_date := NULL]
  res[, checked_at := Sys.time()]
  res[, new := FALSE]
  res[]
}


.remember <- function(res, notify) {
  events <- .watch_record(res)
  res[events, new := TRUE, on = .(target, edition)]
  if (notify) .watch_notify(events)
  res
}


#' Check Whether Next Cycle's CMS and NBER Files Are Out
#'
#' Checks the CMS and NBER pages for the files the next pipeline cycle needs
#' and reports, for each, whether it is out. By default these follow from
#' R/config.R:
#' * `crosswalk`: the Part C&D Plan Crosswalk for the first year not in
#'   `MAEXITS_XWALK_FILES` (usually posted early October, but the 2026 one
#'   came on 4 November 2025);
#' * `landscape`: the MA landscape for the first year not in
#'   `MAEXITS_LANDSCAPE_FILES` (late September);
#' * `cpsc_dec`, `cpsc_jan`: the December and January CPSC enrollment
#'   files for the next transition (mid-December; mid-January to
#'   mid-February);
#' * `ratebook`: the NBER `countyrate` file for the next `dec_year`.
#'
#' The check reads the Plan Crosswalks and CPSC RSS feeds, the landscape
#' page and NBER's ratebook folder, and sends HEAD requests to file links.
#' It never downloads a data file. Statuses: `released` (listed,
#' downloadable and named for the right year or month), `hint` (a file is
#' up but not listed yet), `listed` (listed, file not confirmed yet),
#' `anomaly` (a file is there but not as expected), `not_yet`, `unknown`
#' (CMS/NBER could not be reached) and `broken` (a page loaded but did not
#' show last year's file either, so the page has probably changed and the
#' check needs updating).
#'
#' With `remember = TRUE` the results are stored in the folder of
#' [release_check_status()], so each finding is reported (and notified) only
#' once, and [schedule_release_check()] runs the same check daily.
#'
#' @param targets Files to check: any of `"crosswalk"`, `"landscape"`,
#'   `"cpsc_dec"`, `"cpsc_jan"`, `"ratebook"`; `NULL` checks all.
#' @param notify If `TRUE`, show a desktop notification (macOS) for new
#'   findings.
#' @param remember If `TRUE`, record the results so findings are reported
#'   once.
#' @param quiet If `TRUE`, print nothing.
#' @return Invisibly, a data.table with one row per file: target, edition,
#'   label, status, detail, url (the file), page, last_modified, size,
#'   next_step, overdue, checked_at, new (a finding not reported before).
#' @examples
#' \dontrun{
#' check_cms_releases()
#' check_cms_releases("crosswalk")
#' }
#' @export
check_cms_releases <- function(targets = NULL, notify = FALSE, remember = TRUE, quiet = FALSE) {
  res <- .run_checks(.resolve_targets(targets))
  if (remember) res <- .remember(res, notify)
  if (!quiet) .print_watch(res)
  invisible(res)
}


.print_watch <- function(res) {
  cat(sprintf("CMS/NBER release check, %s\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
  lab <- formatC(res$label, width = -max(nchar(res$label)))
  st <- formatC(sub("_", " ", res$status), width = -8)
  pad <- function(i) sprintf("  %s  %s  ", strrep(" ", nchar(lab[i])), strrep(" ", 8))
  for (i in seq_len(nrow(res))) {
    cat(sprintf("  %s  %s  %s%s\n", lab[i], st[i], res$detail[i],
                if (isTRUE(res$overdue[i])) " [later than usual]" else ""))
    if (res$status[i] %in% .WATCH_NOTIFY) {
      link <- if (!is.na(res$url[i])) res$url[i] else res$page[i]
      if (!is.na(link)) cat(pad(i), link, "\n", sep = "")
    }
    if (res$status[i] == "released") cat(pad(i), "Next: ", res$next_step[i], "\n", sep = "")
  }
  invisible(res)
}


# --- State and notifications -----------------------------------------------

.watch_dir <- function() {
  getOption("maexits.watch_dir", file.path(tools::R_user_dir("maexitsv2", "data"), "release-watch"))
}


.watch_read <- function(dir = .watch_dir()) {
  p <- file.path(dir, "state.rds")
  if (!file.exists(p)) return(list())
  st <- tryCatch(readRDS(p), error = function(e) list())
  # Tables read from disk need copying before they can be changed in place
  for (nm in c("targets", "results", "events", "streak")) {
    if (!is.null(st[[nm]])) st[[nm]] <- copy(as.data.table(st[[nm]]))
  }
  st
}


.watch_write <- function(st, dir = .watch_dir()) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("state", tmpdir = dir, fileext = ".rds")
  saveRDS(st, tmp)
  if (!file.rename(tmp, file.path(dir, "state.rds"))) {
    unlink(tmp)
    stop("could not write ", file.path(dir, "state.rds"), call. = FALSE)
  }
  invisible(st)
}


.empty_events <- function() {
  data.table(target = character(), edition = character(), label = character(), event = character(),
             message = character(), at = as.POSIXct(character()), acknowledged = logical())
}


.event <- function(target, edition, label, event, message) {
  data.table(target = target, edition = edition, label = label, event = event,
             message = message, at = Sys.time(), acknowledged = FALSE)
}


#' Update the stored state with new results
#'
#' A release finding (released, hint, listed, anomaly) and a late file are
#' reported once per file. A page that stops looking as expected
#' ('broken') is reported at once and CMS being unreachable after three
#' runs in a row; both again after a later run succeeds and a new failure
#' begins.
#' @return data.table of events not reported before.
#' @keywords internal
.watch_record <- function(res, dir = .watch_dir()) {
  st <- .watch_read(dir)
  events <- st$events %||% .empty_events()
  streak <- st$streak %||% data.table(target = character(), edition = character(), n = integer(),
                                      alerted = character())
  seen <- function(t, e, ev) nrow(events[target == t & edition == e & event == ev]) > 0
  new <- list()
  for (i in seq_len(nrow(res))) {
    r <- res[i]
    link <- if (!is.na(r$url)) r$url else r$page
    add <- function(ev, msg) new[[length(new) + 1L]] <<- .event(r$target, r$edition, r$label, ev, msg)
    if (r$status %in% .WATCH_NOTIFY && !seen(r$target, r$edition, r$status)) {
      add(r$status, if (r$status == "released") {
        sprintf("%s is out: %s. Next: %s", r$label, link, r$next_step)
      } else sprintf("%s: %s (%s).", r$label, r$detail, link))
    }
    if (isTRUE(r$overdue) && !seen(r$target, r$edition, "overdue")) {
      add("overdue", sprintf("%s is still not out, later than in past years; check %s by hand.",
                             r$label, r$page %||% "the CMS page"))
    }
    # alerted: which failure ("unknown" or "broken") this streak has reported
    prev <- streak[target == r$target & edition == r$edition]
    failing <- r$status %in% c("unknown", "broken")
    n <- if (failing) (if (nrow(prev)) prev$n[1] else 0L) + 1L else 0L
    alerted <- if (failing && nrow(prev)) as.character(prev$alerted[1]) else ""
    if (is.na(alerted)) alerted <- ""
    if ((r$status == "broken" && alerted != "broken") ||
        (r$status == "unknown" && n >= 3L && alerted == "")) {
      add(if (r$status == "broken") "broken" else "cannot_check",
          if (r$status == "broken") {
            sprintf(paste0("%s: the release check needs updating; the page no longer looks as ",
                           "expected (%s). Check %s by hand."),
                    r$label, r$detail, r$page %||% "the CMS page")
          } else {
            sprintf("The release check has not been able to reach CMS/NBER for %s for %d runs: %s.",
                    r$label, n, r$detail)
          })
      alerted <- r$status
    }
    streak <- rbind(streak[!(target == r$target & edition == r$edition)],
                    data.table(target = r$target, edition = r$edition, n = as.integer(n),
                               alerted = alerted), fill = TRUE)
  }
  new <- if (length(new)) rbindlist(new) else .empty_events()
  latest <- st$results %||% res[0]
  st$results <- rbind(latest[!res, on = .(target, edition)], res, fill = TRUE)
  st$events <- rbind(events, new)
  st$streak <- streak
  st$last_run <- Sys.time()
  .watch_write(st, dir)
  new
}


.watch_notify <- function(events) {
  if (!nrow(events)) return(invisible(FALSE))
  for (m in events$message) message("[maexitsv2] ", m)
  if (isFALSE(getOption("maexits.notify", TRUE))) return(invisible(FALSE))
  title <- if (nrow(events) == 1L) "maexitsv2: CMS release check"
           else sprintf("maexitsv2: %d CMS release updates", nrow(events))
  text <- if (nrow(events) == 1L) events$message
          else paste(sprintf("%s (%s)", events$label, sub("_", " ", events$event)), collapse = "; ")
  text <- paste(substr(text, 1, 220), "Details: release_check_status()")
  f <- getOption("maexits.notifier")
  if (is.function(f)) return(invisible(f(title, text)))
  .notify_macos(title, text)
}


.notify_macos <- function(title, text) {
  if (Sys.info()[["sysname"]] != "Darwin") return(invisible(FALSE))
  esc <- function(s) gsub("([\"\\\\])", "\\\\\\1", s)
  script <- sprintf("display notification \"%s\" with title \"%s\" sound name \"Glass\"",
                    esc(text), esc(title))
  status <- suppressWarnings(system2("osascript", c("-e", shQuote(script)),
                                     stdout = FALSE, stderr = FALSE))
  invisible(identical(as.integer(status), 0L))
}


# Startup reminder while findings are unacknowledged
.watch_pending_message <- function(dir = .watch_dir()) {
  if (!file.exists(file.path(dir, "state.rds"))) return(NULL)
  ev <- .watch_read(dir)$events
  if (is.null(ev)) return(NULL)
  ev <- ev[acknowledged == FALSE]
  if (!nrow(ev)) return(NULL)
  sprintf("maexitsv2 release check: %s. See release_check_status().",
          paste(sprintf("%s (%s)", ev$label, sub("_", " ", ev$event)), collapse = "; "))
}


.onAttach <- function(libname, pkgname) {
  msg <- tryCatch(.watch_pending_message(), error = function(e) NULL)
  if (length(msg)) packageStartupMessage(msg)
}


#' What the Daily Release Check Has Seen
#'
#' Shows whether the daily check from [schedule_release_check()] is
#' installed, when it last ran, the latest status of each file, and
#' findings not yet acknowledged. Findings are also shown when the package
#' is attached until they are acknowledged.
#'
#' @param acknowledge If `TRUE`, mark the current findings as seen.
#' @return Invisibly, a list with `schedule`, `results` and `events`.
#' @export
release_check_status <- function(acknowledge = FALSE) {
  dir <- .watch_dir()
  st <- .watch_read(dir)
  mac <- Sys.info()[["sysname"]] == "Darwin"
  plist <- .launch_agent_path()
  installed <- file.exists(plist)
  loaded <- installed && mac && .launchctl(c("print", .launchd_service()))$status == 0L
  cat("Daily release check: ",
      if (loaded) sprintf("scheduled at %s", st$at %||% "?")
      else if (installed) "installed but not loaded (log out and in, or rerun schedule_release_check())"
      else if (isTRUE(st$finished)) "finished"
      else if (!mac && !is.null(st$targets)) "set up for cron / Task Scheduler (not managed here)"
      else "not scheduled", "\n", sep = "")
  if (!is.null(st$until)) cat("Runs until: ", format(st$until), "\n", sep = "")
  if (!is.null(st$last_run)) cat("Last check: ", format(st$last_run, "%Y-%m-%d %H:%M"), "\n", sep = "")
  if (!is.null(st$results) && nrow(st$results)) {
    r <- st$results
    lab <- formatC(r$label, width = -max(nchar(r$label)))
    cat("Latest status:\n")
    for (i in seq_len(nrow(r))) {
      cat(sprintf("  %s  %s  %s (%s)\n", lab[i], formatC(sub("_", " ", r$status[i]), width = -8),
                  r$detail[i], format(r$checked_at[i], "%b %d")))
    }
  }
  ev <- st$events %||% .empty_events()
  pending <- ev[acknowledged == FALSE]
  if (nrow(pending)) {
    cat("New findings:\n")
    for (i in seq_len(nrow(pending))) {
      cat(sprintf("  %s  %s\n", format(pending$at[i], "%b %d"), pending$message[i]))
    }
    if (acknowledge) {
      st$events[, acknowledged := TRUE]
      .watch_write(st, dir)
      cat("Marked as seen.\n")
    } else {
      cat("Mark them as seen with release_check_status(acknowledge = TRUE).\n")
    }
  }
  cat("Log: ", file.path(dir, "check.log"), "\n", sep = "")
  invisible(list(schedule = list(installed = installed, loaded = loaded, plist = plist),
                 results = st$results, events = ev))
}


# --- Daily schedule (macOS launchd) -----------------------------------------

.launch_agent_path <- function() {
  file.path(getOption("maexits.launch_agents_dir", path.expand("~/Library/LaunchAgents")),
            paste0(.WATCH_LABEL, ".plist"))
}


.launchd_service <- function() sprintf("gui/%s/%s", .uid(), .WATCH_LABEL)


.uid <- function() {
  f <- getOption("maexits.uid")
  if (!is.null(f)) return(f)
  trimws(system2("id", "-u", stdout = TRUE))
}


.launchctl <- function(args) {
  f <- getOption("maexits.launchctl")
  if (is.function(f)) return(f(args))
  out <- suppressWarnings(system2("launchctl", args, stdout = TRUE, stderr = TRUE))
  list(status = as.integer(attr(out, "status") %||% 0L), output = out)
}


.xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}


#' launchd property list for the daily check
#' @keywords internal
.plist_xml <- function(program, hour, minute, log, workdir) {
  e <- .xml_escape
  c('<?xml version="1.0" encoding="UTF-8"?>',
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">',
    '<plist version="1.0">',
    '<dict>',
    sprintf("  <key>Label</key><string>%s</string>", e(.WATCH_LABEL)),
    "  <key>ProgramArguments</key>",
    "  <array>",
    sprintf("    <string>%s</string>", e(program)),
    "  </array>",
    "  <key>StartCalendarInterval</key>",
    "  <dict>",
    sprintf("    <key>Hour</key><integer>%d</integer>", as.integer(hour)),
    sprintf("    <key>Minute</key><integer>%d</integer>", as.integer(minute)),
    "  </dict>",
    "  <key>RunAtLoad</key><true/>",
    "  <key>ProcessType</key><string>Background</string>",
    sprintf("  <key>WorkingDirectory</key><string>%s</string>", e(workdir)),
    sprintf("  <key>StandardOutPath</key><string>%s</string>", e(log)),
    sprintf("  <key>StandardErrorPath</key><string>%s</string>", e(log)),
    "</dict>",
    "</plist>")
}


#' The script the scheduled job runs
#'
#' Base R only, so that if the package can no longer be loaded (removed, or
#' R upgraded) the job still removes itself instead of failing every day:
#' after three runs without the package, or once `until` has passed.
#' @keywords internal
.job_script <- function(libs, dir, until, plist, service) {
  q <- function(x) paste(deparse(x), collapse = "")
  c("# Written by maexitsv2::schedule_release_check(): the daily CMS release check.",
    "# Stop it with maexitsv2::unschedule_release_check().",
    sprintf(".libPaths(c(%s))", paste(vapply(libs, q, ""), collapse = ", ")),
    sprintf("dir <- %s", q(dir)),
    sprintf("until <- as.Date(%s)", q(format(until))),
    "remove_job <- function(why) {",
    "  cat(format(Sys.time()), why, \"- removing the daily release check.\\n\")",
    sprintf("  unlink(%s)", q(plist)),
    sprintf("  if (Sys.info()[[\"sysname\"]] == \"Darwin\") system2(\"launchctl\", c(\"bootout\", %s))",
            q(service)),
    "}",
    "job <- tryCatch(get(\".release_watch_job\", envir = asNamespace(\"maexitsv2\")),",
    "                error = function(e) NULL)",
    "misses <- file.path(dir, \"missing-package-runs\")",
    "if (is.function(job)) {",
    "  unlink(misses)",
    "  tryCatch(job(dir = dir), error = function(e) {",
    "    cat(format(Sys.time()), \"release check failed:\", conditionMessage(e), \"\\n\")",
    "    if (Sys.Date() > until) remove_job(\"the check fails and its end date has passed\")",
    "  })",
    "} else {",
    "  n <- if (file.exists(misses)) as.integer(readLines(misses)[1]) + 1L else 1L",
    "  writeLines(as.character(n), misses)",
    "  cat(format(Sys.time()), \"maexitsv2 (with its release check) could not be loaded\\n\")",
    "  if (n >= 3L || Sys.Date() > until) remove_job(\"maexitsv2 is no longer installed\")",
    "}")
}


#' Check Once a Day Until Next Cycle's Files Are Out
#'
#' Installs a daily job that runs [check_cms_releases()] and shows a desktop
#' notification when a file comes out (or is late, or the check stops
#' working). The job also runs at login, so a day the Mac was off is caught
#' up, retries twice when CMS cannot be reached (e.g. just after waking),
#' and notifies each finding only once. It removes itself once every file
#' checked is out, or after `until`.
#'
#' On macOS the job is a launchd agent in `~/Library/LaunchAgents`; nothing
#' needs to stay open. On other systems this prints a cron or Task
#' Scheduler line to add yourself. The job runs the *installed* copy of the
#' package, so install it first (`remotes::install_github(...)` or
#' `devtools::install()`), and run [unschedule_release_check()] before
#' removing the package. (If the package disappears anyway, the job removes
#' itself after three runs; by hand: `launchctl bootout
#' gui/$(id -u)/com.github.matthewlavallee.maexitsv2.release-check` and
#' delete the file of that name in `~/Library/LaunchAgents`.) The first
#' check runs right away.
#'
#' Notifications come from macOS "Script Editor"; if none appear, allow
#' them in System Settings > Notifications > Script Editor. Findings also
#' show in [release_check_status()] and when the package is attached.
#'
#' @param at Time of day, `"HH:MM"` (local). CMS usually posts in the
#'   afternoon (Eastern), so the default is 17:45.
#' @param targets Files to watch (see [check_cms_releases()]); `NULL`
#'   watches all.
#' @param until Last day to check; default 31 March after the next January
#'   file.
#' @param check_now Run the first check now.
#' @return Invisibly, the path of the installed job file (macOS) or the
#'   command to schedule (elsewhere).
#' @seealso [unschedule_release_check()], [release_check_status()]
#' @examples
#' \dontrun{
#' schedule_release_check()                        # every file, daily at 17:45
#' schedule_release_check(targets = "crosswalk")   # just the crosswalk
#' }
#' @export
schedule_release_check <- function(at = "17:45", targets = NULL, until = NULL, check_now = TRUE) {
  tm <- regmatches(at, regexec("^([01]?[0-9]|2[0-3]):([0-5][0-9])$", at))[[1]]
  if (length(tm) != 3) .api_stop("at must be a time like \"17:45\"; got \"%s\"", at)
  tg <- .resolve_targets(targets)
  until <- as.Date(until %||% .default_until())
  if (until < Sys.Date()) .api_stop("until (%s) is in the past", format(until))

  mac <- Sys.info()[["sysname"]] == "Darwin"
  dir <- .watch_dir()
  libs <- normalizePath(.libPaths(), mustWork = FALSE)
  lib <- find.package("maexitsv2", lib.loc = libs, quiet = TRUE)
  if (!length(lib)) {
    .api_stop(paste0("the daily job runs the installed package, and maexitsv2 is not installed; ",
                     "install it first (remotes::install_github(\"%s\") or devtools::install())"),
              MAEXITS_DATA_REPO)
  }
  libs <- unique(c(dirname(lib[1]), libs))
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

  # The installed copy must have the job (it may be older than this one)
  probe <- suppressWarnings(system2(rscript, c("--vanilla", "-e", shQuote(sprintf(
    ".libPaths(c(%s)); cat(exists('.release_watch_job', envir = asNamespace('maexitsv2')))",
    paste(vapply(libs, deparse, ""), collapse = ", ")))), stdout = TRUE, stderr = TRUE))
  if (!identical(tail(probe, 1), "TRUE")) {
    .api_stop(paste0("the installed maexitsv2 (%s) has no daily job; reinstall it from this ",
                     "version (devtools::install() or remotes::install_github(\"%s\")). %s"),
              lib[1], MAEXITS_DATA_REPO, paste(probe, collapse = " "))
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  st <- .watch_read(dir)
  st$targets <- tg
  st$until <- until
  st$at <- at
  st$finished <- FALSE
  st$scheduled_at <- Sys.time()
  .watch_write(st, dir)
  if (check_now) {
    res <- check_cms_releases(tg, notify = TRUE)
    if (any(!res$status %in% c("unknown", "broken"))) {
      st <- .watch_read(dir)
      st$last_job_success <- Sys.time()   # the run at load can skip
      .watch_write(st, dir)
    }
  }

  script <- file.path(dir, "run-check.R")
  plist <- .launch_agent_path()
  writeLines(.job_script(libs, dir, until, plist, if (mac) .launchd_service() else ""), script)
  log <- file.path(dir, "check.log")

  if (!mac) {
    line <- if (.Platform$OS.type == "windows") {
      sprintf("schtasks /Create /SC DAILY /TN maexitsv2-release-check /ST %s /TR \"\\\"%s\\\" --vanilla \\\"%s\\\"\"",
              sprintf("%02d:%02d", as.integer(tm[2]), as.integer(tm[3])), rscript, script)
    } else {
      sprintf("%d %d * * * %s --vanilla %s >> %s 2>&1", as.integer(tm[3]), as.integer(tm[2]),
              shQuote(rscript), shQuote(script), shQuote(log))
    }
    message("Automatic scheduling is set up on macOS only. Add this to ",
            if (.Platform$OS.type == "windows") "Task Scheduler (run it in cmd)" else "your crontab (crontab -e)",
            ", and remove it once release_check_status() reports the check finished:\n", line)
    return(invisible(line))
  }

  dir.create(dirname(plist), recursive = TRUE, showWarnings = FALSE)
  writeLines(.plist_xml(c(rscript, "--vanilla", script), tm[2], tm[3], log, dir), plist)
  .launchctl(c("bootout", .launchd_service()))
  unlink(file.path(dir, "lock"), recursive = TRUE)
  domain <- sprintf("gui/%s", .uid())
  r <- .launchctl(c("bootstrap", domain, plist))
  if (r$status != 0L) {
    Sys.sleep(2)
    r <- .launchctl(c("bootstrap", domain, plist))
  }
  if (r$status != 0L) {
    unlink(plist)
    .api_stop("launchctl could not load the daily check: %s", paste(r$output, collapse = " "))
  }
  message(sprintf(paste0("Daily release check scheduled at %s (and at login) until %s. It stops by ",
                         "itself once %s out. See release_check_status(); stop it with ",
                         "unschedule_release_check()."),
                  at, format(until), if (nrow(tg) == 1L) "the file is" else "all files are"))
  invisible(plist)
}


#' Stop the Daily Release Check
#'
#' Removes the job installed by [schedule_release_check()].
#'
#' @param forget If `TRUE`, also delete what the check has recorded.
#' @return Invisibly, `TRUE` if a job was removed.
#' @export
unschedule_release_check <- function(forget = FALSE) {
  plist <- .launch_agent_path()
  had <- file.exists(plist)
  if (Sys.info()[["sysname"]] == "Darwin") .launchctl(c("bootout", .launchd_service()))
  unlink(plist)
  if (forget) {
    unlink(.watch_dir(), recursive = TRUE)
  } else {
    unlink(file.path(.watch_dir(), "lock"), recursive = TRUE)
    st <- .watch_read()
    if (length(st)) {
      st$finished <- TRUE
      .watch_write(st)
    }
  }
  message(if (had) "Daily release check removed." else "No daily release check was scheduled.")
  invisible(had)
}


# Remove the job from inside it: delete the file first, so a failed or
# interrupted bootout still leaves nothing to reload at the next login.
.unschedule_self <- function() {
  unlink(.launch_agent_path())
  if (Sys.info()[["sysname"]] == "Darwin") .launchctl(c("bootout", .launchd_service()))
  invisible(TRUE)
}


.watch_lock <- function(path, stale_hours = 1) {
  if (dir.create(path, showWarnings = FALSE)) return(TRUE)
  age <- difftime(Sys.time(), file.mtime(path), units = "hours")
  if (!is.na(age) && age > stale_hours) {
    unlink(path, recursive = TRUE)
    return(dir.create(path, showWarnings = FALSE))
  }
  FALSE
}


#' Body of the scheduled job
#'
#' Checks the files not yet out, notifies new findings, and removes the
#' schedule once every file is out or the end date has passed. If CMS/NBER
#' cannot be reached (e.g. just after waking), tries twice more 90 seconds
#' apart. Skips if another run is in progress or the job last ran under 4
#' hours ago (the login run and the daily run can fall close together).
#' @keywords internal
.release_watch_job <- function(dir = .watch_dir(), force = FALSE) {
  options(maexits.watch_dir = dir)
  cat(sprintf("\n== %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  st <- .watch_read(dir)
  if (is.null(st$targets) || isTRUE(st$finished)) {
    cat("Nothing left to check; removing the daily job.\n")
    .unschedule_self()
    return(invisible(FALSE))
  }
  if (!force && !is.null(st$last_job_success) &&
      difftime(Sys.time(), st$last_job_success, units = "hours") < 4) {
    cat(sprintf("Checked at %s; skipping.\n", format(st$last_job_success, "%H:%M")))
    return(invisible(FALSE))
  }
  lock <- file.path(dir, "lock")
  if (!.watch_lock(lock)) {
    cat("Another check is running; skipping.\n")
    return(invisible(FALSE))
  }
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)

  released <- if (is.null(st$results)) character() else
    st$results[status == "released", paste(target, edition)]
  todo <- st$targets[!paste(target, edition) %in% released]
  if (nrow(todo)) {
    wait <- getOption("maexits.retry_wait", 90)
    for (attempt in 1:3) {
      res <- .run_checks(todo)
      if (!all(res$status == "unknown") || attempt == 3L) break
      cat(sprintf("Could not reach CMS/NBER; trying again in %d seconds.\n", as.integer(wait)))
      Sys.sleep(wait)
    }
    res <- .remember(res, notify = TRUE)
    .print_watch(res)
    if (any(!res$status %in% c("unknown", "broken"))) {
      st <- .watch_read(dir)
      st$last_job_success <- Sys.time()
      .watch_write(st, dir)
    }
  }

  st <- .watch_read(dir)
  released <- st$results[status == "released", paste(target, edition)]
  missing <- st$targets[!paste(target, edition) %in% released]
  expired <- Sys.Date() > as.Date(st$until)
  if (!nrow(missing) || expired) {
    msg <- if (!nrow(missing)) "Every file is out; the daily release check has stopped."
      else sprintf("The daily release check reached its end date (%s) and stopped; still not out: %s.",
                   format(as.Date(st$until)), paste(missing$label, collapse = ", "))
    if (Sys.info()[["sysname"]] != "Darwin") msg <- paste(msg, "Remove its cron / Task Scheduler entry.")
    ev <- .event("schedule", "", "Daily release check", "stopped", msg)
    st$finished <- TRUE
    st$events <- rbind(st$events %||% .empty_events(), ev)
    .watch_write(st, dir)
    .watch_notify(ev)
    cat(msg, "\n")
    unlink(lock, recursive = TRUE)   # bootout stops this process before on.exit runs
    .unschedule_self()
  }
  invisible(TRUE)
}
