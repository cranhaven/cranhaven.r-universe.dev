# Try a sequence of OBR download URLs until one responds with status < 400.
# Returns a list with: $url        (the working candidate URL or NA),
#                      $final_url  (URL after redirects, used to recover the
#                                  publication vintage when the slug is stable),
#                      $source     ("live" if a candidate worked, "none" if not).
# OBR URLs follow the pattern: https://obr.uk/download/{month}-{year}-{publication}/
# Stable slugs (like public-finances-databank/) redirect to the latest file;
# capturing the redirect target lets us recover the vintage.
obr_resolve_url <- function(url_candidates) {
  for (url in url_candidates) {
    # GET (the OBR's WordPress endpoints don't reliably support HEAD).
    # Redirects are followed so the final URL can be recovered for
    # publications that use a stable slug.
    resp <- tryCatch(
      httr2::request(url) |>
        httr2::req_user_agent("obr R package (https://github.com/charlescoverdale/obr)") |>
        httr2::req_throttle(rate = 5 / 10) |>
        # The OBR CDN sometimes 403s bursts of probes (each vintage probe is
        # a fresh GET). Those are rate-limit responses, not missing files, so
        # back off and retry them; genuine 404s fail fast.
        httr2::req_retry(
          max_tries    = 3,
          is_transient = function(resp) {
            httr2::resp_status(resp) %in% c(403L, 429L, 503L)
          },
          backoff      = function(i) min(10, 2 ^ i)
        ) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform(),
      error = function(e) NULL
    )
    if (!is.null(resp) && httr2::resp_status(resp) < 400L) {
      # Soft-404 guard: a candidate slug that resolves to an HTML page is a
      # WordPress error/landing page, not a data file. Only accept responses
      # whose content type is not text/html.
      ctype <- tryCatch(httr2::resp_content_type(resp),
                        error = function(e) NA_character_)
      if (!is.na(ctype) && grepl("text/html", ctype, fixed = TRUE)) next
      final_url <- tryCatch(httr2::resp_url(resp), error = function(e) url)
      return(list(url = url, final_url = final_url, source = "live"))
    }
  }
  list(url = NA_character_, final_url = NA_character_, source = "none")
}

# Build EFO URL candidates for recent publication cycles (most recent first).
# Months are ordered latest-in-year first so that, once an autumn EFO
# publishes, it wins over that year's spring EFO. With march probed first
# (the pre-0.6.0 order), the resolver kept returning the spring EFO for the
# rest of the year: exactly the failure mode a Budget-day user would hit.
efo_url_candidates <- function(suffix) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  months <- c("november", "october", "march")
  candidates <- character(0)
  for (yr in seq(current_year, current_year - 2L)) {
    for (mn in months) {
      candidates <- c(candidates, paste0(
        "https://obr.uk/download/", mn, "-", yr,
        "-economic-and-fiscal-outlook-", suffix, "/"
      ))
    }
  }
  candidates
}

# Build WTR URL candidates.
wtr_url_candidates <- function() {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  months <- c("october", "june", "march")
  candidates <- character(0)
  for (yr in seq(current_year, current_year - 2L)) {
    for (mn in months) {
      candidates <- c(candidates, paste0(
        "https://obr.uk/download/welfare-trends-report-", mn, "-", yr,
        "-charts-and-tables/"
      ))
    }
  }
  candidates
}

# Build Historical Official Forecasts Database URL candidates.
# OBR sometimes re-uses an older slug across vintages, so we try recent slugs
# first and fall through to the known-stable slug as a fallback.
forecasts_url_candidates <- function() {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  months <- c("november", "october", "july", "march")
  candidates <- character(0)
  for (yr in seq(current_year, current_year - 3L)) {
    for (mn in months) {
      candidates <- c(candidates, paste0(
        "https://obr.uk/download/historical-official-forecasts-database-",
        mn, "-", yr, "/"
      ))
    }
  }
  candidates
}

# Cache directory (platform-aware, base R).
obr_cache_dir <- function() {
  d <- getOption("obr.cache_dir", default = tools::R_user_dir("obr", "cache"))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

# Download a file and cache it; return local path.
obr_fetch <- function(url, filename, refresh = FALSE) {
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    cli::cli_abort("{.arg refresh} must be a single {.cls logical} value.")
  }

  path <- file.path(obr_cache_dir(), filename)

  if (file.exists(path) && !refresh) {
    cli::cli_inform(c("i" = "Loading from cache. Use {.code refresh = TRUE} to re-download."))
    return(path)
  }

  cli::cli_inform(c("i" = "Downloading {.file {filename}} from OBR..."))

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent("obr R package (https://github.com/charlescoverdale/obr)") |>
      httr2::req_throttle(rate = 5 / 10) |>
      httr2::req_retry(
        # 403 is included alongside 429/503 because the OBR's CDN occasionally
        # returns 403 to legitimate requests when probing many URLs in quick
        # succession; backing off and retrying typically clears it.
        max_tries    = 4,
        is_transient = function(resp) {
          httr2::resp_status(resp) %in% c(403L, 429L, 503L)
        },
        backoff      = function(i) min(30, 2 ^ i)
      ) |>
      httr2::req_perform(),
    error = function(e) {
      cli::cli_abort(
        c("Failed to download {.url {url}}.",
          "x" = conditionMessage(e)),
        call = NULL
      )
    }
  )

  writeBin(httr2::resp_body_raw(resp), path)
  cli::cli_inform(c("v" = "Saved to cache."))
  path
}

# Resolve URL with fallback, download, return list with full source metadata.
# Raises a clear warning if the live candidates all failed and a fallback was
# used: this is the audit fix for the silent-fallback issue.
obr_get_xlsx <- function(candidates, fallback, filename, refresh = FALSE,
                         label = "OBR publication") {
  resolved <- obr_resolve_url(candidates)
  if (resolved$source == "live") {
    url       <- resolved$url
    final_url <- resolved$final_url
    source    <- "live"
  } else {
    cli::cli_warn(c(
      "Could not resolve a current {label} URL from {length(candidates)} candidate{?s}.",
      "i" = "Falling back to {.url {fallback}}.",
      "!" = "Returned data may be older than expected. Run with internet access, or pin a vintage explicitly when that feature ships."
    ))
    url       <- fallback
    final_url <- fallback
    source    <- "fallback"
  }

  path <- obr_fetch(url, filename, refresh = refresh)

  retrieved <- tryCatch(file.info(path)$mtime,
                        error = function(e) Sys.time())
  md5 <- tryCatch(unname(tools::md5sum(path)),
                  error = function(e) NA_character_)

  list(
    path      = path,
    url       = url,
    final_url = final_url,
    source    = source,
    retrieved = retrieved,
    file_md5  = md5
  )
}

# Tolerant sheet-name resolver. Pass a primary expected name and an optional
# regex fallback; returns the first match found in the file. Errors clearly
# (with the available sheet list) if neither matches.
resolve_sheet_name <- function(path, primary, fallback_pattern = NULL) {
  available <- readxl::excel_sheets(path)
  if (primary %in% available) return(primary)
  if (!is.null(fallback_pattern)) {
    m <- grep(fallback_pattern, available, value = TRUE)
    if (length(m) >= 1L) return(m[1L])
  }
  cli::cli_abort(c(
    "Could not find sheet matching {.val {primary}} in {.file {basename(path)}}.",
    "i" = "Available sheets: {.val {available}}.",
    "!" = "OBR may have changed the sheet name. Please file an issue at https://github.com/charlescoverdale/obr/issues."
  ))
}

# Allowed values for the schema metadata columns.
# These are the controlled vocabularies that all data-returning functions
# populate from 0.4.0 onwards.
OBR_PERIOD_TYPES <- c("fiscal_year", "quarter", "calendar_year", "month")
OBR_METRIC_TYPES <- c("level", "yoy_pct", "index", "pct", "pct_pts")
OBR_UNITS        <- c("gbp_bn", "gbp_mn", "pct", "pct_pts", "index",
                      "count_mn", "count_k", "count", "hours", "ratio")

# Internal: classify a series name into a metric_type.
# Heuristic; returns one of OBR_METRIC_TYPES. Defaults to "level".
# Used so users can tell apart, e.g. CPI Index (135.2) vs CPI YoY (2.1) which
# previously sat in the same `value` column with no machine-readable distinction.
classify_metric_type <- function(series) {
  if (length(series) == 0L) return(character(0))
  s <- tolower(as.character(series))
  vapply(s, function(x) {
    if (is.na(x) || x == "") return(NA_character_)
    # YoY: explicit growth-rate signals only. Plain "change" is too eager
    # because OBR uses it for level-difference series too (e.g. "Adjustment
    # for the change in pension entitlements", which is in £bn).
    if (grepl(paste0("\\binflation\\b|\\bgrowth\\b|",
                     "\\by[/-]y\\b|\\byoy\\b|year[ -]on[ -]year|",
                     "annual\\s*%|%\\s*change"), x)) {
      "yoy_pct"
    # Index: "Index" must be a standalone word at end of string, or an
    # explicit "(2015=100)" / "(2010=100)" base-year tag. "Index-linked
    # gilts" or "Index of X" must NOT match because the values are levels
    # in £bn or % of GDP, not index points.
    } else if (grepl("\\bindex\\s*$|\\(20[12][05][ =]100\\)", x)) {
      "index"
    } else if (grepl("percentage points?|\\bpp\\b", x)) {
      "pct_pts"
    } else if (grepl("\\brate\\b|\\bratio\\b|\\bshare\\b|\\(%\\)|\\bper cent\\b|\\bpercent\\b|\\bpct\\b|%\\s*$", x)) {
      "pct"
    } else {
      "level"
    }
  }, character(1), USE.NAMES = FALSE)
}

# Internal: default unit for a metric_type. For "level" the caller must supply
# the actual unit (gbp_bn, count_mn, etc.) since level can be many things.
default_unit_for_metric <- function(metric_type) {
  vapply(metric_type, function(m) {
    if (is.na(m)) return(NA_character_)
    switch(m,
           "index"   = "index",
           "yoy_pct" = "pct",
           "pct"     = "pct",
           "pct_pts" = "pct_pts",
           NA_character_)
  }, character(1), USE.NAMES = FALSE)
}

# Internal: build a standard tidy-long observation frame using the v0.4.0
# schema. All data-returning functions use this so outputs can be rbind()'d
# across publications.
#
# Columns: period, period_type, series, metric_type, value, unit
obr_long <- function(period, period_type, series, value,
                     unit = NA_character_, metric_type = NULL) {
  if (is.null(metric_type)) {
    metric_type <- classify_metric_type(series)
  }
  if (length(period_type) == 1L) period_type <- rep(period_type, length(period))
  if (length(unit) == 1L)        unit        <- rep(unit,        length(period))
  data.frame(
    period      = as.character(period),
    period_type = as.character(period_type),
    series      = as.character(series),
    metric_type = as.character(metric_type),
    value       = as.numeric(value),
    unit        = as.character(unit),
    stringsAsFactors = FALSE
  )
}

#' Clear cached OBR files
#'
#' Deletes all files downloaded and cached by the obr package. The next
#' function call will re-download fresh data from the OBR website.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' clear_cache()
#' options(op)
#' }
#'
#' @family data access
#' @export
clear_cache <- function() {
  files <- list.files(obr_cache_dir(), full.names = TRUE)
  n <- length(files)
  if (n > 0) file.remove(files)
  cli::cli_inform("Removed {n} cached file{?s}.")
  invisible(NULL)
}
