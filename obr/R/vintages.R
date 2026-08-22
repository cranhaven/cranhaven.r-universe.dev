# Vintage layer for the obr package.
#
# `obr_efo_vintages()` returns a frame of every EFO published since June 2010,
# with publication date and URL slug. `obr_as_of(date)` resolves the EFO that
# was current on a given calendar date. `obr_pin()` / `obr_unpin()` set a
# session-scoped EFO override that flows into get_efo_fiscal() and
# get_efo_economy() unless those functions are called with an explicit
# `vintage =` argument.

# Hardcoded EFO publication calendar. Dates are the publication date of the
# EFO alongside its associated Budget / Statement, taken from obr.uk/efo/.
# Used both by obr_efo_vintages() and by URL construction for pinned vintages.
efo_vintage_table <- function() {
  vintage <- c(
    "June 2010", "November 2010",
    "March 2011", "November 2011",
    "March 2012", "December 2012",
    "March 2013", "December 2013",
    "March 2014", "December 2014",
    "March 2015", "July 2015", "November 2015",
    "March 2016", "November 2016",
    "March 2017", "November 2017",
    "March 2018", "October 2018",
    "March 2019",
    "March 2020", "November 2020",
    "March 2021", "October 2021",
    "March 2022", "November 2022",
    "March 2023", "November 2023",
    "March 2024", "October 2024",
    "March 2025", "November 2025",
    "March 2026"
  )
  date <- as.Date(c(
    "2010-06-22", "2010-11-29",
    "2011-03-23", "2011-11-29",
    "2012-03-21", "2012-12-05",
    "2013-03-20", "2013-12-05",
    "2014-03-19", "2014-12-03",
    "2015-03-18", "2015-07-08", "2015-11-25",
    "2016-03-16", "2016-11-23",
    "2017-03-08", "2017-11-22",
    "2018-03-13", "2018-10-29",
    "2019-03-13",
    "2020-03-11", "2020-11-25",
    "2021-03-03", "2021-10-27",
    "2022-03-23", "2022-11-17",
    "2023-03-15", "2023-11-22",
    "2024-03-06", "2024-10-30",
    "2025-03-26", "2025-11-26",
    "2026-03-26"
  ))
  slug <- vapply(vintage, function(v) {
    parts <- strsplit(tolower(v), " ", fixed = TRUE)[[1]]
    paste0(parts[1], "-", parts[2], "-economic-and-fiscal-outlook")
  }, character(1), USE.NAMES = FALSE)
  data.frame(
    publication = "EFO",
    vintage     = vintage,
    date        = date,
    slug        = slug,
    stringsAsFactors = FALSE
  )
}

# TRUE if a label looks like an EFO vintage ("November 2026") even when it
# is not (yet) in the hardcoded calendar. Lets users pin a brand-new EFO on
# publication day, before a package release adds it to the table.
is_wellformed_vintage <- function(vintage) {
  is.character(vintage) && length(vintage) == 1L && !is.na(vintage) &&
    grepl(paste0("^(January|February|March|April|May|June|July|August|",
                 "September|October|November|December) [0-9]{4}$"), vintage)
}

# Build a download URL for a given EFO vintage and table suffix. Vintages
# outside the hardcoded calendar are accepted if well-formed: the slug is
# constructed by the OBR's naming convention, with a warning.
efo_url_for_vintage <- function(vintage, suffix) {
  v <- efo_vintage_table()
  match <- v[v$vintage == vintage, , drop = FALSE]
  if (nrow(match) == 0L) {
    if (!is_wellformed_vintage(vintage)) {
      cli::cli_abort(c(
        "Unknown EFO vintage {.val {vintage}}.",
        "i" = "Run {.fn obr_efo_vintages} to see all known vintages, or pass a label like {.val November 2026}."
      ))
    }
    cli::cli_warn(c(
      "{.val {vintage}} is not in this version's EFO calendar.",
      "i" = "Constructing the download URL from the OBR's slug convention. This works on publication day for a brand-new EFO, but will 404 if no such EFO exists."
    ))
    slug <- paste0(tolower(gsub(" ", "-", vintage)),
                   "-economic-and-fiscal-outlook")
    return(sprintf("https://obr.uk/download/%s-%s/", slug, suffix))
  }
  sprintf("https://obr.uk/download/%s-%s/", match$slug[1L], suffix)
}

# Cache filename component derived from a vintage string ("March 2026" → "march_2026").
vintage_cache_tag <- function(vintage) {
  gsub("[^a-z0-9]+", "_", tolower(vintage))
}

# Resolve which vintage is currently in force for EFO calls. Precedence:
#   1. explicit `vintage` argument to the user-facing function
#   2. pin set via obr_pin()
#   3. NULL (= latest, via dynamic resolver)
resolve_efo_vintage <- function(vintage = NULL) {
  if (!is.null(vintage)) return(vintage)
  pinned <- getOption("obr.efo_vintage", default = NULL)
  if (!is.null(pinned) && !is.na(pinned) && nzchar(pinned)) return(pinned)
  NULL
}

#' List known OBR Economic and Fiscal Outlook vintages
#'
#' Returns a data frame of every EFO published since the OBR was created in
#' June 2010, with publication dates and the URL slug used in the OBR's
#' download links. Use this to look up which vintages can be pinned via
#' [obr_pin()] or passed to `vintage =` arguments on [get_efo_fiscal()] and
#' [get_efo_economy()].
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{publication}{Always `"EFO"`.}
#'   \item{vintage}{Vintage label, e.g. `"March 2026"`.}
#'   \item{date}{Publication date of the EFO (Date).}
#'   \item{slug}{URL slug used by the OBR for that vintage's download pages.}
#' }
#'
#' @examples
#' head(obr_efo_vintages())
#' tail(obr_efo_vintages(), 5)
#'
#' @family vintage
#' @export
obr_efo_vintages <- function() {
  efo_vintage_table()
}

#' Find the OBR publication that was current on a given date
#'
#' Returns the most recent EFO that had been published on or before `date`.
#' Useful for reproducing analyses as they would have looked at a given point
#' in time, before subsequent forecast revisions.
#'
#' @param date A Date, or anything coercible to one (e.g. `"2024-11-15"`).
#' @param publication Currently only `"EFO"` is supported.
#'
#' @return A length-one character vector containing the vintage label,
#'   e.g. `"October 2024"`.
#'
#' @examples
#' obr_as_of("2024-11-15")
#' obr_as_of(Sys.Date())
#'
#' @family vintage
#' @export
obr_as_of <- function(date, publication = "EFO") {
  publication <- match.arg(publication, choices = "EFO")
  date_d <- tryCatch(as.Date(date),
                     error = function(e) {
                       cli::cli_abort("{.arg date} could not be coerced to a {.cls Date}.")
                     })
  if (is.na(date_d)) {
    cli::cli_abort("{.arg date} could not be coerced to a {.cls Date}.")
  }
  v <- efo_vintage_table()
  past <- v[v$date <= date_d, , drop = FALSE]
  if (nrow(past) == 0L) {
    cli::cli_abort(c(
      "No EFO had been published on or before {format(date_d)}.",
      "i" = "The OBR was established in June 2010; the first EFO was published 22 June 2010."
    ))
  }
  past$vintage[which.max(past$date)]
}

#' Pin a session-wide OBR EFO vintage
#'
#' Sets the EFO vintage that [get_efo_fiscal()] and [get_efo_economy()] will
#' use when called without an explicit `vintage =` argument. The pin is stored
#' as the option `obr.efo_vintage` and lasts for the R session unless
#' overwritten or removed via [obr_unpin()].
#'
#' @param vintage Vintage label such as `"October 2024"`. See
#'   [obr_efo_vintages()] for the full list. A well-formed label that is not
#'   yet in the package's EFO calendar (e.g. a brand-new EFO published after
#'   this package version was released) is accepted with a warning: download
#'   URLs are then constructed from the OBR's slug convention, so a new EFO
#'   can be pinned on publication day. If `NULL`, this function clears
#'   the pin (equivalent to calling [obr_unpin()]).
#'
#' @return Invisibly returns the pinned vintage string, or `NULL` after
#'   clearing.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' obr_pin("October 2024")
#' obr_pinned()
#' obr_unpin()
#' options(op)
#' }
#'
#' @family vintage
#' @export
obr_pin <- function(vintage = NULL) {
  if (is.null(vintage)) {
    return(invisible(obr_unpin()))
  }
  v <- efo_vintage_table()
  if (!vintage %in% v$vintage) {
    if (!is_wellformed_vintage(vintage)) {
      cli::cli_abort(c(
        "Unknown EFO vintage {.val {vintage}}.",
        "i" = "Run {.fn obr_efo_vintages} to see all known vintages."
      ))
    }
    cli::cli_warn(c(
      "{.val {vintage}} is not in this version's EFO calendar.",
      "i" = "Pinning it anyway; download URLs will be constructed from the OBR's slug convention. Useful on publication day for a brand-new EFO."
    ))
  }
  options(obr.efo_vintage = vintage)
  cli::cli_inform(c("v" = "Pinned EFO to {.val {vintage}}."))
  invisible(vintage)
}

#' Clear the pinned OBR EFO vintage
#'
#' Removes any pin set by [obr_pin()]. After unpinning, [get_efo_fiscal()]
#' and [get_efo_economy()] revert to resolving the most recent live EFO via
#' the dynamic URL resolver.
#'
#' @return Invisibly returns the previously pinned vintage (or `NULL` if no
#'   pin was set).
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' obr_pin("March 2025")
#' obr_unpin()
#' options(op)
#' }
#'
#' @family vintage
#' @export
obr_unpin <- function() {
  prev <- getOption("obr.efo_vintage", default = NULL)
  options(obr.efo_vintage = NULL)
  if (!is.null(prev)) {
    cli::cli_inform(c("v" = "Unpinned EFO (was {.val {prev}})."))
  }
  invisible(prev)
}

#' Show the currently pinned OBR EFO vintage
#'
#' Returns the vintage string currently active via [obr_pin()], or `NULL`
#' if no pin is set.
#'
#' @return Either a length-one character vector or `NULL`.
#'
#' @examples
#' obr_pinned()
#'
#' @family vintage
#' @export
obr_pinned <- function() {
  pinned <- getOption("obr.efo_vintage", default = NULL)
  if (is.null(pinned) || (length(pinned) == 1L && (is.na(pinned) || !nzchar(pinned)))) {
    return(NULL)
  }
  pinned
}
