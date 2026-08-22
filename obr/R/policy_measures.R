# OBR Policy Measures Database.
# A single Excel workbook with two main sheets ("Tax Measures" and
# "Spending Measures"), one row per individual policy measure since 1970
# (tax) or 2010 (spending). Each row carries the fiscal event that scored
# the measure, a description, the Treasury head, and the Exchequer effect
# in GBP million for each fiscal year out to year five.

PMD_FALLBACK <- "https://obr.uk/download/policy-measures-database-march-2025/"
PMD_FILENAME <- "policy_measures_database.xlsx"

# Build PMD URL candidates for recent fiscal events. OBR re-uses an older
# slug across vintages, so we still include the known-stable slug as a
# fallback after probing the most recent slugs.
pmd_url_candidates <- function() {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  months <- c("november", "october", "july", "march")
  candidates <- character(0)
  for (yr in seq(current_year, current_year - 3L)) {
    for (mn in months) {
      candidates <- c(candidates, paste0(
        "https://obr.uk/download/policy-measures-database-", mn, "-", yr, "/"
      ))
    }
  }
  candidates
}

pmd_source <- function(refresh = FALSE) {
  obr_get_xlsx(
    candidates = pmd_url_candidates(),
    fallback   = PMD_FALLBACK,
    filename   = PMD_FILENAME,
    refresh    = refresh,
    label      = "Policy Measures Database"
  )
}

pmd_obr_tbl <- function(data, src) {
  new_obr_tbl(
    data        = data,
    publication = "PMD",
    vintage     = obr_url_vintage(src$final_url %||% src$url),
    source_url  = src$url,
    retrieved   = src$retrieved,
    file_md5    = src$file_md5
  )
}

# Parse a "Tax Measures" or "Spending Measures" sheet into long format.
# Layout assumption: row 1 = sheet title, row 2 = blank, row 3 = headers
# ("Event", "Measure description", "Tax head" / "Spending head", then
# fiscal-year columns), row 4+ = data. Trailing footnote rows where the
# event column is NA are dropped automatically.
parse_pmd_measures <- function(path, sheet) {
  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, .name_repair = "minimal")
  hdr <- as.character(unlist(raw[3, ]))
  year_cols <- which(grepl("^[0-9]{4}-[0-9]{2}$", hdr))
  if (length(year_cols) == 0L) {
    cli::cli_abort(c(
      "No fiscal-year header columns found in sheet {.val {sheet}}.",
      "i" = "OBR may have changed the PMD layout. Please file an issue at https://github.com/charlescoverdale/obr/issues."
    ))
  }
  fiscal_years <- hdr[year_cols]

  body <- raw[4:nrow(raw), ]
  events  <- as.character(unlist(body[, 1]))
  descs   <- as.character(unlist(body[, 2]))
  heads   <- as.character(unlist(body[, 3]))

  result <- do.call(rbind, lapply(seq_along(year_cols), function(i) {
    yr_col <- year_cols[i]
    data.frame(
      event       = events,
      measure     = descs,
      head        = heads,
      fiscal_year = fiscal_years[i],
      value_mn    = suppressWarnings(
        as.numeric(as.character(unlist(body[, yr_col])))
      ),
      stringsAsFactors = FALSE
    )
  }))

  result <- result[!is.na(result$event) & !is.na(result$value_mn), ]
  rownames(result) <- NULL
  result
}

#' Get OBR policy measures by fiscal event
#'
#' Downloads (and caches) the OBR Policy Measures Database, a single workbook
#' that lists every tax or spending measure scored at a UK fiscal event,
#' with the Exchequer effect in GBP million for each year of the forecast
#' horizon. Tax measures cover 1970 to date; spending measures cover 2010
#' to date.
#'
#' This is the only programmatic access to the PMD: the OBR otherwise
#' distributes it as a single Excel file.
#'
#' @param type Character vector. Which measures to return: `"tax"`,
#'   `"spending"`, or both (the default).
#' @param search Optional character. A regular expression used to filter
#'   `measure` and `head` (case-insensitive). For example,
#'   `search = "alcohol"` keeps any measure whose description or head
#'   mentions alcohol.
#' @param since Optional fiscal-year cut-off (e.g. `"2020-21"`). Rows whose
#'   `fiscal_year` is earlier are dropped.
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with columns:
#' \describe{
#'   \item{event}{Fiscal event that scored the measure, e.g. `"Budget 2024"` (character)}
#'   \item{measure}{Plain-English description of the measure (character)}
#'   \item{head}{Tax head (for tax measures) or spending head (character)}
#'   \item{fiscal_year}{Fiscal year being scored, e.g. `"2024-25"` (character)}
#'   \item{value_mn}{Exchequer effect in GBP million. For tax measures, a
#'     positive value raises revenue; for spending measures, a positive
#'     value increases spending (numeric)}
#' }
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#'
#' # The OBR CDN occasionally rate-limits or returns 403 to repeated
#' # requests. tryCatch so the example degrades gracefully if that happens
#' # during the CRAN check.
#' oct24 <- tryCatch(get_policy_measures(),
#'                   error = function(e) NULL)
#' if (!is.null(oct24)) {
#'   head(oct24[grepl("2024", oct24$event) &
#'              oct24$fiscal_year == "2025-26", ])
#'
#'   # All alcohol-duty measures since 2010
#'   tryCatch(
#'     get_policy_measures(type = "tax", search = "alcohol",
#'                         since = "2010-11"),
#'     error = function(e) NULL
#'   )
#' }
#'
#' options(op)
#' }
#'
#' @family policy measures
#' @export
get_policy_measures <- function(type = c("tax", "spending"),
                                search = NULL,
                                since  = NULL,
                                refresh = FALSE) {
  type <- match.arg(type, several.ok = TRUE)

  src <- pmd_source(refresh)

  parts <- list()
  if ("tax" %in% type) {
    tx <- parse_pmd_measures(src$path, "Tax Measures")
    tx$type <- "tax"
    parts$tax <- tx
  }
  if ("spending" %in% type) {
    sp <- parse_pmd_measures(src$path, "Spending Measures")
    sp$type <- "spending"
    parts$spending <- sp
  }

  data <- do.call(rbind, parts)
  rownames(data) <- NULL
  data <- data[, c("type", "event", "measure", "head", "fiscal_year", "value_mn")]

  if (!is.null(search)) {
    if (!is.character(search) || length(search) != 1L) {
      cli::cli_abort("{.arg search} must be a single character string.")
    }
    keep <- grepl(search, data$measure, ignore.case = TRUE) |
            grepl(search, data$head,    ignore.case = TRUE)
    data <- data[keep, ]
  }
  if (!is.null(since)) {
    if (!is.character(since) || length(since) != 1L ||
        !grepl("^[0-9]{4}-[0-9]{2}$", since)) {
      cli::cli_abort("{.arg since} must be a fiscal-year string like {.val 2020-21}.")
    }
    data <- data[data$fiscal_year >= since, ]
  }
  rownames(data) <- NULL

  pmd_obr_tbl(data, src)
}

#' Summarise policy measures by fiscal event
#'
#' Aggregates the measures returned by [get_policy_measures()] to give the
#' net Exchequer effect (positive = revenue-raising / spending-reducing for
#' tax, spending-increasing for spending) by fiscal event and fiscal year.
#'
#' @param x An `obr_tbl` returned by [get_policy_measures()].
#'
#' @return An `obr_tbl` with columns:
#' \describe{
#'   \item{type}{`"tax"` or `"spending"`}
#'   \item{event}{Fiscal event}
#'   \item{fiscal_year}{Fiscal year}
#'   \item{value_mn}{Sum of the Exchequer effect across all measures
#'     scored at that event, in GBP million}
#' }
#' Provenance is preserved.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' pm <- get_policy_measures(type = "tax", since = "2024-25")
#' policy_measures_summary(pm)
#' options(op)
#' }
#'
#' @family policy measures
#' @export
policy_measures_summary <- function(x) {
  if (!inherits(x, "obr_tbl")) {
    cli::cli_abort("{.arg x} must be an {.cls obr_tbl} from {.fn get_policy_measures}.")
  }
  prov <- attr(x, "obr_provenance")
  df <- as.data.frame(x)
  required <- c("type", "event", "fiscal_year", "value_mn")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "{.arg x} is missing required column{?s} {.val {missing}}.",
      "i" = "Pass the result of {.fn get_policy_measures} directly."
    ))
  }
  agg <- aggregate(value_mn ~ type + event + fiscal_year, data = df, FUN = sum)
  agg <- agg[order(agg$event, agg$type, agg$fiscal_year), ]
  rownames(agg) <- NULL
  out <- new_obr_tbl(
    data        = agg,
    publication = "PMD",
    vintage     = prov$vintage,
    source_url  = prov$source_url,
    retrieved   = prov$retrieved,
    file_md5    = prov$file_md5,
    notes       = "Summed across measures by event and fiscal year."
  )
  out
}
