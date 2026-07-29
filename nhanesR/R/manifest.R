# R/manifest.R

#' List available NHANES cycles
#'
#' Returns a data frame of all NHANES cycles known to nhanesR, including
#' metadata about survey weights, pandemic adjustment status, and mortality
#' linkage availability.
#'
#' @param include_iii Logical. Include NHANES III (1988-1994)? Default `FALSE`
#'   because its file naming conventions differ from continuous NHANES.
#'
#' @return A tibble with one row per cycle and columns:
#'   \describe{
#'     \item{cycle}{Character. Cycle label (e.g. `"2015-2016"`).}
#'     \item{begin_year, end_year}{Integer. Survey years.}
#'     \item{suffix}{Character. Letter suffix appended to file codes.}
#'     \item{wt_mec_2yr}{Character. 2-year MEC exam weight variable name.}
#'     \item{wt_int_2yr}{Character. 2-year interview weight variable name.}
#'     \item{wt_mec_4yr}{Character. 4-year combined weight, where available.}
#'     \item{wt_prepan}{Character. Pre-pandemic weight for 2017-2020 cycle.}
#'     \item{pandemic_adj}{Logical. Was this cycle pandemic-adjusted?}
#'     \item{has_lmf_public}{Logical. Is a public-use LMF available?}
#'     \item{censor_date}{Character. Mortality follow-up censor date.}
#'   }
#'
#' @seealso [nhanes_manifest()] to see what files are available within a cycle;
#'   [nhanes_download()] to download files; [nhanes_lmf_cycles()] for cycles
#'   that have public-use mortality linkage.
#' @export
#' @examples
#' nhanes_cycles()
#' nhanes_cycles(include_iii = TRUE)
#'
#' # Extract cycle labels as a character vector for use in download functions
#' cycles <- nhanes_cycles()[["cycle"]]
#' cycles[1:10]  # first ten continuous cycles (1999-2018)
nhanes_cycles <- function(include_iii = FALSE) {
  out <- .nhanes_cycles
  if (include_iii) {
    out <- rbind(.nhanes_iii, out)
  }
  out
}

#' List available files for a NHANES cycle and component
#'
#' Queries the CDC NHANES data page for a given cycle and component,
#' returning a data frame of available files with their download URLs
#' and documentation links.
#'
#' Results are cached locally for the session to avoid repeated HTTP
#' requests. Use `refresh = TRUE` to force re-query.
#'
#' @param cycle Character. A cycle string, e.g. `"2015-2016"`. See
#'   [nhanes_cycles()] for valid values.
#' @param component Character. One of `"Demographics"`, `"Dietary"`,
#'   `"Examination"`, `"Laboratory"`, `"Questionnaire"`. Case-insensitive.
#' @param refresh Logical. Force re-query of CDC website even if cached?
#'   Default `FALSE`.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{cycle}{Cycle label.}
#'     \item{component}{Component name.}
#'     \item{file_name}{File code (e.g. `"DEMO_I"`).}
#'     \item{description}{Plain-text description from CDC.}
#'     \item{xpt_url}{Direct URL to the XPT data file.}
#'     \item{doc_url}{URL to the HTML documentation/codebook page.}
#'     \item{date_published}{Date published, if available.}
#'   }
#'
#' @seealso [nhanes_cycles()] for valid cycle labels; [nhanes_download()] to
#'   download a file by its base code; [nhanes_search_variables()] to search
#'   the variable catalog by keyword.
#' @export
#' @examples
#' \donttest{
#' nhanes_manifest("2015-2016", "Laboratory")
#' nhanes_manifest("2015-2016", "Demographics")
#' }
nhanes_manifest <- function(cycle, component, refresh = FALSE) {
  .nhanes_check_pkg("rvest")

  component <- .nhanes_validate_component(component)

  # Session-level cache using environment (avoids disk round-trip per call)
  cache_key <- paste0(cycle, "_", component)
  cached <- .nhanes_manifest_cache[[cache_key]]
  if (!is.null(cached) && !refresh) return(cached)

  url_path <- .nhanes_cycle_field(cycle, "url_path")
  page_url  <- sprintf(
    "https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=%s&Cycle=%s",
    component, url_path
  )

  if (getOption("nhanesR.verbose")) {
    cli::cli_inform("Fetching manifest from CDC for {cycle} / {component}")
  }

  page <- tryCatch(
    rvest::read_html(page_url),
    error = function(e) {
      cli::cli_abort(
        "Failed to reach CDC data page: {.url {page_url}}\\n{conditionMessage(e)}"
      )
    }
  )

  # Parse the CDC data table
  rows <- rvest::html_elements(page, "table tr")

  if (length(rows) <= 1L) {
    cli::cli_warn("No data files found for {cycle} / {component}. The page may be empty or the cycle may not have this component.")
    result <- .nhanes_empty_manifest(cycle, component)
    .nhanes_manifest_cache[[cache_key]] <- result
    return(result)
  }

  result <- lapply(rows[-1L], function(row) {
    cells <- rvest::html_elements(row, "td")
    if (length(cells) < 3L) return(NULL)

    desc      <- trimws(rvest::html_text(cells[[1L]]))
    file_name <- trimws(rvest::html_text(cells[[2L]]))
    date_pub  <- if (length(cells) >= 4L) trimws(rvest::html_text(cells[[4L]])) else NA_character_

    # Pull href for data and doc links from the second cell
    links <- rvest::html_elements(cells[[2L]], "a")
    hrefs <- rvest::html_attr(links, "href")
    hrefs <- hrefs[!is.na(hrefs)]

    xpt_href <- hrefs[grepl("\\.XPT$", hrefs, ignore.case = TRUE)]
    doc_href <- hrefs[grepl("\\.htm$",  hrefs, ignore.case = TRUE)]

    xpt_url <- if (length(xpt_href) > 0L) {
      if (grepl("^https?://", xpt_href[1L])) xpt_href[1L]
      else paste0("https://wwwn.cdc.gov", xpt_href[1L])
    } else NA_character_

    doc_url <- if (length(doc_href) > 0L) {
      if (grepl("^https?://", doc_href[1L])) doc_href[1L]
      else paste0("https://wwwn.cdc.gov", doc_href[1L])
    } else NA_character_

    data.frame(
      cycle          = cycle,
      component      = component,
      file_name      = file_name,
      description    = desc,
      xpt_url        = xpt_url,
      doc_url        = doc_url,
      date_published = date_pub,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, Filter(Negate(is.null), result))
  if (is.null(result)) result <- .nhanes_empty_manifest(cycle, component)

  .nhanes_manifest_cache[[cache_key]] <- result
  result
}

# Session-level in-memory cache for manifests
.nhanes_manifest_cache <- new.env(parent = emptyenv())

.nhanes_empty_manifest <- function(cycle, component) {
  data.frame(
    cycle = character(), component = character(), file_name = character(),
    description = character(), xpt_url = character(), doc_url = character(),
    date_published = character(), stringsAsFactors = FALSE
  )
}

.nhanes_validate_component <- function(component) {
  valid <- c("Demographics", "Dietary", "Examination", "Laboratory", "Questionnaire")
  # Case-insensitive match
  matched <- valid[tolower(valid) == tolower(component)]
  if (length(matched) == 0L) {
    cli::cli_abort(
      "{.arg component} must be one of {.val {valid}}, not {.val {component}}."
    )
  }
  matched
}
