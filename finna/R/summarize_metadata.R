#' Summarize metadata fields for reporting and quality control
#'
#' Create a publication-ready summary table for selected metadata fields. For each
#' field, the summary reports the number of records, number of non-missing values,
#' proportion missing, number of unique (non-missing) values, and the top-N most
#' frequent values with counts.
#'
#' This helper is intended for quick dataset characterization (e.g., Methods,
#' appendices, QC notes) after retrieving records with the package.
#'
#' @importFrom utils head
#' @param data A data frame (or tibble) containing metadata records.
#' @param fields Character vector of column names to summarize. If `NULL` (default),
#'   all columns in `data` are summarized.
#' @param top_n Integer. Number of most frequent values to report per field.
#'   Defaults to 3. Must be >= 0.
#'
#' @return A data frame with one row per summarized field and the following columns:
#' \describe{
#'   \item{field}{Field name.}
#'   \item{n}{Total number of rows in `data`.}
#'   \item{n_non_missing}{Number of non-missing values (`!is.na`).}
#'   \item{prop_missing}{Proportion missing in \[0, 1\], rounded to 3 decimals.}
#'   \item{n_unique}{Number of unique non-missing values.}
#'   \item{top_values}{Top values formatted as `"value (count); ..."`. `NA` if no non-missing values.}
#' }
#'
#' @examples
#' \dontrun{
#' record <- search_finna("sibelius")
#' overview <- summarize_metadata(
#'   record,
#'   fields = c("id", "Title","Author","Year", "Language", "Formats",
#'   "Subjects", "Library", "Series", "last_indexed"))
#'   overview
#' }
#' @export
summarize_metadata <- function(data, fields = NULL, top_n = 3) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame (or tibble).", call. = FALSE)
  }
  if (!is.null(fields) && !is.character(fields)) {
    stop("`fields` must be a character vector of column names or NULL.", call. = FALSE)
  }
  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n)) {
    stop("`top_n` must be a single non-missing numeric value.", call. = FALSE)
  }
  top_n <- as.integer(top_n)
  if (top_n < 0L) {
    stop("`top_n` must be >= 0.", call. = FALSE)
  }

  if (is.null(fields)) {
    fields <- names(data)
  } else {
    missing_fields <- setdiff(fields, names(data))
    if (length(missing_fields) > 0) {
      stop(
        "The following `fields` are not present in `data`: ",
        paste(missing_fields, collapse = ", "),
        call. = FALSE
      )
    }
  }

  summary_list <- lapply(fields, function(f) {
    x <- data[[f]]

    n_total <- length(x)
    n_non_missing <- sum(!is.na(x))
    prop_missing <- if (n_total == 0L) NA_real_ else round(1 - (n_non_missing / n_total), 3)

    x_non_na <- x[!is.na(x)]
    n_unique <- length(unique(x_non_na))

    top_values <- if (top_n == 0L) {
      NA_character_
    } else if (length(x_non_na) > 0L) {
      tbl <- sort(table(x_non_na), decreasing = TRUE)
      top <- head(tbl, top_n)
      paste(paste0(names(top), " (", as.integer(top), ")"), collapse = "; ")
    } else {
      NA_character_
    }

    data.frame(
      field = f,
      n = n_total,
      n_non_missing = n_non_missing,
      prop_missing = prop_missing,
      n_unique = n_unique,
      top_values = top_values,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, summary_list)
  rownames(out) <- NULL
  out
}
