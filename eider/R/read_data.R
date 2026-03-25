#' Process the list of tables given, returning a list of tibbles. Each entry in
#' the list can either be a tibble itself (or a data.frame), or a string, in
#' which case it is assumed to be a file and is read in.
#'
#' @param filenames A vector of strings or dataframe-like objects.
#' @return A list of data frames. The names of the list are the same as the
#' names of the input list.
#' @noRd
read_data <- function(source_names) {
  purrr::imap(source_names, read_one_table)
}

#' Helper function to read a single table from a file.
#' @noRd
read_one_table <- function(filepath_or_df, name) {
  if (is.character(filepath_or_df)) {
    df <- utils::read.csv(filepath_or_df, header = TRUE)
  } else if (is.data.frame(filepath_or_df)) {
    df <- filepath_or_df
  } else {
    stop(paste0("Data must be provided as a data frame or a string.,
      The data source ", name, " is of class ", class(filepath_or_df), "."))
  }

  df %>%
    coerce_dates() %>%
    tibble::as_tibble()
}

#' Helper function to coerce YYYYMMDD columns in a data frame to dates where
#' possible, using `lubridate::ymd()`. This function only converts columns
#' where:
#'  - at least one value is successfully coerced to a date
#'  - all non-empty values can be coerced to a date (empty values are left as
#'    NA)
#'
#' @param table A data frame
#' @return A data frame with columns coerced to dates where possible
#' @noRd
coerce_dates <- function(table) {
  cols <- names(table)

  output_na_and_input_nonempty <- function(output, input) {
    is.na(output) & !is.na(input) & input != ""
  }

  for (col in cols) {
    maybe_dates <- lubridate::ymd(table[[col]], quiet = TRUE)
    if (!any(output_na_and_input_nonempty(maybe_dates, table[[col]]))) {
      table[[col]] <- maybe_dates
    }
  }

  table
}
