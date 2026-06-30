#' @title Refine Finna Metadata
#'
#' @description
#' The `refine_metadata` function cleans and standardizes Finna metadata by:
#' - **Validating Required Fields:** Ensures the presence of specified fields and returns `NULL` if any are missing.
#' - **Selecting Relevant Fields:** Allows users to specify which metadata fields to retain.
#' - **Handling Missing Values (Optional):** If `fill_na = TRUE`, replaces `NA` values with placeholders.
#' - **Logging Missing Data (Optional):** If `verbose = TRUE`, prints a summary of missing values.
#'
#' @param data A tibble containing raw Finna metadata.
#' @param fields A character vector of metadata fields to retain. Defaults to standard fields.
#' @param fill_na Logical. If `TRUE`, replaces `NA` values with placeholders. Defaults to `FALSE`.
#' @param verbose Logical. If `TRUE`, prints a summary of missing values. Defaults to `FALSE`.
#' @return A tibble with selected metadata fields, or NULL if required fields are missing.
#'
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refine_metadata(sibelius_data, fill_na = TRUE, verbose = TRUE)
#' }
refine_metadata <- function(data,
                            fields = c("Title", "Author", "Year", "Language",
                                       "Formats", "Subjects", "Library", "Series"),
                            fill_na = FALSE,
                            verbose = FALSE) {

  # Check if the required columns exist in the data
  missing_columns <- setdiff(fields, names(data))

  if (length(missing_columns) > 0) {
    warning(paste("The following required columns are missing:", paste(missing_columns, collapse = ", ")))
    return(NULL)  # Return NULL if any required columns are missing
  }

  # Select relevant fields
  refined <- data %>% select(all_of(fields))

  # Handle missing values if requested
  if (fill_na) {
    refined <- refined %>%
      mutate(across(everything(), ~ ifelse(is.na(.), paste("Unknown", cur_column()), .)))
  }

  # Print missing value summary if verbose mode is enabled
  if (verbose) {
    missing_summary <- colSums(is.na(refined))
    missing_summary <- missing_summary[missing_summary > 0]  # Keep only columns with missing values

    if (length(missing_summary) > 0) {
      message("Missing values detected:\n",
              paste(names(missing_summary), ":", missing_summary, collapse = "\n"))
    } else {
      message("No missing values found.")
    }
  }

  return(refined)
}
