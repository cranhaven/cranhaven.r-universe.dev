#' @title Save 'Finna' Search Results for Offline Access
#'
#' @description
#' This function saves 'Finna' search results and metadata locally to a file in `.rds` format,
#' allowing users to access and analyze the data offline without an internet connection.
#'
#' @param data A tibble or data frame containing the 'Finna' search results.
#' @param file_name A string representing the name of the file to save.
#'   The function automatically appends ".rds" to the name if not already included.
#' @return No return value. Called for its side effects of saving the data to a file.
#'
#' @examples
#' \dontrun{
#' search_results <- search_finna("sibelius")
#' save_for_offline(search_results, "sibelius_search_results")
#' }
#' @export
save_for_offline <- function(data, file_name = "offline_search_results") {
  # Ensure the file name has the .rds extension
  if (!grepl("\\.rds$", file_name)) {
    file_name <- paste0(file_name, ".rds")
  }

  # Define the full file path in a temporary directory
  full_path <- file.path(tempdir(), file_name)

  if (!is.null(data) && nrow(data) > 0) {
    saveRDS(data, full_path)
    message("Search results saved successfully to ", full_path)
  } else {
    stop("No data to save. Ensure that the search results are valid.")
  }
}

#' @title Load 'Finna' Search Results from Offline File
#'
#' @description
#' This function loads previously saved 'Finna' search results from a local `.rds` file for offline access.
#'
#' @param file_name A string representing the name of the file to load.
#'   The function automatically appends ".rds" if not already included.
#' @return A tibble or data frame containing the loaded search results.
#' @examples
#' \dontrun{
#' search_results <- search_finna("sibelius")
#' save_for_offline(search_results, "sibelius_search_results")
#' offline_data <- load_offline_data("sibelius_search_results")
#' print(offline_data)
#' }
#' @export
load_offline_data <- function(file_name = "offline_search_results") {
  # Ensure the file name has the .rds extension
  if (!grepl("\\.rds$", file_name)) {
    file_name <- paste0(file_name, ".rds")
  }

  # Define the full file path in the temporary directory
  full_path <- file.path(tempdir(), file_name)

  if (file.exists(full_path)) {
    data <- readRDS(full_path)
    message("Search results loaded successfully from ", full_path)
    return(data)
  } else {
    stop("File not found. Please ensure the file exists and try again.")
  }
}
