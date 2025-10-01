# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.


#' View Available Datasets in SouthKoreAPIs
#'
#' This function lists all datasets available in the 'SouthKoreAPIs' package.
#' If the 'SouthKoreAPIs' package is not loaded, it stops and shows an error message.
#' If no datasets are available, it returns a message and an empty vector.
#'
#' @return A character vector with the names of the available datasets.
#'         If no datasets are found, it returns an empty character vector.
#' @examples
#' if (requireNamespace("SouthKoreAPIs", quietly = TRUE)) {
#'   library(SouthKoreAPIs)
#'   view_datasets_SouthKoreAPIs()
#' }
#' @export
view_datasets_SouthKoreAPIs <- function() {
  # Check if the package is loaded
  if (!"SouthKoreAPIs" %in% .packages()) {
    stop("The 'SouthKoreAPIs' package must be loaded to view its datasets.")
  }

  # Extract dataset information
  datasets_info <- utils::data(package = "SouthKoreAPIs")$results

  # Check if there are datasets available
  if (nrow(datasets_info) == 0) {
    message("No datasets are currently available in the 'SouthKoreAPIs' package.")
    return(character(0))
  }

  # Extract dataset names
  datasets <- datasets_info[, "Item"]

  # Display the message with available datasets
  message("Datasets available in the 'SouthKoreAPIs' package:")

  # Return a vector of datasets without printing to the console
  return(datasets)
}
