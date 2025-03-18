#' Apply registration
#'
#' @param data Input data frame, either containing all replicates of gene expression or not.
#' @param stretches Candidate registration stretch factors to apply to query data.
#' @param shifts Candidate registration shift values to apply to query data.
#'
#' @noRd
apply_registration <- function(data, stretch, shift) {
  # Suppress "no visible binding for global variable" note
  accession <- NULL
  timepoint <- NULL

  data <- data.table::copy(data)

  # Apply registration
  data[, timepoint := as.numeric(timepoint)]
  data[, timepoint := if (accession == "query") timepoint * stretch + shift else timepoint, by = .(accession)]

  return(data)
}
