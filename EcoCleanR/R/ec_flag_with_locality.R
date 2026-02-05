#' Filter records to georeference using GEOLocate
#' @details
#' Records those does not have coordinates assigned but has locality and varbatim locality information to assign coordinates by using external tools such as GEOLocate
#' @param data data table with occurrence information
#' @param uncertainty Mendatory to have coordinateUncertaintyInMeters column in the data table
#' @param locality Mandatory to have locality column in the data table.
#' @param verbatimLocality Mandatory to have verbatimLocality in the data table.
#'
#' @return A column with flagged records as 1, which means these records has potential to be georeferenced.
#'
#' @examples
#' data <- data.frame(
#'   coordinateUncertaintyInMeters = c(NA, "N/A", 50, "30", NA, "N/A", NA),
#'   locality = c("Santa Cruz", NA, "Los Angeles", "N/A", "", "San Diego", NA),
#'   verbatimLocality = c(NA, "CA coast", "", "N/A", "Long Beach", NA, "")
#' )

#' data$flag_check_geolocate <- ec_flag_with_locality(
#' data, uncertainty = "coordinateUncertaintyInMeters",
#' locality = "locality",
#' verbatimLocality = "verbatimLocality"
#' )
#'

#' @export
ec_flag_with_locality <- function(data,
                                  uncertainty = "coordinateUncertaintyInMeters",
                                  locality = "locality",
                                  verbatimLocality = "verbatimLocality") {
  # Step 1: Create a new column with flags
  data <- data %>%
    dplyr::mutate(
      locality_flag = case_when(
        is.na(.data[[uncertainty]]) | .data[[uncertainty]] == "N/A" ~
          ifelse(
            # Check for NA or "N/A" in locality and verbatimLocality
            (!is.na(.data[[locality]]) & .data[[locality]] != "" & .data[[locality]] != "N/A") |
              (!is.na(.data[[verbatimLocality]]) & .data[[verbatimLocality]] != "" & .data[[verbatimLocality]] != "N/A"),
            1, 0
          ),
        TRUE ~ 0
      )
    )

  # Return only the flagged values
  return(data$locality_flag)
}
