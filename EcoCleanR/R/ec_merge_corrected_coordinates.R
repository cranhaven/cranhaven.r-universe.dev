#' Merge the Update Georeferenced Occurrence Points back to the Main Data File.
#' @param data_corrected After assigning coordinate values using online georeference tools such as GeoLocate, upload the csv file back to R with the name call data_corrected, we hardcoded the field names as "corrected_longitude", "corrected_latitude" and "corrected_uncertainty" and "cleaned_catalog" for column names of data_corrected dataset" which will be merge with "decimalLongitude", "decimalLantitude",  "coordinateUncertaintyInMeters" and "cleaned_catalog" of data table.
#' @param data data table which needs to updated with the assign coordiantes
#' @param catalog this is an important attribute to use matching the records back to the main data file.
#' @param latitude default set to "decimalLatitude", this is a column name of data
#' @param longitude default set to "decimalLongitude", this is a column name of data
#' @param uncertainty_col this is a column name of data and default set to "coordinateUncertaintyInMeters"
#'
#' @return A data frame with updated coordinate information
#' @importFrom dplyr coalesce
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' data <- data.frame(
#'   species = "A",
#'   decimalLongitude = c(-120, -119.8, NA, NA),
#'   decimalLatitude = c(20, 34, NA, NA),
#'   cleaned_catalog = c("12345", "89888", "LACM8898", "SDNHM6767"),
#'   locality = c(NA, NA, "Los Angeles, CA", "San Pedro, CA"),
#'   coordinateUncertaintyInMeters = c(9999, NA, NA, NA)
#' )

#' data_corrected <- data.frame(
#' corrected_longitude = c(-120, -119.8, 118, 118.3),
#'  corrected_latitude = c(20, 34, 33, 32.9),
#'  cleaned_catalog = c("12345", "89888", "LACM8898", "SDNHM6767"),
#'  corrected_uncertainty = c(9999, NA, 5000, 1000)
#' )
#'
#'
#' data<- ec_merge_corrected_coordinates(data_corrected, data,
#' catalog = "cleaned_catalog",
#' latitude = "decimalLatitude",
#' longitude = "decimalLongitude",
#' uncertainty_col = "coordinateUncertaintyInMeters" )
#'
ec_merge_corrected_coordinates <- function(
  data_corrected,
  data, catalog = "cleaned_catalog",
  latitude = "decimalLatitude",
  longitude = "decimalLongitude",
  uncertainty_col = "coordinateUncertaintyInMeters"
) {
  idx <- match(data[[catalog]], data_corrected[[catalog]])

  # Merge corrected values into the original data
  data <- data %>%
    dplyr::mutate(
      !!latitude := dplyr::coalesce(data_corrected$corrected_latitude[idx], .data[[latitude]]),
      !!longitude := dplyr::coalesce(data_corrected$corrected_longitude[idx], .data[[longitude]]),
      !!uncertainty_col := dplyr::coalesce(data_corrected$corrected_uncertainty[idx], .data[[uncertainty_col]])
    )

  return(data)
}
