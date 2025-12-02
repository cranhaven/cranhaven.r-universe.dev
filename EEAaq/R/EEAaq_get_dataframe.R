#'@title EEAaq_get_dataframe
#'
#' @description Retrieve one of the metadata (i.e., LAU, NUTS, stations, or pollutant) tables from the EEA and Eurostat
#' dataflows. This function downloads and loads one dataset at a time from a predefined list of available datasets.
#' Ensure that the dataset name is written correctly. See details for further details.
#' @param dataframe name of the \code{data.frame} to retrieve. Select among:
#' \itemize{
#' \item{'LAU': \code{data.frame} containing metadata information on all the local administrative units (i.e., municipalities) in Europe according to the NUTS nomenclature by Eurostat.
#' Information includes geometries.}
#' \item{'NUTS: \code{data.frame} containing metadata information on all the major socio-economic regions in Europe according to the NUTS nomenclature by Eurostat.
#' Information includes geometries.}
#' \item{'stations': \code{data.frame} containing metadata information on all the monitoring stations maintained (both currencly active and de-activated) by the EEA and available in \code{EEAaq}.
#' Information include: unique identifiers, extended descriptions, and technical details on operations and data collected.}
#' \item{'pollutant': \code{data.frame} containing metadata information on all the available pollutants monitored by the EEA and available in \code{EEAaq}.
#' Information include: unique identifiers, extended descriptions, and unit of measure.}
#' }
#' @details
#' The function retrieves information from the \code{EEAaq} GitHub folder one of the available metadata.
#' Since the end of 2024, the data EEA air quality retrieving dataflow is undergoing a major re-organization. In particular, since January 2025, raw data are accessible only through an online platform/dashboard.
#' While \code{EEAaq} is build to explicitly deal with the automatic and constantly-updated system for raw data, the same process is not always possible for the metadata.
#' Indeed, most of the metadata information are updated and require relevant pre-processing (i.e., data manipulation and cleaning) steps to make them consistent with the main database on pollutants concentrations.
#' For this reasons, all the metadata files are periodically pre-processed and updated (on GitHub) by the package maintainers. For issues with the data or code, please contact the development team at <pmaranzano.ricercastatistica@gmail.com>
#' @return a dataframe
#' @export
#'
#' @examples
#' \donttest{
#' LAU <- EEAaq_get_dataframe(dataframe= "LAU")
#' pollutant <- EEAaq_get_dataframe(dataframe = "pollutant")
#' stations <- EEAaq_get_dataframe(dataframe = "stations")
#' NUTS <- EEAaq_get_dataframe(dataframe = "NUTS")
#' }
#'
EEAaq_get_dataframe <- function(dataframe = NULL){

  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.")
  }

  file_urls <- list(
    LAU = "https://github.com/PaoloMaranzano/EEAaq_R/raw/refs/heads/main/Support_files/LAU.rds",
    NUTS = "https://github.com/PaoloMaranzano/EEAaq_R/raw/refs/heads/main/Support_files/NUTS.rds",
    stations = "https://github.com/PaoloMaranzano/EEAaq_R/raw/refs/heads/main/Support_files/stations.rds",
    pollutant = "https://github.com/PaoloMaranzano/EEAaq_R/raw/refs/heads/main/Support_files/pollutant.rds"
  )
  # Check if the required data.frame is valid
  if (!dataframe %in% names(file_urls)) {
    stop("The requested dataframe is not available. Please choose one of the following: ",
         paste(names(file_urls), collapse = ", "), ".")
  }

  # Download the corresponding file
  temp <- tempfile()
  res <- curl::curl_fetch_disk(file_urls[[dataframe]], temp)

  if (res$status_code == 200) {
    result <- readRDS(temp)
    return(result)
  } else {
    stop("The internet resource is not available at the moment. Try again later. If the problem persists, contact the package mainteners.")
  }
}
