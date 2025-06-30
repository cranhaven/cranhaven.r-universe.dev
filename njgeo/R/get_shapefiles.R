#' Download state boundaries
#'
#' @return an sf object with boundaries for the State of New Jersey
#' @importFrom sf st_read
#' @importFrom sf st_transform
#' @importFrom httr http_error
#' @importFrom curl has_internet
#' @param crs coordinate reference system code. This is passed to `sf::st_transform()`
#' @export
#'
get_state_bounds <- function(crs = 4326) {
  # Check if internet connection exists before attempting data download
  if (curl::has_internet() == FALSE) {
    message("No internet connection. Please connect to the internet and try again.")
    return(NULL)
  }

  # Check if data is available and download the data
  if (httr::http_error("https://opendata.arcgis.com/")) {
    message("Data source broken. Please try again.")
    return(NULL)
  } else {
    message("njgeo: downloading data")
    shape <- sf::st_read("https://opendata.arcgis.com/datasets/e4bb3aeab5e14422be1727ccbe0c1fb9_0.geojson", quiet = TRUE)
    shape <- sf::st_transform(shape, crs)
    return(shape)
  }
}

#' Download county boundaries
#'
#' @return an sf object with boundaries for New Jersey counties
#' @importFrom sf st_read
#' @importFrom sf st_transform
#' @importFrom httr http_error
#' @importFrom curl has_internet
#' @param crs coordinate reference system code. This is passed to `sf::st_transform()`
#' @export
#'
get_county_bounds <- function(crs = 4326) {
  # Check if internet connection exists before attempting data download
  if (curl::has_internet() == FALSE) {
    message("No internet connection. Please connect to the internet and try again.")
    return(NULL)
  }

  # Check if data is available and download the data
  if (httr::http_error("https://opendata.arcgis.com/")) {
    message("Data source broken. Please try again.")
    return(NULL)
  } else {
    message("njgeo: downloading data")
    shape <- sf::st_read("https://opendata.arcgis.com/datasets/86d33b1f539843268de673825ebd47ec_0.geojson", quiet = TRUE)
    shape <- sf::st_transform(shape, crs)
    return(shape)
  }
}

#' Download municipal boundaries
#'
#' @importFrom sf st_read
#' @importFrom sf st_transform
#' @importFrom httr http_error
#' @importFrom curl has_internet
#' @param crs coordinate reference system code. This is passed to `sf::st_transform()`
#' @return an sf object with boundaries for all New Jersey municipalities
#' @export
get_muni_bounds <- function(crs = 4326) {
  # Check if internet connection exists before attempting data download
  if (curl::has_internet() == FALSE) {
    message("No internet connection. Please connect to the internet and try again.")
    return(NULL)
  }

  # Check if data is available and download the data
  if (httr::http_error("https://opendata.arcgis.com/")) {
    message("Data source broken. Please try again.")
    return(NULL)
  } else {
    message("njgeo: downloading data")
    shape <- sf::st_read("https://opendata.arcgis.com/datasets/1c6b26a9a14e4132895194e80d6b30f8_0.geojson", quiet = TRUE)
    shape <- sf::st_transform(shape, crs)
    return(shape)
  }
}
