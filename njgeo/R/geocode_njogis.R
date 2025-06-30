#' Geocode an address and retrieve all candidates using the NJOGIS ArcGIS REST API
#'
#' @param address First line of address. Don't use the other address arguments if geocoding a single line address
#' @param address2 Second line of address
#' @param address3 Third line of address
#' @param city Name of city or municipality
#' @param zip 5-digit ZIP code of city
#' @param crs Four digit coordinate reference system code. Defaults to 4326, 3424 and 4269 are also supported
#' @param max_results Max number of address candidates to return
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_as_sf
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr http_error
#' @importFrom curl has_internet
#'
#' @return an sf object with geocoded address candidates for a single address
#' @export
#'
#' @examples
#' geocode_address_candidates(address = "33 Livingston Ave", city = "New Brunswick")
geocode_address_candidates <- function(address = NULL,
                                       address2 = NULL,
                                       address3 = NULL,
                                       city = NULL,
                                       zip = NULL,
                                       max_results = NULL,
                                       crs = 4326) {
  # URL of state geocoding service
  baseurl <- "https://geo.nj.gov/"

  # Check if internet connection exists before attempting data download
  if (curl::has_internet() == FALSE) {
    message("No internet connection. Please connect to the internet and try again.")
    return(NULL)
  }

  # Check if data is available and download the data
  if (httr::http_error(baseurl)) {
    message("Data source broken. Please try again.")
    return(NULL)
  } else {
    message("njgeo: downloading data")
    # Construct the API call
    response <- httr::GET(baseurl,
      path = "arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer/findAddressCandidates",
      query = list(
        f = "pjson",
        outSR = crs,
        Address = address,
        Address2 = address2,
        Address3 = address3,
        City = city,
        Postal = zip,
        maxLocations = max_results
      )
    )
  }

  candidates <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = TRUE, flatten = TRUE)

  candidates <- sf::st_as_sf(candidates[["candidates"]], coords = c("location.x", "location.y"), remove = FALSE, crs = crs)

  return(candidates)
}

#' Reverse geocode a set of coordinates
#'
#' @return Place name and address corresponding to coordinates
#' @param lng Longitude
#' @param lat Latitude
#' @param crs Coordinate reference system code of points
#' @param distance Max distance to search around coordinates (in feet)
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_transform
#' @importFrom sf st_point
#' @importFrom sf st_sfc
#' @importFrom sf st_coordinates
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr http_error
#' @importFrom curl has_internet
#' @export
#'
#' @examples
#' reverse_geocode(-74.44513, 40.49297)
reverse_geocode <- function(lng, lat, distance = NULL, crs = 4326) {

  # Convert inputted arguments to an sf point
  p <- sf::st_point(c(lng, lat))

  # Transform inputed points to NJ's projected coordinate system
  p <- sf::st_transform(sf::st_sfc(p, crs = crs), 3424)

  # Extract individual coordinates
  coords <- sf::st_coordinates(p)

  # Use the converted coordinates for the API call
  lng <- coords[1]
  lat <- coords[2]

  # URL of state geocoding service
  baseurl <- "https://geo.nj.gov/"

  # Check if internet connection exists before attempting data download
  if (curl::has_internet() == FALSE) {
    message("No internet connection. Please connect to the internet and try again.")
    return(NULL)
  }

  # Check if data is available and download the data
  if (httr::http_error(baseurl)) {
    message("Data source broken. Please try again.")
    return(NULL)
  } else {
    message("njgeo: downloading data")
    # Construct the API call
    response <- httr::GET(baseurl,
      path = "arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer/reverseGeocode",
      query = list(
        f = "pjson",
        location = I(paste0(lng, "%2C+", lat)),
        Distance = distance
      )
    )
    response <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)
    response <- data.frame(do.call(cbind, response[["address"]]))
  }
  return(response)
}


#' Batch geocode addresses
#'
#' @param df dataframe with addresses to be geocoded
#' @param id primary key that uniquely identifies rows
#' @param street street address column
#' @param city city column
#' @param state state column
#' @param zip zip code column, expects a 5-digit zip code
#' @param crs coordinate reference system to use for output
#'
#' @return an sf object containing geocoding results
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom dplyr select
#' @export
#'
batch_geocode_addresses <- function(df, id, street, city, state, zip, crs = 4326) {

  # Stop if dataframe is too large for limits
  if (nrow(df) > 1000) {
    stop("Only 1000 addresses can be geocoded at a time. Please reduce the size of your dataframe and try again")
  }

  # Prepare data
  adr_df <- dplyr::select(df, c(id, street, city, state, zip))
  names(adr_df) <- c("OBJECTID", "Street", "City", "State", "Zip")

  # Set missing ZIP codes to empty strings
  adr_df$Zip <- ifelse(is.na(adr_df$Zip), "", as.character(adr_df$Zip))

  # Generate JSON
  tmp_list <- apply(adr_df, 1, function(i) list(attributes = as.list(i)))
  # need to coerce OBJECTID to numeric
  tmp_list <- lapply(tmp_list, function(i) {
    i$attributes$OBJECTID <- as.numeric(i$attributes$OBJECTID)
    i
  })

  adr_json <- jsonlite::toJSON(list(records = tmp_list), auto_unbox = TRUE)

  # URL of state geocoding service
  baseurl <- "https://geo.nj.gov/"

  # Check if data is available and download the data
  if (httr::http_error(baseurl)) {
    message("Data source broken. Please try again.")
    return(NULL)
  } else {
    message("njgeo: downloading data")
    # Construct the API call
    response <- httr::POST(
      url = "https://geo.nj.gov/arcgis/rest/services/Tasks/Addr_NJ_cascade/GeocodeServer/geocodeAddresses",
      body = list(addresses = adr_json, f = "pjson", outSR = crs),
      encode = "form",
    )

    response <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)

    # Clean up rows that didn't geocode
    results <- response[["locations"]]
    results[is.na(results)] <- NA
  }
  return(results)
}




#' Batch geocode addresses in single line format
#'
#' @param df dataframe with addresses to be geocoded
#' @param id primary key that uniquely identifies rows
#' @param address street address column
#' @param crs coordinate reference system to use for output
#'
#' @return a dataframe containing geocoding results
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom httr POST
#' @importFrom dplyr select
#' @export
#'
batch_geocode_sl <- function(df, id, address, crs = 4326) {

  # Stop if dataframe is too large for limits
  if (nrow(df) > 1000) {
    stop("Only 1000 addresses can be geocoded at a time. Please reduce the size of your dataframe and try again")
  }

  # Prepare data
  adr_df <- dplyr::select(df, c(id, address))
  names(adr_df) <- c("OBJECTID", "SingleLine")


  # Generate JSON
  tmp_list <- apply(adr_df, 1, function(i) list(attributes = as.list(i)))
  # need to coerce OBJECTID to numeric
  tmp_list <- lapply(tmp_list, function(i) {
    i$attributes$OBJECTID <- as.numeric(i$attributes$OBJECTID)
    i
  })

  adr_json <- jsonlite::toJSON(list(records = tmp_list), auto_unbox = TRUE)

  # URL of state geocoding service
  baseurl <- "https://geo.nj.gov/"

  # Check if data is available and download the data
  if (httr::http_error(baseurl)) {
    message("Data source broken. Please try again.")
    return(NULL)
  } else {
    message("njgeo: downloading data")
    # Construct the API call
    response <- httr::POST(
      url = "https://geo.nj.gov/arcgis/rest/services/Tasks/Addr_NJ_cascade/GeocodeServer/geocodeAddresses",
      body = list(addresses = adr_json, f = "pjson", outSR = crs),
      encode = "form",
    )

    response <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)

    # Clean up rows that didn't geocode
    results <- response[["locations"]]
    results[is.na(results)] <- NA
  }
  return(results)
}
