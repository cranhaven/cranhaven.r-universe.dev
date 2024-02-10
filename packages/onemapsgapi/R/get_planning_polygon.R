#' Get Planning Polygon for a Specific Point
#'
#' @description
#' This function is a wrapper for the \href{https://www.onemap.gov.sg/docs/#planning-area-query}{Planning Area Query API}. It returns the spatial polygon data matching the specified location point, either in raw format, as an sf or sp object.
#'
#' @param token User's API token. This can be retrieved using \code{\link{get_token}}
#' @param lat Latitude of location point
#' @param lon Longitude of location point
#' @param year Optional, check \href{https://www.onemap.gov.sg/docs/#planning-area-query}{documentation} for valid options. Invalid requests will are ignored by the API.
#' @param read Optional, defaults to \code{tibble}. Package to use to read geojson object. For "sf" objects, specify \code{read = "sf"} and for "sp" objects use \code{read = "rgdal"}.
#'
#' @return If the parameter \code{read} is not specified, the function returns a raw JSON object a list containing the planning area name and a geojson string representing the polygon. \cr \cr
#' If \code{read = "sf"}, the function returns a 1 x 2 "sf" dataframe: "name" (name of planning area) and "geometry", which contains the simple feature. \cr \cr
#' If \code{read = "rgdal"}, the function returns a SpatialPolygonsDataFrame of "sp" class. The names of the planning area is recorded in the "name" column of the dataframe. \cr \cr
#' If an error occurs, the function returns NULL and a warning message is printed.
#'
#' @note
#' If the user specifies a \code{read} method but does not have the corresponding package installed, the function will return the raw JSON and print a warning message.
#'
#' @export
#'
#' @examples
#' # returns raw JSON object
#' \dontrun{get_planning_polygon(token, lat = 1.429443081, lon = 103.835005)}
#' \dontrun{get_planning_polygon(token, lat = 1.429443081, lon = 103.835005, year = 2008)}
#'
#' # returns dataframe of class "sf"
#' \dontrun{get_planning_polygon(token, lat = 1.429443081, lon = 103.835005, read = "sf")}
#'
#' # returns SpatialPolygonsDataFrame ("sp" object)
#' \dontrun{get_planning_polygon(token, lat = 1.429443081, lon = 103.835005, read = "rgdal")}
#'
#' # error: output is NULL, warning message shows status code
#' \dontrun{get_planning_polygon("invalid_token")}
#' \dontrun{get_planning_polygon(token, "invalidlat", "invalidlon")}


get_planning_polygon <- function(token, lat, lon, year = NULL, read = "tibble") {

  # query API
  url <- "https://developers.onemap.sg/privateapi/popapi/getPlanningarea"

  query <- paste(url, "?",
                 "token=", token,
                 "&lat=", lat,
                 "&lng=", lon,
                 sep = "")
  if (!is.null(year)) {
    query <- paste(query,
                   "&year=", year,
                   sep = "")
  }

  response <- GET(query)

  # error handling
  if (http_error(response)) {
    status <- status_code(response)
    output <- NULL
    warning(paste("The request produced a", status, "error", sep = " "))
    return(output)

  }

  # return output
  output <- content(response)

  # read into requested format
  if (read %in% c("sf", "rgdal") & requireNamespace("sf", quietly = TRUE)) {
    geom_str <- str_replace(str_replace(str_replace(output[[1]][["geojson"]], 'coordinates', '"coordinates"'), 'type', '"type"'), 'MultiPolygon', '"MultiPolygon"')
    output <- bind_cols(name = output[[1]][["pln_area_n"]], sf::st_read(geom_str, quiet = TRUE))
    sf::st_geometry(output) <- output$geometry

    if (read == "rgdal") {
      output <- sf::as_Spatial(output)
    }

  } else if (read != "tibble") {
    warning(paste0("Failed to read geojson. Please ensure you have package ", read, " installed.
                   Only packages sf and rgdal (for sp) are supported currently."))
  }

  return(output)

}
