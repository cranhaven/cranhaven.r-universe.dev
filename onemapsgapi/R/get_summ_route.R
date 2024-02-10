#' Get Summary Route Information
#'
#' @description
#' This function is a wrapper for the \href{https://www.onemap.gov.sg/docs/#route}{Route Service API}. It is similar to \code{\link{get_route}}, except it returns a tibble with only total time and total distance, and also optionally, the start coordinates and end coordinates.
#' If \code{route = "pt"}, only the best route is chosen (i.e. \code{n_itineraries = 1}).
#'
#' @param token User's API token. This can be retrieved using \code{\link{get_token}}
#' @param start Vector of c(lat, lon) coordinates for the route start point
#' @param end Vector of c(lat, lon) coordinates for the route end point
#' @param route Type of route. Accepted values are \code{walk}, \code{drive}, \code{pt} (public transport), or \code{cycle}
#' @param date Default = current date. Date for which route is requested.
#' @param time Default = current time. Time for which route is requested.
#' @param mode Required if \code{route = "pt"}. Accepted values are \code{transit}, \code{bus} or \code{rail}
#' @param max_dist Optional if \code{route = "pt"}. Maximum walking distance
#' @param route_geom Default = FALSE. Whether to return decoded route_geometry. Please ensure packages \code{googlePolylines} and \code{sf} are installed and note that this is a lossy conversion.
#' @return If no error occurs, a tibble of 1 x 2 with the variables:
#' \describe{
#'   \item{total_time}{The total time taken for this route}
#'   \item{total_dist}{The total distance travelled for this route}
#' }
#'
#' If an error occurs, the output will be \code{NA}, along with a warning message.

get_summ_route <- function(token, start, end, route, date = Sys.Date(), time = format(Sys.time(), format = "%T"), mode = NULL, max_dist = NULL, route_geom = FALSE) {
  # query API
  url <- "https://developers.onemap.sg/privateapi/routingsvc/route?"
  route <- str_to_lower(route)
  query <- paste(url,
                 "start=", str_c(start, collapse = ","),
                 "&end=", str_c(end, collapse = ","),
                 "&routeType=", route,
                 "&token=", token,
                 sep = "")
  if (route == "pt") {
    query <- paste(query,
                   "&date=", date,
                   "&time=", time,
                   "&mode=", str_to_upper(mode),
                   "&maxWalkDistance=", max_dist,
                   "&numItineraries=", "1",
                   sep = "")
  }
  response <- GET(query)

  # error handling
  if (http_error(response)) {
    status <- status_code(response)
    output <- tibble(total_time = NA,
                     total_dist = NA)
    warning(paste("The request (", start , "/", end, "/", route, "/", mode, ") ",
                  "produced a ",
                  status, " error", sep = ""))

    # break function
    return(output)
  }

  # else return output
  output <- content(response)


  # error check: invalid parameters
  if (names(output)[1] == "error") {
    warning("The request (", paste(start, sep = ","), "/", paste(end, sep = ","), "/",
            route, "/", mode, ") ",
            "produced an error: ",
            output$error, sep = "")

    output <- tibble(total_time = NA,
                     total_dist = NA)

    return(output)
  }

  # else return results (pt)
  if (route == "pt") {
    total_dist <- sum(map_dbl(output$plan$itineraries[[1]]$legs, function(x) x$distance))
    result <- tibble(total_time = output$plan$itineraries[[1]]$duration,
                     total_dist = round(total_dist, 0))

  # else return results (non-pt)
  } else {
    result <- tibble(total_time = output$route_summary$total_time,
                     total_dist = output$route_summary$total_dist)
  }

  # append route_geom if requested
  if (route_geom & requireNamespace("googlePolylines", quietly = TRUE) & requireNamespace("sf", quietly = TRUE)) {

    if (route == "pt") {
      route_geometry <- map_chr(output$plan$itineraries[[1]]$legs, function(x) x$legGeometry$points) %>%
        map(function(x) googlePolylines::decode(x)) %>%
        map(function(x) map(x, function(x) select(x, "lon", "lat") %>% data.matrix())) %>%
        map(function(x) sf::st_multilinestring(x)) %>%
        sf::st_sfc(crs=4326)
    } else {
      dec <- googlePolylines::decode(output$route_geom[[1]])
      route_geometry <- dec[[1]] %>%
        select("lon", "lat") %>%
        data.matrix %>%
        sf::st_linestring() %>%
        sf::st_sfc(crs = 4326)
    }
    sf::st_geometry(result) <- sf::st_combine(route_geometry)
  }

  return(result)

}
