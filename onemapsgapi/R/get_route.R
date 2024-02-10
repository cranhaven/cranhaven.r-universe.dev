#' Get Route Information
#'
#' @description
#' This function is a wrapper for the \href{https://www.onemap.gov.sg/docs/#route}{Route Service API}. It returns the full route data in a tibble format, or a list of 2 tibbles with results and status information if desired.
#'
#' @param token User's API token. This can be retrieved using \code{\link{get_token}}
#' @param start Vector of c(lat, lon) coordinates for the route start point
#' @param end Vector of c(lat, lon) coordinates for the route end point
#' @param route Type of route. Accepted values are \code{walk}, \code{drive}, \code{pt} (public transport), or \code{cycle}
#' @param date Default = current date. Date for which route is requested.
#' @param time Default = current time. Time for which route is requested.
#' @param mode Required if \code{route = "pt"}. Accepted values are \code{transit}, \code{bus} or \code{rail}
#' @param max_dist Optional if \code{route = "pt"}. Maximum walking distance
#' @param n_itineraries Optional if \code{route = "pt"}. Default = 3. The number of potential routes to provide.
#' @param status_info Default = \code{FALSE}. Whether to return output as a list including a list of status information and a tibble of output
#' @param decode Default = \code{FALSE}. If \code{TRUE}, output will be a sf dataframe displaying route geometry (`route_geom`) instead of a tibble. Requires the `sf` and `googlePolylines` packages. Do note that the decoding of `route_geom` is a lossy conversion.
#' @return If no error occurs and \code{status_info = TRUE}:
#' \describe{
#'   \item{status_info}{A list containing information about the query status. If \code{route = "pt"}, the output contains lists \code{request_params}, \code{debug_output} and \code{elevation}. Else, the list contains the variables \code{status} and \code{status_msg}}
#'   \item{result}{A tibble or sf dataframe containing the data retrieved from the query. This is the only output if status_info = \code{FALSE}. Each row is an itinerary. Output dimensions vary between \code{route = "pt"} and other routes}
#' }
#'
#' If an error occurs, the output will be \code{NULL}, along with a warning message.
#'
#' @export
#'
#' @examples
#' # returns output tibble
#' \dontrun{get_route(token, c(1.319728, 103.8421), c(1.319728905, 103.8421581), "drive")}
#' \dontrun{get_route(token, c(1.319728, 103.8421), c(1.319728905, 103.8421581), "pt",
#'     mode = "bus", max_dist = 300, n_itineraries = 2)}
#'
#' # returns output sf dataframe
#' \dontrun{get_route(token, c(1.319728, 103.8421), c(1.319728905, 103.8421581),
#'     "drive", decode = TRUE)}
#' \dontrun{get_route(token, c(1.319728, 103.8421), c(1.319728905, 103.8421581), "pt",
#'     mode = "bus", max_dist = 300, n_itineraries = 2, decode = TRUE)}
#'
#' # returns list of status list and output tibble
#' \dontrun{get_route(token, c(1.319728, 103.8421), c(1.319728905, 103.8421581),
#'     "drive", status_info = TRUE)}
#'
#' # error: output is NULL, warning message shows status code
#' \dontrun{get_route("invalid_token", c(1.319728, 103.8421), c(1.319728905, 103.8421581), "drive")}
#'
#' # error: output is NULL, warning message shows error message from request
#' \dontrun{get_route(token, c(300, 300), c(400, 500), "cycle")}
#' \dontrun{get_route(token, c(1.319728, 103.8421), c(1.319728905, 103.8421581), "fly")}

get_route <- function(token, start, end, route, date = Sys.Date(), time = format(Sys.time(), format = "%T"), mode = NULL, max_dist = NULL, n_itineraries = 3, status_info = FALSE, decode = FALSE) {
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
                   "&numItineraries=", n_itineraries,
                   sep = "")
  }
  response <- GET(query)

  # error handling
  if (http_error(response)) {
    status <- status_code(response)
    output <- NULL
    warning(paste("The request produced a", status, "error", sep = " "))

    # break function
    return(output)
  }

# else return output
  output <- content(response)

  # error check: invalid parameters
  if (names(output)[1] == "error") {
    warning(output$error)
    output <- NULL
    return(output)

  } else if (route == "pt") {
    # clean raw JSON output to tibble, with each itinerary representing one row.
    # in raw output, each itinerary contains a list named legs. the sum of distances covered in each leg, and number of legs, is extracted.
    n_legs <- map_int(output$plan$itineraries, function(x) length(x$legs))

    total_dist <- map_dbl(output$plan$itineraries, function(x) sum(map_dbl(x$legs, function(x) x$distance)))

    # create itineraries df
    itineraries <- output$plan$itineraries %>%
      map(function(x) replace(x, 12, 0)) %>% # replace legs list with 0
      reduce(bind_rows) %>%
      bind_cols(total_dist = total_dist) %>%
      mutate(legs = n_legs)

    # create other columns df
    query_details <- tibble(date = rep(output$plan$date, nrow(itineraries)),
                            from_lon = rep(output$plan$from$lon, nrow(itineraries)),
                            from_lat = rep(output$plan$from$lat, nrow(itineraries)),
                            to_lon = rep(output$plan$to$lon, nrow(itineraries)),
                            to_lat = rep(output$plan$to$lat, nrow(itineraries)))

    result <- query_details %>%
      bind_cols(itineraries)  %>%
      select(.data$from_lon, .data$from_lat, .data$to_lon, .data$to_lat,
             total_time = .data$duration, .data$total_dist, .data$fare,
             transit = .data$transitTime, wait = .data$waitingTime, walk_dist = .data$walkDistance,
             walk_limit_exceeded = .data$walkLimitExceeded, elevation_lost = .data$elevationLost,
             elevation_gain = .data$elevationGained, .data$transfers, .data$legs, slope = .data$tooSloped,
             .data$date, start_time = .data$startTime, end_time = .data$endTime)

    if (decode & requireNamespace("googlePolylines", quietly = TRUE) & requireNamespace("sf", quietly = TRUE)) {
      route_geom <- map(output$plan$itineraries, function(x) map_chr(x$legs, function(x) x$legGeometry$points)) %>%
        map(function(x) googlePolylines::decode(x)) %>%
        map(function(x) map(x, function(x) select(x, "lon", "lat") %>% data.matrix())) %>%
        map(function(x) sf::st_multilinestring(x)) %>%
        sf::st_sfc(crs=4326)

      sf::st_geometry(result) <- route_geom
    }

  } else {
    route_name <- str_c(output$route_name, collapse = ",")

    # route instructions comes as a list of steps, with each step being an unnamed list of instructions.
    # Concat each part of a step using tab, concat steps using newline.
    route_instructions <- output$route_instructions %>%
      map(str_c, collapse = "\t") %>%
      str_c(collapse = "\n")

    result <- tibble(from_lon = start[1], from_lat = start[2],
                     to_lon = end[1], to_lat = end[2],
                     total_time = output$route_summary$total_time,
                     total_dist = output$route_summary$total_dist,
                     start = output$route_summary$start_point,
                     end = output$route_summary$end_point,
                     route_geom = output$route_geometry,
                     route_name = route_name,
                     route_instruct = route_instructions)

    if (decode & requireNamespace("googlePolylines", quietly = TRUE) & requireNamespace("sf", quietly = TRUE)) {
      dec <- googlePolylines::decode(result$route_geom[[1]])
      route_geom <- dec[[1]] %>%
        select("lon", "lat") %>%
        data.matrix %>%
        sf::st_linestring() %>%
        sf::st_sfc(crs = 4326)

      sf::st_geometry(result) <- route_geom
    }

  }

  # create status_info if requested by user
  if (status_info) {
    if (route == "pt") {
      status_info <- list(request_params = output$requestParameters,
                          debug_output = flatten(output$debugOutput),
                          elevation = output$elevationMetadata)
    } else {
      status_info <- list(status = output$status,
                          status_msg = output$status_message)
    }

    output <- list(status_info = status_info,
                   result = result)

  } else {
    output <- result
  }

  return(output)

}
