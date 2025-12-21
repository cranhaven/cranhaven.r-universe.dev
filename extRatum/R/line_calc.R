#' Line data calculation
#'
#' Computes three different summary statistics:
#' (1) `TotalArea` total area of each polygon;
#' (2) `TotalLength` total length of a multilinestring object within a polygon
#' (3) `Ratio` ratio between `TotalLength` and `TotalArea` i.e.
#' the ratio between the total length and total area of a higher-order geography polygon.
#'
#' @param line_layer multilinestring object of class \code{sf}, \code{sfc} or \code{sfg}.
#'
#' @param higher_geo_lay multipologon object of class \code{sf}, \code{sfc} or \code{sfg}.
#'
#' @param unique_id_code a string; indicating a unique ID column of \code{higher_geo_lay},
#' used as the summary areas.
#'
#' @param crs coordinate reference system: integer with the EPSG code, or character based on proj4string.
#'
#' @return a \code{tibble} data frame object containing four columns:
#'
#' the \code{unique_id_code} of \code{higher_geo_lay}
#'
#' the total area of each polygon
#' in \code{higher_geo_lay} (TotalArea)
#'
#' the total length of \code{line_layer} features (TotalLength)
#'
#' the ratio between the total length of \code{line_layer} and the the total area of
#' \code{higher_geo_lay} polygon (Ratio).
#'
#' @examples
#' ## Run line_calc() using the packages' dummy data sets.
#' ## The data sets are georeferenced on wgs84. However, a planar system is used to measure areas.
#' ## For the examples provided here, points and polygons relate to the United Kingdom.
#' ## So the British National Grid is used.
#'
#' ## Not run:
#' #outcome <- line_calc(
#' # line_layer = lines,
#' # higher_geo_lay = pol_large,
#' # unique_id_code = "large_pol_",
#' # crs = "epsg:27700")
#' ## End(Not run)
#'
#' @importFrom dplyr "%>%"
#'
#' @export
line_calc <- function(line_layer,
                      higher_geo_lay,
                      unique_id_code,
                      crs) {

  crs = crs
  # make sure that all layers have consistent CRS- in this case is WGS84
  line_layer <- sf::st_transform(line_layer, crs)
  higher_geo_lay <- sf::st_transform(higher_geo_lay, crs)

  # calculate total area of the higher geography layer
  higher_geo_lay$TotalArea <-
    sf::st_area(higher_geo_lay$geometry)
  # convert area of the higher geography layer to numeric too
  higher_geo_lay$TotalArea <-
    as.numeric(higher_geo_lay$TotalArea)

  # assume that the attribute is constant throughout the geometry
  sf::st_agr(line_layer) = "constant"
  sf::st_agr(higher_geo_lay) = "constant"

  #run the intersect function, converting the output to a tibble in the process
  int <- dplyr::as_tibble(sf::st_intersection(line_layer, higher_geo_lay))

  # calculate the length of each line in metres
  int$foot_len <- sf::st_length(int$geometry)

  # convert area to numeric
  int$foot_len <- as.numeric(int$foot_len)

  # remove polygons that are outside the grid boundaries to avoid getting errors
  int <- int %>%
    tidyr::drop_na(!!as.name(unique_id_code))

  LengthByGeo <- int %>%
    dplyr::group_by(!!as.name(unique_id_code)) %>% # '!!' this evaluates if it is true, when it is '!' evaluates if it is false
    dplyr::summarise(TotalLength = sum(foot_len), .groups = 'drop_last')

  # to calculate the ratio of length by the total area of the higher geography layer
  combined_data <- dplyr::left_join(LengthByGeo, higher_geo_lay, by = unique_id_code)
  combined_data$Ratio <- combined_data$TotalLength / combined_data$TotalArea



  results <- combined_data[,c(unique_id_code, "TotalArea", "TotalLength", "Ratio")]

  return(results)

}

