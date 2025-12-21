#' Areal data calculation
#'
#' Computes three different summary statistics:
#' (1) `TotalArea` total area of each polygon;
#' (2) `AreaCovered` area covered by a multipolygon object within a high order polygon; and,
#' (3) `Ratio` ratio between `AreaCovered` and `TotalArea` i.e.
#' ratio between an area covered by a given set of features and total area of a higher-order geography polygon.
#'
#' The function requires two sets of polygon data: high-order and low-order geographic polygons
#'
#' @param polygon_layer multipolygon object of class \code{sf}, \code{sfc} or \code{sfg}.
#'
#' @param higher_geo_lay multipolygon object of class \code{sf}, \code{sfc} or \code{sfg}.
#'
#' @param unique_id_code a string; indicating a unique ID column of \code{higher_geo_lay},
#' used as the summary areas.
#'
#' @param crs coordinate reference system: integer with the EPSG code, or character based on proj4string.
#'
#' @return a \code{tibble} data frame object containing four columns is returned:
#'
#' - the \code{unique_id_code} of \code{higher_geo_lay}
#'
#' - the total area of each polygon
#' in \code{higher_geo_lay} (TotalArea),
#'
#' - the total area covered by \code{polygon_layer} features (AreaCovered),
#'
#' - the ratio between the total area covered by \code{polygon_layer} and total area of
#' \code{higher_geo_lay} polygon (Ratio).
#'
#' @examples
#' ## Run areal_calc() using the packages' dummy data sets.
#' ## The data sets are georeferenced on wgs84. However, a planar system is used to measure areas.
#' ## For the examples provided here, points and polygons relate to the United Kingdom.
#' ## So the British National Grid is used.
#'
#' ## Not run:
#' #outcome <- areal_calc(polygon_layer = pol_small,
#' #higher_geo_lay = pol_large,
#' #unique_id_code = "large_pol_",
#' #crs = "epsg:27700")
#' ## End(Not run)
#'
#'
#' @importFrom dplyr "%>%"
#'
#' @export

areal_calc <- function(polygon_layer,
                       higher_geo_lay,
                       unique_id_code,
                       crs) {


  # we need a crs that is planar
  crs = crs
  # make sure that all layers have consistent CRS- in this case is WGS84
  polygon_layer <- sf::st_transform(polygon_layer, crs)
  higher_geo_lay <- sf::st_transform(higher_geo_lay, crs)


  # calculate total area of the higher geography layer
  higher_geo_lay$TotalArea <-
    sf::st_area(higher_geo_lay$geometry)
  # convert area of the higher geography layer to numeric too
  higher_geo_lay$TotalArea <-
    as.numeric(higher_geo_lay$TotalArea)

  # assume that the attribute is constant throughout the geometry
  sf::st_agr(polygon_layer) = "constant"
  sf::st_agr(higher_geo_lay) = "constant"


  #run the intersect function, converting the output to a tibble in the process
  int <- dplyr::as_tibble(sf::st_intersection(polygon_layer, higher_geo_lay))

  int$area <- sf::st_area(int$geometry)

  # convert area to numeric
  int$area <- as.numeric(int$area)

  # remove polygons that are outside the grid boundaries to avoid getting errors
  int <- int %>%
    tidyr::drop_na(!!as.name(unique_id_code))

  CoverByGeo <- int %>%
    dplyr::group_by(!!as.name(unique_id_code)) %>% # '!!' this evaluates if it is true, when it is '!' evaluates if it is false
    dplyr::summarise(AreaCovered = sum(area), .groups = 'drop_last')


  # to calculate the ratio of area covered by the total area of the higher geography layer
  combined_data <- dplyr::left_join(CoverByGeo, higher_geo_lay, by = unique_id_code)
  combined_data$Ratio <- combined_data$AreaCovered / combined_data$TotalArea



  results <- combined_data[,c(unique_id_code, "TotalArea", "AreaCovered", "Ratio")]
  return(results)

}
