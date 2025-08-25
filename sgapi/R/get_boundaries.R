#' @title Get Boundaries Using Geospatial Filter
#' 
#' @usage get_boundaries(boundary, geometry_filter = NULL, 
#' base_url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services")
#'
#' @description
#' Retrieve boundaries from the Office for National Statistics (ONS) 'ONS Geography Portal'
#' given a valid boundary name and layer name. 
#' If the submitted geometry is outwith the ONS Boundary,
#' e.g. the geometry is in France, the function will return 
#' an empty shape file.
#'
#' @param boundary A valid ONS boundary name given as a string.
#' @param geometry_filter geospatial shape or point (using latitude and longitude).
#' Currently limited to a rectangular box or dropped pin.
#' @param base_url Open Geography Portal base url
#'
#' @examples 
#' \dontrun{
#' get_boundaries(boundary="MSOA_Dec_2011_Boundaries_Generalised_Clipped_BGC_EW_V3_2022",
#' geometry_filter="-1.282825,52.354169,0.206626,52.7106")
#' }
#' 
#' @returns An sf object for all constituencies in the geospatial area submitted through 
#' the geometry_filter, at the chosen ONS Boundary. 
#' @export

get_boundaries <- function(boundary,
                          geometry_filter = NULL,
                          base_url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services") {

  url_queries <- list(
    "prefix" = "query?",
    "where" = "1%3D1",
    "outFields" = "*",
    "outSR" = 4326,
    "geometryPrecision" = 3,
    "f" = "geojson")

  if (!is.null(geometry_filter)) {
    geometry_filter <- gsub("\\,", "%2C", geometry_filter)
    url_queries[["geometry"]] <- geometry_filter
    url_queries[["geometryType"]] <- "esriGeometryEnvelope"
    url_queries[["inSR"]] <- 4326
    url_queries[["spatialRel"]] <- "esriSpatialRelIntersects"
  }
  
  url_query_string <- do.call(build_url_query_string, url_queries)
  ons_url <- sprintf("%s/%s/FeatureServer/0/%s", base_url, boundary, url_query_string)
  
  spatial_object <- sf::st_set_crs(sf::st_sf(sf::st_sfc()), value = "WGS84")
  
  result_offset <- 0
  curr_url <- ons_url
  
  while ({new_sf <- sf::st_read(curr_url); length(new_sf$geometry) > 0}) {
    spatial_object <- rbind(spatial_object, new_sf)
    result_offset <- result_offset + 2000
    curr_url <- sprintf("%s&resultOffset=%s", ons_url, result_offset)
  }

  if (length(spatial_object) == 1L) {
    warning("OUT OF BOUNDS. The selected geometry does not contain any areas in chosen boundary scale")
  }
  
  return(spatial_object)
}

