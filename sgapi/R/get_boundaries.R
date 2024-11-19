#' @title Get Boundaries Using Geospatial Filter
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
                          geometry_filter = NULL) {
  base_url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/"

  output_fields="*"
  layer=0
  geometry_filter <-gsub("\\,","%2C",geometry_filter)
  assert_function(grepl("\\s",boundary),"Boundary must be not contain any spaces, see https://geoportal.statistics.gov.uk/search?q=Boundary&sort=Date%20Created%7Ccreated%7Cdesc for available boundaries")
  tryCatch(
    {  
  
  if (is.null(geometry_filter)) {
    spatial_object <- sf::st_read(
      paste0(
        base_url,
        boundary,
        "/FeatureServer/",
        layer,
        "/query?where=1%3D1&outFields=",
        output_fields,
        "&outSR=4326&geometryPrecision=3&f=geojson"
      )
    )
  } else{
    i <- 0
    x <- TRUE
    spatial_object <- sf::st_set_crs(sf::st_sf(sf::st_sfc()), value = "WGS84")
    while (x == TRUE)
    {
      result_offset <- i * 200 #this is used move through the dataset as you loop through the different spatial areas
      
      new_sf <- sf::st_read(
        paste0(
          base_url,
          boundary,
          "/FeatureServer/",
          layer,
          "/query?where=1%3D1&outFields=",
          output_fields,
          "&geometry=",
          geometry_filter,
          "&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&geometryPrecision=3&",
          "resultOffset=",
          result_offset, 
          "&",
          "f=geojson"
        )
      )
      
      if (length(new_sf$geometry) == 0) {
        x <- FALSE
      } else{
        spatial_object <- rbind(spatial_object, new_sf)
        i <- i + 1
      }
    }
  }

  if(length(spatial_object)==1L){warning("OUT OF BOUNDS. The selected geometry does not contain any areas in chosen boundary scale")}
  return(spatial_object)
    },
  error = function(e) {
    message("Error in boundary name or geometry filter, check your chosen boundary scale on https://geoportal.statistics.gov.uk/")
    return(NULL)
    }   
  )
}

