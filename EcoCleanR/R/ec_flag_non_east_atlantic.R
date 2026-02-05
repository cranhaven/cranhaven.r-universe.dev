#' Flag the occurrences those are not in east Atlantic and are inland

#' @param ocean_names, Insert the name of oceans:
#'  "South Pacific Ocean", "North Pacific Ocean", North Atlantic Ocean", "South Atlantic Ocean"
#' @param buffer_distance, Its a certain buffer distance to consider if a data point is inland. Beyond this distance data points consider as bad data points. e.g. buffer_distance <- 25000
#' @param data, Data table which has latitude and longitude information
#' @param latitude default set to "decimalLatitude"
#' @param longitude default set to "decimalLongitude"
#'
#' @return A new column with flagged values, 1 means bad records 0 means good record. Column name: flag_non_region
#' @import sf
#' @importFrom mregions2 mrp_get
#' @export
#'
#' @examples
#' \donttest{
#' ocean_names <- c("North Atlantic Ocean", "South Atlantic Ocean")
#' buffer_distance <- 25000
#' data <- data.frame(
#'   species = "A",
#'   decimalLongitude = c(-120, -78, -110, -60, -75, -130, -10, 5),
#'   decimalLatitude = c(20, 34, 30, 10, 40, 25, 15, 35)
#' )
#' data$flag_non_region <- ec_flag_non_east_atlantic(
#'   ocean_names,
#'   buffer_distance,
#'   data,
#'   latitude = "decimalLatitude",
#'   longitude = "decimalLongitude"
#' )
#' }

#'
ec_flag_non_east_atlantic <- function(ocean_names,
                                      buffer_distance = 50000,
                                      data,
                                      latitude = "decimalLatitude",
                                      longitude = "decimalLongitude") {
  # Validate inputs
  if (missing(ocean_names) || length(ocean_names) == 0) {
    stop("Please provide at least one ocean name.")
  }
  if (missing(data)) {
    stop("Please provide the dataset.")
  }
  ecodata_sf <- sf::st_sf(geometry = st_sfc())
  # Get ocean shapes
  ocean_shapes <- lapply(ocean_names, function(name) {
    mrp_get("goas", cql_filter = paste0("name = '", name, "'"))
  })

  # Combine all ocean shapes into one
  combined_ocean <- do.call(rbind, ocean_shapes)

  # Define a bounding box
  bbox <- sf::st_bbox(c(xmin = -40, xmax = 20, ymin = -60, ymax = 60), crs = st_crs(combined_ocean))
  suppressWarnings({
    # Crop the ocean shapes to the bounding box
    cropped_ocean <- sf::st_crop(combined_ocean, bbox)

    # Apply buffer and transform back
    ocean_proj <- sf::st_transform(cropped_ocean, crs = 3395)
    ocean_buffered <- sf::st_buffer(ocean_proj, dist = buffer_distance)
    ocean_buffered <- sf::st_transform(ocean_buffered, crs = 4326)

    # Crop the buffered ocean shapes again with bounding box
    cropped_buffered_ocean <- sf::st_crop(ocean_buffered, bbox)
  })


  valid_ocean <- sf::st_make_valid(cropped_buffered_ocean)

  # Convert ecodata to sf object
  ecodata_sf <- sf::st_as_sf(data, coords = c(longitude, latitude), crs = 4326)
  ecodata_sf <- sf::st_transform(ecodata_sf, st_crs(valid_ocean))

  # Identify points inside or outside the ocean boundary
  intersect_indices <- sf::st_intersects(ecodata_sf, valid_ocean, sparse = FALSE)

  # Add a new column to flag points
  # ecodata_sf$flag_non_region <- ifelse(rowSums(intersect_indices) > 0, 0, 1)

  # ecodata$flag_non_region <- ecodata_sf$flag_non_region
  # Add a new column to flag points (1 = outside, 0 = inside)
  flag_non_region <- ifelse(rowSums(intersect_indices) > 0, 0, 1)

  # Return only the flag_non_region column
  return(flag_non_region)
}
