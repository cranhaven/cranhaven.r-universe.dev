#' Flag Occurrences those are in wrong ocean basins and are inland

#' @param direction, values as "east" or "west". These values help to filter the shape files for east or west of select ocean (e.g. pacific) for both north and south hemisphere.
#' @param ocean, values such as "pacific" or "atlantic"
#' @param buffer, Its a certain buffer distance to consider if a data point is inland. Beyond this distance data points consider as bad data points. e.g. buffer <- 25000
#' @param data, Data table which has latitude and longitude information
#' @param latitude default set to "decimalLatitude"
#' @param longitude default set to "decimalLongitude"
#' @return A new column with flagged values, 1 means bad records 0 means good record. Column name: flag_non_region
#' @import sf
#' @importFrom mregions2 mrp_get
#'
#' @export
#'
#' @examples
#' \donttest{
#' direction <- "east"
#' buffer <- 25000
#' ocean <- "pacific"
#' data <- data.frame(
#'   species = "A",
#'   decimalLongitude = c(-120, -78, -110, -60, -75, -130, -10, 5),
#'   decimalLatitude = c(20, 34, 30, 10, 40, 25, 15, 35)
#' )
#' data$flag_non_region <- ec_flag_non_region(
#'   direction,
#'   ocean,
#'   buffer = 50000,
#'   data
#' )
#' }
ec_flag_non_region <- function(direction, ocean, buffer = 50000, data, latitude = "decimalLatitude", longitude = "decimalLongitude") {
  # Sanitize inputs
  # Sanitize inputs
  direction <- tolower(direction)
  ocean <- tolower(ocean)
  key <- paste0(direction, "_", ocean)

  # Lookup table of region names
  ocean_lookup <- list(
    east_pacific = c("North Pacific Ocean", "South Pacific Ocean"),
    west_pacific = c("North Pacific Ocean", "South Pacific Ocean"),
    east_atlantic = c("North Atlantic Ocean", "South Atlantic Ocean"),
    west_atlantic = c("North Atlantic Ocean", "South Atlantic Ocean")
  )

  # Validate
  if (!key %in% names(ocean_lookup)) {
    stop("Invalid combination. Use direction = 'east' or 'west', and ocean = 'pacific' or 'atlantic'.")
  }

  # Choose which ec_flag function to call (e.g., ec_flag_non_east_pacific)
  func_name <- paste0("ec_flag_non_", key)
  if (!exists(func_name, mode = "function")) {
    stop("Function ", func_name, " does not exist in the environment.")
  }

  # Get correct region names and call the function
  region_names <- ocean_lookup[[key]]
  flag_function <- get(func_name)
  return(flag_function(
    ocean_names = region_names,
    buffer_distance = buffer,
    data = data,
    latitude = latitude,
    longitude = longitude
  ))
}
