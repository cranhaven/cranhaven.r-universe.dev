
has_sf <- function() {
  requireNamespace("sf", quietly = TRUE)
}

convert_df2sf <- function(obj, crs = 4326) {
  if (has_sf() && length(obj)) {
    obj <- sf::st_as_sf(obj, wkt = "wkt", crs = 4326)
  }

  obj
}
