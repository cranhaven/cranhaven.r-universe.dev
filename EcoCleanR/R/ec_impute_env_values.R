#' Impute Environmental Variables using Mean Values of occurrences within a certain radius
#'
#' @param data_x this is data_x which is the output of ec_extract_env_layers
#' @param latitude default set to "decimalLatitude"
#' @param longitude default set to "decimalLongitude"
#' @param radius_km radius to average the values of data points within the circle to imput the values for missing datta points
#' @param iter number of times to iterate the imputation, e.g. 1 or 2 or 3
#'
#' @return An updated table of data_x which has imputed values for the missing env variables, condition applies that the this imputation wont work if the data points are too sparse.

#' @importFrom geosphere distVincentySphere
#' @examples
#'
#' data_x <- data.frame(
#'   scientificName = "Mexacanthina lugubris",
#'   decimalLongitude = c(-117, -117.8, -116.9),
#'   decimalLatitude = c(32.9, 33.5, 31.9),
#'   BO_sstmean = c(12, NA, 14),
#'   BO_sstmin = c(9, NA, 10),
#'   BO_sstmax = c(14, NA, 18)
#' )
#' radius_km <- 10
#' iter <- 3
#' data_x <- ec_impute_env_values(data_x,
#'   latitude = "decimalLatitude",
#'   longitude = "decimalLongitude",
#'   radius_km, iter
#' )
#'
#' @export
ec_impute_env_values <- function(data_x,
                                 latitude = "decimalLatitude",
                                 longitude = "decimalLongitude",
                                 radius_km = 10,
                                 iter = 3) {
  # Identify inland points with missing mean temperature
  env_cols <- setdiff(names(data_x), c(latitude, longitude))
  for (it in seq_len(iter)) {
    message("Imputation iteration ", it)


    inland_points <- data_x[apply(data_x[, env_cols], 1, function(row) any(is.na(row))), ]
    if (nrow(inland_points) == 0) {
      message("No more missing values to impute.")
      break
    }
    # Preallocate vectors for assigned values
    n <- nrow(inland_points)
    assigned_values <- lapply(env_cols, function(x) numeric(n))
    names(assigned_values) <- env_cols

    # Loop through each inland point
    for (i in seq_len(n)) {
      lat <- inland_points[[latitude]][i]
      lon <- inland_points[[longitude]][i]

      # Calculate distances to all other points
      distances <- geosphere::distVincentySphere(
        c(lon, lat),
        cbind(data_x[[longitude]], data_x[[latitude]])
      )

      # Select nearby valid points within the given radius
      nearby_points <- data_x[distances <= radius_km * 1000 & apply(data_x[, env_cols], 1, function(row) all(!is.na(row))), ]

      if (nrow(nearby_points) > 0) {
        for (var in env_cols) {
          assigned_values[[var]][i] <- mean(nearby_points[[var]], na.rm = TRUE)
        }
      } else {
        for (var in env_cols) {
          assigned_values[[var]][i] <- NA
        }
      }
    }

    missing_rows <- which(apply(data_x[, env_cols], 1, function(row) any(is.na(row))))
    for (var in env_cols) {
      data_x[[var]][missing_rows] <- assigned_values[[var]]
    }
  }
  return(data_x)
}
