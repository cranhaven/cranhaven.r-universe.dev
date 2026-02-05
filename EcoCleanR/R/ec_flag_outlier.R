#' Flag Outlier Occurrences - using Spatial and Non-spatial Attributes

#' @param data data table with spatial and environmental variables
#' @param latitude default set to "deciamlLatitude"
#' @param longitude default set to "decimalLongitude"
#' @param env_layers header names of env variables. env_layers <- c("Temperature", "pH")
#' @param itr iteration to run the clustering 100 or 1000 times
#' @param k number of cluster to choose in each iteration
#' @param geo_quantile value with geo_quantile percentile would consider has threshold for geo_distance to derive the outlier. e.g. default 0.99
#' @param maha_quantile value with maha_quantile percentile would consider has threshold for maha_distance to derive the outlier. e.g. default 0.99
#'
#' @return A column call flag_outlier which has outlier probability from 0 to 1. 1 is more towards outlier, 0 more towards good data points.
#' @importFrom geosphere distm
#' @importFrom geosphere distHaversine
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'   scientificName = "Mexacanthina lugubris",
#'   decimalLongitude = c(-117, -117.8, -116.9),
#'   decimalLatitude = c(32.9, 33.5, 31.9),
#'   BO_sstmean = c(12, 13, 14),
#'   BO_sstmin = c(9, 6, 10),
#'   BO_sstmax = c(14, 16, 18)
#' )
#'
#' env_layers <- c("BO_sstmean", "BO_sstmin", "BO_sstmax")
#' res <- ec_flag_outlier(data,
#'   latitude = "decimalLatitude",
#'   longitude = "decimalLongitude",
#'   env_layers,
#'   itr = 100,
#'   k = 3,
#'   geo_quantile = 0.99,
#'   maha_quantile = 0.99
#' )
#' data$outlier <- res$outlier
#' iteration_list <- res$result$list
#'
ec_flag_outlier <- function(data,
                            latitude = "decimalLatitude",
                            longitude = "decimalLongitude",
                            env_layers,
                            itr = 50,
                            k = 3,
                            geo_quantile = 0.99,
                            maha_quantile = 0.99) {
  outliers <- 0
  result_list <- distance_calc(data, latitude = latitude, longitude = longitude, env_layers, itr, k = 3)
  result_list <- result_list[!sapply(result_list, is.null)]
  result_list <- result_list[!sapply(result_list, function(p) anyNA(p$maha_distance))]


  for (j in seq_along(result_list)) {
    maha_thresh <- quantile(result_list[[j]]$maha_distance, geo_quantile, na.rm = TRUE)
    geo_thresh <- quantile(result_list[[j]]$geo_distance, maha_quantile, na.rm = TRUE)

    result_list[[j]]$outliers <- ifelse(result_list[[j]]$geo_distance < geo_thresh & result_list[[j]]$maha_distance < maha_thresh, 0, 1)
    outliers <- outliers + result_list[[j]]$outliers
  }
  outliers <- outliers / length(result_list)
  return(list(outliers = outliers, result_list = result_list))
}
