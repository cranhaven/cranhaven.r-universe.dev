#' Calculate geographic distance and mahalanobis distance to estimate outlier probability of a data point
#'
#' @param data data table with spatial and environmental variables
#' @param latitude nested input from ec_flag_outlier
#' @param longitude nested input from ec_flag_outlier
#' @param env_layers header names of env variables. env_layers <- c("Temperature", "pH")
#' @param itr iteration to run the clustering 100 or 1000 times
#' @param k number of cluster to choose in each iteration
#'
#' @return A list of results that shows result of calculated distance for each iteration
#' @importFrom geosphere distm
#' @importFrom geosphere distHaversine
#' @importFrom stats cov
#' @importFrom stats mahalanobis
#'
#' @export
#'
#' @examples
#'
#' data <- data.frame(
#'   scientificName = "Mexacanthina lugubris",
#'   decimalLongitude = c(-117, -117.8, -116.9),
#'   decimalLatitude = c(32.9, 33.5, 31.9),
#'   temperature_mean = c(12, 13, 14),
#'   temperature_min = c(9, 6, 10),
#'   temperature_max = c(14, 16, 18)
#' )
#'
#' env_layers <- c("temperature_mean", "temperature_min", " temperature_max")
#'
#' result_list <- distance_calc(data,
#'   latitude = "decimalLatitude",
#'   longitude = "decimalLongitude",
#'   env_layers,
#'   itr = 100,
#'   k = 3
#' )
#'
distance_calc <- function(data, latitude, longitude, env_layers, itr = 15, k = 3) {
  result_list <- list()
  for (n in 1:itr) {
    result <- haversine_kmeans(data, latitude = latitude, longitude = longitude, k)
    result_df <- result$clusters
    centroid_df <- result$centroids
    temp_data <- data
    temp_data$cluster <- result_df
    temp_data$geo_distance <- NA
    temp_data$maha_distance <- NA
    cluster_counts <- table(temp_data$cluster$cluster)
    # Check if any cluster in this set has less than 3 points
    if (any(cluster_counts < 4)) {
      next # Skip this clustering set if any cluster has <3 points
    }
    for (i in 1:nrow(temp_data)) {
      cluster_id <- temp_data$cluster$cluster[i]
      # Ensure valid cluster_id
      if (!is.na(cluster_id) && cluster_id > 0 && cluster_id <= nrow(centroid_df)) {
        # Retrieve centroid coordinates
        centroid <- centroid_df[cluster_id, ]
        centroid_lon_lat <- as.numeric(c(centroid[[longitude]], centroid[[latitude]]))
        temp_data$geo_distance[i] <- distHaversine(
          c(temp_data[[longitude]][i], temp_data[[latitude]][i]), centroid_lon_lat
        ) / 1000
        # Get data points belonging to the same cluster
        cluster_data <- temp_data[temp_data$cluster$cluster == cluster_id, env_layers]
        cluster_mean <- colMeans(cluster_data, na.rm = TRUE)
        cov_matrix <- cov(cluster_data, use = "complete.obs")
        if (det(cov_matrix) < 1e-10) {
          temp_data$maha_distance[i] <- NA
          next # Move to the next i without computing mahalanobis distance
        }
        cov_inv <- solve(cov_matrix)

        # Ensure covariance matrix is invertible
        cluster_indices <- which(temp_data$cluster$cluster == cluster_id)
        relative_index <- which(cluster_indices == i)

        if (length(relative_index) > 0) {
          temp_data$maha_distance[i] <- mahalanobis(cluster_data[relative_index, ], cluster_mean, cov_inv)
        } else {
          temp_data$maha_distance[i] <- NA
        }
      }
    }
    # end of temp_data for 1 iter
    result_list[[n]] <- temp_data
  }
  return(result_list)
}
