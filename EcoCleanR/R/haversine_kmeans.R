#' Calculate Harversine distance
#'
#' @param data is a dataframe with spatial attributes - Latitude and Logitude
#' @param latitude nested imput from ec_flag_outlier
#' @param longitude nested imput from ec_flag_outlier
#' @param k is number of cluster required for the data set you have. Normally visual inspection can give a sense on number of clusters. Cautious to have more than expected clusters to fit all data points, as overfitting can end up inluding bad data points in the analysis. e.g. k = 3
#'
#' @return A data frame with centroid and clusters using Harversine distance matrix
#' @importFrom geosphere distm
#' @importFrom geosphere distHaversine
#'
#' @export
#'
#' @examples
#' data_x <- data.frame(
#'   scientificName = "Mexacanthina lugubris",
#'   decimalLongitude = c(-117, -117.8, -116.9),
#'   decimalLatitude = c(32.9, 33.5, 31.9),
#'   BO_sstmean = c(12, 13, 14),
#'   BO_sstmin = c(9, 6, 10),
#'   BO_sstmax = c(14, 16, 18)
#' )
#'
#' result <- haversine_kmeans(
#'   data_x,
#'   latitude = "decimalLatitude",
#'   longitude = "decimalLongitude",
#'   k = 3
#' )
#'
haversine_kmeans <- function(data, latitude, longitude, k) {
  # Step 1: Randomly select k initial centroids from data points
  initial_idx <- sample(1:nrow(data), k) # Ensure randomness
  centroids <- data[initial_idx, c(longitude, latitude)]

  # Step 2: Compute distances and assign clusters
  dist_matrix <- geosphere::distm(data[, c(longitude, latitude)], centroids, fun = geosphere::distHaversine)
  cluster_assignment <- apply(dist_matrix, 1, which.min) # Assign to nearest centroid

  # Step 3: Recompute centroids
  new_centroids <- do.call(rbind, lapply(1:k, function(i) {
    cluster_points <- data[cluster_assignment == i, c(longitude, latitude), drop = FALSE]
    if (nrow(cluster_points) > 0) colMeans(cluster_points) else centroids[i, ] # Prevent empty clusters
  }))

  # Return both clustered data and centroids
  data$cluster <- cluster_assignment
  return(list(clusters = data, centroids = as.data.frame(new_centroids)))
}
