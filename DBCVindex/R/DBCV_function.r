#' Function to compute pairwise distances and ensure matrix format
#'
#' @param data input clustering results
#' @param metric metric of the distance, Euclidean by default
#' @export
#' @import Matrix
#' @return a pairwise distances' matrix
#' @examples
#'
#' n = 300; noise = 0.05; seed = 1782;
#' theta <- seq(0, pi, length.out = n / 2)
#'  x1 <- cos(theta) + rnorm(n / 2, sd = noise)
#'  y1 <- sin(theta) + rnorm(n / 2, sd = noise)
#'  x2 <- cos(theta + pi) + rnorm(n / 2, sd = noise)
#'  y2 <- sin(theta + pi) + rnorm(n / 2, sd = noise)
#'  X <- rbind(cbind(x1, y1), cbind(x2, y2))
#'
#' dist_matrix <- compute_pair_to_pair_dists(X)
compute_pair_to_pair_dists <- function(data, metric = "euclidean") {
  # Ensure the distance is returned as a matrix
  return(as.matrix(stats::dist(data, method = metric)))
}

#' Function to remove duplicate samples from the input data
#'
#' @param data input clustering results
#' @param labels labels of the clustering
#' @export
#' @import Matrix
#' @return a list of data and labels without duplicates
#' @examples
#'
#' n = 300; noise = 0.05; seed = 1782;
#' theta <- seq(0, pi, length.out = n / 2)
#'  x1 <- cos(theta) + rnorm(n / 2, sd = noise)
#'  y1 <- sin(theta) + rnorm(n / 2, sd = noise)
#'  x2 <- cos(theta + pi) + rnorm(n / 2, sd = noise)
#'  y2 <- sin(theta + pi) + rnorm(n / 2, sd = noise)
#'  X <- rbind(cbind(x1, y1), cbind(x2, y2))
#'  y <- c(rep(0, n / 2), rep(1, n / 2))
#'
#' cat("remove_duplicates(X, y) = ")
#' print(remove_duplicates(X, y))
remove_duplicates <- function(data, labels) {
  unique_data <- unique(data)
  unique_labels <- labels[!duplicated(data)]
  return(list(X = unique_data, y = unique_labels))
}


#' Function that calculates the Density-Based Clustering Validation index (DBCV) of clustering results
#'
#' @param data input clustering results
#' @param labels labels of the clustering
#' @param metric metric of the distance, Euclidean by default
#' @param noise_id the code of the noise cluster points, -1 by default
#' @export
#' @import Matrix
#' @return a real value containing the Saturn coefficient
#' @examples
#'
#' n = 300; noise = 0.05; seed = 1782;
#' theta <- seq(0, pi, length.out = n / 2)
#'  x1 <- cos(theta) + rnorm(n / 2, sd = noise)
#'  y1 <- sin(theta) + rnorm(n / 2, sd = noise)
#'  x2 <- cos(theta + pi) + rnorm(n / 2, sd = noise)
#'  y2 <- sin(theta + pi) + rnorm(n / 2, sd = noise)
#'  X <- rbind(cbind(x1, y1), cbind(x2, y2))
#'  y <- c(rep(0, n / 2), rep(1, n / 2))
#'
#' cat("dbcv(X, y) = ", dbcv(X, y), "\n", sep="")
dbcv <- function(data, labels, metric = "euclidean", noise_id = -1) {

  # Remove duplicates
  data <- remove_duplicates(data, labels)
  X <- data$X
  y <- data$y

  # Compute pairwise distances
  these_dists <- compute_pair_to_pair_dists(X, metric)

  # Initialize the score
  score <- 0.0
  unique_labels <- unique(y)

  # Compute DBCV score for each cluster
  for (label in unique_labels) {
    if (label == noise_id) next
    indices <- which(y == label)
    cluster_dists <- these_dists[indices, indices]
    score <- score + mean(cluster_dists)
  }

  # Return the normalized DBCV score
  return(score / length(unique_labels))
}
