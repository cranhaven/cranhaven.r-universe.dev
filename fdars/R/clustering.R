#' Clustering Functions for Functional Data
#'
#' Functions for clustering functional data, including k-means and related
#' algorithms.

#' Functional K-Means Clustering
#'
#' Performs k-means clustering on functional data using the specified metric.
#' Uses k-means++ initialization for better initial centers.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param ncl Number of clusters.
#' @param metric Either a string ("L2", "L1", "Linf") for fast Rust-based
#'   distance computation, or a metric/semimetric function (e.g., \code{metric.lp},
#'   \code{metric.hausdorff}, \code{semimetric.pca}). Using a function provides
#'   flexibility but may be slower for semimetrics computed in R.
#' @param max.iter Maximum number of iterations (default 100).
#' @param nstart Number of random starts (default 10). The best result
#'   (lowest within-cluster sum of squares) is returned.
#' @param seed Optional random seed for reproducibility.
#' @param draw Logical. If TRUE, plot the clustered curves (not yet implemented).
#' @param ... Additional arguments passed to the metric function.
#'
#' @return A list of class 'cluster.kmeans' with components:
#' \describe{
#'   \item{cluster}{Integer vector of cluster assignments (1 to ncl).}
#'   \item{centers}{An fdata object containing the cluster centers.}
#'   \item{withinss}{Within-cluster sum of squares for each cluster.}
#'   \item{tot.withinss}{Total within-cluster sum of squares.}
#'   \item{size}{Number of observations in each cluster.}
#'   \item{fdataobj}{The input functional data object.}
#' }
#'
#' @details
#' When \code{metric} is a string ("L2", "L1", "Linf"), the entire k-means
#' algorithm runs in Rust with parallel processing, providing 50-200x speedup.
#'
#' When \code{metric} is a function, distances are computed using that function.
#' Functions like \code{metric.lp}, \code{metric.hausdorff}, and \code{metric.DTW}
#' have Rust backends and remain fast. Semimetric functions (\code{semimetric.*})
#' are computed in R and will be slower for large datasets.
#'
#' @export
#' @examples
#' # Create functional data with two groups
#' t <- seq(0, 1, length.out = 50)
#' n <- 30
#' X <- matrix(0, n, 50)
#' true_cluster <- rep(1:2, each = 15)
#' for (i in 1:n) {
#'   if (true_cluster[i] == 1) {
#'     X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.1)
#'   } else {
#'     X[i, ] <- cos(2*pi*t) + rnorm(50, sd = 0.1)
#'   }
#' }
#' fd <- fdata(X, argvals = t)
#'
#' # Cluster with string metric (fast Rust path)
#' result <- cluster.kmeans(fd, ncl = 2, metric = "L2")
#' table(result$cluster, true_cluster)
#'
#' # Cluster with metric function (also fast - Rust backend)
#' result2 <- cluster.kmeans(fd, ncl = 2, metric = metric.lp)
#'
#' # Cluster with semimetric (flexible but slower)
#' result3 <- cluster.kmeans(fd, ncl = 2, metric = semimetric.pca, ncomp = 3)
cluster.kmeans <- function(fdataobj, ncl, metric = "L2",
                           max.iter = 100, nstart = 10, seed = NULL,
                           draw = FALSE, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("kmeans.fd for 2D functional data not yet implemented")
  }

  n <- nrow(fdataobj$data)

  if (ncl < 1 || ncl > n) {
    stop("ncl must be between 1 and the number of curves")
  }

  # Check if metric is a string or function
  if (is.character(metric)) {
    metric <- match.arg(metric, c("L2", "L1", "Linf"))
    result <- .kmeans_fd_rust(fdataobj, ncl, metric, max.iter, nstart, seed)
  } else if (is.function(metric)) {
    result <- .kmeans_fd_metric(fdataobj, ncl, metric, max.iter, nstart, seed, ...)
  } else {
    stop("metric must be a string ('L2', 'L1', 'Linf') or a metric/semimetric function")
  }

  # Create fdata object for centers
  centers <- fdata(result$centers, argvals = fdataobj$argvals,
                   names = list(main = "Cluster Centers",
                                xlab = fdataobj$names$xlab,
                                ylab = fdataobj$names$ylab))

  structure(
    list(
      cluster = result$cluster,
      centers = centers,
      withinss = result$withinss,
      tot.withinss = result$tot_withinss,
      size = result$size,
      fdataobj = fdataobj
    ),
    class = "cluster.kmeans"
  )
}

#' Internal: K-means using Rust backend (string metrics)
#' @noRd
.kmeans_fd_rust <- function(fdataobj, ncl, metric, max.iter, nstart, seed) {
  seed_val <- if (!is.null(seed)) as.integer(seed) else NULL

  .Call("wrap__kmeans_fd", fdataobj$data,
        as.numeric(fdataobj$argvals),
        as.integer(ncl), as.integer(max.iter),
        as.integer(nstart), metric, seed_val)
}

#' Internal: K-means using metric/semimetric function
#' @noRd
.kmeans_fd_metric <- function(fdataobj, ncl, metric_func, max.iter, nstart, seed, ...) {
  if (!is.null(seed)) set.seed(seed)

  n <- nrow(fdataobj$data)
  m <- ncol(fdataobj$data)

  best_result <- NULL
  best_tot_withinss <- Inf

  for (start in seq_len(nstart)) {
    # K-means++ initialization
    centers_idx <- .kmeans_pp_init(fdataobj, ncl, metric_func, ...)

    centers <- fdataobj$data[centers_idx, , drop = FALSE]
    cluster <- integer(n)

    for (iter in seq_len(max.iter)) {
      old_cluster <- cluster

      # Assignment step: assign each curve to nearest center
      centers_fd <- fdata(centers, argvals = fdataobj$argvals)
      dist_to_centers <- metric_func(fdataobj, centers_fd, ...)

      cluster <- apply(dist_to_centers, 1, which.min)

      # Check convergence
      if (identical(cluster, old_cluster)) break

      # Update step: recompute centers
      for (k in seq_len(ncl)) {
        members <- which(cluster == k)
        if (length(members) > 0) {
          centers[k, ] <- colMeans(fdataobj$data[members, , drop = FALSE])
        }
      }
    }

    # Compute within-cluster sum of squares
    withinss <- numeric(ncl)
    for (k in seq_len(ncl)) {
      members <- which(cluster == k)
      if (length(members) > 0) {
        center_fd <- fdata(matrix(centers[k, ], nrow = 1), argvals = fdataobj$argvals)
        members_fd <- fdataobj[members, ]
        dists <- metric_func(members_fd, center_fd, ...)
        withinss[k] <- sum(dists^2)
      }
    }
    tot_withinss <- sum(withinss)

    # Keep best result
    if (tot_withinss < best_tot_withinss) {
      best_tot_withinss <- tot_withinss
      best_result <- list(
        cluster = cluster,
        centers = centers,
        withinss = withinss,
        tot_withinss = tot_withinss,
        size = tabulate(cluster, ncl)
      )
    }
  }

  best_result
}

#' Internal: K-means++ initialization with metric function
#' @noRd
.kmeans_pp_init <- function(fdataobj, ncl, metric_func, ...) {
  n <- nrow(fdataobj$data)
  centers_idx <- integer(ncl)

  # First center: random
  centers_idx[1] <- sample(n, 1)

  for (k in 2:ncl) {
    # Compute distances to nearest existing center
    current_centers <- fdataobj[centers_idx[1:(k-1)], ]
    dists <- metric_func(fdataobj, current_centers, ...)

    # Min distance to any center
    min_dists <- apply(dists, 1, min)

    # Probability proportional to D^2
    probs <- min_dists^2
    probs[centers_idx[1:(k-1)]] <- 0
    probs <- probs / sum(probs)

    centers_idx[k] <- sample(n, 1, prob = probs)
  }

  centers_idx
}

#' K-Means++ Center Initialization
#'
#' Initialize cluster centers using the k-means++ algorithm, which selects
#' centers with probability proportional to squared distance from existing
#' centers.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param ncl Number of clusters.
#' @param metric Metric to use. One of "L2", "L1", or "Linf".
#' @param seed Optional random seed.
#'
#' @return An fdata object containing the initial cluster centers.
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(rnorm(30 * 50), 30, 50)
#' fd <- fdata(X, argvals = t)
#' init_centers <- cluster.init(fd, ncl = 3)
cluster.init <- function(fdataobj, ncl, metric = "L2", seed = NULL) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  n <- nrow(fdataobj$data)

  if (ncl < 1 || ncl > n) {
    stop("ncl must be between 1 and the number of curves")
  }

  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)

  # Compute distance matrix
  if (metric == "L2") {
    dist_mat <- as.matrix(metric.lp(fdataobj))
  } else if (metric == "L1") {
    dist_mat <- as.matrix(metric.lp(fdataobj, p = 1))
  } else {
    dist_mat <- as.matrix(metric.lp(fdataobj, p = Inf))
  }

  # K-means++ initialization
  centers_idx <- integer(ncl)

  # First center: random
  centers_idx[1] <- sample(n, 1)

  # Remaining centers: probability proportional to D^2
  for (k in 2:ncl) {
    # Min distance to current centers
    min_dists <- apply(dist_mat[, centers_idx[1:(k-1)], drop = FALSE], 1, min)

    # Square and normalize
    probs <- min_dists^2
    probs[centers_idx[1:(k-1)]] <- 0  # Don't reselect existing centers
    probs <- probs / sum(probs)

    # Sample next center
    centers_idx[k] <- sample(n, 1, prob = probs)
  }

  # Return centers as fdata
  fdataobj[centers_idx, ]
}

#' Optimal Number of Clusters for Functional K-Means
#'
#' Determines the optimal number of clusters for functional k-means clustering
#' using various criteria: elbow method, silhouette score, or Calinski-Harabasz index.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param ncl.range Range of number of clusters to evaluate. Default is 2:10.
#' @param criterion Criterion to use for selecting optimal k:
#'   \describe{
#'     \item{"silhouette"}{Mean silhouette coefficient (default). Higher is better.}
#'     \item{"CH"}{Calinski-Harabasz index. Higher is better.}
#'     \item{"elbow"}{Within-cluster sum of squares. Look for elbow in plot.}
#'   }
#' @param metric Either a string ("L2", "L1", "Linf") or a metric function.
#' @param max.iter Maximum iterations for k-means (default 100).
#' @param nstart Number of random starts (default 10).
#' @param seed Random seed for reproducibility.
#' @param ... Additional arguments passed to cluster.kmeans.
#'
#' @return A list of class 'cluster.optim' with components:
#' \describe{
#'   \item{optimal.k}{Optimal number of clusters based on criterion}
#'   \item{criterion}{Name of criterion used}
#'   \item{scores}{Vector of criterion values for each k}
#'   \item{ncl.range}{Range of k values tested}
#'   \item{models}{List of cluster.kmeans objects for each k}
#'   \item{best.model}{The cluster.kmeans object for optimal k}
#' }
#'
#' @details
#' \strong{Silhouette score}: Measures how similar each curve is to its own cluster
#' compared to other clusters. Values range from -1 to 1, with higher being better.
#' Optimal k maximizes the mean silhouette.
#'
#' \strong{Calinski-Harabasz index}: Ratio of between-cluster to within-cluster
#' dispersion. Higher values indicate better defined clusters. Optimal k maximizes CH.
#'
#' \strong{Elbow method}: Plots total within-cluster sum of squares vs k. The optimal
#' k is at the "elbow" where adding more clusters doesn't significantly reduce WSS.
#' This is subjective and best assessed visually using `plot()`.
#'
#' @export
#' @examples
#' # Create functional data with 3 groups
#' set.seed(42)
#' t <- seq(0, 1, length.out = 50)
#' n <- 60
#' X <- matrix(0, n, 50)
#' true_k <- rep(1:3, each = 20)
#' for (i in 1:n) {
#'   if (true_k[i] == 1) {
#'     X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.1)
#'   } else if (true_k[i] == 2) {
#'     X[i, ] <- cos(2*pi*t) + rnorm(50, sd = 0.1)
#'   } else {
#'     X[i, ] <- sin(4*pi*t) + rnorm(50, sd = 0.1)
#'   }
#' }
#' fd <- fdata(X, argvals = t)
#'
#' # Find optimal k using silhouette
#' opt <- cluster.optim(fd, ncl.range = 2:6, criterion = "silhouette")
#' print(opt)
#' plot(opt)
cluster.optim <- function(fdataobj, ncl.range = 2:10,
                            criterion = c("silhouette", "CH", "elbow"),
                            metric = "L2", max.iter = 100, nstart = 10,
                            seed = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  criterion <- match.arg(criterion)
  n <- nrow(fdataobj$data)

  # Validate ncl.range
  ncl.range <- ncl.range[ncl.range >= 2 & ncl.range < n]
  if (length(ncl.range) == 0) {
    stop("ncl.range must contain values between 2 and n-1")
  }

  # Compute distance matrix once for silhouette calculation
  if (is.character(metric)) {
    if (metric == "L2") {
      dist_mat <- as.matrix(metric.lp(fdataobj))
    } else if (metric == "L1") {
      dist_mat <- as.matrix(metric.lp(fdataobj, p = 1))
    } else {
      dist_mat <- as.matrix(metric.lp(fdataobj, p = Inf))
    }
  } else {
    dist_mat <- as.matrix(metric(fdataobj, ...))
  }

  # Run k-means for each k and compute criterion
  models <- list()
  scores <- numeric(length(ncl.range))

  for (i in seq_along(ncl.range)) {
    k <- ncl.range[i]

    # Run k-means
    model <- cluster.kmeans(fdataobj, ncl = k, metric = metric,
                            max.iter = max.iter, nstart = nstart, seed = seed, ...)
    models[[i]] <- model

    # Compute criterion
    if (criterion == "silhouette") {
      scores[i] <- .Call("wrap__silhouette_score", dist_mat, as.integer(model$cluster))
    } else if (criterion == "CH") {
      scores[i] <- .Call("wrap__calinski_harabasz", fdataobj$data, as.integer(model$cluster))
    } else {  # elbow
      scores[i] <- model$tot.withinss
    }
  }

  names(scores) <- ncl.range

  # Find optimal k
  if (criterion == "elbow") {
    # For elbow, we don't automatically select - use second derivative
    # or let user decide from plot
    if (length(ncl.range) >= 3) {
      # Compute second derivative to find elbow
      d1 <- diff(scores)
      d2 <- diff(d1)
      optimal_idx <- which.max(d2) + 1
    } else {
      optimal_idx <- 1
    }
  } else {
    # For silhouette and CH, higher is better
    optimal_idx <- which.max(scores)
  }

  optimal_k <- ncl.range[optimal_idx]

  structure(
    list(
      optimal.k = optimal_k,
      criterion = criterion,
      scores = scores,
      ncl.range = ncl.range,
      models = models,
      best.model = models[[optimal_idx]]
    ),
    class = "cluster.optim"
  )
}

#' Print Method for cluster.optim Objects
#'
#' @param x An object of class 'cluster.optim'.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.cluster.optim <- function(x, ...) {
  cat("Optimal K-Means Clustering\n")
  cat("==========================\n")
  cat("Criterion:", x$criterion, "\n")
  cat("K range tested:", min(x$ncl.range), "-", max(x$ncl.range), "\n")
  cat("Optimal k:", x$optimal.k, "\n\n")

  cat("Scores by k:\n")
  score_df <- data.frame(k = x$ncl.range, score = round(x$scores, 4))
  print(score_df, row.names = FALSE)

  invisible(x)
}

#' Plot Method for cluster.optim Objects
#'
#' @param x An object of class 'cluster.optim'.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A ggplot object.
#'
#' @export
plot.cluster.optim <- function(x, ...) {
  ylab <- switch(x$criterion,
    silhouette = "Mean Silhouette Score",
    CH = "Calinski-Harabasz Index",
    elbow = "Total Within-Cluster SS"
  )

  df <- data.frame(
    k = x$ncl.range,
    score = x$scores
  )

  optimal_score <- x$scores[as.character(x$optimal.k)]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$k, y = .data$score)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_vline(xintercept = x$optimal.k, linetype = "dashed",
                        color = "red", linewidth = 0.8) +
    ggplot2::geom_point(
      data = data.frame(k = x$optimal.k, score = optimal_score),
      ggplot2::aes(x = .data$k, y = .data$score),
      color = "red", size = 5
    ) +
    ggplot2::labs(
      x = "Number of clusters (k)",
      y = ylab,
      title = paste("Optimal k Selection:", x$criterion)
    ) +
    ggplot2::scale_x_continuous(breaks = x$ncl.range)

  p
}

#' Print Method for cluster.kmeans Objects
#'
#' @param x An object of class 'cluster.kmeans'.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.cluster.kmeans <- function(x, ...) {
  cat("Functional K-Means Clustering\n")
  cat("=============================\n")
  cat("Number of clusters:", length(x$size), "\n")
  cat("Number of observations:", sum(x$size), "\n\n")

  cat("Cluster sizes:\n")
  print(x$size)

  cat("\nWithin-cluster sum of squares:\n")
  print(round(x$withinss, 4))

  cat("\nTotal within-cluster SS:", round(x$tot.withinss, 4), "\n")

  invisible(x)
}

#' Plot Method for cluster.kmeans Objects
#'
#' @param x An object of class 'cluster.kmeans'.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A ggplot object.
#'
#' @export
plot.cluster.kmeans <- function(x, ...) {
  fd <- x$fdataobj
  ncl <- length(x$size)
  n <- nrow(fd$data)
  m <- ncol(fd$data)

  # Reshape curves to long format
  df_curves <- data.frame(
    curve_id = rep(seq_len(n), each = m),
    argval = rep(fd$argvals, n),
    value = as.vector(t(fd$data)),
    cluster = factor(rep(x$cluster, each = m))
  )

  # Reshape centers to long format
  df_centers <- data.frame(
    center_id = rep(seq_len(ncl), each = m),
    argval = rep(fd$argvals, ncl),
    value = as.vector(t(x$centers$data)),
    cluster = factor(rep(seq_len(ncl), each = m))
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = df_curves,
      ggplot2::aes(x = .data$argval, y = .data$value,
                   group = .data$curve_id, color = .data$cluster),
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      data = df_centers,
      ggplot2::aes(x = .data$argval, y = .data$value,
                   group = .data$center_id, color = .data$cluster),
      linetype = "dashed", linewidth = 1.2
    ) +
    ggplot2::labs(
      x = fd$names$xlab %||% "t",
      y = fd$names$ylab %||% "X(t)",
      title = "Functional K-Means Clustering",
      color = "Cluster"
    )

  p
}

#' Fuzzy C-Means Clustering for Functional Data
#'
#' Performs fuzzy c-means clustering on functional data, where each curve has
#' a membership degree to each cluster rather than a hard assignment.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param ncl Number of clusters.
#' @param m Fuzziness parameter (default 2). Must be > 1. Higher values give
#'   softer cluster boundaries.
#' @param max.iter Maximum number of iterations (default 100).
#' @param tol Convergence tolerance (default 1e-6).
#' @param seed Optional random seed for reproducibility.
#'
#' @return A list of class 'fuzzycmeans.fd' with components:
#' \describe{
#'   \item{membership}{Matrix of membership degrees (n x ncl). Each row sums to 1.}
#'   \item{cluster}{Hard cluster assignments (argmax of membership).}
#'   \item{centers}{An fdata object containing the cluster centers.}
#'   \item{objective}{Final value of the objective function.}
#'   \item{fdataobj}{The input functional data object.}
#' }
#'
#' @details
#' Fuzzy c-means minimizes the objective function:
#' \deqn{J = \sum_{i=1}^n \sum_{c=1}^k u_{ic}^m ||X_i - v_c||^2}
#' where u_ic is the membership of curve i in cluster c, v_c is the cluster
#' center, and m is the fuzziness parameter.
#'
#' The membership degrees are updated as:
#' \deqn{u_{ic} = 1 / \sum_{j=1}^k (d_{ic}/d_{ij})^{2/(m-1)}}
#'
#' When m approaches 1, FCM becomes equivalent to hard k-means.
#' As m increases, the clusters become softer (more overlap).
#' m = 2 is the most common choice.
#'
#' @seealso \code{\link{cluster.kmeans}} for hard clustering
#'
#' @export
#' @examples
#' # Create functional data with THREE groups - one genuinely overlapping
#' set.seed(42)
#' t <- seq(0, 1, length.out = 50)
#' n <- 45
#' X <- matrix(0, n, 50)
#'
#' # Group 1: Sine waves centered at 0
#' for (i in 1:15) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.2)
#' # Group 2: Sine waves centered at 1.5 (clearly separated from group 1)
#' for (i in 16:30) X[i, ] <- sin(2*pi*t) + 1.5 + rnorm(50, sd = 0.2)
#' # Group 3: Between groups 1 and 2 (true overlap - ambiguous membership)
#' for (i in 31:45) X[i, ] <- sin(2*pi*t) + 0.75 + rnorm(50, sd = 0.3)
#'
#' fd <- fdata(X, argvals = t)
#'
#' # Fuzzy clustering reveals the overlap
#' fcm <- cluster.fcm(fd, ncl = 3, seed = 123)
#'
#' # Curves in group 3 (31-45) have split membership - this is the key benefit!
#' cat("Membership for curves 31-35 (overlap region):\n")
#' print(round(fcm$membership[31:35, ], 2))
#'
#' # Compare to hard clustering which forces a decision
#' km <- cluster.kmeans(fd, ncl = 3, seed = 123)
#' cat("\nHard vs Fuzzy assignment for curve 35:\n")
#' cat("K-means cluster:", km$cluster[35], "\n")
#' cat("FCM memberships:", round(fcm$membership[35, ], 2), "\n")
cluster.fcm <- function(fdataobj, ncl, m = 2, max.iter = 100, tol = 1e-6, seed = NULL) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("cluster.fcm for 2D functional data not yet implemented")
  }

  n <- nrow(fdataobj$data)

  if (ncl < 1 || ncl > n) {
    stop("ncl must be between 1 and the number of curves")
  }

  if (m <= 1) {
    stop("m (fuzziness) must be > 1")
  }

  seed_val <- if (!is.null(seed)) as.integer(seed) else NULL

  result <- .Call("wrap__fuzzycmeans_fd", fdataobj$data,
                  as.numeric(fdataobj$argvals), as.integer(ncl),
                  as.numeric(m), as.integer(max.iter), as.numeric(tol), seed_val)

  # Create fdata object for centers
  centers <- fdata(result$centers, argvals = fdataobj$argvals,
                   names = list(main = "Cluster Centers",
                                xlab = fdataobj$names$xlab,
                                ylab = fdataobj$names$ylab))

  structure(
    list(
      membership = result$membership,
      cluster = result$cluster,
      centers = centers,
      objective = result$objective,
      m = m,
      fdataobj = fdataobj
    ),
    class = "cluster.fcm"
  )
}

#' Print Method for cluster.fcm Objects
#'
#' @param x An object of class 'cluster.fcm'.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.cluster.fcm <- function(x, ...) {
  cat("Fuzzy C-Means Clustering\n")
  cat("========================\n")
  cat("Number of clusters:", ncol(x$membership), "\n")
  cat("Number of observations:", nrow(x$membership), "\n")
  cat("Fuzziness parameter m:", x$m, "\n\n")

  cat("Cluster sizes (hard assignment):\n")
  print(table(x$cluster))

  cat("\nObjective function:", round(x$objective, 4), "\n")

  # Show average membership
  cat("\nAverage membership per cluster:\n")
  avg_mem <- colMeans(x$membership)
  names(avg_mem) <- paste0("C", seq_along(avg_mem))
  print(round(avg_mem, 3))

  invisible(x)
}

#' Plot Method for cluster.fcm Objects
#'
#' @param x An object of class 'cluster.fcm'.
#' @param type Type of plot: "curves" (default) or "membership".
#' @param ... Additional arguments (currently ignored).
#'
#' @return A ggplot object.
#'
#' @export
plot.cluster.fcm <- function(x, type = c("curves", "membership"), ...) {
  type <- match.arg(type)

  if (type == "curves") {
    .plot_fuzzycmeans_curves(x)
  } else {
    .plot_fuzzycmeans_membership(x)
  }
}

#' @noRd
.plot_fuzzycmeans_curves <- function(x) {
  fd <- x$fdataobj
  ncl <- ncol(x$membership)
  n <- nrow(fd$data)
  m <- ncol(fd$data)

  # Reshape curves to long format
  df_curves <- data.frame(
    curve_id = rep(seq_len(n), each = m),
    argval = rep(fd$argvals, n),
    value = as.vector(t(fd$data)),
    cluster = factor(rep(x$cluster, each = m))
  )

  # Reshape centers to long format
  df_centers <- data.frame(
    center_id = rep(seq_len(ncl), each = m),
    argval = rep(fd$argvals, ncl),
    value = as.vector(t(x$centers$data)),
    cluster = factor(rep(seq_len(ncl), each = m))
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = df_curves,
      ggplot2::aes(x = .data$argval, y = .data$value,
                   group = .data$curve_id, color = .data$cluster),
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      data = df_centers,
      ggplot2::aes(x = .data$argval, y = .data$value,
                   group = .data$center_id, color = .data$cluster),
      linetype = "dashed", linewidth = 1.2
    ) +
    ggplot2::labs(
      x = fd$names$xlab %||% "t",
      y = fd$names$ylab %||% "X(t)",
      title = "Fuzzy C-Means Clustering",
      color = "Cluster"
    )

  p
}

#' @noRd
.plot_fuzzycmeans_membership <- function(x) {
  n <- nrow(x$membership)
  ncl <- ncol(x$membership)

  # Create long format data
  df <- data.frame(
    curve = rep(seq_len(n), ncl),
    cluster = factor(rep(seq_len(ncl), each = n)),
    membership = as.vector(x$membership)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$curve, y = .data$membership,
                                         fill = .data$cluster)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::labs(
      x = "Curve Index",
      y = "Membership Degree",
      title = "Fuzzy Cluster Membership",
      fill = "Cluster"
    )

  p
}
