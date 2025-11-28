#' QTS K-Means Alignment Algorithm
#'
#' This function massages the input quaternion time series to feed them into the
#' k-means alignment algorithm for jointly clustering and aligning the input
#' QTS.
#'
#' @param x Either a numeric matrix of data, or an object that can be coerced to
#'   such a matrix (such as a numeric vector or a data frame with all numeric
#'   columns) or an object of class [qts_sample].
#' @param n_clusters An integer value specifying the number of clusters to be
#'   look for.
#' @param iter_max An integer value specifying the maximum number of iterations
#'   for terminating the k-mean algorithm. Defaults to `10L`.
#' @inheritParams stats::kmeans
#' @inheritParams fdacluster::fdakmeans
#'
#' @return An object of class [`stats::kmeans`] or [`stats::hclust`] or
#'   `dbscan_fast` if the input `x` is NOT of class [`qts_sample`]. Otherwise,
#'   an object of class `qtsclust` which is effectively a list with four
#'   components:
#' - `qts_aligned`: An object of class [`qts_sample`] storing the sample of
#' aligned QTS;
#' - `qts_centers`: A list of objects of class [`qts`] representing the centers
#' of the clusters;
#' - `best_clustering`: An object of class [`fdacluster::caps`] storing the
#' results of the best k-mean alignment result among all initialization that
#' were tried.
#' - `call_name`: A string storing the name of the function that was used to
#' produce the clustering structure;
#' - `call_args`: A list containing the exact arguments that were passed to
#' the function `call_name` that produced this output.
#'
#'
#' @export
#' @examples
#' out <- kmeans(vespa64$igp[1:10], n_clusters = 2)
kmeans <- function(x, n_clusters, ...) {
  UseMethod("kmeans")
}

#' @export
#' @rdname kmeans
kmeans.default <- function(x,
                           n_clusters = 1,
                           iter_max = 10,
                           nstart = 1,
                           algorithm =  c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
                           trace = FALSE,
                           ...) {
  stats::kmeans(
    x = x,
    centers = n_clusters,
    iter.max = iter_max,
    nstart = nstart,
    algorithm = algorithm,
    trace = trace
  )
}

#' @export
#' @rdname kmeans
kmeans.qts_sample <-function(x,
                             n_clusters = 1L,
                             seeds = NULL,
                             seeding_strategy = c("kmeans++", "exhaustive-kmeans++", "exhaustive", "hclust"),
                             is_domain_interval = FALSE,
                             transformation = c("identity", "srvf"),
                             warping_class = c("none", "shift", "dilation", "affine", "bpd"),
                             centroid_type = "mean",
                             metric = c("l2", "normalized_l2", "pearson"),
                             cluster_on_phase = FALSE,
                             use_fence = FALSE,
                             ...) {
  call <- rlang::call_match(defaults = TRUE)

  call_args <- rlang::call_args(call)

  seeding_strategy <- rlang::arg_match(seeding_strategy)
  call_args$seeding_strategy <- seeding_strategy

  transformation <- rlang::arg_match(transformation)
  call_args$transformation <- transformation

  warping_class <- rlang::arg_match(warping_class)
  call_args$warping_class <- warping_class

  metric <- rlang::arg_match(metric)
  call_args$metric <- metric

  l <- prep_data(x)

  out <- fdacluster::fdakmeans(
    x = l$grid,
    y = l$values,
    n_clusters = n_clusters,
    seeds = seeds,
    seeding_strategy = seeding_strategy,
    is_domain_interval = is_domain_interval,
    transformation = transformation,
    warping_class = warping_class,
    centroid_type = centroid_type,
    metric = metric,
    cluster_on_phase = cluster_on_phase,
    use_fence = use_fence
  )

  res <- list(
    qts_aligned = as_qts_sample(lapply(1:l$N, \(.id) {
      exp(as_qts(tibble::tibble(
        time = out$aligned_grids[.id, ],
        w    = 0,
        x    = out$original_curves[.id, 1, ],
        y    = out$original_curves[.id, 2, ],
        z    = out$original_curves[.id, 3, ]
      )))
    })),
    qts_centers = lapply(1:out$n_clusters, \(.id) {
      exp(as_qts(tibble::tibble(
        time = out$center_grids[.id, ],
        w    = 0,
        x    = out$center_curves[.id, 1, ],
        y    = out$center_curves[.id, 2, ],
        z    = out$center_curves[.id, 3, ]
      )))
    }),
    best_clustering = out,
    call_name = rlang::call_name(call),
    call_args = call_args
  )

  class(res) <- "qtsclust"
  res
}

#' Plot for `qtsclust` objects
#'
#' This function creates a visualization of the clustering results obtained on a
#' sample of QTS and returns the corresponding [ggplot2::ggplot] object which
#' enable further customization of the plot.
#'
#' @param object An object of class `qtsclust` as produced by
#'   [kmeans.qts_sample()] or [hclust.qts_sample()].
#' @param ... Further arguments to be passed to other methods.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @importFrom ggplot2 autoplot .data
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' out <- kmeans(vespa64$igp[1:10], n_clusters = 2)
#' ggplot2::autoplot(out)
autoplot.qtsclust <- function(object, ...) {
  data <- as_qts_sample(c(object$qts_centers, object$qts_aligned))
  n <- length(object$qts_aligned)
  k <- length(object$qts_centers)
  memb <- c(1:k, object$best_clustering$memberships)
  high <- c(rep(TRUE, k), rep(FALSE, n))
  fnm <- strsplit(object$call_name, split = '\\.')[[1]][1]
  wcn <- object$call_args$warping_class
  autoplot(data, memberships = memb, highlighted = high) +
    ggplot2::labs(
      title = cli::format_inline("Clustering structure obtained from {.fn {fnm}}"),
      subtitle = cli::format_inline("Using {k} cluster{?s} and {wcn} warping functions")
    )
}

#' Plot for `qtsclust` objects
#'
#' This function creates a visualization of the clustering results obtained on a
#' sample of QTS **without** returning the plot data as an object.
#'
#' @param x An object of class `qtsclust` as produced by [kmeans.qts_sample()]
#'   or [hclust.qts_sample()].
#' @inheritParams autoplot.qtsclust
#'
#' @return No return value, called for side effects.
#'
#' @importFrom graphics plot
#' @export
#' @examples
#' out <- kmeans(vespa64$igp[1:10], n_clusters = 2)
#' plot(out)
plot.qtsclust <- function(x, ...) {
  print(autoplot(x, ...))
}
