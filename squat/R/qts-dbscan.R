#' QTS Nearest-Neighbor Clustering
#'
#' This function massages the input quaternion time series to apply DBSCAN
#' clustering on them, with the possibility of separating amplitude and phase
#' variability and of choosing the source of variability through which clusters
#' should be searched.
#'
#' @param x Either a numeric matrix of data, or an object that can be coerced to
#'   such a matrix (such as a numeric vector or a data frame with all numeric
#'   columns) or an object of class [qts_sample].
#' @param metric A character string specifying the distance measure to be used.
#'   This must be one of `"euclidean"`, `"maximum"`, `"manhattan"`,
#'   `"canberra"`, `"binary"` or `"minkowski"` if `x` is not a QTS sample.
#'   Otherwise, it must be one of `"l2"`, `"pearson"` or `"dtw"`.
#' @inheritParams dbscan::dbscan
#' @inheritParams fdacluster::fdadbscan
#'
#' @inherit kmeans return
#'
#' @export
#' @examples
#' out <- dbscan(vespa64$igp[1:10])
#' plot(out)
dbscan <- function(x, ...) {
  UseMethod("dbscan")
}

#' @export
#' @rdname dbscan
dbscan.default <- function(x,
                           eps,
                           minPts = 5,
                           weights = NULL,
                           borderPoints = TRUE,
                           ...) {
  dbscan::dbscan(
    x, eps,
    minPts = minPts,
    weights = weights,
    borderPoints = borderPoints,
    ...
  )
}

#' @export
#' @rdname dbscan
dbscan.qts_sample <-function(x,
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

  transformation <- rlang::arg_match(transformation)
  call_args$transformation <- transformation

  warping_class <- rlang::arg_match(warping_class)
  call_args$warping_class <- warping_class

  metric <- rlang::arg_match(metric)
  call_args$metric <- metric

  l <- prep_data(x)

  out <- fdacluster::fdadbscan(
    x = l$grid,
    y = l$values,
    is_domain_interval = is_domain_interval,
    transformation = transformation,
    warping_class = warping_class,
    centroid_type = centroid_type,
    metric = metric,
    cluster_on_phase = cluster_on_phase,
    use_verbose = FALSE,
    use_fence = use_fence
  )

  res <- list(
    qts_aligned = lapply(1:l$N, \(.id) {
      exp(as_qts(tibble::tibble(
        time = out$aligned_grids[.id, ],
        w    = 0,
        x    = out$original_curves[.id, 1, ],
        y    = out$original_curves[.id, 2, ],
        z    = out$original_curves[.id, 3, ]
      )))
    }),
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
