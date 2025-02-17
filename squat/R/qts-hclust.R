#' QTS Hierarchical Agglomerative Clustering
#'
#' This function massages the input quaternion time series to apply hierarchical
#' agglomerative clustering on them, with the possibility of separating
#' amplitude and phase variability and of choosing the source of variability
#' through which clusters should be searched.
#'
#' @param x Either a numeric matrix of data, or an object that can be coerced to
#'   such a matrix (such as a numeric vector or a data frame with all numeric
#'   columns) or an object of class [qts_sample].
#' @param metric A character string specifying the distance measure to be used.
#'   This must be one of `"euclidean"`, `"maximum"`, `"manhattan"`,
#'   `"canberra"`, `"binary"` or `"minkowski"` if `x` is not a QTS sample.
#'   Otherwise, it must be one of `"l2"`, `"pearson"` or `"dtw"`.
#' @inheritParams stats::hclust
#' @inheritParams fdacluster::fdahclust
#'
#' @inherit kmeans return
#'
#' @export
#' @examples
#' out <- hclust(vespa64$igp[1:10], n_clusters = 2)
#' plot(out)
hclust <- function(x, metric, linkage_criterion, ...) {
  UseMethod("hclust")
}

#' @export
#' @rdname hclust
hclust.default <- function(x,
                           metric = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                           linkage_criterion = c("complete", "average", "single", "ward.D2"),
                           ...) {
  d <- dist(x, metric = metric)
  stats::hclust(d, method = linkage_criterion)
}

#' @export
#' @rdname hclust
hclust.qts_sample <-function(x,
                             metric = c("l2", "pearson"),
                             linkage_criterion = c("complete", "average", "single", "ward.D2"),
                             n_clusters = 1L,
                             warping_class = c("affine", "dilation", "none", "shift", "srsf"),
                             centroid_type = "mean",
                             cluster_on_phase = FALSE,
                             ...) {
  call <- rlang::call_match(defaults = TRUE)
  call_args <- rlang::call_args(call)
  metric <- rlang::arg_match(metric)
  linkage_criterion <- rlang::arg_match(linkage_criterion)
  warping_class <- rlang::arg_match(warping_class)
  call_args$metric <- metric
  call_args$linkage_criterion <- linkage_criterion
  call_args$warping_class <- warping_class

  l <- prep_data(x)

  out <- fdacluster::fdahclust(
    x = l$grid,
    y = l$values,
    n_clusters = n_clusters,
    warping_class = warping_class,
    centroid_type = centroid_type,
    metric = metric,
    linkage_criterion = linkage_criterion,
    cluster_on_phase = cluster_on_phase,
    use_verbose = FALSE
  )

  res <- list(
    qts_aligned = purrr::map(1:l$N, \(.id) {
      exp(as_qts(tibble::tibble(
        time = out$aligned_grids[.id, ],
        w    = 0,
        x    = out$original_curves[.id, 1, ],
        y    = out$original_curves[.id, 2, ],
        z    = out$original_curves[.id, 3, ]
      )))
    }),
    qts_centers = purrr::map(1:out$n_clusters, \(.id) {
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
