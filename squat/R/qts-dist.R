#' QTS Distance Matrix Computation
#'
#' This function massages an input sample of quaternion time series to turn it
#' into a pairwise distance matrix.
#'
#' @param x A numeric matrix, data frame, [stats::dist] object or object of
#'   class [qts_sample] specifying the sample on which to compute the pairwise
#'   distance matrix.
#' @param metric A character string specifying the distance measure to be used.
#'   This must be one of `"euclidean"`, `"maximum"`, `"manhattan"`,
#'   `"canberra"`, `"binary"` or `"minkowski"` if `x` is not a QTS sample.
#'   Otherwise, it must be one of `"l2"`, `"pearson"` or `"dtw"`.
#' @inheritParams stats::dist
#' @inheritParams fdacluster::fdadist
#' @param ... not used.
#'
#' @return An object of class [stats::dist].
#'
#' @export
#' @examples
#' D <- dist(vespa64$igp[1:5])
dist <- function(x, metric, ...) {
  UseMethod("dist")
}

#' @export
#' @rdname dist
dist.default <- function(x,
                         metric = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                         diag = FALSE,
                         upper = FALSE,
                         p =  2,
                         ...) {
  metric <- match.arg(metric, choices = c(
    "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
  ))
  stats::dist(
    x = x,
    method = metric,
    diag = diag,
    upper = upper,
    p = p
  )
}

#' @export
#' @rdname dist
dist.qts_sample <-function(x,
                           metric = c("l2", "pearson", "dtw"),
                           warping_class = c("affine", "dilation", "none", "shift", "srsf"),
                           cluster_on_phase = FALSE,
                           labels = NULL,
                           ...) {
  metric <- match.arg(metric, choices = c(
    c("l2", "pearson", "dtw")
  ))

  if (metric == "dtw") {
    return(distDTW(
      qts_list = x,
      normalize_distance = TRUE,
      labels = labels,
      resample = FALSE,
      disable_normalization = TRUE,
      step_pattern = dtw::symmetric2
    ))
  }

  l <- prep_data(x)

  fdacluster::fdadist(
    x = l$grid,
    y = l$values,
    warping_class = warping_class,
    metric = metric,
    cluster_on_phase = cluster_on_phase,
    labels = labels
  )
}

distDTW <- function(qts_list,
                    normalize_distance = TRUE,
                    labels = NULL,
                    resample = TRUE,
                    disable_normalization = FALSE,
                    step_pattern = dtw::symmetric2) {
  if (!is_qts_sample(qts_list))
    cli::cli_abort("The input argument {.arg qts_list} should be of class {.cls qts_sample}. You can try {.fn as_qts_sample()}.")

  if (normalize_distance && is.na(attr(step_pattern, "norm")))
    stop("The provided step pattern is not normalizable.")

  if (!disable_normalization) {
    qts_list <- purrr::map(qts_list, normalize_qts)
  }

  if (resample) {
    qts_list <- purrr::map(qts_list, resample_qts, disable_normalization = TRUE)
  }

  n <- length(qts_list)
  if (is.null(labels))
    labels <- 1:n

  indices <- linear_index(n)

  .pairwise_distances <- function(linear_indices) {
    pb <- progressr::progressor(along = linear_indices)
    furrr::future_map_dbl(linear_indices, ~ {
      pb()
      i <- indices$i[.x]
      j <- indices$j[.x]
      dtw_data <- DTW(
        qts1 = qts_list[[i]],
        qts2 = qts_list[[j]],
        resample = FALSE,
        disable_normalization = TRUE,
        distance_only = TRUE,
        step_pattern = step_pattern
      )
      if (normalize_distance)
        dtw_data$normalizedDistance
      else
        dtw_data$distance
    }, .options = furrr::furrr_options(seed = TRUE))
  }

  d <- .pairwise_distances(indices$k)

  attributes(d) <- NULL
  attr(d, "Labels") <- labels
  attr(d, "Size") <- n
  attr(d, "call") <- match.call()
  class(d) <- "dist"
  d
}

linear_index <- function(n) {
  res <- tidyr::expand_grid(i = 1:n, j = 1:n)
  res <- subset(res, res$j > res$i)
  res$k <- n * (res$i - 1) - res$i * (res$i - 1) / 2 + res$j - res$i
  res
}
