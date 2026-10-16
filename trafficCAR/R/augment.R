#' Summarize draws into mean and equal-tail interval
#' @param draws numeric matrix S x n (S draws, n locations) OR numeric vector length S for scalar
#' @param probs length-2 numeric in (0,1), e.g. c(0.025, 0.975)
#' @return list(mean=..., lo=..., hi=...)
#' @keywords internal
.summarize_draws <- function(draws, probs = c(0.025, 0.975)) {
  if (is.vector(draws)) {
    m <- mean(draws)
    qs <- stats::quantile(draws, probs = probs, names = FALSE)
    return(list(mean = m, lo = qs[1], hi = qs[2]))
  }
  if (!is.matrix(draws)) stop("`draws` must be matrix or vector.")
  m <- colMeans(draws)
  qs <- apply(draws, 2, stats::quantile, probs = probs, names = FALSE)
  list(mean = m, lo = qs[1, ], hi = qs[2, ])
}






#' Extract draws from a base fit object (adapter)
#' EDIT FIELDS LATER
#'
#' Expected:
#' - x_draws: matrix S x n
#' - beta_draws: matrix S x p (optional; if absent we treat X beta = 0)
#' @keywords internal
.extract_gaussian_draws <- function(base_fit) {
  if (is.null(base_fit$draws) || !is.list(base_fit$draws)) {
    stop("Expected `fit$draws` to be a list with x/beta/sigma2 draws.")
  }

  x_draws <- base_fit$draws$x
  beta_draws <- base_fit$draws$beta
  sigma2_draws <- base_fit$draws$sigma2

  if (is.null(x_draws) || !is.matrix(x_draws)) {
    stop("Expected `fit$draws$x` to be a matrix (S x n).")
  }

  if (!is.null(beta_draws) && !is.matrix(beta_draws)) {
    stop("Expected `fit$draws$beta` to be a matrix (S x p) or NULL.")
  }

  if (is.null(sigma2_draws) || !(is.numeric(sigma2_draws) && is.vector(sigma2_draws))) {
    stop("Expected `fit$draws$sigma2` to be a numeric vector (length S).")
  }

  if (nrow(x_draws) != length(sigma2_draws)) {
    stop("Inconsistent draws: nrow(x) must equal length(sigma2).")
  }
  if (!is.null(beta_draws) && nrow(beta_draws) != nrow(x_draws)) {
    stop("Inconsistent draws: nrow(beta) must equal nrow(x).")
  }

  list(x = x_draws, beta = beta_draws, sigma2 = sigma2_draws)
}





#' Augment roads with predicted traffic quantities
#'
#' Adds predicted traffic outcomes (e.g. speed or volume) and
#' relative congestion measures to a road network.
#'
#' @param fit a `traffic_fit` object from `fit_traffic()`.
#' @param roads an sf object or data.frame with a segment id column.
#' @param probs length-2 numeric for equal-tail intervals.
#' @param keep_geometry logical; if FALSE drops sf geometry.
#'
#' @return roads with added columns:
#'   predicted_mean, predicted_lo, predicted_hi,
#'   relative_congestion
#' @export
augment_roads <- function(fit,
                          roads,
                          probs = c(0.025, 0.975),
                          keep_geometry = TRUE) {
  if (!inherits(fit, "traffic_fit"))
    stop("`fit` must be a `traffic_fit`.")
  if (!(is.data.frame(roads) || inherits(roads, "sf")))
    stop("`roads` must be a data.frame or sf object.")

  id_col <- fit$segment_id_col
  if (!id_col %in% names(roads))
    stop("roads is missing join column: ", id_col)

  # extract draws
  base_fit <- fit$fit
  draws <- .extract_gaussian_draws(base_fit)

  x_draws <- draws$x        # S x n
  beta_draws <- draws$beta
  S <- nrow(x_draws)
  n <- ncol(x_draws)

  if (length(fit$segment_id) != n)
    stop("Segment id length does not match number of road segments.")

  # linear predictor
  X <- fit$X
  if (!is.matrix(X) || nrow(X) != n)
    stop("`fit$X` is missing or has wrong dimension.")

  if (is.null(beta_draws)) {
    xb <- matrix(0, nrow = S, ncol = n)
  } else {
    if (ncol(beta_draws) != ncol(X))
      stop("beta draws do not match design matrix.")
    xb <- beta_draws %*% t(X)   # S x n
  }

  mu_draws <- xb + x_draws

  # summarize draws
  mu_sum <- .summarize_draws(mu_draws, probs = probs)

  # back-transform to data scale
  inv     <- fit$transform_meta$inv
  inv_int <- fit$transform_meta$inv_interval

  predicted_mean <- inv(mu_sum$mean)

  pred_lohi <- vapply(
    seq_len(n),
    function(i) inv_int(mu_sum$lo[i], mu_sum$hi[i]),
    numeric(2)
  )

  predicted_lo <- pred_lohi[1, ]
  predicted_hi <- pred_lohi[2, ]

  # relative congestion (standardized spatial deviation)
  spatial_mean <- colMeans(x_draws)
  relative_congestion <- spatial_mean / stats::sd(spatial_mean)

  aug <- data.frame(
    segment_id = fit$segment_id,
    predicted_mean = predicted_mean,
    predicted_lo   = predicted_lo,
    predicted_hi   = predicted_hi,
    relative_congestion = relative_congestion,
    stringsAsFactors = FALSE
  )
  names(aug)[1] <- id_col

  out <- merge(roads, aug, by = id_col, all.x = TRUE, sort = FALSE)

  if (!keep_geometry && inherits(out, "sf")) {
    out <- sf::st_drop_geometry(out)
  }

  out
}





#' Quick map helper for augmented roads
#'
#' Plots road geometries colored by an augmented numeric column (e.g.,
#' posterior mean predictions or relative congestion).
#'
#' @param roads_aug An `sf` object returned by [augment_roads()].
#' @param fill Character scalar. Which column of `roads_aug` to map.
#'   One of `"predicted_mean"` or `"relative_congestion"`.
#'
#' @return An invisible copy of `roads_aug`, returned as an `sf` object with the
#'   augmented columns. The function is called for its plotting side effect.
#' @export
plot_traffic_map <- function(
    roads_aug,
    fill = c("predicted_mean", "relative_congestion")
) {
  if (!inherits(roads_aug, "sf"))
    stop("`roads_aug` must be an sf object.")

  fill <- match.arg(fill)

  # base plotting to avoid hard ggplot2 dependency
  vals <- roads_aug[[fill]]
  if (!is.numeric(vals))
    stop("Selected fill column is not numeric.")

  op <- graphics::par(mar = c(0, 0, 0, 0))
  on.exit(graphics::par(op), add = TRUE)

  pal <- grDevices::colorRampPalette(
    if (fill == "relative_congestion")
      c("blue", "white", "red")
    else
      c("navy", "skyblue", "yellow", "orange", "red")
  )

  # simple continuous palette
  k <- 200
  cuts <- stats::quantile(
    vals,
    probs = seq(0, 1, length.out = k + 1),
    na.rm = TRUE,
    names = FALSE
  )
  idx <- findInterval(vals, cuts, all.inside = TRUE)
  cols <- pal(k)[idx]

  plot(sf::st_geometry(roads_aug),
       col = cols, lwd = 2, axes = FALSE)

  invisible(roads_aug)
}
