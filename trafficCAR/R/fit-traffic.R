#' Prepare speed outcome for Gaussian modeling
#'
#' Produces a transformed response and metadata needed to back-transform.
#' Default transform is log(speed).
#'
#' @param speed numeric vector (e.g., mph, km/h).
#' @param transform character; currently supports "log" or "identity".
#' @param eps small positive constant added before log to avoid log(0).
#' @return list with y (transformed), meta (transform info), and original scale label.
#' @keywords internal
prep_speed <- function(speed, transform = c("log", "identity"), eps = 1e-6) {
  transform <- match.arg(transform)
  if (!is.numeric(speed) || any(!is.finite(speed))) stop("`speed` must be finite numeric.")
  if (any(speed < 0)) stop("`speed` must be nonnegative.")
  if (!is.numeric(eps) || length(eps) != 1 || eps <= 0) stop("`eps` must be positive scalar.")

  if (transform == "log") {
    y <- log(speed + eps)
    meta <- list(
      outcome = "speed",
      transform = "log",
      eps = eps,
      # back-transform for means on link scale:
      inv = function(mu) pmax(exp(mu) - eps, 0),
      # back-transform for interval endpoints on link scale:
      inv_interval = function(lo, hi) c(pmax(exp(lo) - eps, 0), pmax(exp(hi) - eps, 0))
    )
  } else {
    y <- speed
    meta <- list(
      outcome = "speed",
      transform = "identity",
      eps = NA_real_,
      inv = function(mu) mu,
      inv_interval = function(lo, hi) c(lo, hi)
    )
  }

  list(y = y, meta = meta)
}





#' Prepare travel time outcome for Gaussian modeling
#'
#' Common choice is log(travel_time). If `distance` is provided, you can
#' optionally model time-per-distance.
#'
#' @param travel_time numeric vector (e.g., seconds).
#' @param distance optional numeric vector of same length (e.g., meters).
#' @param per_distance logical; if TRUE and distance provided, model travel_time / distance.
#' @param transform character; currently supports "log" or "identity".
#' @param eps small positive constant added before log to avoid log(0).
#' @return list with y (transformed), meta (transform info).
#' @keywords internal
prep_travel_time <- function(travel_time,
                             distance = NULL,
                             per_distance = FALSE,
                             transform = c("log", "identity"),
                             eps = 1e-6) {
  transform <- match.arg(transform)
  if (!is.numeric(travel_time) || any(!is.finite(travel_time))) stop("`travel_time` must be finite numeric.")
  if (any(travel_time < 0)) stop("`travel_time` must be nonnegative.")
  if (!is.numeric(eps) || length(eps) != 1 || eps <= 0) stop("`eps` must be positive scalar.")

  base <- travel_time
  scale_label <- "travel_time"
  if (!is.null(distance)) {
    if (!is.numeric(distance) || any(!is.finite(distance))) stop("`distance` must be finite numeric when provided.")
    if (any(distance <= 0)) stop("`distance` must be positive when provided.")
    if (isTRUE(per_distance)) {
      base <- travel_time / distance
      scale_label <- "travel_time_per_distance"
    }
  } else if (isTRUE(per_distance)) {
    stop("`per_distance = TRUE` requires `distance`.")
  }

  if (transform == "log") {
    y <- log(base + eps)
    meta <- list(
      outcome = "travel_time",
      base = scale_label,
      transform = "log",
      eps = eps,
      per_distance = isTRUE(per_distance),
      inv = function(mu) pmax(exp(mu) - eps, 0),
      inv_interval = function(lo, hi) c(pmax(exp(lo) - eps, 0), pmax(exp(hi) - eps, 0))
    )
  } else {
    y <- base
    meta <- list(
      outcome = "travel_time",
      base = scale_label,
      transform = "identity",
      eps = NA_real_,
      per_distance = isTRUE(per_distance),
      inv = function(mu) mu,
      inv_interval = function(lo, hi) c(lo, hi)
    )
  }

  list(y = y, meta = meta)
}




#' Fit a Gaussian CAR traffic model (speed or travel time)
#'
#' This is a thin wrapper around \code{fit_car()} that:
#' 1) preprocesses the outcome (log/per-distance options),
#' 2) fits the Gaussian CAR model,
#' 3) returns a traffic-flavored object that can be augmented back onto roads.
#'
#' @param data data.frame with at least `segment_id` and the outcome column.
#' @param roads optional; an sf object or similar that contains adjacency info already
#'   used by your `fit_car()` pipeline (depends on your package design).
#' @param A adjacency matrix or object accepted by `fit_car()` (recommended explicit).
#' @param segment_id_col character; column in `data` used to join back to roads.
#' @param outcome character; one of "speed" or "travel_time".
#' @param outcome_col optional character; if NULL uses `outcome`.
#' @param distance_col optional character; used only for travel_time when per_distance=TRUE.
#' @param per_distance logical; only for travel_time.
#' @param transform character; "log" or "identity".
#' @param X optional design matrix; if NULL, uses intercept-only.
#' @param ... passed to `fit_car()` (e.g., type="proper"/"icar", rho, priors, n_iter, burn, etc.)
#'
#' @return An object of class `traffic_fit` containing the underlying fit and transform metadata.
#' @export
fit_traffic <- function(data,
                        roads = NULL,
                        A = NULL,
                        segment_id_col = "segment_id",
                        outcome = c("speed", "travel_time"),
                        outcome_col = NULL,
                        distance_col = NULL,
                        per_distance = FALSE,
                        transform = c("log", "identity"),
                        X = NULL,
                        ...) {
  outcome <- match.arg(outcome)
  transform <- match.arg(transform)

  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  if (!segment_id_col %in% names(data)) stop("`segment_id_col` not found in `data`.")
  if (is.null(outcome_col)) outcome_col <- outcome
  if (!outcome_col %in% names(data)) stop("`outcome_col` not found in `data`.")

  seg_id <- data[[segment_id_col]]
  if (anyNA(seg_id)) stop("segment IDs contain NA.")
  # allow numeric/integer/character ids, but keep as-is for join keys
  n <- nrow(data)

  y_raw <- data[[outcome_col]]

  if (outcome == "speed") {
    prep <- prep_speed(y_raw, transform = transform)
  } else {
    dist <- NULL
    if (!is.null(distance_col)) {
      if (!distance_col %in% names(data)) stop("`distance_col` not found in `data`.")
      dist <- data[[distance_col]]
    }
    prep <- prep_travel_time(
      y_raw,
      distance = dist,
      per_distance = per_distance,
      transform = transform
    )
  }

  y <- prep$y
  meta <- prep$meta

  # default X: intercept-only
  if (is.null(X)) {
    X <- matrix(1, nrow = n, ncol = 1)
    colnames(X) <- "(Intercept)"
  } else {
    if (!is.matrix(X) || !is.numeric(X)) stop("`X` must be a numeric matrix.")
    if (nrow(X) != n) stop("`X` must have nrow(data) rows.")
  }

  # require explicit adjacency
  if (is.null(A)) {
    stop("Provide `A` (adjacency). `roads` inference not implemented in fit_traffic().")
  }

  # call existing fitter
  base_fit <- fit_car(
    y = y,
    A = A,
    X = X,
    ...
  )

  out <- list(
    fit = base_fit,
    X = X,
    segment_id = seg_id,
    segment_id_col = segment_id_col,
    outcome = outcome,
    outcome_col = outcome_col,
    transform_meta = meta,
    n = n
  )
  class(out) <- c("traffic_fit", class(out))
  out
}





#' @export
print.traffic_fit <- function(x, ...) {
  cat("<traffic_fit>\n")
  cat("  outcome: ", x$outcome, "\n", sep = "")
  cat("  transform: ", x$transform_meta$transform, "\n", sep = "")
  cat("  n: ", x$n, "\n", sep = "")
  invisible(x)
}

