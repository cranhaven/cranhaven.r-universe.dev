#' Residuals for trafficCAR fits
#'
#' Computes raw, structured (spatial), or unstructured residuals
#' from a fitted trafficCAR model.
#'
#' @param object A `traffic_fit` object.
#' @param type Residual type: "raw", "structured", or "unstructured".
#' @param ... Unused.
#' @importFrom stats quantile
#' @importFrom stats sd
#' @method residuals traffic_fit
#'
#' @return Numeric vector of residuals.
#' @export
residuals.traffic_fit <- function(object,
                                  type = c("raw", "structured", "unstructured"),
                                  ...) {
  type <- match.arg(type)

  y  <- object$y
  mu <- object$mu
  x  <- object$x

  if (is.null(y) || is.null(mu)) {
    stop("`traffic_fit` must contain `y` and `mu`.")
  }

  if (!is.numeric(y) || !is.numeric(mu)) {
    stop("`y` and `mu` must be numeric.")
  }

  if (length(y) != length(mu)) {
    stop("`y` and `mu` must have the same length.")
  }

  if (type != "raw") {
    if (is.null(x)) {
      stop("Structured residuals require spatial effect `x`.")
    }
    if (!is.numeric(x) || length(x) != length(y)) {
      stop("`x` must be numeric and the same length as `y`.")
    }
  }

  r <- switch(
    type,
    raw = y - mu,
    structured = x,
    unstructured = y - (mu - x)
  )

  attr(r, "type") <- type
  attr(r, "n") <- length(r)
  class(r) <- c("traffic_residuals", "numeric")
  r
}




#' @method summary traffic_residuals
#' @export
summary.traffic_residuals <- function(object, ...) {
  c(
    mean = mean(object),
    sd   = sd(object),
    quantile(object, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  )
}
