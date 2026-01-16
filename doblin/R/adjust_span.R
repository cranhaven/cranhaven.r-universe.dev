#' Recursively adjust the LOESS span to ensure a stable fit
#'
#' This function attempts to fit a LOESS (Locally Estimated Scatterplot Smoothing)
#' curve to the provided data using a span parameter that controls the degree of smoothing.
#' If the initial fit produces NA values in key components of the model object,
#' the function recursively increases the span until a stable fit is achieved or
#' the span exceeds 1.
#'
#' This approach avoids requiring the user to manually tune the span, instead
#' adapting to the data distribution and scale. The fit is performed on the
#' log-transformed y values to reduce skewness and avoid numerical instability.
#'
#' @param x A numeric vector representing the predictor variable (e.g., time or position).
#' @param y A numeric vector representing the response variable to be smoothed.
#' Must be non-negative due to log-transformation.
#' @param span A numeric value specifying the smoothing parameter for the LOESS fit.
#' Defaults to a low value (e.g., 0.2) and is increased recursively if the fit fails.
#'
#' @return A fitted loess object if a valid fit is achieved.
#' @export
#' @name adjust_span
#' 
#' @examples 
#' set.seed(123)
#' x <- seq(0, 10, length.out = 100)
#' y <- abs(sin(x)) + rnorm(100, sd = 0.1)
#'
#' # Fit using recursive span adjustment
#' fit <- adjust_span(x, y, span = 0.2)

adjust_span <- function(x, y, span) {
  # Check for NaN or Inf values in x and y
  if (any(is.nan(x)) || any(is.infinite(x)) || any(is.nan(y)) || any(is.infinite(y))) {
    stop("NaN or Inf values found in x or y (adjust_span()).")
  }
  
  fit <- suppressWarnings(stats::loess(log10(y + 1e-7) ~ x, span = span))
  
  # Check for NA values in key components of the loess object
  components <- c("enp", "s", "one.delta", "two.delta", "trace.hat",
                  "divisor", "robust", "pars", "kd")
  
  if (any(is.na(fit[components]))) {
    # Try increasing the span recursively if below threshold
    if (span < 1) {
      return(adjust_span(x, y, span + 0.1))
    } else {
      stop("Failed to adjust span. Consider changing the initial span value.")
    }
  } else {
    return(fit)
  }
}
