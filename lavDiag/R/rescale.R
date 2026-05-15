#' Linearly rescale numeric vector to given endpoints
#' @param x Numeric vector.
#' @param endpoints Length-2 numeric, target range (default c(0,1)).
#' @return Numeric vector on the new scale; NAs preserved.
#' @noRd
#' @keywords internal
.rescale <- function(x, endpoints = c(0, 1)) {
  rng <- range(x, finite = TRUE)
  d_in  <- rng[2] - rng[1]
  d_out <- endpoints[2] - endpoints[1]

  if (!is.finite(d_in) || d_in == 0) {
    # Constant or all non-finite: return mid-point of endpoints for finite x
    mid <- endpoints[1] + d_out / 2
    out <- rep(NA_real_, length(x))
    out[is.finite(x)] <- mid
    return(out)
  }

  (x - rng[1]) / d_in * d_out + endpoints[1]
}
