Mgyroreflection <- function(A, B, M, s) {
  circle <- circumcircle(A, B, Mgyromidpoint(A, B, s))
  inversion(circle, M)
}

Ugyroreflection <- function(A, B, M, s) {
  Am <- PhiMU(A, s)
  Bm <- PhiMU(B, s)
  Mm <- PhiMU(M, s)
  Pm <- Mgyroreflection(Am, Bm, Mm, s)
  PhiUM(Pm, s)
}

#' @title Gyroreflection
#' @description Gyroreflection of a point with respect to a gyroline in
#'   a 2D gyrospace.
#'
#' @encoding UTF-8
#'
#' @param A,B,M three 2D points
#' @param s the gyroparameter (radius of the Poincaré disk if \code{model="M"})
#' @param model the hyperbolic model, either \code{"M"} (Möbius model, i.e.
#'   Poincaré model) or \code{"U"} (Ungar model, i.e. Minkowski model)
#'
#' @return A 2D point, the image of \code{M} by the reflection with respect to
#'   the gyroline passing through \code{A} and \code{B}.
#' @export
#'
#' @examples
#' library(gyro)
#' A <- c(1.5, 2); B <- c(2, 1)
#' opar <- par(mar = c(2, 2, 2, 0.5))
#' plot(rbind(A, B), type = "p", pch = 19, xlab = NA, ylab = NA,
#'      xlim = c(0.3, 2), ylim = c(0, 2.5), main = "s = 0.3")
#' s <- 0.3
#' seg <- gyrosegment(A, B, s = s, model = "U")
#' lines(seg, col = "blue", lwd = 2)
#' text(t(A), expression(italic(A)), pos = 3)
#' text(t(B), expression(italic(B)), pos = 3)
#' M <- c(1.3, 1.1)
#' rM <- gyroreflection(A, B, M, s = s, model = "U")
#' points(rbind(M, rM), type = "p", pch = 19)
#' text(t(M), expression(italic(M)), pos = 3)
#' text(t(rM), expression(italic(r(M))), pos = 3)
#' par(opar)
gyroreflection <- function(A, B, M, s, model = "U") {
  model <- match.arg(model, c("M", "U"))
  stopifnot(is2dPoint(A), is2dPoint(B), is2dPoint(M))
  stopifnot(areDistinct(A, B))
  stopifnot(isPositiveNumber(s))
  if(model == "M") {
    if(dotprod(A) >= s*s || dotprod(B) >= s*s || dotprod(M) >= s*s) {
      stop(
        "In the M\u00f6bius gyrovector space, points must be ",
        "strictly inside the centered ball of radius `s`.",
        call. = TRUE
      )
    }
    Mgyroreflection(A, B, M, s)
  } else {
    Ugyroreflection(A, B, M, s)
  }
}
