circumcircle <- function(p1, p2, p3){
  x1 <- p1[1L]
  y1 <- p1[2L]
  x2 <- p2[1L]
  y2 <- p2[2L]
  x3 <- p3[1L]
  y3 <- p3[2L]
  a <- det(cbind(rbind(p1, p2, p3), 1))
  q1 <- dotprod(p1)
  q2 <- dotprod(p2)
  q3 <- dotprod(p3)
  q <- c(q1, q2, q3)
  x <- c(x1, x2, x3)
  y <- c(y1, y2, y3)
  Dx <- det(cbind(q, y, 1))
  Dy <- -det(cbind(q, x, 1))
  c <- det(cbind(q, x, y))
  center <- 0.5 * c(Dx, Dy) / a
  r <- sqrt(dotprod(center - p1))
  list("center" = center, "radius" = r)
}

inversion <- function(circle, M) {
  v <- M - circle[["center"]]
  circle[["center"]] + circle[["radius"]]^2 * v / dotprod(v)
}

.hreflection <- function(A, B, M){
  circle <- circumcircle(A, B, Mgyromidpoint(A, B, 1))
  inversion(circle, M)
}

#' @title Hyperbolic reflection
#' @description Hyperbolic reflection in the Poincaré disk.
#'
#' @encoding UTF-8
#'
#' @param A,B two points in the Poincaré disk defining the reflection line
#' @param M a point in the Poincaré disk to be reflected
#'
#' @return A point in the Poincaré disk, the image of \code{M} by the
#'   hyperbolic reflection with respect to the line passing through
#'   \code{A} and \code{B}.
#' @export
#'
#' @examples
#' library(gyro)
#' library(plotrix)
#' A <- c(0.45, 0.78)
#' B <- c(0.1, -0.5)
#' M <- c(0.7, 0)
#' opar <- par(mar = c(0, 0, 0, 0))
#' plot(NULL, type = "n", xlim = c(-1, 1), ylim = c(-1, 1), asp = 1,
#'      axes = FALSE, xlab = NA, ylab = NA)
#' draw.circle(0, 0, radius = 1, lwd = 2)
#' lines(gyrosegment(A, B, model = "M"))
#' points(rbind(A, B), pch = 19)
#' points(rbind(M), pch = 19, col = "blue")
#' P <- hreflection(A, B, M)
#' points(rbind(P), pch = 19, col = "red")
#' par(opar)
hreflection <- function(A, B, M){
  stopifnot(is2dPoint(A), is2dPoint(B), is2dPoint(M))
  stopifnot(dotprod(A) < 1, dotprod(B) < 1, dotprod(M) < 1)
  .hreflection(A, B, M)
}

sommets <- function(n, p){
  d <- sqrt(
    cos(pi/n + pi/p)*cos(pi/p) /
      (sin(2*pi/p) * sin(pi/n) + cos(pi/n + pi/p)* cos(pi/p))
  )
  zSommets <- c(d, vapply(1:(n-1), function(k){
    exp(1i*2*k*pi/n)*d}, complex(1L)
  ))
  vapply(zSommets, function(z) c(Re(z), Im(z)), numeric(2L))
}

#' @importFrom graphics polypath
#' @noRd
pavage <- function(
  triangle, symetrie, niveau, Centroids, i, n, Sommets, colors
){
  # if(dup <- anyDuplicated(round(Centroids, 6L))){
  #   Centroids <- Centroids[-dup, ]
  # }
  color <- ifelse(i == 1L, colors[1L], colors[2L])
  polypath(
    rbind(
      Mgyrosegment(triangle[, 1L], triangle[, 2L], s = 1, n = 50L)[-1L, ],
      Mgyrosegment(triangle[, 2L], triangle[, 3L], s = 1, n = 50L)[-1L, ],
      Mgyrosegment(triangle[, 3L], triangle[, 1L], s = 1, n = 50L)[-1L, ]
    ),
    col = color, border = NA
  )
  if(niveau > 0L){
    for(k in 1:n){
      if(k != symetrie){
        kp1 <- ifelse(k == n, 1L, k+1L)
        newtriangle <- vapply(1L:3L, function(j){
          .hreflection(Sommets[, k], Sommets[, kp1], triangle[, j])
        }, numeric(2L))
        Centroids <- rbind(
          Centroids,
          Mgyrocentroid(
            newtriangle[, 1L], newtriangle[, 2L], newtriangle[, 3L], s = 1
          )
        )
        pavage(newtriangle, k, niveau-1L, Centroids, -i, n, Sommets, colors)
      }
    }
  }
}

#' @title Hyperbolic tiling
#' @description Draw a hyperbolic tiling of the Poincaré disk.
#'
#' @encoding UTF-8
#'
#' @param n,p two positive integers satisfying \code{1/n + 1/p < 1/2}
#' @param depth positive integer, the number of recursions
#' @param colors two colors to fill the hyperbolic tiling
#' @param circle Boolean, whether to draw the unit circle
#' @param ... additional arguments passed to \code{\link[plotrix]{draw.circle}}
#'
#' @return No returned value, just draws the hyperbolic tiling.
#' @export
#'
#' @importFrom graphics par
#' @importFrom plotrix draw.circle
#'
#' @note The higher value of \code{n}, the slower. And of course
#'   increasing \code{depth} slows down the rendering. The value of \code{p}
#'   has no influence on the speed.
#'
#' @examples
#' library(gyro)
#' tiling(3, 7, border = "orange")
tiling <- function(
  n, p, depth = 4, colors = c("navy", "yellow"), circle = TRUE, ...
){
  stopifnot(isPositiveInteger(p), isPositiveInteger(n))
  stopifnot(1/n + 1/p < 0.5)
  stopifnot(isPositiveInteger(depth))
  stopifnot(length(colors) >= 2L)
  stopifnot(isBoolean(circle))
  Sommets <- sommets(n, p)
  Centroids <- matrix(numeric(0L), nrow = 0L, ncol = 2L)
  O <- c(0, 0)
  opar <- par(mar = c(0, 0, 0, 0))
  plot(NULL, type = "n", xlim = c(-1, 1), ylim = c(-1, 1), asp = 1,
       axes = FALSE, xlab = NA, ylab = NA)
  if(circle){
    draw.circle(0, 0, radius = 1, ...)
  }
  for(i in 1:n){
    ip1 <- ifelse(i == n, 1L, i+1L)
    pavage(
      cbind(O, Sommets[, i], Mgyromidpoint(Sommets[, i], Sommets[, ip1], 1)),
      0L, depth, Centroids, 1L, n, Sommets, colors
    )
    im1 <- ifelse(i == 1L, n, i-1L)
    pavage(
      cbind(O, Sommets[, i], Mgyromidpoint(Sommets[, i], Sommets[, im1], 1)),
      0L, depth, Centroids, -1L, n, Sommets, colors
    )
  }
  par(opar)
  invisible(NULL)
}
