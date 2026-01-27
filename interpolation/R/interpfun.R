#' @title Interpolation function
#' @description Generates a function \code{f(x,y)} that interpolates the known 
#'   function values at some given \code{(x,y)}-coordinates. 
#'
#' @param x,y two numeric vectors of the same size
#' @param z a numeric vector or matrix of the same size as \code{x} and 
#'   \code{y}, with two or three columns if it is a matrix
#' @param method method of interpolation, either \code{"linear"} or 
#'   \code{"sibson"}; the \code{"sibson"} method is not available for 
#'   vector-valued functions, i.e. if \code{z} is a matrix
#'
#' @return A function whose graph interpolates the data \code{((x,y),z)}. 
#' @export
#'
#' @details The new pairs of coordinates must be in the convex hull of the
#'   points \code{(x,y)}. If a new pair is outside the convex hull, the 
#'   interpolating function returns \code{NA} for this pair. 
#'   The linear method is exact for a function of the form 
#'   \code{f(x,y) = a + bx*x + by*y}. The Sibson method is exact for a function 
#'   of the form \code{f(x,y) = a + bx*x + by*y + c*(x^2 + y^2)}. This method 
#'   estimates the gradient of the function and this can fail if the data are 
#'   insufficient, in which case \code{NA} is returned.
#'   
#' @examples 
#' library(interpolation)
#' a <- 0.2; bx <- 0.3; by <- -0.4
#' x0 <- y0 <- seq(1, 10, by = 1)
#' Grid <- expand.grid(X = x0, Y = y0)
#' x <- Grid$X; y <- Grid$Y
#' z <- a + bx*x + by*y 
#' xnew <- ynew <- seq(2.5, 8.5, by = 1)
#' fun <- interpfun(x, y, z, "linear")
#' # computed values:
#' ( znew <- fun(xnew, ynew) )
#' # true values:
#' a + bx*xnew + by*ynew
#' 
#' # a vector-valued example ####
#' x <- y <- c(-5, -4, -3, -2, 2, 3, 4, 5)
#' From <- as.matrix(expand.grid(x0 = x, y0 = y))
#' f <- function(x0y0) {
#'   d <- c(-10, -5) - x0y0
#'   x0y0 + 0.8 * d / sqrt(c(crossprod(d)))
#' }
#' To <- t(apply(From, 1L, f))
#' x0 <- From[, "x0"]; y0 <- From[, "y0"]
#' x1 <- To[, 1L]; y1 <- To[, 2L]
#' # plot data
#' plot(
#'   x0, y0, asp = 1, pch = 19, xlab = "x", ylab = "y"
#' )
#' arrows(x0, y0, x1, y1, length = 0.1)
#' # interpolate
#' library(interpolation)
#' fun <- interpfun(x0, y0, To, method = "linear")
#' From_new <- rbind(
#'   as.matrix(expand.grid(x0 = c(-1, 0, 1), y0 = (-5):5)),
#'   as.matrix(expand.grid(x0 = c(-5, -4, -3, -2), y0 = c(-1, 0, 1))),
#'   as.matrix(expand.grid(x0 = c(2, 3, 4, 5), y0 = c(-1, 0, 1)))
#' )
#' To_new   <- fun(From_new)
#' x0 <- From_new[, "x0"]; y0 <- From_new[, "y0"]
#' x1 <- To_new[, 1L]; y1 <- To_new[, 2L]
#' points(x0, y0, pch = 19, col = "red")
#' arrows(x0, y0, x1, y1, length = 0.1, col = "red")
interpfun <- function(x, y, z, method = "linear") {
  method <- match.arg(method, c("linear", "sibson"))
  if(is.matrix(z)) {
    z <- t(z)
  }
  XYZ <- rbind(x, y, z)
  storage.mode(XYZ) <- "double"
  if(anyNA(XYZ)) {
    stop("Found missing values.")
  }
  if(method == "linear") {
    if(nrow(XYZ) == 3L) {
      delxyz <- delaunayXYZ_linear(XYZ)
      return(function(xnew, ynew) {
        interpolate_linear(delxyz, rbind(xnew, ynew))
      })
    } else if(nrow(XYZ) == 4L) {
      delxyzz <- delaunayXYZZ_linear(XYZ)
      return(function(XYnew) {
        interpolate_linear2(delxyzz, t(XYnew))
      })
    } else if(nrow(XYZ) == 5L) {
      delxyzzz <- delaunayXYZZZ_linear(XYZ)
      return(function(XYnew) {
        interpolate_linear3(delxyzzz, t(XYnew))
      })
    } else {
      stop("Something wrong regarding x, y, z.")
    }
  } else {
    if(nrow(XYZ) > 3L) {
      stop("Sibson interpolation is not available for vector-valued functions.")
    }
    delxyz <- delaunayXYZ_sibson(XYZ)
    return(function(xnew, ynew) {
      interpolate_sibson(delxyz, rbind(xnew, ynew))
    })
  }
}
