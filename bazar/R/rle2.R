#' @title 
#' Run length encoding (modified version)
#' 
#' @description 
#' Compute the lengths and values of runs of 
#' \code{\link[bazar]{almost.equal}} values in a vector. 
#' 
#' @param x
#' numeric vector. 
#' 
#' @param tolerance 
#' numeric. Differences smaller than tolerance are considered as equal.  
#' The default value is close to \code{1.5e-8}. 
#' 
#' @return 
#' An object of class \code{"rle"} which is a list with components:
#' \itemize{
#' \item \code{lengths}: an integer vector containing the length of each run.
#' \item \code{values}: a vector of the same length as lengths with the 
#' corresponding values.
#' }
#' 
#' @export
#' 
#' @seealso 
#' \code{\link[bazar]{almost.equal}} in this package; 
#' \code{\link[base]{rle}} in package \pkg{base}. 
#' 
rle2 <- 
function(x, 
         tolerance = sqrt(.Machine$double.eps))
{
  if (!is.vector(x) && !is.list(x)) {
    stop("'x' must be a vector of an atomic type")
  }
  n <- length(x)
  if (n == 0L) {
    return(structure(list(lengths = integer(), values = x), 
                     class = "rle"))
  }
  y <- !almost.equal(x[-1L], x[-n], tolerance = tolerance)
  i <- c(which(y | is.na(y)), n)
  structure(list(lengths = diff(c(0L, i)), values = x[i]), 
            class = "rle")
}
