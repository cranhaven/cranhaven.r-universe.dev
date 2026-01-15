## **************************************************************************
##
##    (c) 2023-2024 Guillaume Guénard
##        Department de sciences biologiques,
##        Université de Montréal
##        Montreal, QC, Canada
##
##    **Distance Metric Generator Function**
##
##    This file is part of pMEM
##
##    pMEM is free software: you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation, either version 3 of the License, or
##    (at your option) any later version.
##
##    pMEM is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.
##
##    You should have received a copy of the GNU General Public License
##    along with pMEM. If not, see <https://www.gnu.org/licenses/>.
##
##    R source code file
##
## **************************************************************************
##
#' Distance Metric Function Generator
#'
#' Function \code{genDistMetric} generates a distance metric function, which
#' calculate pairwise distance values on the basis of given arguments.
#'
#' @param delta Optional: the asymmetry parameter of the distance metric.
#' @param theta The influence angle of the distance metric (default: 0).
#' 
#' @returns A two-argument function (\code{x} and \code{y}) calculating the
#' distances between the rows of \code{x} and the rows of \code{y}. When
#' \code{y} is omitted, the pairwise distances between the rows of \code{x} are
#' calculated instead.
#' 
#' @details When argument \code{delta} is omitted, the returned function
#' calculates the Euclidean distance, whereas when is it provided with a value,
#' the returned function  calculates a complex-values distance metric whose
#' modulus is the Euclidean distance and argument is related with \code{delta}.
#' For one-dimensional data (transects), the argument is ±\code{delta}, with
#' negative value for every second object located before the first, and positive
#' values for every second object located after the first. For two-dimensional
#' data, the value of the argument is the cosine of the angular difference
#' between the angle of the line traversing the two points, defined in the
#' direction going from the first to the second point, and the influence angle.
#' In any case, the argument of the distance metric from a point A to a point B
#' has the opposite sign as that of the distance metric from point B to point A.
#' Therefore, the pairwise distance matrix is Hermitian and, as such, has
#' eigenvalues that are strictly real-valued.
#' 
#' It is noteworthy that \code{genDistMetric} does not calculate the distances
#' directly, as is the most common workflow, but generate a function that
#' calculate the metric on the basis of the specified parameters (arguments
#' \code{delta} and \code{theta}). The values of these parameters are embedded
#' together with distance metric function in the returned object's namespace and
#' can only be changed by generating a new function.
#' 
#' @author \packageAuthor{pMEM}
#' 
#' @examples  ## A five point equidistant transect:
#' n <- 5
#' x <- (n - 1)*seq(0, 1, length.out=n)
#' 
#' ## The symmetric (Euclidean metric) function is obtained by calling
#' ## the generator function with not arguments as follows:
#' mSym <- genDistMetric()
#' 
#' ## The pairwise symmetric metric between the rows of x:
#' mSym(x)
#' 
#' ## A second set of points in the same range as the previous one, but at a
#' ## distance of 0.05 from one another:
#' xx <- (n - 1)*seq(0, 1, 0.05)
#' 
#' ## The same metrix, but between x and xx:
#' mSym(x,xx)
#' 
#' ## The asymmetric function with a delta of 0.2:
#' mAsy <- genDistMetric(0.2)
#' 
#' ## The pairwise asymmetric metric between the rows of x:
#' mAsy(x)
#' 
#' ## The same metrix, but between x and xx:
#' mAsy(x,xx)
#' 
#' 
#' @importFrom Rcpp evalCpp
#' 
#' @useDynLib pMEM, .registration = TRUE
#' 
#' @export
genDistMetric <- function(delta, theta = 0)
  if(missing(delta)) {
    function(x, y) {
      if(!is.matrix(x)) x <- as.matrix(x)
      if(missing(y)) y <- x
      if(!is.matrix(y)) y <- as.matrix(y)
      .Call("pMEM_EuclidReal", PACKAGE="pMEM", x, y)
    }
  } else {
    function(x, y) {
      if(!is.matrix(x)) x <- as.matrix(x)
      if(missing(y)) y <- x
      if(!is.matrix(y)) y <- as.matrix(y)
      .Call("pMEM_EuclidCplx2D", PACKAGE="pMEM", x, y, delta, theta)
    }
  }
#'
