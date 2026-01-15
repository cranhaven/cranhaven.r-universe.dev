## **************************************************************************
##
##    (c) 2023-2024 Guillaume Guénard
##        Department de sciences biologiques,
##        Université de Montréal
##        Montreal, QC, Canada
##
##    **Spatial eigenvector map class generator and methods**
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
#' Class and Methods for Predictive Moran's Eigenvector Maps (pMEM)
#' 
#' Generator function, class, and methods to handle predictive Moran's
#' eigenvector maps (pMEM).
#' 
#' @docType class
#' 
#' @name SEMap-class
#' 
#' @aliases pMEM
#' 
#' @param x a set of coordinates to be given to the distance metric function
#' (argument \code{m} below) to obtain the distance metric (\code{genSEF}) or an
#' \code{SEMap-class} object (methods).
#' @param m a distance metric function, such as one of those returned by
#' \code{\link{genDistMetric}}.
#' @param f a distance weighting function, such as one of those returned by
#' \code{\link{genDWF}}.
#' @param tol a tolerance threshold for absolute eigenvalues, below which to
#' discard spatial eigenfunctions.
#' @param object an \code{SEMap-class} object.
#' @param ... further arguments to be passed to other functions or methods.
#' @param row.names \code{NULL} or a character vector giving the row names for
#' the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#' column names (to syntactic names: see \code{\link{make.names}}) is optional.
#' See \bold{base} \code{\link{as.data.frame}} for further details on this
#' argument.
#' @param newdata a set of new coordinates from which to calculate pMEM
#' predictor scores.
#' 
#' @return
#' \describe{
#'   \item{ genSEF }{ a \code{SEMap-class} object. }
#'   \item{ print.SEMap }{ \code{NULL} (invisibly). }
#'   \item{ as.data.frame.SEMap }{ A \code{data.frame} with the spatial
#'   eigenvectors. }
#'   \item{ as.matrix.SEMap }{ A matrix with the spatial eigenvectors. }
#'   \item{ predict.SEMap }{ A matrix with the spatial eigenfunction values }
#' }
#' 
#' @details Predictive Moran's Eigenvector Maps (pMEM) allows one to model the
#' spatial variability of an environmental variable and use the resulting model
#' for making prediction at any location on and around the sampling points. They
#' originate from coordinates in one or more dimensions, which are used to
#' calculate distances. The distances are obtained from the coordinates using a
#' function given through argument \code{m} (see \code{\link{genDistMetric}} for
#' further details). The distances are then transformed to weights using a
#' spatial weighting function given as argument \code{f} (see
#' \code{\link{genDWF}} for implementations of spatial weighting function). The
#' resulting weights are row- and column-centred to the value 0 before being
#' submitted to an eigenvalue decomposition. Eigenvectors associated to
#' eigenvalues whose absolute value are above the threshold value set through
#' argument \code{tol} are retained as part of the resulting eigenvector map.
#' 
#' In a standard workflow, a model is built for the locations where values of
#' the response variable are known using the eigenvectors (or a subset thereof).
#' This model may be build using any model building approach using descriptors.
#' The scores obtained for new coordinates from method \code{predict} are used
#' given to the model for making predictions.
#' 
#' The function can handle real-valued as well as complex-valued distance
#' metrics. The latter is useful to represent asymmetric (i.e., directed)
#' spatial processes.
#' 
#' @format A \code{SEMap-class} object contains:
#' \describe{
#'   \item{ show }{ A printing function. }
#'   \item{ getIMoran }{ A function (with no argument) returning the Moran's I
#'   coefficients associated with the spatial eigenfunctions. }
#'   \item{ getSEF }{ A function that return the spatial eigenvectors. It has an
#'   argument \code{wh} which allows one to specify a selection of the
#'   eigenvectors that are to be returned. }
#'   \item{ getLambda }{ A function that returns the eigenvalues. }
#'   \item{ getPredictor }{ A function that calculate the spatial eigenfunction
#'   values for arbitrary locations about the sampling points. The coordinates
#'   of these locations are given as a vector or matrix through argument
#'   \code{xx}. It also has an argument \code{wh} which allows one to specify a
#'   selection of the eigenfunctions that are to be returned. }
#' }
#' 
#' @author \packageAuthor{pMEM}
#' 
#' Maintainer: \packageMaintainer{pMEM}
#' 
#' @importFrom utils head tail
#' 
#' @examples ## Store graphical parameters:
#' tmp <- par(no.readonly = TRUE)
#' par(las=1)
#' 
#' ## Case 1: one-dimensional symmetrical
#' 
#' n <- 11
#' x <- (n - 1)*seq(0, 1, length.out=n)
#' xx <- (n - 1)*seq(0, 1, 0.01)
#' 
#' sefSym <- genSEF(x, genDistMetric(), genDWF("Gaussian",3))
#' 
#' plot(y = predict(sefSym, xx, wh=1), x = xx, type = "l", ylab = "PMEM_1",
#'      xlab = "x")
#' points(y = as.matrix(sefSym, wh=1), x = x)
#' 
#' plot(y = predict(sefSym, xx, wh=2), x = xx, type = "l", ylab = "PMEM_2",
#'      xlab = "x")
#' points(y = as.matrix(sefSym, wh=2), x = x)
#' 
#' plot(y = predict(sefSym, xx, wh=5), x = xx, type = "l", ylab = "PMEM_5",
#'      xlab = "x")
#' points(y = as.matrix(sefSym, wh=5), x = x)
#' 
#' ## Case 2: one-dimensional asymmetrical (each has a real and imaginary parts)
#' 
#' sefAsy <- genSEF(x, genDistMetric(delta = pi/8), genDWF("Gaussian",3))
#' 
#' plot(y = Re(predict(sefAsy, xx, wh=1)), x = xx, type = "l", ylab = "PMEM_1",
#'      xlab = "x", ylim=c(-0.35,0.35))
#' lines(y = Im(predict(sefAsy, xx, wh=1)), x = xx, col="red")
#' points(y = Re(as.matrix(sefAsy, wh=1)), x = x)
#' points(y = Im(as.matrix(sefAsy, wh=1)), x = x, col="red")
#' 
#' plot(y = Re(predict(sefAsy, xx, wh=2)), x = xx, type = "l", ylab = "PMEM_2",
#'      xlab = "x", ylim=c(-0.45,0.35))
#' lines(y = Im(predict(sefAsy, xx, wh=2)), x = xx, col="red")
#' points(y = Re(as.matrix(sefAsy, wh=2)), x = x)
#' points(y = Im(as.matrix(sefAsy, wh=2)), x = x, col="red")
#' 
#' plot(y = Re(predict(sefAsy, xx, wh=5)), x = xx, type = "l", ylab = "PMEM_5",
#'      xlab = "x", ylim=c(-0.45,0.35))
#' lines(y = Im(predict(sefAsy, xx, wh=5)), x = xx, col="red")
#' points(y = Re(as.matrix(sefAsy, wh=5)), x = x)
#' points(y = Im(as.matrix(sefAsy, wh=5)), x = x, col="red")
#' 
#' ## A function to display combinations of the real and imaginary parts:
#' plotAsy <- function(object, xx, wh, a, ylim) {
#'   pp <- predict(object, xx, wh=wh)
#'   plot(y = cos(a)*Re(pp) + sin(a)*Im(pp), x = xx, type = "l",
#'        ylab = "PMEM_5", xlab = "x", ylim=ylim, col="green")
#'   invisible(NULL)
#' }
#' 
#' ## Display combinations at an angle of 45° (pMEM_5):
#' plotAsy(sefAsy, xx, 5, pi/4, ylim=c(-0.45,0.45))
#' 
#' ## Display combinations for other angles:
#' for(i in 0:15) {
#'   plotAsy(sefAsy, xx, 5, i*pi/8, ylim=c(-0.45,0.45))
#'   if(is.null(locator(1))) break
#' }
#' 
#' ## Case 3: two-dimensional symmetrical
#' 
#' cbind(
#'   x = c(-0.5,0.5,-1,0,1,-0.5,0.5),
#'   y = c(rep(sqrt(3)/2,2L),rep(0,3L),rep(-sqrt(3)/2,2L))
#' ) -> x2
#' 
#' seq(min(x2[,1L]) - 0.3, max(x2[,1L]) + 0.3, 0.05) -> xx
#' seq(min(x2[,2L]) - 0.3, max(x2[,2L]) + 0.3, 0.05) -> yy
#' 
#' list(
#'   x = xx,
#'   y = yy,
#'   coords = cbind(
#'     x = rep(xx, length(yy)),
#'     y = rep(yy, each = length(xx))
#'   )
#' ) -> ss
#' 
#' cc <- seq(0,1,0.01)
#' cc <- c(rgb(cc,cc,1),rgb(1,1-cc,1-cc))
#' 
#' sefSym2D <- genSEF(x2, genDistMetric(), genDWF("Gaussian",3))
#' 
#' scr <- predict(sefSym2D, ss$coords)
#' 
#' par(mfrow = c(2,3), mar=0.5*c(1,1,1,1))
#' 
#' for(i in 1L:6) {
#'   image(z=matrix(scr[,i],length(ss$x),length(ss$y)), x=ss$x, y=ss$y, asp=1,
#'         zlim=max(abs(scr[,i]))*c(-1,1), col=cc, axes=FALSE)
#'   points(x = x2[,1L], y = x2[,2L])
#' }
#' 
#' ## Case 4: two-dimensional asymmetrical
#' 
#' sefAsy2D0 <- genSEF(x2, genDistMetric(delta=pi/8), genDWF("Gaussian",1))
#' ## Note: default influence angle is 0 (with respect to the abscissa)
#' 
#' ## A function to display combinations of the real and imaginary parts (2D):
#' plotAsy2 <- function(object, ss, a) {
#'   pp <- predict(object, ss$coords)
#'   for(i in 1:6) {
#'     z <- cos(a)*Re(pp[,i]) + sin(a)*Im(pp[,i])
#'     image(z=matrix(z,length(ss$x),length(ss$y)), x=ss$x, y=ss$y, asp=1,
#'           zlim=max(abs(z))*c(-1,1), col=cc, axes=FALSE)
#'   }
#'   invisible(NULL)
#' }
#' 
#' ## Display combinations at an angle of 22°:
#' plotAsy2(sefAsy2D0, ss, pi/8)
#' 
#' ## Display combinations at other angles:
#' for(i in 0:23) {
#'   plotAsy2(sefAsy2D0, ss, i*pi/12)
#'   if(is.null(locator(1))) break
#' }
#' 
#' ## With an influence of +45° (with respect to the abscissa)
#' sefAsy2D1 <- genSEF(x2, genDistMetric(delta=pi/8, theta = pi/4),
#'                     genDWF("Gaussian",1))
#' 
#' for(i in 0:23) {
#'   plotAsy2(sefAsy2D1, ss, i*pi/12)
#'   if(is.null(locator(1))) break
#' }
#' 
#' ## With an influence of +90° (with respect to the abscissa)
#' sefAsy2D2 <- genSEF(x2, genDistMetric(delta=pi/8, theta = pi/2),
#'                     genDWF("Gaussian",1))
#' 
#' for(i in 0:23) {
#'   plotAsy2(sefAsy2D2, ss, i*pi/12)
#'   if(is.null(locator(1))) break
#' }
#' 
#' ## With an influence of -45° (with respect to the abscissa)
#' sefAsy2D3 <- genSEF(x2, genDistMetric(delta=pi/8, theta = -pi/4),
#'                     genDWF("Gaussian",1))
#' 
#' for(i in 0:23) {
#'   plotAsy2(sefAsy2D3, ss, i*pi/12)
#'   if(is.null(locator(1))) break
#' }
#' 
#' ## Reverting to initial graphical parameters:
#' par(tmp)
#' 
#' @importFrom Rcpp evalCpp
#' 
#' @useDynLib pMEM, .registration = TRUE
#' 
NULL
#' 
#' @describeIn SEMap-class
#' 
#' Predictive Moran's Eigenvector Map (pMEM) Generation
#' 
#' Generates a predictive spatial eigenvector map (a SEMap-class object).
#' 
#' @export
genSEF <- function(x, m, f, tol = .Machine$double.eps^0.5) {
  n <- NROW(x)
  d <- m(x,x)
  complex <- is.complex(d)
  w <- f(d)
  if(complex) {
    g <- .Call("pMEM_centerCplx", PACKAGE="pMEM", w, TRUE)
  } else {
    g <- .Call("pMEM_centerReal", PACKAGE="pMEM", w, TRUE)
  }
  eigen(g$centered, symmetric = TRUE) -> eig
  eig$vectors <- eig$vectors[,abs(eig$values) > tol, drop=FALSE]
  eig$values <- eig$values[abs(eig$values) > tol]
  names(eig$values) <- paste("pMEM",1L:length(eig$values),sep="_")
  list(
    if(is.matrix(x)) rownames(x) else names(x),
    names(eig$values)
  ) -> dimnames(eig$vectors)
  p <- eig$vectors %*% diag(eig$values^(-1))
  colnames(p) <- colnames(eig$vectors)
  structure(
    list(
      show = function() {
        cat("A SEMap-class object\n--------------------\n")
        cat(sprintf("Number of sites: %d\n",n))
        cat(sprintf("Directional: %s\n",if(complex) "yes" else "no"))
        cat(sprintf("Number of components: %d\n",length(eig$values)))
        ev <- sprintf("%.5f", eig$values)
        if(length(eig$values) > 6L)
          ev <- c(head(ev,3L),"...",tail(ev,2))
        cat(sprintf("Eigenvalues: %s\n", paste(ev, collapse=",")))
        cat("--------------------\n")
        invisible(NULL)
      },
      getIMoran = function() {
        I <- (eig$values - 1)*n/(sum(w) - n)
        if(complex) Re(I) else I
      },
      getSEF = function(wh)
        if(missing(wh)) eig$vectors else eig$vectors[,wh,drop=FALSE],
      getLambda = function() eig$values,
      getPredictor = function(xx, wh) {
        dd <- m(x,xx)
        ww <- f(dd)
        if(complex) {
          gg <- .Call("pMEM_recenterCplx", PACKAGE="pMEM", ww, g$centers, TRUE)
        } else {
          gg <- .Call("pMEM_recenterReal", PACKAGE="pMEM", ww, g$centers, TRUE)
        }
        if(missing(wh)) gg %*% p else gg %*% p[,wh,drop=FALSE]
      }
    ),
    class = "SEMap"
  )
}
#'
#'@describeIn SEMap-class
#' 
#' Print SEMap-class
#' 
#' A print method for \code{SEMap-class} objects.
#' 
#' @method print SEMap
#'
#' @export
print.SEMap <- function(x, ...)
  x$show()
#' 
#' @describeIn SEMap-class
#' 
#' An \code{as.data.frame} Method for \code{SEMap-class} Objects
#' 
#' A method to extract the spatial eigenvectors from an \code{SEMap-class}
#' object as a data frame.
#' 
#' @method as.data.frame SEMap
#' 
#' @export
as.data.frame.SEMap <- function(x, row.names = NULL, optional = FALSE, ...)
  as.data.frame(x$getSEF(...), row.names = row.names, optional = optional)
#' 
#' @describeIn SEMap-class
#' 
#' An \code{as.matrix} Method for \code{SEMap-class} Objects
#' 
#' A method to extract the spatial eigenvectors from an \code{SEMap-class}
#' object as a matrix.
#' 
#' @method as.matrix SEMap
#' 
#' @export
as.matrix.SEMap <- function(x, ...)
  x$getSEF(...)
#' 
#' @describeIn SEMap-class
#' 
#' A \code{predict} Method for \code{SEMap-class} Objects
#' 
#' A method to obtain predictions from an \code{SEMap-class} object.
#' 
#' @method predict SEMap
#' 
#' @export
predict.SEMap <- function(object, newdata, ...)
  object$getPredictor(newdata, ...)
#' 
