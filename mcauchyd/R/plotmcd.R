plotmcd <- function(mu, Sigma, xlim = c(mu[1] + c(-10, 10)*Sigma[1, 1]),
                      ylim = c(mu[2] + c(-10, 10)*Sigma[2, 2]), n = 101,
                      xvals = NULL, yvals = NULL, xlab = "x", ylab = "y",
                      zlab = "f(x,y)", col = "gray", tol = 1e-6, ...) {
  #' Plot of the Bivariate Cauchy Density
  #'
  #' Plots the probability density of the multivariate Cauchy distribution with 2 variables
  #' with location parameter \code{mu} and scatter matrix \code{Sigma}.
  #'
  #' @aliases plotmcd
  #'
  #' @usage plotmcd(mu, Sigma, xlim = c(mu[1] + c(-10, 10)*Sigma[1, 1]),
  #'                 ylim = c(mu[2] + c(-10, 10)*Sigma[2, 2]), n = 101,
  #'                 xvals = NULL, yvals = NULL, xlab = "x", ylab = "y",
  #'                 zlab = "f(x,y)", col = "gray", tol = 1e-6, ...)
  #' @param mu length 2 numeric vector.
  #' @param Sigma symmetric, positive-definite square matrix of order 2. The scatter matrix.
  #' @param xlim,ylim x-and y- limits.
  #' @param n A one or two element vector giving the number of steps in the x and y grid, passed to \code{\link[rgl]{plot3d.function}}.
  #' @param xvals,yvals The values at which to evaluate \code{x} and \code{y}. If used, \code{xlim} and/or \code{ylim} are ignored.
  #' @param xlab,ylab,zlab The axis labels.
  #' @param col The color to use for the plot. See \code{\link[rgl]{plot3d.function}}.
  #' @param tol tolerance (relative to largest variance) for numerical lack of positive-definiteness in Sigma, for the estimation of the density. see \code{\link{dmcd}}.
  #' @param ... Additional arguments to pass to \code{\link[rgl]{plot3d.function}}.
  #' @return Returns invisibly the probability density function.
  #'
  #' @author Pierre Santagostini, Nizar Bouhlel
  #' @references N. Bouhlel, D. Rousseau, A Generic Formula and Some Special Cases for the Kullback–Leibler Divergence between Central Multivariate Cauchy Distributions.
  #' Entropy, 24, 838, July 2022.
  #' \doi{10.3390/e24060838}
  #'
  #' @seealso \code{\link{dmcd}}: probability density of a multivariate Cauchy density
  #' 
  #' \code{\link{contourmcd}}: contour plot of a bivariate Cauchy density.
  #' 
  #' \code{\link[rgl]{plot3d.function}}: plot a function of two variables.
  #'
  #' @examples
  #' mu <- c(1, 4)
  #' Sigma <- matrix(c(0.8, 0.2, 0.2, 0.2), nrow = 2)
  #' plotmcd(mu, Sigma)
  #'
  #' @import rgl
  #' @importFrom rgl plot3d
  #' @export
  
  if (length(mu)!=2 | nrow(Sigma)!=2 | ncol(Sigma)!=2)
    stop(paste("plotmcd only allows plotting a Cauchy density with 2 variables.",
               "mu must be a length 2 numeric vector and Sigma must be a 2*2 square matrix.",
               sep = "\n"))
  
  # Estimation of the density
  f <- function(x) dmcd(x, mu = mu, Sigma = Sigma, tol = tol)
  ff <- function(x, y) sapply(1:length(x), function(i) as.numeric(f(c(x[i], y[i]))))
  
  if (length(n) == 1)
    n <- rep(n, 2)
  if (is.null(xvals))
    xvals = seq.int(min(xlim), max(xlim), length.out = n[1])
  if (is.null(yvals))
    yvals = seq.int(min(ylim), max(ylim), length.out = n[2])
  
  # Plot
  plot3d(ff, xlim = xlim, ylim = ylim, n = n, xvals = xvals, yvals = yvals,
         xlab = xlab, ylab = ylab, zlab = zlab, col = col, ...)
  
  return(invisible(f))
}
