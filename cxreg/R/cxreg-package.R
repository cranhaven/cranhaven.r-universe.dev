#' Simulated data for the cxreg vignette
#'
#' Simple simulated data, used to demonstrate the features of cxreg
#'
#' These datasets are artificial, and are used to test out some of the
#' features of cxreg.
#' @name cxreg
#' @aliases x y cxreg
#' @format Data objects used to demonstrate features in the cxreg vignette
#' @keywords datasets
#' @useDynLib cxreg
#' @import methods
#' @import Matrix
#' @import foreach
#' @importFrom utils packageDescription
#' @importFrom graphics abline axis matplot points segments text par plot
#' @importFrom stats approx coef median predict runif weighted.mean family rnorm gaussian
#' @importFrom grDevices rainbow
#' @importFrom Rcpp sourceCpp
#' @importFrom fields image.plot
#' @importFrom mvtnorm rmvnorm
#' @importFrom gdata upperTriangle
#' @examples
#' \donttest{ 
#' data(classo_example)
#' x <- classo_example$x
#' y <- classo_example$y
#' classo(x,y)
#' 
#' data(cglasso_example)
#' f_hat <- cglasso_example$f_hat
#' n <- cglasso_example$n
#' cglasso(S=f_hat,type="I",nobs=n)
#' cglasso(S=f_hat,type="II",nobs=n)
#' }
NULL

#' Internal classo functions
#'
#' @description
#' These are not intended for use by users. \code{lambda.interp} does linear
#' interpolation of the lambdas to obtain a prediction at a new point s.
#' \code{nonzeroCoef} determines in an efficient manner which variables are
#' nonzero in each fit.
#' \code{jerr} (not currently available) prints out error messages from the C++ routines.
#' \code{plotCoef} is called by the \code{plot} method for \code{cxreg}
#' objects. \code{check_dots} is used in \code{coef} and \code{predict} with
#' argument \code{exact=TRUE}, to make sure user supplies original data used to
#' fit the \code{"classo"} object.
#'
#' @name classo-internal
#' @aliases cvtype cvstats#'
#' @author Younghoon Kim
#' @keywords internal
NULL

#' Complex-valued Lasso and graphical Lasso paths
#'
#' This package fits complex-valued Lasso for regression using coordinate descent. The algorithm is extremely fast, and exploits sparsity in the input x matrix where it exists.
#' A variety of predictions can be made from the fitted models.
#' 
#' This package also provides fitting for complex-valued graphical Lasso using coordinate descent. 
#' The function is built upon classo with covariate updates, just as the regular real-valued coordinate descent algorithm for graphical Lasso.
#'
#' \tabular{ll}{
#' Package: \tab cxreg \cr
#' Type: \tab Package \cr
#' Version: \tab 1.0.0 \cr
#' Date: \tab 2025-07-01 \cr
#' License: \tab MIT + file LICENSE \cr
#' }
#' 
#' Very simple to use. Accepts \code{x,y} data for penalized regression models, and
#' produces the regularization paths over a grid of values for the tuning
#' parameters \code{lambda}. Similarly, accepts \code{S,n} data for penalized Gaussian likelihood, 
#' and produce the regularization paths over a grid of values for the tuning parameter \code{lambda}.
#'
#' @name cxreg-package
#' @author Younghoon Kim, Navonil Deb, Sumanta Basu \cr Maintainer:
#' Younghoon Kim <yk748@cornell.edu>
#' @references Deb, N., Kuceyeski, A., Basu, S. (2024)
#' \emph{Regularized Estimation of Sparse Spectral Precision Matrices},
#' \url{https://arxiv.org/abs/2401.11128}.
#' @keywords models regression package
#' @examples
#' \donttest{ 
#' set.seed(1234)
#' x <- array(rnorm(100*20), c(100,20)) + (1+1i) * array(rnorm(100*20), c(100,20))
#' for (j in 1:20) x[,j] <- x[,j] / sqrt(mean(Mod(x[,j])^2))
#' e <- rnorm(100) + (1+1i) * rnorm(100)
#' b <- c(1, -1, rep(0, 18)) + (1+1i) * c(-0.5, 2, rep(0, 18))
#' y <- x %*% b + e
#' fit <- classo(x, y)
#' predict(fit, newx = x[1:5, ], s = c(0.01, 0.005))
#' predict(fit, type = "coef")
#' plot(fit, xvar = "lambda")
#' }
#'
#' \donttest{ 
#' p <- 30
#' n <- 500
#' C <- diag(0.7, p)
#' C[row(C) == col(C) + 1] <- 0.3  
#' C[row(C) == col(C) - 1] <- 0.3  
#' Sigma <- solve(C)
#' set.seed(1010)
#' m <- floor(sqrt(n)); j <- 1
#' X_t <- mvtnorm::rmvnorm(n = n, mean = rep(0, p), sigma = Sigma)
#' d_j <- dft.X(X_t,j,m)
#' f_j_hat <- t(d_j) %*% Conj(d_j) / (2*m+1)
#' fit <- cglasso(S=f_j_hat, nobs=n,type="I")
#' plot(fit$Theta_list,index=fit$min_index,type="mod",label=FALSE)
#' }
NULL

