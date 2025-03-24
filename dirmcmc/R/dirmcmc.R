#' dirmcmc: A package implementing Directional Metropolis Hastings for MCMC
#' 
#' dirmcmc package provides functions for simulating from a target distribution with known log unnormalized density and its derivative. The derivative information is needed to construct the DMH kernel, which is a generalization of random walk Metropolis Hastings kernel. 
#' @section dirmcmc functions:
#' \code{\link{metropdir}}: Implements the dmh algorithm to simulate from a given density.
#' 
#' \code{\link{metropdir.adapt}}: Adaptive version of DMH algorithm.
#' 
#' \code{\link{iact}}: Integrated auto correlation times of a univariate chain.
#' 
#' \code{\link{msjd}}: Mean square jump distance of a multivariate chain.
#' 
#' \code{\link{mcmcdiag}}: Some summary of diagnostics of a given chain.
#' @name dirmcmc
#' @docType package
NULL