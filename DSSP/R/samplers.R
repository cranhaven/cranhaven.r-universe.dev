#' Function to sample from the posterior of the smoothing parameter eta conditioned on the data y.
#'
#' This function samples from the log-posterior density of the smoothing parameter from the
#'   thin-plate splines based spatial prior using a ratio-of-uniform sampler.
#' @param N the number of samples desired.
#' @param ND the rank of the precision matrix, the default value is n-3 for spatial data.
#' @param EV eigenvalues of the precision matrix spatial prior from the function make.M().
#' @param Q the data vector from the cross-product of observed data, Y, and eigenvalues from the M matrix, V.
#' @param UL the upper limit for the smoothing parameter value; used for the
#' ratio-of-uniform sampler, default is 1000.
#' @param log_prior a function of x evaluating the log of the prior density for eta
#' @return N samples drawn from the posterior of eta given the data y \eqn{\pi(eta | y)}.
#' @export
#' @examples
#' ## Use the Meuse River dataset from the package 'gstat'
#'
#' library(sp)
#' library(gstat)
#' data(meuse.all)
#' coordinates(meuse.all) <- ~ x + y
#' X <- scale(coordinates(meuse.all))
#' tmp <- make.M(X)
#'
#' EV <- tmp$M.eigen$values
#' V <- tmp$M.eigen$vectors
#'
#' M <- tmp$M
#'
#' Y <- scale(log(meuse.all$zinc))
#' Q <- crossprod(Y, V)
#'
#' ND <- nrow(X) - 3
#' f <- function(x) -x ## log-prior for exponential distribution for the smoothing parameter
#'
#' ## Draw 100 samples from the posterior of eta given the data y.
#' sample.eta(100, ND, EV, Q, UL = 1000, f)
sample.eta <- function(N, ND, EV, Q, UL = 1000, log_prior) {
  RES <- rust::ru(function(x) .eta_post_cpp(x, list(ND = ND, EV = EV, Q = Q)) + log_prior(x), n = N, d = 1, init = 1, trans = "BC", upper = UL)
  RES$sim_vals
}


#' Function to sample from the posterior of the variance parameter
#'
#' This function samples from the log-posterior density of the variance parameter from the likelihood
#' @param eta samples of the smoothing parameter from the sample.eta function.
#' @param ND the rank of the precision matrix, the default value is n-3 for spatial data.
#' @param EV eigenvalues of the precision matrix spatial prior from the function make.M().
#' @param Q the data vector from the cross-product of observed data, Y, and eigenvalues from the M matrix, V.
#' @param pars a vector of the prior shape and rate parameters for the
#' inverse-gamma prior distribution of delta.
#' @return N samples drawn from the posterior of \eqn{\pi(delta | eta, y)}.
#' @export
#' @examples
#' ## Use the Meuse River dataset from the package 'gstat'
#'
#' library(sp)
#' library(gstat)
#' data(meuse.all)
#' coordinates(meuse.all) <- ~ x + y
#' X <- scale(coordinates(meuse.all))
#' tmp <- make.M(X)
#'
#' M <- tmp$M
#'
#' Y <- scale(log(meuse.all$zinc))
#'
#' ND <- nrow(X) - 3
#' M.list <- make.M(X) ##  Only Needs to return the eigenvalues and vectors
#' M <- M.list$M
#' EV <- M.list$M.eigen$values
#' V <- M.list$M.eigen$vectors
#' Q <- crossprod(Y, V)
#'
#' f <- function(x) -x ## log-prior for exponential distribution for the smoothing parameter
#' ## Draw 100 samples from the posterior of eta given the data y.
#'
#' ETA <- sample.eta(100, ND, EV, Q, f, UL = 1000)
#' DELTA <- sample.delta(ETA, ND, EV, Q, pars = c(0.001, 0.001))
#' ##  Old Slow Version of sample.nu()
#' ## sample.delta<-function(eta,nd,ev,Q,pars)
#' ## {
#' ##   N<-length(eta)
#' ##   f.beta<-function(x)
#' ##   {
#' ##     lambda<-1/(1+x*ev)
#' ##     b<-tcrossprod(Q,diag(1-lambda))
#' ##     beta<-0.5*tcrossprod(Q,b)+pars[2]
#' ##     return(beta)
#' ##   }
#' ##   alpha<-pars[1]+nd*0.5
#' ##   beta<-sapply(eta,f.beta)
#' ##   delta<-1/rgamma(N,shape=alpha,rate=beta)
#' ##   return(delta)
#' ## }
sample.delta <- function(eta, ND, EV, Q, pars) {
  .sample_delta_cpp(eta, list(ND = ND, EV = EV, Q = Q, PARS = pars))
}


#' Function to sample from the posterior of the spatial effects
#'
#' This function samples from the posterior density of the spatial effects from the direct sampling
#'  spatial prior (DSSP) model.
#' @param Y vector of observed data.
#' @param eta samples of the smoothing parameter from the \code{sample.eta} function.
#' @param delta samples of the variance parameter from the \code{sample.delta} function.
#' @param EV eigenvalues of the precision matrix spatial prior from the function \code{make.M()}.
#' @param V eigenvectors of the precision matrix spatial prior from the function \code{make.M()}.
#' @return A matrix of samples with each column a random draw from the posterior
#' of the spatial effects from the DSSP model \eqn{\pi(nu | eta, delta, y)}.
#' @export
#' @examples
#' ## Use the Meuse River dataset from the package 'gstat'
#'
#' library(sp)
#' library(gstat)
#' data(meuse.all)
#' coordinates(meuse.all) <- ~ x + y
#' X <- scale(coordinates(meuse.all))
#' tmp <- make.M(X)
#'
#' EV <- tmp$M.eigen$values
#' V <- tmp$M.eigen$vectors
#'
#' Y <- scale(log(meuse.all$zinc))
#' Q <- crossprod(Y, V)
#'
#' ND <- nrow(X) - 3
#' f <- function(x) -x ## log-prior for exponential distribution for the smoothing parameter
#' ## Draw 100 samples from the posterior of eta given the data y.
#'
#' ETA <- sample.eta(100, ND, EV, Q, f, UL = 1000)
#' DELTA <- sample.delta(ETA, ND, EV, Q, pars = c(0.001, 0.001))
#' NU <- sample.nu(Y, ETA, DELTA, EV, V)
sample.nu <- function(Y, eta, delta, EV, V) {
  .sample_nu_cpp(Y, list(eta = eta, delta = delta, EV = EV, V = V))
}
