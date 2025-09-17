#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################


#' Given that the prior on M is a dirac delta, find the \eqn{\gamma} hyperparameter of the weights prior to match \eqn{E(K)=K*}, 
#' where \eqn{K*} is user-specified 
#'
#' Once a fixed value of the number of components \eqn{M^*} is specified, this function  adopts a \emph{bisection method} to find the value of \eqn{\gamma} 
#' such that the induced distribution on the number of clusters is centered around a user specifed value \eqn{K^*}, i.e. the function uses
#' a bisection method to solve for \eqn{\gamma} \insertCite{argiento2019infinity}{AntMAN}. The user can provide a lower \eqn{\gamma_{l}} and
#'  an upper \eqn{\gamma_{u}} bound for the possible values of \eqn{\gamma}. The default values are \eqn{\gamma_l= 10^{-3}} and \eqn{\gamma_{u}=10}.
#'  A default value for the tolerance is \eqn{\epsilon=0.1}. Moreover, after a maximum number of iteration (default is 31), the function 
#'  stops warning that convergence has not been reached.
#'
#' @param n             sample size.
#' @param Mstar         number of components of the mixture.
#' @param Kstar         mean number of clusters the user wants to specify.
#' @param gam_min       lower bound of the interval in which \code{gamma} should lie (default 1e-4).
#' @param gam_max       upper bound of the interval in which \code{gamma} should lie (default 10).
#' @param tolerance     Level of tolerance for the method.
#'
#'
#' @return A value of \code{gamma} such that \eqn{E(K)=K^*} 
#'
#' @keywords prior number of clusters
#'
#' @export
#' 
#' @examples
#' n <- 82
#' Mstar <- 12
#' gam_de <- AM_find_gamma_Delta(n,Mstar,Kstar=6, gam_min=1e-4,gam_max=10, tolerance=0.1)
#' prior_K_de <-  AM_prior_K_Delta(n,gam_de,Mstar)
#' prior_K_de\\%*\\%1:n

AM_find_gamma_Delta <- function (n,Mstar,Kstar=6, gam_min=0.0001,gam_max=10, tolerance=0.1) {
	return(find_gamma_Delta(n,Mstar,Kstar, gam_min,gam_max, tolerance));
}


#' Given that the prior on M is a shifted Poisson, find the \eqn{\gamma} hyperparameter of the weights prior to match \eqn{E(K)=K^{*}}, where \eqn{K^{*}} is user-specified 
#'
#' Once the prior on the number of mixture components M is assumed to be a Shifted Poisson of parameter \code{Lambda}, 
#' this function  adopts a \emph{bisection method} to find the value of \eqn{\gamma} such that the induced distribution
#' on the number of clusters is centered around a user specifed value \eqn{K^{*}}, i.e. the function uses a bisection
#' method to solve for \eqn{\gamma} \insertCite{argiento2019infinity}{AntMAN}. The user can provide a lower \eqn{\gamma_{l}} 
#' and an upper \eqn{\gamma_{u}} bound for the possible values of \eqn{\gamma}. The default values are \eqn{\gamma_l= 10^{-3}} and \eqn{\gamma_{u}=10}.
#' A defaault value for the tolerance is \eqn{\epsilon=0.1}. Moreover, after a maximum number of iteration (default is 31),
#'the function stops warning that convergence has not bee reached.
#'
#' @param n             The sample size.
#' @param Lambda        The parameter of the Shifted Poisson for the number of components of the mixture.
#' @param Kstar         The mean number of clusters the user wants to specify.
#' @param gam_min       The lower bound of the interval in which \code{gamma} should lie.
#' @param gam_max       The upper bound of the interval in which \code{gamma} should lie.
#' @param tolerance     Level of tolerance of the method.
#'
#'
#' @return A value of \code{gamma} such that \eqn{E(K)=K^{*}} 
#'
#' @keywords prior number of clusters
#'
#' @export
#' 
#' @examples
#' n <- 82
#' Lam  <- 11
#' gam_po <-  AM_find_gamma_Pois(n,Lam,Kstar=6, gam_min=0.0001,gam_max=10, tolerance=0.1)
#' prior_K_po <-  AM_prior_K_Pois(n,gam_po,Lam)
#' prior_K_po\\%*\\%1:n

AM_find_gamma_Pois <- function (n,Lambda,Kstar=6, gam_min=0.0001,gam_max=10, tolerance=0.1) {
	return (find_gamma_Pois(n,Lambda,Kstar, gam_min,gam_max, tolerance));
}

#' Given that the prior on M is a Negative Binomial, find the \eqn{\gamma} hyperparameter of the weights 
#' prior to match \eqn{E(K)=K*}, where \eqn{K*} is user-specified 
#'
#' Once the prior on the number of mixture components M is assumed to be a Negative Binomial with 
#' parameter \code{r>0} and \code{0<p<1}, with  mean is 1+ r*p/(1-p), this function adopts a \emph{bisection method} 
#' to find the value of \code{gamma} such that the induced distribution on the number of clusters is centered around a 
#' user specifed value \eqn{K^{*}}, i.e. the function uses a bisection method to solve for \eqn{\gamma} \insertCite{argiento2019infinity}{AntMAN}.  
#' The user can provide a lower \eqn{\gamma_{l}} and an upper \eqn{\gamma_{u}} bound for the possible values of \eqn{\gamma}. The default values 
#' are \eqn{\gamma_l= 10^{-3}} and \eqn{\gamma_{u}=10}.  A defaault value for the tolerance is \eqn{\epsilon=0.1}. Moreover, after a 
#' maximum number of iteration (default is 31), the function stops warning that convergence has not bee reached.
#'
#' @param n      The sample size.
#' @param r      The dispersion parameter \code{r} of the Negative Binomial.
#' @param p      The probability of failure parameter \code{p} of the Negative Binomial.
#' @param Kstar         The mean number of clusters the user wants to specify.
#' @param gam_min  The lower bound of the interval in which \code{gamma} should lie.
#' @param gam_max   The upper bound of the interval in which \code{gamma} should lie.
#' @param tolerance   Level of tolerance of the method.
#'
#'
#' @return A value of \code{gamma} such that \eqn{E(K)=K^{*}} 
#'
#' @keywords prior number of clusters
#'
#' @export
#' 
#' @examples
#' n <- 82
#' r <- 1
#' p <- 0.8571
#' gam_nb= AM_find_gamma_NegBin(n,r,p,Kstar=6, gam_min=0.001,gam_max=10000, tolerance=0.1)
#' prior_K_nb= AM_prior_K_NegBin(n,gam_nb, r, p)
#' prior_K_nb\\%*\\%1:n

AM_find_gamma_NegBin <- function (n,r,p,Kstar=6, gam_min=0.001,gam_max=10000, tolerance=0.1){
	return (find_gamma_NegBin(n,r,p,Kstar, gam_min,gam_max, tolerance));
	}

