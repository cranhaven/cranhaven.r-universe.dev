#' Mean and Variance for Truncated Multivariate Elliptical Distributions
#'
#' This function approximates the mean vector and variance-covariance matrix for some specific truncated elliptical distributions.
#' The argument \code{dist} specifies the distribution to be used and accepts
#' the values \code{"Normal"}, \code{"t"}, \code{"PE"}, \code{"PVII"},
#' \code{"Slash"}, and \code{"CN"}, corresponding to the truncated Normal,
#' Student-t, Power Exponential, Pearson type VII, Slash, and Contaminated Normal
#' distributions, respectively.
#'
#' Moments associated with the truncated components are estimated using a
#' Monte Carlo approach, while moments for the non-truncated components are
#' obtained by exploiting properties of conditional expectation.
#'
#' @param lower vector of lower truncation points of length \eqn{p}.
#' @param upper vector of upper truncation points of length \eqn{p}.
#' @param mu numeric vector of length \eqn{p} representing the location parameter.
#' @param Sigma numeric positive definite matrix with dimension \eqn{p}x\eqn{p} representing the
#' scale parameter.
#' @param dist represents the truncated distribution to be used. The values are \code{'Normal'},
#' \code{'t'}, \code{'PE'}, \code{'PVII'}, \code{'Slash'}, and \code{'CN'} for the truncated Normal, Student-t,
#' Power Exponential, Pearson VII, Slash, and Contaminated Normal distributions, respectively.
#' @param nu additional parameter or vector of parameters depending on the
#' density generating function. See Details.
#' @param n number of Monte Carlo samples to be generated.
#' @param burn.in number of samples to be discarded as a burn-in phase.
#' @param thinning factor for reducing the autocorrelation of random points.
#'
#' @details This function also supports the univariate case. The argument \code{nu} denotes
#' a parameter or a vector of parameters, depending on the underlying
#' density generating function (DGF). For the truncated Student-t, Power Exponential, and Slash distributions,
#' \code{nu} must be a positive scalar. For the truncated Pearson type VII distribution, \code{nu} is a vector of length two,
#' where the first element must be greater than \eqn{p/2} and the second element
#' must be strictly positive. For the truncated Contaminated Normal distribution, \code{nu} is a vector of length two
#' with components taking values in the interval \eqn{(0,1)}.
#'
#' @note The Normal distribution is a special case of the Power Exponential distribution
#' obtained when \code{nu = 1}. The Student-t distribution with \eqn{\nu} degrees of freedom
#' arises as a particular case of the Pearson type VII distribution when
#' \code{nu = } ((\eqn{\nu}+p)/2, \eqn{\nu}).
#'
#' For the Student-t distribution, if \code{nu >= 300}, the Normal approximation is used.
#' The algorithm also supports Student-t distributions with degrees of freedom
#' \code{nu <= 2}. For the Pearson type VII distribution, the algorithm supports
#' values of \code{m <= (p + 2)/2}, where \code{m} corresponds to the first component
#' of \code{nu}.
#'
#' @return It returns a list with three elements:
#' \item{EY}{the mean vector of length \eqn{p}.}
#' \item{EYY}{the second moment matrix of dimensions \eqn{p}x\eqn{p}.}
#' \item{VarY}{the variance-covariance matrix of dimensions \eqn{p}x\eqn{p}.}
#'
#' @author Katherine L. Valeriano, Christian E. Galarza and Larissa A. Matos
#'
#' @seealso \code{\link{rtelliptical}}
#'
#' @examples
#' # Truncated Student-t distribution
#' set.seed(5678)
#' mu = c(0.1, 0.2, 0.3)
#' Sigma = matrix(data = c(1,0.2,0.3,0.2,1,0.4,0.3,0.4,1), nrow=length(mu),
#'                ncol=length(mu), byrow=TRUE)
#'
#' # Example 1: considering nu = 0.80 and one doubly truncated variable
#' a = c(-0.8, -Inf, -Inf)
#' b = c(0.5, 0.6, Inf)
#' MC11 = mvtelliptical(a, b, mu, Sigma, "t", 0.80)
#'
#' # Example 2: considering nu = 0.80 and two doubly truncated variables
#' a = c(-0.8, -0.70, -Inf)
#' b = c(0.5, 0.6, Inf)
#' MC12 = mvtelliptical(a, b, mu, Sigma, "t", 0.80) # By default n=1e4
#'
#' # Truncated Pearson VII distribution
#' set.seed(9876)
#' MC21 = mvtelliptical(a, b, mu, Sigma, "PVII", c(1.90,0.80), n=1e6) # More precision
#' c(MC12$EY); c(MC21$EY)
#' MC12$VarY;  MC21$VarY
#' \donttest{
#' # Truncated Normal distribution
#' set.seed(1234)
#' MC31 = mvtelliptical(a, b, mu, Sigma, "Normal", n=1e4)
#' MC32 = mvtelliptical(a, b, mu, Sigma, "Normal", n=1e6) # More precision}
#' @references{
#'   \insertRef{fang2018symmetric}{relliptical}
#'
#'   \insertRef{galarza2020moments}{relliptical}
#'
#'   \insertRef{valeriano2021moments}{relliptical}
#' }
#'
#' @import Ryacas
#' @importFrom FuzzyNumbers.Ext.2 is.decreasing
#' @importFrom matrixcalc is.positive.definite is.symmetric.matrix
#' @importFrom methods is
#' @importFrom stats qchisq uniroot
#' @importFrom Rdpack reprompt
#'
#' @export mvtelliptical
#' @export rtelliptical


mvtelliptical = function(lower, upper=rep(Inf,length(lower)), mu=rep(0,length(lower)),
                         Sigma=diag(length(lower)), dist="Normal", nu=NULL, n=1e4,
                         burn.in=0, thinning=3){

  #---------------------------------------------------------------------#
  #                              Validations                            #
  #---------------------------------------------------------------------#

  # Validating mu, Sigma, lower and upper dimensions
  if (!(is.vector(lower) | is.matrix(lower)) | any(is.na(lower)) | !is.numeric(lower)) stop("lower is not specified or contains NA")
  if (!(is.vector(upper) | is.matrix(upper)) | any(is.na(upper)) | !is.numeric(upper)) stop("upper is not specified or contains NA")
  if (length(c(lower))!=length(c(upper))) stop ("lower bound must have same dimension than upper")
  if (ncol(as.matrix(lower))>1 | ncol(as.matrix(upper))>1) stop ("lower and upper must have just one column")
  if(!all(lower<upper)) stop ("lower must be smaller than upper")

  if (!all(is.finite(mu)) | !is.numeric(mu) | !(is.vector(mu) | is.matrix(mu))) stop ("mu is not a numeric vector or contains NA")
  if (!all(is.finite(Sigma)) | !is.numeric(Sigma)) stop ("Sigma is not specified or contains NA")
  if (!is.matrix(Sigma)) { Sigma = as.matrix(Sigma) }

  if (length(c(mu)) == 1){
    if (length(c(Sigma)) != 1) stop ("Unconformable dimensions between mu and Sigma")
    if (Sigma <= 0) stop ("Sigma must be a positive real number")
  } else {
    if (ncol(as.matrix(mu))>1) stop("mu must have just one column")
    if (ncol(Sigma) != length(c(mu))) stop ("Unconformable dimensions between mu and Sigma")
    if ( !is.symmetric.matrix(Sigma) ){
      stop ("Sigma must be a square symmetrical real positive-definite matrix")
    } else {
      if ( !is.positive.definite(Sigma) ) stop ("Sigma must be a real positive-definite matrix")
    }
  }

  # Validating MCMC parameters
  if (length(c(n))>1 | !is.numeric(n)) stop("n must be an integer")
  if (n%%1!=0 | n<2) stop ("n must be an integer")
  if (length(c(burn.in))>1 | !is.numeric(burn.in)) stop("burn.in must be a non-negative integer")
  if (burn.in%%1!=0 | burn.in<0) stop ("burn.in must be a non-negative integer")
  if (length(c(thinning))>1 | !is.numeric(thinning)) stop("thinning must be a non-negative integer")
  if (thinning%%1!=0 | thinning<1) stop ("thinning must be a non-negative integer")

  if (!is.null(dist)){

    if (dist=="Normal"){
      output = Nmoment(mu, Sigma, lower, upper, n, burn.in, thinning)
    } else {

      if (dist=="t"){
        if (!is.numeric(nu) | length(c(nu))>1){
          stop ("Degrees of freedom (nu) must be a positive number")
        } else {
          if (nu <= 0){
            stop ("Degrees of freedom (nu) must be a positive number")
          } else {
            if (nu < 300){ # t distribution
              output = Tmoment(mu, Sigma, nu, lower, upper, n, burn.in, thinning)
            } else { # Normal distribution
              output = Nmoment(mu, Sigma, lower, upper, n, burn.in, thinning)
            }
          }
        }
      } else {

        if (dist=="PVII"){
          if (!is.numeric(nu)){
            stop ("The vector of parameters nu must be provided for the PVII case")
          } else {
            if (length(c(nu))!=2){
              stop ("The vector of parameters nu must be of length 2")
            } else {
              if (nu[1] <= length(c(mu))/2 | nu[2] <= 0){
                stop("The first element of nu must be greater than p/2 and the second element must be non-negative")
              } else {
                output = PVIImoment(mu, Sigma, nu[1], nu[2], lower, upper, n, burn.in, thinning)
              }
            }
          }
        } else {

          if (dist=="PE"){
            if (!is.numeric(nu) | length(c(nu))>1){
              stop ("Kurtosis parameter (nu) must be a positive number")
            } else {
              if (nu <= 0){
                stop ("Kurtosis parameter (nu) must be a positive number")
              } else {
                if (nu != 1){ # Power exponential distribution
                  output = PEmoment(mu, Sigma, nu, lower, upper, n, burn.in, thinning)
                } else { # Normal distribution
                  output = Nmoment(mu, Sigma, lower, upper, n, burn.in, thinning)
                }
              }
            }
          } else {

            if (dist=="Slash"){
              if (!is.numeric(nu) | length(c(nu))>1){
                stop ("Degrees of freedom (nu) must be non-negative")
              } else {
                if (nu <= 0){
                  stop ("Degrees of freedom (nu) must be non-negative")
                } else {
                  output = Slashmoment(mu, Sigma, nu, lower, upper, n, burn.in, thinning)
                }
              }
            } else {

              if (dist=="CN"){
                if (!is.numeric(nu)){
                  stop ("The vector of parameters nu must be provided for the CN case")
                } else {
                  if (length(c(nu)) != 2){
                    stop ("The vector of parameters nu must be of length 2")
                  } else {
                    if (!all(nu > 0 & nu < 1)){
                      stop ("The values of nu must be between 0 and 1")
                    } else {
                      output = CNmoment(mu, Sigma, nu[1], nu[2], lower, upper, n, burn.in, thinning)
                    }
                  }
                }
              } else {
                stop ("The dist values are Normal, t, PE, PVII, Slash, CN")
              } # End CN
            } # End Slash
          } # End PE
        } # End PVII
      } # End t
    } # End Normal
  } else {
    stop ("The dist values are Normal, t, PE, PVII, Slash, CN")
  }

  return(output)
}
