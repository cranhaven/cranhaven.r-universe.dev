#' @title The Lasso Distribution
#' @name LassoDistribution
#' @aliases zlasso dlasso plasso qlasso rlasso elasso vlasso mlasso MillsRatio Modified_Hans_Gibbs Modified_PC_Gibbs
#' @description
#' Provides functions related to the Lasso distribution, including the normalizing constant,
#' probability density function, cumulative distribution function, quantile function, and
#' random number generation for given parameters \code{a}, \code{b}, and \code{c}.
#' Additional utilities include the Mills ratio, expected value, and variance of the distribution.
#' The package also implements modified versions of the Hans and Park–Casella Gibbs sampling algorithms
#' for Bayesian Lasso regression.
#' 
#' @usage
#' zlasso(a, b, c, logarithm)
#' dlasso(x, a, b, c, logarithm)
#' plasso(q, a, b, c)
#' qlasso(p, a, b, c)
#' rlasso(n, a, b, c)
#' elasso(a, b, c)
#' vlasso(a, b, c)
#' mlasso(a, b, c)
#' MillsRatio(d)
#' Modified_Hans_Gibbs(X, y, a1, b1, u1, v1,
#'               nsamples, beta_init, lambda_init, sigma2_init, verbose)
#' Modified_PC_Gibbs(X, y, a1, b1, u1, v1, 
#'               nsamples, lambda_init, sigma2_init, verbose)               
#'          
#'
#' @details
#' If \eqn{X \sim \text{Lasso}(a, b, c)} then its density function is:
#' \deqn{
#' p(x;a,b,c) = Z^{-1} \exp\left(-\frac{1}{2} a x^2 + bx - c|x| \right)
#' }
#' where \eqn{x \in \mathbb{R}}, \eqn{a > 0}, \eqn{b \in \mathbb{R}}, \eqn{c > 0}, and \eqn{Z} is the normalizing constant.
#'
#' More details are included for the CDF, quantile function, and normalizing constant in the original documentation.
#'
#' @param x,q Vector of quantiles (vectorized).
#' @param p Vector of probabilities.
#' @param a  Vector of precision parameter which must be non-negative.
#' @param b  Vector of off set parameter.
#' @param c  Vector of tuning parameter which must be non-negative values.
#' @param n Number of observations.
#' @param logarithm Logical. If \code{TRUE}, probabilities are returned on the log scale.
#' @param d A scalar numeric value. Represents the point at which the Mills ratio is evaluated.
#' @param X Design matrix (numeric matrix).
#' @param y Response vector (numeric vector).
#' @param a1 Shape parameter of the prior on \eqn{\lambda^2}.
#' @param b1 Rate parameter of the prior on \eqn{\lambda^2}.
#' @param u1 Shape parameter of the prior on \eqn{\sigma^2}.
#' @param v1 Rate parameter of the prior on \eqn{\sigma^2}.
#' @param nsamples Number of Gibbs samples to draw.
#' @param beta_init Initial value for the model parameter \eqn{\beta}.
#' @param lambda_init Initial value for the shrinkage parameter \eqn{\lambda^2}.
#' @param sigma2_init Initial value for the error variance \eqn{\sigma^2}.
#' @param verbose Integer. If greater than 0, progress is printed every \code{verbose} iterations during sampling. Set to 0 to suppress output.
#' @return
#' \itemize{
#'
#'   \item \code{zlasso}, \code{dlasso}, \code{plasso}, \code{qlasso}, \code{rlasso}, \code{elasso}, \code{vlasso}, \code{mlasso}, \code{MillsRatio}: 
#'   return the corresponding scalar or vector values related to the Lasso distribution and a numeric value representing the Mills ratio.
#'   \item \code{Modified_Hans_Gibbs}: returns a list containing:
#'     \describe{
#'       \item{\code{mBeta}}{Matrix of MCMC samples for the regression coefficients \eqn{\beta}, with \code{nsamples} rows and \code{p} columns.}
#'       \item{\code{vsigma2}}{Vector of MCMC samples for the error variance \eqn{\sigma^2}.}
#'       \item{\code{vlambda2}}{Vector of MCMC samples for the shrinkage parameter \eqn{\lambda^2}.}
#'       \item{\code{mA}}{Matrix of sampled values for parameter \eqn{a_j} of the Lasso distribution for each \eqn{\beta_j}.}
#'       \item{\code{mB}}{Matrix of sampled values for parameter \eqn{b_j} of the Lasso distribution for each \eqn{\beta_j}.}
#'       \item{\code{mC}}{Matrix of sampled values for parameter \eqn{c_j} of the Lasso distribution for each \eqn{\beta_j}.}
#'     }
#'     \item \code{Modified_PC_Gibbs}: returns a list containing:
#'     \describe{
#'       \item{\code{mBeta}}{Matrix of MCMC samples for the regression coefficients \eqn{\beta}.}
#'       \item{\code{vsigma2}}{Vector of MCMC samples for the error variance \eqn{\sigma^2}.}
#'       \item{\code{vlambda2}}{Vector of MCMC samples for the shrinkage parameter \eqn{\lambda^2}.}
#'       \item{\code{mM}}{Matrix of estimated means of the full conditional distributions of each \eqn{\beta_j}.}
#'       \item{\code{mV}}{Matrix of estimated variances of the full conditional distributions of each \eqn{\beta_j}.}
#'       \item{\code{va_til}}{Vector of estimated shape parameters for the full conditional inverse-gamma distribution of \eqn{\sigma^2}.}
#'       \item{\code{vb_til}}{Vector of estimated rate parameters for the full conditional inverse-gamma distribution of \eqn{\sigma^2}.}
#'       \item{\code{vu_til}}{Vector of estimated shape parameters for the full conditional inverse-gamma distribution of \eqn{\lambda^2}.}
#'       \item{\code{vv_til}}{Vector of estimated rate parameters for the full conditional inverse-gamma distribution of \eqn{\lambda^2}.}
#'     }
#'     
#' }
#' 
#' @seealso \code{\link{normalize}} for preprocessing input data before applying the samplers.
#'
#' @examples
#' a <- 2; b <- 1; c <- 3
#' x <- seq(-3, 3, length.out = 1000)
#' plot(x, dlasso(x, a, b, c, logarithm = FALSE), type = 'l')
#'
#' r <- rlasso(1000, a, b, c)
#' hist(r, breaks = 50, probability = TRUE, col = "grey", border = "white")
#' lines(x, dlasso(x, a, b, c, logarithm = FALSE), col = "blue")
#'
#' plasso(0, a, b, c)
#' qlasso(0.25, a, b, c)
#' elasso(a, b, c)
#' vlasso(a, b, c)
#' mlasso(a, b, c)
#' MillsRatio(2)
#' 
#' 
#' 
#' 
#' # The Modified_Hans_Gibbs() function uses the Lasso distribution to draw 
#' # samples from the full conditional distribution of the regression coefficients.
#' 
#' y <- 1:20
#' X <- matrix(c(1:20,12:31,7:26),20,3,byrow = TRUE)
#' 
#' a1 <- b1 <- u1 <- v1 <- 0.01
#' sigma2_init <- 1
#' lambda_init <- 0.1
#' beta_init <- rep(1, ncol(X))
#' nsamples <- 1000
#' verbose <- 100
#' 
#' Output_Hans <- Modified_Hans_Gibbs(
#'                 X, y, a1, b1, u1, v1,
#'                 nsamples, beta_init, lambda_init, sigma2_init, verbose
#' )
#' 
#' colMeans(Output_Hans$mBeta)
#' mean(Output_Hans$vlambda2)
#' 
#' 
#' Output_PC <- Modified_PC_Gibbs(
#'                X, y, a1, b1, u1, v1, 
#'                nsamples, lambda_init, sigma2_init, verbose)
#' 
#' colMeans(Output_PC$mBeta)
#' mean(Output_PC$vlambda2)
#' 
#' @useDynLib BayesianLasso, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' 
#' @rdname LassoDistribution
#' @export zlasso
#' @export dlasso
#' @export plasso
#' @export qlasso
#' @export rlasso
#' @export elasso
#' @export vlasso
#' @export mlasso
#' @export Modified_Hans_Gibbs
#' @export Modified_PC_Gibbs
#' @export MillsRatio
NULL  # no function definition here
