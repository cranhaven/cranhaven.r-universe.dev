#' Conditional Mean Independence Tests
#'
#' \code{cmdm_test} tests conditional mean independence of \code{Y} given \code{X} conditioning on \code{Z},
#' where each contains one variable (univariate) or more variables (multivariate).
#' All tests are implemented as permutation tests.
#'
#' @param X A vector, matrix or data frame, where rows represent samples, and columns represent variables.
#' @param Y A vector, matrix or data frame, where rows represent samples, and columns represent variables.
#' @param Z A vector, matrix or data frame, where rows represent samples, and columns represent variables.
#' @param num_perm The number of permutation samples drawn to approximate the asymptotic distributions
#'   of mutual dependence measures.
#' @param type The type of conditional mean dependence measures, including
#' \itemize{
#'   \item \code{linmdd}: martingale difference divergence under a linear assumption; 
#'   \item \code{pmdd}: partial martingale difference divergence.
#' }
#' @param compute The computation method for martingale difference divergence, including
#' \itemize{
#'   \item \code{C}: computation implemented in C code;
#'   \item \code{R}: computation implemented in R code.
#' }
#' @param center The centering approach for martingale difference divergence, including
#' \itemize{
#'   \item \code{U}: U-centering which leads to an unbiased estimator;
#'   \item \code{D}: double-centering which leads to a biased estimator.
#' }
#'
#' @return \code{cmdm_test} returns a list including the following components:
#' \item{stat}{The value of the conditional mean dependence measure.}
#' \item{dist}{The p-value of the conditional mean independence test.}
#'
#' @references Shao, X., and Zhang, J. (2014).
#'   Martingale difference correlation and its use in high-dimensional variable screening.
#'   Journal of the American Statistical Association, 109(507), 1302-1318.
#'   \url{http://dx.doi.org/10.1080/01621459.2014.887012}.
#' @references Park, T., Shao, X., and Yao, S. (2015).
#'   Partial martingale difference correlation.
#'   Electronic Journal of Statistics, 9(1), 1492-1517.
#'   \url{http://dx.doi.org/10.1214/15-EJS1047}.    
#'
#' @importFrom stats lm
#'
#' @include mdd.R
#' @include pmdd.R
#'    
#' @export
#'
#' @examples
#' \dontrun{
#' # X, Y, Z are vectors with 10 samples and 1 variable
#' X <- rnorm(10)
#' Y <- rnorm(10)
#' Z <- rnorm(10)
#'
#' cmdm_test(X, Y, Z, type = "linmdd")
#'
#' # X, Y, Z are 10 x 2 matrices with 10 samples and 2 variables
#' X <- matrix(rnorm(10 * 2), 10, 2)
#' Y <- matrix(rnorm(10 * 2), 10, 2)
#' Z <- matrix(rnorm(10 * 2), 10, 2)
#'
#' cmdm_test(X, Y, Z, type = "pmdd")
#' }

cmdm_test <- function(X, Y, Z, num_perm = 500, type = "linmdd", compute = "C", center = "U") {
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  Z <- as.matrix(Z)

  n <- nrow(X)
  if (n != nrow(Y) || n != nrow(Z)) {
    stop("The dimensions of X, Y, Z do not agree.")
  }

  p <- ncol(X)
  q <- ncol(Y)
  r <- ncol(Z)

  if (type == "linmdd") {
  	V <- Y - lm(Y ~ Z - 1)$"fitted.values"
  	U <- cbind(X, Z)
  	cmdm_sample <- mdd(U, V, compute = compute, center = center)
  } else if (type == "pmdd") {
  	cmdm_sample <- pmdd(X, Y, Z)
  } else {
  	stop("Invalid type. Read ?cmdm_test for proper syntax.")
  }

  count <- 0
  for (i in 1:num_perm) {  
  	index_perm <- sample(n)

  	if (type == "linmdd") {
  	  U_perm <- cbind(X[index_perm, ], Z)
  	  cmdm_perm <- mdd(U_perm, V, compute = compute, center = center)
  	} else if (type == "pmdd") {
  	  cmdm_perm <- pmdd(X[index_perm, ], Y, Z)
  	}

  	if (cmdm_perm >= cmdm_sample) {
  	  count <- count + 1
  	}
  }

  return(list(stat = cmdm_sample, pval = count / num_perm))
}
