##
#' Modified Bayesian Information Criterion
#'
#' Calculate the modified Bayesian information criterion for estimated model.
#'
#'
#' @param beta the estimated coefficients.
#' @param Y the response.
#' @param X design matrix with the same order of the columns in \code{beta}.
#'
#' @return Returns an object with
#' @return \item{BIC}{a numeric value with the corresponding BIC.}
#' @return \item{K}{the corresponding number of groups.}
#'
#' @export mBIC
#' @seealso BIC
#' @references {'Pairwise Fusion Approach Incorporating Prior Constraint Information' by Yaguang Li}


mBIC <- function(beta, Y, X) {

  n <- dim(X)[1]
  p <- dim(X)[2]

  rss <- norm(Y - X %*% beta, type = "2")^2
  bb <- beta[beta != 0]
  nnz <- length(bb)
  K <- length(unique(bb))

  BIC <- log(rss/n) + log(p)/n * log(log(n)) * (K + nnz)

  return(list(BIC = BIC, K = K))

}

