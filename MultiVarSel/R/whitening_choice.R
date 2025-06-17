#' This function helps to choose the best whitening strategy among the following types of dependence
#' modellings: AR1, ARMA, non parametric and without any whitening.
#'
#' @param  residuals the residuals matrix obtained by fitting a linear model to each column of the response matrix as if they were independent
#' @param  typeDeps character in c("AR1", "ARMA", "nonparam", "no_whitening") defining which dependence structure to use to whiten the residuals.
#' @param  pAR numerical, only use if typeDep = "ARMA", the parameter p for the ARMA(p, q) process
#' @param  qMA numerical, only use if typeDep = "ARMA", the parameter q for the ARMA(p, q) process
#' @param  threshold significance level of the test
#' @return It provides a table giving the p-values for the different whitening tests applied to the residuals multiplied on the right
#' by the inverse of the square root of the estimated covariance matrix.
#' If the p-value is small (in general smaller than 0.05)
#' it means that the hypothesis that each row of the  residuals "whitened" matrix is a white noise, is rejected.
#' @examples
#' data(copals_camera)
#' Y <- scale(Y[, 1:100])
#' X <- model.matrix(~ group + 0)
#' residuals <- lm(as.matrix(Y) ~ X - 1)$residuals
#' whitening_choice(residuals, c("AR1", "nonparam", "ARMA", "no_whitening"),
#'   pAR = 1, qMA = 1 )
#' @export
whitening_choice <-
  function(residuals, typeDeps = "AR1", pAR = 1, qMA = 0, threshold = 0.05) {
    get_pvalue <- function(typeDep) {
      square_root_inv_hat_Sigma <- whitening(residuals, typeDep, pAR = pAR, qMA = qMA)
      whitened_residuals <- residuals %*% square_root_inv_hat_Sigma
      pvalue <- whitening_test(whitened_residuals)
      return(pvalue)
    }

    Pvals <- sapply(typeDeps, get_pvalue)
    Decision <- ifelse(Pvals < threshold, "NO WHITE NOISE", "WHITE NOISE")
    names(Pvals)[which(names(Pvals) == "ARMA")] <- paste("ARMA", pAR, qMA, sep = " ")
    Result <- as.data.frame(cbind(Pvalue = round(Pvals, 3), Decision))

    return(Result)
  }
