#' varjmcm: Estimations for the Covariance of Estimated Parameters in Joint Mean-Covariance Models
#'
#' The package provides estimations of the covariance of estimated parameters in joint
#' mean-covariance models, which is fitted in 'jmcm' package. Two methods are available.
#' \code{bootcovjmcm} calculates the covariance estimation via a bootstrap based method. \code{covjmcm} uses explicit formula, i.e. the inverse of the estimated Fisher's information, to calculate the covariance estimation.
#' The bootstrap method may need large number of replications and thus may be time consuming.
#' The explicit formula in the second method is asymptotically correct, and thus is valid only when the sample size is large.
#'
#' @seealso \code{\link{covjmcm}} and \code{\link{bootcovjmcm}} for more details and examples.
#'
#' @references [1] Pan J, Pan Y (2017). "jmcm: An R Package for Joint Mean-Covariance Modeling of Longitudinal Data."  Journal of Statistical Software, 82(9), 1--29.
#' @references [2] Pourahmadi, M., "Maximum likelihood estimation of generalised linear models for multivariate normal covariance matrix," Biometrika
#' 87(2), 425–435 (2000).
#' @references [3] M. Maadooliat, M. Pourahmadi and J. Z. Huang, "Robust estimation of the correlation
#' matrix of longitudinal data", Statistics and Computing 23, 17-28, (2013).
#' @references [4] W. Zhang, C. Leng, and C. Y. Tang(2015), "A joint modelling approach for longitudinal studies,"
#' Journal of the Royal Statistical Society. Series B. 77, 219-238.
#' @docType package
#' @name varjmcm
NULL
