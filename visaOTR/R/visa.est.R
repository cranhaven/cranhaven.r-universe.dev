#' @title Valid Improved Sparsity A-Learning for Optimal Treatment Decision
#' @param y Vector of response (the larger the better)
#' @param x Matrix of model covariates.
#' @param a Vector of treatment received. It is a 0/1 index vector representing the subject is in control/treatment group. For details see Example section.
#' @param IC Information criterion used in determining the regularization parameter. Users can choose among \code{BIC}, \code{CIC} and \code{VIC}.
#' @param kap The model complexity penalty used in the information criteria. By default, kappa = 1 if BIC or CIC is used and kap = 4 if VIC is used.
#' @param lambda.list A list of regularization parameter values. Default is exp(seq(-3.5, 2, 0.1))
#' @param refit logical. If \code{TRUE}, the coefficients should be refitted using A-learning estimating equation. Default is \code{TRUE}.
#' @details See the paper provided in Reference section.
#'
#' @return an object of class "visa" is a list containing at least the following components:
#' \item{beta.est}{A vector of coefficients of optimal treatment regime.}
#' \item{pi.est}{A vector of estimated propensity score.}
#' \item{h.est}{A vector of estimated baseline function.}
#'
#' @references
#' {Shi, C., Fan, A., Song, R. and Lu, W. (2018) High-Dimensional A-Learing for Optimal Dynamic Treatment Regimes. \emph{Annals of Statistics,} \bold{ 46:} 925-957. DOI:10.1214/17-AOS1570}
#'
#' {Shi, C.,Song, R. and Lu, W. (2018) Concordance and Value Information Criteria for Optimal Treatment Decision. \emph{Annals of Statistics,} \bold{ 49:} 49-75. DOI:10.1214/19-AOS1908}
#'
#' {Zhan, Z. and Zhang, J. (2022+) Valid Improved Sparsity A-learning for Optimal Treatment Decision. Under review.}
#'
#' @export
#' @import stats Rglpk
#' @importFrom kernlab ksvm predict
#' @importFrom e1071 svm
#' @importFrom xgboost xgboost xgb.DMatrix
#' @importFrom randomForest randomForest
#' @importFrom mboost glmboost Binomial
#' @importFrom Matrix Matrix
#' @examples
#' data(visa_SimuData)
#' y = visa_SimuData$y
#' a = visa_SimuData$a
#' x = visa_SimuData$x
#' # estimation
#' result <- visa.est(y, x, a, IC = "BIC", lambda.list = c(0.1, 0.5))
#' result$beta.est
#' result$pi.est
#' result$h.est
visa.est <- function(y, x, a, IC = c("BIC", "CIC", "VIC"), kap = NULL,
                     lambda.list = exp(seq(-3.5, 2, 0.1)), refit = TRUE)
{
  if (missing(IC))
    IC <- "CIC"
  if (is.null(kap)) {
    if (IC == "CIC") {
      kap <- 1
    }
    else if (IC == "BIC") {
      kap <- 1
    }
    else {
      kap <- 4
    }
  }
  n <- length(y)
  if (!all(a %in% c(0, 1)))
    stop("Treatment must be binary variable!")
  if (length(y) < 1)
    stop("empty model")
  ## Compute the propensity score
  pi.weight <- visa.weight(x, a, family = 'binomial')$weight
  pi.est <- as.vector(visa.mix(x, a, family='binomial', weight = pi.weight))
  ## Compute the baseline
  h.weight <- visa.weight(x, y, subset = which(a==1), family = 'gaussian')$weight
  h.est <- as.vector(visa.mix(x, y, family='gaussian', weight = h.weight))

  beta.est <- visa.IC.Dantzig(x, y, a, pi.est = pi.est,
                              h.est = h.est, lambda.list = lambda.list, IC = IC,
                              kap = kap, refit = refit)
  object <- list(beta.est = beta.est, pi.est = pi.est, h.est = h.est)
  return(object)
}
