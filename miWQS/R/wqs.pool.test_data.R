#' Combining WQS Regression Estimates
#'
#' @docType data
#' @name wqs.pool.test
#' @usage data(wqs.pool.test)
#' @keywords datasets
#'
#' @description \emph{wqs.pool.test} was produced to demonstrate \code{pool.mi}(). First, the univariate Bayesian imputation approach (using \code{impute.univariate.bayesian.mi}) imputed the \code{X.bdl} element of \code{simdata87} multiple times to form an imputed X array. Multiple WQS regressions were run on the imputed X array to produce an array of WQS parameter estimates, \emph{wqs.pool.test}.
#'
#' @format An array of 16 x 2 x 3, with \itemize{
#' \item 16 parameters as the rows (The 14 weights, intercept, and WQS estimate of a WQS model),
#' \item 2 refers to the parameters of mean and standard deviation
#' \item K=3 complete imputed datasets.
#' }
#'
#' @seealso \code{pool.mi}
#'
#' @examples
#' wqs.pool.test <- data(wqs.pool.test)
#' # stage_3_pool_mi_example
NULL

# Code to generate wqs.pool.test -----------------------------------------------
# #data(simdata87)
# #data(result.imputed.UBMI)
#  y <- simdata87$y.scenario
#  X.imputed <- result.imputed.UBMI$X.imputed
#  K <- dim(X.imputed)[[3]] wqs.pool.test <- array(NA, dim = c(16, 2, K),
#                                 dimnames = list(NULL, c("Estimate",  "Std.Error"),  paste0("Imputed.",  1:K) ))   #same as X.imputed
#  for (k in 1:K) {
#     wqs.imputed <- estimate.wqs(
#     y = y, X.imputed[, , k], Z = NULL,
#     proportion.train = 0.5,  n.quantiles = 4,
#                                place.bdls.in.Q1 = FALSE, B = 30,
#                                b1.pos = TRUE,
#                                family = "binomial", debug = FALSE)
#
#    # Keep estimates for pooling
#    a <- rbind(
#      wqs.imputed$processed.weights,   #weights
#      summary(wqs.imputed$fit)$coefficients[ ,1:2]    #Intercept and WQS, covariates.
#    )
#    wqs.pool.test[ , , k] <- a
#    wqs.pool.test <- wqs.pool.test[1:nrow(a), , , drop = FALSE]   #Get rid of the extras.
#    dimnames(wqs.pool.test)[[1]] <- dimnames(a)[[1]]
#  }  #end K draw loop
#
