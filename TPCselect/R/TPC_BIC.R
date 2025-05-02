#' Variable Selection via Thresholded Partial Correlation
#'
#' Use BIC to select the best \code{s} and \code{constant} over grids.
#'
#' @param y response vector;
#' @param x covariate matrix;
#' @param s a value or a vector that used as significance level(s) for partial
#' correlation test. BIC will be used to select the best \code{s}.
#' @param constant a value or a vector that used as the tuning constant for partial
#' correlation test. BIC will be used to select the best \code{constant}.
#' \code{constant} is treated as 1 when method is "simple".
#' @param method the method to be used; default set as method = "threshold";
#'  "simple" is also available.
#'
#'
#' @return TPC.object a TPC object, which extends the \code{lm} object. New attributes are:
#' \itemize{
#'   \item beta - the fitted coefficients
#'   \item selected_index - the selected coefficients indices
#' }
#'
#' @examples
#' #generate sample data
#' p = 200
#' n = 200
#' truebeta <- c(c(3,1.5,0,0,2),rep(0,p-5))
#' rho = 0.3
#' sigma =  matrix(0,p+1,p+1)
#' for(i in 1:(p+1)){
#'   for(j in 1:(p+1)){
#'     sigma[i,j] = rho^(abs(i-j))
#'   }
#' }
#' x_error = 0.9*MASS::mvrnorm(n,rep(0,p+1),sigma) + 0.1*MASS::mvrnorm(n,rep(0,p+1),9*sigma)
#' x = x_error[,1:p]
#' error = x_error[,p+1]
#' y = x%*%truebeta + error
#'
#' #perform variable selection via partial correlation
#' TPC.fit = TPC_BIC(y,x,0.05,c(1,1.5),method="threshold")
#' TPC.fit$beta
#'
#'
#' @export
TPC_BIC <- function(y, x, s = 0.05, constant = 1, method = "threshold") {
  p <- NCOL(x)
  if (method == "simple") {
    constant = c(1)
  } #ignore constant when using simple algorithm

  # step 1: BIC select
  criteria <- array(0, c(length(s), length(constant)))
  for (j in 1:length(s)) {
    for (k in 1:length(constant)) {
      pcresult <- TPC(y, x, s[j],constant[k], method)
      index  <- pcresult$selected_index
      newcovar <- x[, index]

      beta.fit <- as.matrix(stats::lm(y ~ newcovar - 1)$coef)
      fit <- x[, index] %*% beta.fit
      sigmalambda <- mean((y - fit) ^ 2)
      df <-  sum(beta.fit != 0)
      len <- length(y)
      criteria[j, k] <- log(sigmalambda) + df * log(len) / len
    }
  }

  #print(criteria)

  # step 2: determine the tuning parameter
  min_bic_index <- which(criteria == min(criteria), arr.ind = TRUE)
  alpha <- s[min_bic_index[1, 1]]
  constant <- constant[min_bic_index[1, 2]]


  # step 3: TPC algorithm
  index <- TPC(y, x, alpha,constant, method)$selected_index
  newcovar <- x[, index]
  betapred <- stats::lm(y ~ newcovar - 1)

  betahat_pc <- rep(0, p)
  betahat_pc[index] <- betapred$coef

  TPC_ojb <- betapred
  TPC_ojb$beta=betahat_pc
  TPC_ojb$selected_index = index
  class(TPC_ojb) <- c("lm","TPC")

  return(TPC_ojb)
}
