#' Variable Selection via Thresholded Partial Correlation
#'
#' These are the main selection functions with fixed significance level \code{s} and \code{constant}.
#' The function \code{TPC} implements the thresholded partial correlation (TPC) approach to selecting important 
#' variables in linear models of Li et al. (2017).
#' The function \code{TPC_pl} implements the thresholded partial correlation approach to selecting important 
#' variables in partial linear models of Liu et al. (2018). 
#' This function also extends the PC-simple algorithm of Bühlmann et al. (2010) to partial linear models.
#'
#' @param y response vector;
#' @param x covariate matrix;
#' @param s a numeric value that used as significance level(s) for partial
#' correlation test.
#' @param constant a value that used as the tuning constant for partial
#' correlation test. \code{constant} is treated as 1 when method is "simple".
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
#' TPC.fit = TPC(y,x,0.05,1,method="threshold")
#' TPC.fit$beta
#'
#' @import stats
#' @importFrom utils combn
#' @export
TPC <- function(y, x, s = 0.05, constant = 1, method = "threshold") {

  #validate inputs
  stopifnot(is.numeric(y),is.vector(y) || (is.matrix(y) && NCOL(y)==1 ))
  stopifnot(is.numeric(x),is.matrix(x))
  stopifnot( (is.numeric(s)), (s > 0) )
  stopifnot( (is.numeric(constant)), (constant > 0)  )

  p <- NCOL(x)
  n <- length(y)
  y_x_cor <- cor(y,x)

  if (method == "threshold") {
    estkur <- estkurtosis(x)
    cutoff <- constant * qnorm(1 - s / 2, mean = 0, sd = (1 + estkur) ^ (1/2))
  } else if (method == "simple") {
    cutoff <- qnorm(1 - s / 2)
  } else{
    cutoff <- qnorm(1 - s / 2)
  }


  activeindexnew <- 1:p
  size <- length(activeindexnew)

  #######################################################################################
  # marginal correlation
  activeindexnew <-
    activeindexnew[abs(psych::fisherz(y_x_cor)) * sqrt(n - 3) > cutoff]

  #######################################################################################
  # partial correlations
  activeindexold <- activeindexnew
  size <- length(activeindexnew)
  loopnum <- 0

  #######################################################################################
  while (size > loopnum) {
    loopnum <- loopnum + 1

    xindex <- 1
    while (xindex <= size) {
      #obtain cor matrix
      newcovar <- x[, activeindexnew]
      corMatrix <- cor(cbind(y, newcovar))

      #loop over combinations
      comb <- t(combn(size, loopnum))
      #remove comb that has x
      nox_index = rowSums(comb==xindex)==0
      comb <- as.matrix(comb[nox_index,])
      if(sum(nox_index)==1){comb = t(comb)}
      cindex <- 1
      activeindexold <- activeindexnew
      while (cindex <= nrow(comb)) {
        cond <- comb[cindex, ]
        judge <- Threshold(1, xindex+1, cond+1, corMatrix, n, cutoff)
        if (judge == TRUE){ # if independent, then delete
          activeindexnew <- activeindexold[-xindex]
          #print(xindex)
          break
        }else{cindex <- cindex + 1}
      }
      #only when no var was removed, move to next index.
      if (length(activeindexnew) == length(activeindexold)) {
        xindex <- xindex + 1
      }
      #update size
      size <- length(activeindexnew)
    }


  }

  selected <- x[, activeindexold]
  betapred <- stats::lm(y ~ selected - 1)

  betahat_pc <- rep(0, p)
  betahat_pc[activeindexold] <- betapred$coef

  TPC_ojb <- betapred
  TPC_ojb$beta=betahat_pc
  TPC_ojb$selected_index = activeindexnew
  class(TPC_ojb) <- c("lm","TPC")

  return(TPC_ojb)
}

#' @rdname TPC
#' @export
TPCselect <- TPC
