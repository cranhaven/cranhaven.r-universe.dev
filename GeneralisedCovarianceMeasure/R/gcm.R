#' Test for Conditional Independence Based on the Generalized Covariance Measure (GCM)
#'
#' @param X A (nxp)-dimensional matrix (or data frame) with n observations of p variables.
#' If set to NULL, resid.XonZ has to be provided. 
#' @param Y A (nxp)-dimensional matrix (or data frame) with n observations of p variables.
#' If set to NULL, resid.YonZ has to be provided. 
#' @param Z A (nxp)-dimensional matrix (or data frame) with n observations of p variables.
#' If set to NULL and resid.XonZ set to NULL, then resid.XonZ is set to X.
#' If set to NULL and resid.YonZ set to NULL, then resid.YonZ is set to Y.
#' @param alpha Significance level of the test.
#' @param regr.method A string indicating the regression method that is used. Currently implemented are 
#' "gam", "xgboost", "kernel.ridge". The regression is performed only if 
#' not both resid.XonZ and resid.YonZ are set to NULL.
#' @param regr.pars Some regression methods require a list of additional options.
#' @param plot.residuals A Boolean indicating whether some plots should be shown.
#' @param nsim An integer indicating the number of bootstrap samples used to approximate the null distribution of the
#' test statistic.
#' @param resid.XonZ It is possible to directly provide the residuals instead of performing a regression. 
#' If not set to NULL, X is ignored.
#' If set to NULL, the regression method specified in regr.method is used. 
#' @param resid.YonZ It is possible to directly provide the residuals instead of performing a regression. 
#' If not set to NULL, Y is ignored.
#' If set to NULL, the regression method specified in regr.method is used. 
#' 
#' @return The function tests whether X is conditionally independent of Y given Z. The output is a list containing
#' \itemize{
#' \item \code{p.value}: P-value of the test. 
#' \item \code{test.statistic}: Test statistic of the test.
#' \item \code{reject}: Boolean that is true iff p.value < alpha.
#' }
#' 
#' @references
#' Please cite the following paper.
#' Rajen D. Shah, Jonas Peters: 
#' "The Hardness of Conditional Independence Testing and the Generalised Covariance Measure"
#' Annals of Statistics 48(3), 1514--1538, 2020.
#' 
#' @examples 
#' set.seed(1)
#' n <- 250 
#' Z <- 4*rnorm(n) 
#' X <- 2*sin(Z) + rnorm(n)
#' Y <- 2*sin(Z) + rnorm(n)
#' Y2 <- 2*sin(Z) + X + rnorm(n)
#' gcm.test(X, Y, Z, regr.method = "gam")
#' gcm.test(X, Y2, Z, regr.method = "gam")
#' 
#' @export
gcm.test <- function(X = NULL, Y = NULL, Z = NULL, alpha = 0.05, regr.method = "xgboost", regr.pars = list(),
                     plot.residuals = FALSE, nsim=499L, resid.XonZ=NULL, resid.YonZ=NULL) {
  if (is.null(Z)) {
    if(is.null(resid.XonZ)){
      resid.XonZ <- X
    }
    if(is.null(resid.YonZ)){
      resid.YonZ <- Y
    }
  } else {
    if (is.null(resid.XonZ)) {
      if(is.null(X)){
        stop("Either X or resid.XonZ must be provided.")
      }
      resid_func <- function(V) comp.resids(V, Z, regr.pars, regr.method)
      if (is.matrix(X)) {
        # For KRR this approach should be much faster as the kernel matrix doesn't change
        # for each of the regressions
        resid.XonZ <- apply(X, 2, resid_func)
      } else {
        resid.XonZ <- resid_func(X)
      }
    }
    if (is.null(resid.YonZ)) {
      if(is.null(Y)){
        stop("Either Y or resid.YonZ must be provided.")
      }
      resid_func <- function(V) comp.resids(V, Z, regr.pars, regr.method)
      if (is.matrix(Y)) {
        resid.YonZ <- apply(Y, 2, resid_func)
      } else {
        resid.YonZ <- resid_func(Y)
      }
    }
  }
  nn <- NA
  if (NCOL(resid.XonZ) > 1 || NCOL(resid.YonZ) > 1) {
    d_X <- NCOL(resid.XonZ); d_Y <- NCOL(resid.YonZ)
    #n <- nrow(resid.XonZ)
    nn <- NROW(resid.XonZ)
    #R_mat <- rep(resid.XonZ, times=d_Y) * as.numeric(resid.YonZ[, rep(seq_len(d_X), each=d_Y)])
    R_mat <- rep(resid.XonZ, times=d_Y) * as.numeric(as.matrix(resid.YonZ)[, rep(seq_len(d_Y), each=d_X)])
    dim(R_mat) <- c(nn, d_X*d_Y)
    R_mat <- t(R_mat)
    #R_mat < R_mat / sqrt((rowMeans(R_mat^2) - rowMeans(R_mat)^2))
    R_mat <- R_mat / sqrt((rowMeans(R_mat^2) - rowMeans(R_mat)^2))
    
    # there are faster approaches if nsim > nn
    
    test.statistic <- max(abs(rowMeans(R_mat))) * sqrt(nn)
    test.statistic.sim <- apply(abs(R_mat %*% matrix(rnorm(nn*nsim), nn, nsim)), 2, max) / sqrt(nn)
    p.value <- (sum(test.statistic.sim >= test.statistic)+1) / (nsim+1)
    
    #plotting
    if(plot.residuals){
      par(mfrow = c(NCOL(resid.XonZ), NCOL(resid.YonZ)))
      for(ii in 1:NCOL(resid.XonZ)){
        for(jj in 1:NCOL(resid.YonZ)){
          plot(resid.XonZ[,ii], resid.YonZ[,jj], main = "scatter plot of residuals")
        }
      }
      par(mfrow = c(1,1))
    }
  } else {
    nn <- ifelse(is.null(dim(resid.XonZ)), length(resid.XonZ), dim(resid.XonZ)[1])
    R <- resid.XonZ * resid.YonZ
    R.sq <- R^2
    meanR <- mean(R)
    test.statistic <- sqrt(nn) * meanR / sqrt(mean(R.sq) - meanR^2)
    p.value <- 2 * pnorm(abs(test.statistic), lower.tail = FALSE)
    if(plot.residuals){
      plot(resid.XonZ, resid.YonZ, main = "scatter plot of residuals")
    }
  }
  
  return(list(p.value = p.value, test.statistic = test.statistic, reject = (p.value < alpha)) )
  
}


#' Wrapper function to computing residuals from a regression method
#'
#' This function is used for the GCM test. 
#' Other methods can be added.
#' 
#' @param V A (nxp)-dimensional matrix (or data frame) with n observations of p variables.
#' @param Z A (nxp)-dimensional matrix (or data frame) with n observations of p variables.
#' @param regr.method A string indicating the regression method that is used. Currently implemented are 
#' "gam", "xgboost", "kernel.ridge", "nystrom". The regression is performed only if 
#' not both resid.XonZ and resid.YonZ are set to NULL.
#' @param regr.pars Some regression methods require a list of additional options.
#' 
#' @return Vector of residuals. 
#' 
#' @references
#' Please cite the following paper.
#' Rajen D. Shah, Jonas Peters: 
#' "The Hardness of Conditional Independence Testing and the Generalised Covariance Measure"
#' Annals of Statistics 48(3), 1514--1538, 2020.
#' 
#' @examples 
#' set.seed(1)
#' n <- 250 
#' Z <- 4*rnorm(n) 
#' X <- 2*sin(Z) + rnorm(n)
#' res <- comp.resids(X, Z, regr.pars = list(), regr.method = "gam")
#' 
#' @export
# V should be one of X or Y
comp.resids <- function(V, Z, regr.pars, regr.method) {
  V <- as.numeric(V)
  switch(regr.method,
         "gam" = {
           mod.VonZ <- train.gam(Z, V, pars = regr.pars) 
         },
         "xgboost" = {
           mod.VonZ <- train.xgboost(Z, V, pars = regr.pars) 
         },
         "kernel.ridge" = {
           mod.VonZ <- train.krr(Z, V, pars = regr.pars) 
#         },
#         "nystrom" = {
#           mod.VonZ <- train.nystrom(Z, V) 
         }
  )
  return(mod.VonZ$residuals)
}



