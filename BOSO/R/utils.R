#' BOSO Internal Functions
#'
#' Internal functions used by BOSO in the different steps of the algorithm
#'
#' @keywords internal
#' @name InternalFunctions
#' @return Internal outputs
#'
#'
NULL

#' Extract coefficients from a BOSO object
#' 
#' @param object Fitted 'BOSO' or 'BOSO.single' object
#' @param beta0 Force beta0 to appear (output of 'p+1' features)
#' @param ... extra arguments for future updates
#'   
#' @description This is an equivalent function to the one offered by 
#'   \code{\link[glmnet]{coef.glmnet}} for extraction of coefficients.
#'   
#' @return A `matrix` object with the corresponding beta values estimated.
#' 
#' @export coef.BOSO
#' @export

coef.BOSO = function(object, beta0=F, ...) {
  
  if (object$standardize){
    beta.mat <- c(object$my - colSums(diag(object$mx[-1] / object$sx[-1]) %*% object$betas[-1]), # intercept
                  object$betas[-1]/ object$sx[-1]) # de-scale betas
  } else {
    beta.mat <- object$betas / c(object$sx)
  }
  if (!object$intercept & length(beta.mat)>object$p & !object$standardize & !beta0){ beta.mat <- beta.mat[-1]}
  
  return(matrix(beta.mat,ncol = 1))
}


#' Predict function for BOSO object.
#' 
#' @param object Fitted 'BOSO' or 'BOSO.single' object
#' @param newx Matrix with new data for prediction with BOSO
#' @param ... extra arguments for future updates
#'   
#' @description This is an equivalent function to the one offered by 
#'   \code{\link[glmnet]{coef.glmnet}} for extraction of coefficients.
#'   
#' @return A `matrix` object with the corresponding beta values estimated.
#' 
#' @export predict.BOSO
#' @export

predict.BOSO = function(object, newx, ...) {
  if (missing(newx)) newx = object$x
  if (object$intercept | object$standardize) newx = cbind(rep(1,nrow(newx)),newx)
  return(newx %*% coef.BOSO(object))
}




#' Centering and scaling
#' 
#' Centering and scaling convenience function
#' based on the function from bestSubset
#' 

#' @rdname InternalFunctions
standardize = function(x, y, intercept=T, normalize=T, mx=NULL, my=NULL, sx=NULL) {
  x = as.matrix(x)
  y = as.numeric(y)
  n = nrow(x)
  p = ncol(x)
  
  if(!(is.null(mx)&&is.null(my)&&is.null(sx))){
    x = scale(x,mx,sx)
    y = y-my
  } else{
    if (intercept) {
      mx = colMeans(x)
      my = mean(y)
      x = scale(x,mx,FALSE)
      y = y-my
    } else {
      bx = rep(0,p)
      by = 0
    }
    if (normalize) {
      sx = sqrt(colSums(x^2))
      x = scale(x,FALSE,sx)
    } else {
      sx = rep(1,p)
    }
  }
  return(list(x=x,y=y,mx=mx,my=my,sx=sx))
}





#' Centering and scaling convenience function
#' 
#' @param object Fitted 'BOSO' or object
#' 
#' @param metric information criteria to be used
#' 
#' @param n Fitted 'BOSO' object
#' 
#' @param p Fitted 'BOSO'  object
#' 

#' @rdname InternalFunctions
ICscoreCalculation = function(object, IC, n, p) {
  if (missing(IC)) stop('Missing IC information criteria for calculation in BOSO')
  if (missing(n)) n = object$n
  if (missing(p)) p = object$p
  
  # set penalization for information criteria (eBIC)
  g <- ifelse(p > n, 0.5, 0)
  
  # check all the input
  if(class(object)!="BOSO") {
    stop("ICscoreCalculation is a hidden function to work with BOSO objects")
  }
  
  k <- ifelse(object$dfmin == object$dfmax, object$dfmin, NULL)
  if(is.null(k)) {stop("ICscoreCalculation is a hidden function to work with a fixed k")}
  
  x = as.matrix(object$x)
  y = as.numeric(object$y)
  xval = as.matrix(object$xval)
  yval = as.numeric(object$yval)
  n = nrow(x)
  
  if (k==0) {
    betas = coef.BOSO(object, beta0 = T)
    
    yall <- matrix(c(y, yval),ncol = 1)
    
    if (object$intercept) {
      errorVal <- yall - mean(yall)
      k = 1; df = 1
    } else {
      errorVal <- yall
      k = 0; df = 0
    }
    n <- length(errorVal)
    SSE <- sum(errorVal * errorVal)
  } else {
    
    xall <- cbind(1, rbind(x, xval)) # add intercept
    xall <- xall[,coef.BOSO(object, beta0 = T)!=0] # force the coefficient of beta0 to be included
    yall <- matrix(c(y, yval),ncol = 1)
    if (sum(coef.BOSO(object)!=0)<2) {xall <- matrix(xall, ncol = 1)}
    
    # calculate betas using the lambda value
    betas <- t(xall) %*% xall + object$lambda.selected * diag(dim(xall)[2])
    betas <- MASS::ginv(betas) %*% t(xall) %*% yall
    
    # calculate the degrees of freedom
    df <- t(xall) %*% xall + object$lambda.selected*diag(dim(xall)[2])
    df <- xall %*% MASS::ginv(df) %*% t(xall)
    df <- Trace(df)
    
    k <- min(dim(xall)[2], ceiling(df))
    if (object$intercept | object$standardize){k = k-1}
    
    errorVal <- yall - xall %*% betas
    n <- length(errorVal)
    SSE <- sum(errorVal * errorVal)
  }
  
  if (IC == "AIC"){ 
    score <- n*log(SSE/n) + 2*df
  } else if (IC == "BIC"){
    score <- n*log(SSE/n) + log(n)*df
  } else if (IC == "eBIC"){
    score <- n*log(SSE/n) + log(n)*df + 2*g*log(choose(p,k))
  } else {
    stop("Information criteria is not known")
  }
  
  
  return(score)
}


#' Trace convenience function
#' 
#' @param matrix a numeric matrix
#' 

#' @rdname InternalFunctions
Trace <- function(mat) sum(diag(mat))