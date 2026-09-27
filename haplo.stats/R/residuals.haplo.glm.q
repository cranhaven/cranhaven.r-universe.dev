
## Purpose: residuals method for haplo.glm object
## Author: Jason Sinnwell
## Created: 9/26/2011

residuals.haplo.glm <- function (object, type = c("deviance", "pearson",
                          "working", "response"), ...) 
{

    type <- match.arg(type)
  
    ## We do not implement predict for haplo.glm, so partial is not allowed
    if(type=="partial")
      stop("partial residuals not supported for haplo.glm\n")
    
    ## Fix elements of haplo.glm object so that it will work
    ## with residuals.glm function
  
    sindx <- object$haplo.post.info$indx
    hpost <- object$haplo.post.info$post

    ## create collapsed y
    mu.eta <- object$family$mu.eta
    y <- object$fitted.values +
        object$residuals * mu.eta(object$linear.predictors)
  
    object$y <- tapply(object$y, sindx, FUN=function(x) signif(x[1],digits=4))
   
    ## collapse mu, and weights, and working residuals
    ## assign them back to object before calling residuals.glm on it
    mu <- tapply(object$fitted.values*hpost, sindx, sum)
    wts <- tapply(object$prior.weights, sindx, FUN <- function(x) x[1])
    object$fitted.values <- mu
    object$residuals <-   tapply(object$residuals*object$prior.weights*hpost,
                                 sindx,sum)
    object$prior.weights <- wts
    
    res <- residuals.glm(object, type=type)

    return(res) 
}


## Purpose: method to get fitted.values for haplo.glm object
## Author: Jason Sinnwell
## Created: 10/3/2011


fitted.haplo.glm <- function(object, ...) {

  sindx <- object$haplo.post.info$indx
  hpost <- object$haplo.post.info$post

  fits <- tapply(object$fitted.values*hpost, sindx, sum)

  return(fits)
}


## Purpose: method to get var-cov matrix for haplo.glm object
## Author: Jason Sinnwell
## Created: 10/4/2011

vcov.haplo.glm <- function(object, freq=TRUE, ...) {
  if(freq) {
    return(object$var.mat)
  } else {
    return(object$var.mat[1:object$rank,1:object$rank])
  }
}
