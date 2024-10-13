update.RAP <-
function(object, Ynew, Xnew, ...){
  # function to update regression coeffients based on new observations
  # updates can be mini-batch or single observations
  #
  
  # first we update the sufficient statistics:
  for (i in 1:nrow(Xnew)){
    object$w = object$r * object$w + 1 # update sample size
    object$xbar = (1-(1/object$w))*object$xbar + (1/object$w) * matrix(c(Ynew[i], Xnew[i,]), nrow=1) # update mean
    object$St = (1-(1/object$w))*object$St + (1/object$w) * t( c(Ynew[i], Xnew[i,]) - object$xbar) %*% ( c(Ynew[i], Xnew[i,]) - object$xbar) # update covariance estimate
  }
  
  # get derivative:
  res = (Ynew - (Xnew %*% t(object$beta))) # residual term
  
  # now update regularization parameter:
  if (object$Approx){
    # we are using the approximate version
    
    dC_l1 = -1 * Xnew %*% getGradientLARS_Approx(XtX = object$St[-1,-1], Xty = object$St[-1,1], beta=object$beta)
    object$regParam = object$regParam + object$eps * as.numeric( t(res) %*% dC_l1 )
    object$regParam = max(0, object$regParam)
    object$l1Track = c(object$l1Track, object$regParam)
    
  } else {
    # we are using 'exact' gradients 
    
    dC_l1 = -1 * Xnew %*% getGradientLARS(XtX = object$St[-1,-1], Xty = object$St[-1,1], beta=object$beta)
    object$regParam = object$regParam + object$eps * as.numeric( t(res) %*% dC_l1 )
    object$regParam = max(0, object$regParam)
    object$l1Track = c(object$l1Track, object$regParam)
    
  }
  
  # finally update the regression coefficients:
  object$beta = matrix(lassoshooting(XtX = object$St[-1,-1], Xty = object$St[-1,1], lambda = object$regParam)$coef, nrow=1)
  
  return(object)
}
