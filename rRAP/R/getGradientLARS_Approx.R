getGradientLARS_Approx <-
function(XtX, Xty, beta, Eps=0.01){
  # calculate gradient of lasso solution as suggested by LARS formulae
  # THIS IS A COMPUTATIONALLY EFFICIENT APPROXIMATION
  # we take the diagonal of the sample covariance and invert
  # this reduces the computational cost from O(p^3) to O(p)
  # Note that the current implementation has not been optimized!
  #
  # INPUT:
  #      - XtX: current estimate of covariance
  #      - Xty: current estimate of t(X)%*%y - only used if all beta entries are 0!
  #      - beta: current estimate of LASSO solution
  
  gradVector = rep(0, length(beta))
  
  s = matrix(sign(beta), ncol=1)
  ActiveID = which(s!=0)
  if (length(ActiveID)>0){
    if (length(ActiveID)>1){
      Ginv_active = diag(1/diag(XtX)[ActiveID])
    } else {
      Ginv_active = diag(1) * (1/diag(XtX)[ActiveID])
    }
    #G_active = (XtX * (s %*% t(s)))[ActiveID, ActiveID]
    
    #Ginv_active = try(solve(G_active), silent = TRUE)
    #if (class(Ginv_active)=="try-error") {
    #  Ginv_active = ginv(G_active)
    #cat("gen inv used...\n")
    #}
    
    A_active =  1/sqrt(sum(Ginv_active)) #1/sqrt(t(rep(1, length(ActiveID))) %*% Ginv_active %*% rep(1, length(ActiveID)))
    
    gradVector[ActiveID] = as.numeric(A_active) * Ginv_active %*% rep(1, length(ActiveID))
    
    return(gradVector * s)
  } else {
    # here the active set is empty! The direction of the gradient is therefore
    # in the direction of the covariate that is maximally correlated with the response (in absolute terms of course!)
    
    gradVector[which.max(abs(Xty))] = Eps*sign(Xty[which.max(Xty)])
    
    return(gradVector)
  }
}
