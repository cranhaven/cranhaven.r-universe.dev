##
beta0 <- function(betaX,
									X,
									N,
									rhoY,
									expandX="all")
{
	##
  value <- optimize(beta0eval,
  									interval=logit(rhoY) + c(-10,10),
                    betaX=betaX,
                    X=X,
                    N=N,
                    rhoY=rhoY,
                    expandX=expandX,
                    tol=.Machine$double.eps^0.5)$minimum
  ##
  return(value)
}

##
beta0eval <- function(beta0,
											betaX,
											X,
											N,
											rhoY,
											expandX)
{
	##
	if(expandX == "none") designX <- X
  ##
  if(expandX == "all")
  {
	  designX <- X[,1]
  	for(i in 2:ncol(X))
  	{
    	for(j in 1:max(X[,i]))
    	{
    		designX <- cbind(designX, as.numeric(X[,i] == j))
    	}
  	}
  }
  
  ##
  etaY  <- as.numeric(designX %*% c(beta0, betaX))
  
  ##
  value <- abs(sum(expit(etaY) * (N/sum(N))) - rhoY)
  return(value)
}
