##
rmvhyper <- function(Mk, m)
{
	##
  K  <- length(Mk)
  M  <- sum(Mk)
	
  ##	
  if(m > M)
  	mk <- Mk
  
  ##
  if(m <= M)
  {
  	if(K == 1)
  		mk <- m
  	if(K > 1)
  	{
		  mk <- rep(0, K)
    	mk[1] <- rhyper(1, Mk[1], M - Mk[1], m)
    	if(K > 2)
    	{
      	for(j in 2:(K-1)) mk[j] <- rhyper(1, Mk[j], M - sum(Mk[1:j]), m - sum(mk[1:(j-1)]))
    	}
    mk[K] <- m - sum(mk[1:(K-1)])
  	}
  }
  
  ##
  return(mk)
}

##
rmvhyper <- function(Mk, m)
{
	##
	K <- length(Mk)
	M <- sum(Mk)
	
	##
	if(m > M)
		mk <- Mk
	else
	{
		mk <- rep(0, K)
		subSample <- table(sample(rep(1:K, Mk), m))
		mk[as.numeric(names(subSample))] <- subSample
	}
	
	##
	return(mk)
}
