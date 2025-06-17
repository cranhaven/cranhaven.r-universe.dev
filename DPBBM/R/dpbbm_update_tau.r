##########################################################################
# update tau parameters
.dpbbm_update_tau <- function(c.cluster, prior_dbeta, tau){
  #  if(debug) print('resampling tau')
  G <- sum(c.cluster)
  k <- length(c.cluster)
  a <- prior_dbeta$a
  b <- prior_dbeta$b
  
  r <- rbeta(1,tau+1,G)
  eta.r <- 1/(G*(b-log(r))/(a+k-1)+1)
  if(runif(1) < eta.r){
    tau <- rgamma(1,shape=a+k,scale=b-log(r))
  } else{
    tau <- rgamma(1,shape=a+k-1,scale=b-log(r))
  }
  return(tau)
}
