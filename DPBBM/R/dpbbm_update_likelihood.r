##########################################################################
# update likelihood for each cluster
# x - observations
# size.x - size of the observation
# Phi - parameters
.dpbbm_update_likelihood <- function(x,size.x,Phi_alpha,Phi_beta){
  likelihood <- sum(dbetabinom.ab(x=x,size=size.x,shape1 = exp(abs(Phi_alpha)),shape2 = exp(abs(Phi_beta)),log=TRUE))
  return(likelihood)
}
