draw_lm_mixed <- function(la,Piv,Pi,Psi,n,TT){

#        [Y,S,yv] = draw_lm_mixed(la,Piv,Pi,Psi,n,TT)
#
# Draw a sample of size n from a mixed Latent Markov model with specific parameters
# Preliminaries
  warning("draw_lm_mixed function is no longer maintained. Please look at drawLMmixed function",call. = FALSE)
	k1 = length(la)
	k2 = nrow(Piv)
	dd = dim(Psi)
	l = apply(Psi, c(2,3), function(x) sum(!is.na(x)))[1,]
	if(length(dd)>2) r = dd[3] else r = 1
	Psi = array(Psi,c(l,k2,r))
# # For each subject
    Y = matrix(0,n,TT*r)
    cat("------------|\n")
    cat(" sample unit|\n")
    cat("------------|\n")
    for(i in 1:n){
    	if(i/100==floor(i/100)) cat(sprintf("%11g",i),"\n",sep=" | ")
    	u = k1+1-sum(runif(1)<cumsum(la))
    	v = k2+1-sum(runif(1)<cumsum(Piv[,u]))
    	ind = 0
    	for(j in 1:r){
    		ind = ind+1
	    	Y[i,ind] = l[j]-sum(runif(1)<cumsum(Psi[,v,j]), na.rm=T)
    	}
   		for(t in 2:TT){
    		v = k2+1-sum(runif(1)<cumsum(Pi[v,,u])) #check se ok k2
	    	for(j in 1:r){
    			ind = ind+1
    			Y[i,ind] = l[j]-sum(runif(1)<cumsum(Psi[,v,j]), na.rm=T)
    		}
   		}
    }
    cat("------------|\n")
	out = aggr_data(Y)
    S = out$data_dis; yv = out$freq
    S = array(t(S),c(r,TT,length(yv)))
    S = aperm(S)
    if(r==1) S = S[,,1]
    out = list(Y=Y,S=S,yv=yv)
    return(out)
}
