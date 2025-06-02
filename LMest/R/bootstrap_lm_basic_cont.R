bootstrap_lm_basic_cont <-
function(piv,Pi,Mu,Si,n,B=100,start=0,mod=0,tol=10^-6){

# Bootstrap EM estimates piv, Pi, Mu and Si

# Preliminaries
	k = length(piv)
# Reparametrize
    mMu = mSi = mpiv = mPi = 0
    m2Mu = m2Si=  m2piv = m2Pi = 0
#    mth = 0; m2th = 0;
    for(b in 1:B){
    	   cat("boostrap sample n. ",b,"\n")
	    	out = draw_lm_basic_cont(piv,Pi,Mu,Si,n)
	    	Yb = out$Y
	  	out = est_lm_basic_cont(Yb,k,start,mod,tol)
	    mMu = mMu + out$Mu/B
      	mSi = mSi + out$Si/B
     	mpiv = mpiv+out$piv/B 
     	mPi = mPi+out$Pi/B
	    	m2Mu = m2Mu + out$Mu^2/B; m2Si = m2Si + out$Si^2/B 
	    m2piv = m2piv+out$piv^2/B; m2Pi = m2Pi+out$Pi^2/B
    }
    seMu = sqrt(m2Mu - mMu^2)
  	seSi = sqrt(m2Si - mSi^2)
    sepiv = sqrt(m2piv-mpiv^2); sePi = sqrt(m2Pi-mPi^2)
    out = list(mMu=mMu,mSi=mSi,mpiv=mpiv,mPi=mPi,seMu=seMu,seSi=seSi,sepiv=sepiv,sePi=sePi)

}
