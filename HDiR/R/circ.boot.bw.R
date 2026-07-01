circ.boot.bw<-function(sample,bw=bw.CV(circular(sample),upper=100),tau=0.5,B=50,upper=1.5*bw){
   if(!is.numeric(bw)|(length(bw)>1)){
		stop("argument 'bw' is a bandwidth parameter that must take a positive value")
   }else if(!is.numeric(tau)|(length(tau)>1)|(tau<0)|(tau>1)){
	      stop("argument 'tau' is a probability that must take a value larger than 0 and smaller than 1")
   }else if(!is.wholenumber(B)|(length(B)>1)|(B<0)){
	      stop("argument 'B' is the number of interactions that must take an integer value larger than 0")
   }else{

   	n=length(sample)
   	hdr.p=circ.plugin.hdr(sample,bw=bw,tau,tau.method="quantile",plot.hdr=FALSE,boot=TRUE)
      if(is.character(hdr.p$levelset)){stop("estimated pilot hdr is the emptyset or the unit circle")}
   	circ.mean.dH.distances<-function(sample,n,hdr.p,tau,bw,B,bw1){
   		dH=numeric(B)
  		for(i in 1:B){
		    sample.boot=circ.boot.sample(sample,n,bw)
	    	    hdr.boot=circ.plugin.hdr(sample.boot,bw=bw1,tau,tau.method="quantile",plot.hdr=FALSE,boot=TRUE)
                if(is.character(hdr.boot$hdr)){
				dH[i]=NA
				warning("a bootstrap hdr was equal to the emptyset or the unit circle","\n")
		     }else{
	                 dH[i]=circ.distances(as.vector(t(hdr.p$levelset)),as.vector(t(hdr.boot$levelset)))$dH
                 }
	       }
	return(mean(dH))
      }

      bw.boot <- optimize(function(bw1) circ.mean.dH.distances(sample,n,hdr.p,tau,bw,B,bw1), interval = c(lower=0,
        upper=upper), tol = .1)$minimum

    return(bw.boot)
  }
 }


