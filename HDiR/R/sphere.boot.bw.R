sphere.boot.bw<-function(sample,bw="none",tau=0.5,ngrid=500,B=50,
                         nborder=500,upper=NULL){
if(!is.matrix(sample)|(ncol(sample)!=3)|any(is.wholenumber(apply((sample)**2,1,sum))!=1)){stop("argument 'sample' must be a matrix of three columns containing points on the unit sphere")}


if(((is.numeric(bw))&(bw>0)&(length(bw)==1))|(bw=="none")|(bw=="rot")){
   	n=nrow(sample)
   	hdr.p=sphere.plugin.hdr(sample,bw=bw,ngrid=ngrid,tau=tau,nborder=nborder,plot.hdr=FALSE)
      if(is.character(hdr.p$hdr)){stop("estimated pilot hdr is the emptyset or the unit sphere. Change the inputs")}

	bw=hdr.p$bw

   	sphere.mean.dH.distances<-function(sample,n,bw,bw.opt,hdr.p,B,tau){
   		dH=numeric(B)
  		for(i in 1:B){
		    sample.boot=sphere.boot.sample(sample,n,bw)
	    	    hdr.boot=sphere.plugin.hdr(sample.boot,bw=bw.opt,ngrid=ngrid,tau,plot.hdr=FALSE)
		    if(is.character(hdr.boot$hdr)){
				dH[i]=NA
				warning("a bootstrap hdr was equal to the emptyset or the unit sphere","\n")
			}else{
	         	 dH[i]=sphere.distances(hdr.p$hdr,hdr.boot$hdr)$dH
	       	}
		}
	return(mean(dH,na.rm = TRUE))
      }

      bw.boot <- optimize(function(bw.opt) sphere.mean.dH.distances(sample,n,bw,bw.opt,hdr.p,B,tau), interval = c(lower=0,
        upper=max(1.5*bw,upper)), tol = .1)$minimum


     return(bw.boot)

}else{
stop("argument 'bw' must be a numeric positive value or equal to 'rot' or 'none'")
}
}


