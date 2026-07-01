sphere.plugin.hdr<-function(sample,bw="none",ngrid=500,
                            tau=NULL,level=NULL,nborder=1000,tol=0.01,
                            mesh=40,deg=3,plot.hdr=TRUE, col=NULL){
	if(!is.matrix(sample)|(ncol(sample)!=3)|any(is.wholenumber(apply((sample)**2,1,sum))!=1)){
	  stop("argument 'sample' must be a matrix of dimension n by 3 of points on the unit sphere")
	}
	eu.sample<- euclid.inv(sample)
	if((is.numeric(bw))&(length(bw)==1)){
		fn=vmf.kerncontour2(eu.sample,h=bw,full=FALSE,ngrid=ngrid)
	}else if (((bw=="none")|(bw=="rot"))&(length(bw)==1)){
		fn=vmf.kerncontour(eu.sample,thumb=bw,den.ret=TRUE,full=FALSE,ngrid=ngrid)
		bw=fn$h
	}else{
		stop("argument 'bw' must be a numeric value or a character equal to rot or none")
	}
	fn_sample=numeric(nrow(eu.sample))
	for(i in 1:nrow(eu.sample)){
	  fn_sample[i]=fn$den[which.min(abs(eu.sample[i,1]-fn$lat)),which.min(abs(eu.sample[i,2]-fn$long))]
	}
	if((!is.null(level))&(!is.numeric(level)) ){
		 stop("argument 'level' must be a numeric value")
	}else if((!is.null(level))&(is.numeric(level)) ){
                             if((level>max(fn$den))){
						warning("level set is equal to the emptyset","\n")
					      return(list(hdr="emptyset",level=level,bw=bw))
				      }else if((level<min(fn$den))){
						warning("level set is equal to the support distribution","\n")
						return(list(hdr="unit sphere",level=level,bw=bw))

					}
  	}else{
              if((tau<1)&(tau>0)){
						       level=quantile(fn_sample,prob=tau,type=1)
		     			}else{
			        	    stop("argument 'tau' is a probability that must take a value larger than 0 and smaller than 1")
		          		}
	 }
       In=(abs(fn$den-level)<tol)
	 In.index=which(In,arr.ind = TRUE)

	 if(length(In.index)==0){stop("argument 'tol' could be too small. Choose a larger value")}
	 hdr.border=euclid(cbind(fn$lat[In.index[,1]],fn$long[In.index[,2]]))

       while(nrow(hdr.border)>nborder){
		  tol=tol/1.1
  		 In=(abs(fn$den-level)<tol)
	       In.index=which(In, arr.ind = TRUE)
	       hdr.border=euclid(cbind(fn$lat[In.index[,1]],fn$long[In.index[,2]]))
       }


 	if(!is.logical(plot.hdr)){
		stop("argument 'plot.hdr' must be logical")
       }else{
            if(plot.hdr){
			if(is.null(col)){col="darkgray"}
			sphereplot(hdr.border,col=col)
             }
        }

        if(!is.null(tau)){
			return(list(hdr=hdr.border,prob.content=(1-tau),level=level,bw=bw))
	  }else{
			return(list(levelset=hdr.border,prob.content=(sum(fn_sample>=level)/nrow(sample)),level=level,bw=bw))
	  }

}


