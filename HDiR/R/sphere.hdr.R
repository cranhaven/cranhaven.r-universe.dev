sphere.hdr<-function(f,tau=NULL,level=NULL,nborder=1000,tol=0.1,
                     mesh=40,deg=6,plot.hdr=TRUE,col=NULL){
      if((is.function(f)==FALSE)){
		stop("argument 'f' must be a function defined on the unit sphere")
	}else if((!is.null(level))&(!is.null(tau))){
		stop("only one argument 'level' or 'tau' must be provided")
	}else if(((is.null(level))&(is.null(tau)))){
		stop("argument 'level' or 'tau' must be provided")
	}else{
		grid=runif_on_sphere(1000000,d=3)
	 	fgrid=f(grid)
		if((!is.null(level))&(!is.numeric(level))){
		        stop("argument 'level' must be numeric")
	      }else if((!is.null(level))&(is.numeric(level)) ){
                      if((level>max(fgrid))){
					stop("level set is equal to the emptyset")
			    }
			    if((level<min(fgrid))){
					stop("level set is equal to the support")
			    }
  		 }else{
                 	    if((tau<1)&(tau>0)){
					hdr.sphere.integration<-function(f,y,mesh,deg){
						fa<-function(x,...){
							fx=f(x)
							if((f(x)-y)>=0){return(fx)}else{return(0)}
						}
					return(sphere.integration(fa,mesh,deg)/(4*pi))
					}
					level=uniroot(g<-function(y){return(hdr.sphere.integration(f,y,mesh,deg)-(1-tau))},lower=0,upper=max(fgrid),maxiter=100,tol=0.1)$root
		     	     }else{
			        	stop("argument tau is a probability that must take a value larger than 0 and smaller than 1")
		           }
	       }
       }
       In=which(abs(fgrid-level)<tol)
	 if(length(In)==0){stop("argument 'tol' could be too small. Choose a larger value")}
	 hdr.border=grid[In,]
       while(nrow(hdr.border)>nborder){
		tol=tol/1.1
  		In=which(abs(fgrid-level)<tol)
           	hdr.border=grid[In,]
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
			return(list(hdr=hdr.border,prob.content=(1-tau),level=level))
	  }else{
			return(list(levelset=hdr.border,level=level))
	  }

}

