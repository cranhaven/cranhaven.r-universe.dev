circ.hdr<-function(f,tau=NULL,level=NULL,plot.hdr=TRUE,col=NULL,lty=NULL,shrink=NULL,
                   cex=NULL,pch=NULL){
      if((is.function(f)==FALSE)){
		stop("argument 'f' must be a function defined on the unit circle")
	}else{
           	    x=seq(0,2*pi,length=1000)
		    fx=f(circular(x))
          	    if((!is.null(level))&(!is.null(tau))){
		            stop("only one argument 'level' or 'tau' must be provided")
		    }else if(((is.null(level))&(is.null(tau)))){
		            stop("argument 'level' or argument 'tau' must be provided")
		    }else{
		     		if((!is.null(level))&(!is.numeric(level)) ){
		        	    stop("argument 'level' must be a numeric value")
	         		}else if((!is.null(level))&(is.numeric(level)) ){
                      		  if((level>max(fx))){
						stop("level set is equal to the emptyset")
					  }
					if((level<min(fx))){
						stop("level set is equal to the support")
					  }
  		     	      }else{
                 			if((tau<1)&(tau>0)){
						step=x[2]-x[1]
                                    level=uniroot(g<-function(y){return(trapezoidal.rule(fx,step,y)-(1-tau))},lower=0,upper=max(fx))$root
		     			}else{
			        	    stop("argument 'tau' is a probability that must take a value larger than 0 and smaller than 1")
		          		}
	                  }
                  }
              }

	        hdr=matrix(find.circ.hdr(x,fx,level),ncol=2,byrow=TRUE)

              if(!is.logical(plot.hdr)){
		        stop("argument 'plot.hdr' must be logical")
              }else{
                     if(plot.hdr){
				if(is.null(col)){col="darkgray"}
				if(is.null(shrink)){shrink=2}
			      if(is.null(lty)){lty=2}
				if(is.null(cex)){cex=.2}
				if(is.null(pch)){pch=19}

                       plot.circular(circular(seq(0,2*pi,length=100),type="angles",units="radians"),shrink=shrink,type="l")
                       lines.circular(circular(x,type="angles",units="radians"), fx,shrink=shrink,col=1)
                       lines.circular(circular(x,type="angles",units="radians"),rep(level,times=length(x)),col=col,lty=lty,shrink=shrink)
                       points.circular(circular(x[(fx>=level)],type="angles",units="radians"),col=col,shrink=shrink,pch=pch,cex=cex)





	                }
              }

          if(!is.null(tau)){
			return(list(hdr=hdr,prob.content=(1-tau),level=level))
	    }else{
			return(list(levelset=hdr,level=level))

	    }

}
