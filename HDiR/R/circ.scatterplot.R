circ.scatterplot<-function(sample,tau=c(0.25,0.5,.75),bw=bw.CV(circular(sample),upper=100),
                           tau.method="quantile",plot.density=TRUE,col=NULL,shrink=NULL,
                           cex=NULL,lty=NULL){
	 if(!is.numeric(sample)|any(sample<0)|any(sample>(2*pi))){
		stop("argument 'sample' must be a numeric vector of angles from 0 to 2*pi")
	 }else if(!is.numeric(bw)|(length(bw)>1)){
		stop("argument 'bw' is a bandwidth parameter that must take a positive value")
       }else{
            if(is.numeric(tau)&(all(tau<1))&(all(tau>0))){
			tau=sort(tau)
 			if(is.null(col)){
				col<- colorRampPalette(c("darkblue","#3333FFE6","lightgray"), alpha = TRUE)(5*length(tau))
				col=col[c(T,F,F,F,F)]
			}
			if(is.null(shrink)){shrink=2}
			if(is.null(cex)){cex=.5}
			if(is.null(lty)){lty=rep(2,times=length(tau))}
			sample=circular(sample,type="angles",units="radians")
      plot.circular(circular(seq(0,2*pi,len=100),type="angles",units="radians"),shrink=shrink,type="l")
			fn=kern.den.circ(sample, bw=bw,n=250)
                  fnx <- kern.den.circ(sample, z=sample, bw=bw)
			sample.in.hdrs=vector("list", length=length(tau))
         	      for(i in 1:length(tau)){
                              if(tau.method=="quantile"){
 		                       		level=quantile(fnx$y,prob=(tau[i]),type=1)
                             	}else if(tau.method=="trapezoidal"){
							f=fn$y
					       	step=fn$x[2]-fn$x[1]
                                      	y<-seq(0,max(f),by=step)
                                    	      level=uniroot(g<-function(y){return(trapezoidal.rule(f,step,y)-(1-tau[i]))},lower=0,upper=max(f))$root
                             	  }else{
			        	  	      stop("argument 'tau.method' must take the values quantile or trapezoidal.rule")

					  }

			              I=(fnx$y>=level)
					  sample.in.hdrs[[i]]=fnx$x[I]
					  points.circular(circular(sample.in.hdrs[[i]],type="angles",units="radians"),col=col[i],shrink=shrink,cex=cex )
                                lines.circular(circular(fn$x,type="angles",units="radians"),rep(level,times=length(fn$x)),col=col[i],lty=lty[i],shrink=shrink)

		       }
			legend("topright", legend =  as.expression(lapply(tau, function(x) bquote(tau==.(x)))), pch=19,col = col)
			if(plot.density){
                          lines.circular(circular(fn$x,type="angles",units="radians"), fn$y,shrink=shrink,col=1)
			}
			return(sample.in.hdrs)
		   }else{
		      stop("argument 'tau' is a vector of probabilities that must take values larger than 0 and smaller than 1")
		   }
           }
}
