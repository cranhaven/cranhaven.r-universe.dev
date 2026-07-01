sphere.scatterplot<-function(sample,tau=c(0.25,0.5,.75),bw="none",ngrid=500,
                             nborder=1000,tol=0.1, col=NULL){
	if(!is.matrix(sample)|(ncol(sample)!=3)|any(is.wholenumber(apply((sample)**2,1,sum))!=1)){stop("argument 'sample' must be a matrix of dimension n by 3 of points on the unit sphere")}
	eu.sample<- euclid.inv(sample)
	if((is.numeric(bw))&(length(bw)==1)&(bw>0)){
		fn=vmf.kerncontour2(eu.sample,h=bw,full=FALSE,ngrid=ngrid)
	}else if (((bw=="none")|(bw=="rot"))&(length(bw)==1)){
		fn=vmf.kerncontour(eu.sample,thumb=bw,den.ret=TRUE,full=FALSE,ngrid=ngrid)
	}else{
		stop("argument 'bw' must be a numeric value or a character equal to rot or none")
	}
      if(is.numeric(tau)&(all(tau<1))&(all(tau>0))){
			tau=sort(tau)
 			if(is.null(col)){
				col<- colorRampPalette(c("darkblue","#3333FFE6","lightgray"), alpha = TRUE)(5*length(tau))
				col=col[c(T,F,F,F,F)]
			}
			fn_sample=numeric(nrow(eu.sample))
		      for(j in 1:nrow(eu.sample)){
				fn_sample[j]=fn$den[which.min(abs(eu.sample[j,1]-fn$lat)),which.min(abs(eu.sample[j,2]-fn$long))]
			}
			level=quantile(fn_sample,prob=tau,type=1)
			hdr.border.aux=matrix(0,ncol =4)
			sample.aux=matrix(0,ncol =4)
          	      for(i in 1:length(tau)){
 		        	In=(abs(fn$den-level[i])<tol)
				In.index=which(In,arr.ind = TRUE)
			      if(length(In.index)==0){stop("argument 'tol' could be too small. Choose a larger value")}
			      hdr.border=euclid(cbind(fn$lat[In.index[,1]],fn$long[In.index[,2]]))
			      tol2=tol
     		            while(nrow(hdr.border)>nborder){
		                   tol2=tol2/1.1
  		                   In=(abs(fn$den-level[i])<tol2)
	                         In.index=which(In, arr.ind = TRUE)
	                         hdr.border=euclid(cbind(fn$lat[In.index[,1]],fn$long[In.index[,2]]))
                        }
				if(i!=length(tau)){
					hdr.border.aux=rbind(hdr.border.aux,cbind(hdr.border,rep(col[i],times=nrow(hdr.border))))
					sample.aux=rbind(sample.aux,cbind(sample[(fn_sample>=level[i])&(fn_sample<level[i+1]),],rep(col[i],times=nrow(sample[(fn_sample>=level[i])&(fn_sample<level[i+1]),]))))

				}else{
                              hdr.border.aux=rbind(hdr.border.aux,cbind(hdr.border,rep(col[i],times=nrow(hdr.border))))
					sample.aux=rbind(sample.aux,cbind(sample[(fn_sample>=level[i]),],rep(col[i],times=nrow(sample[(fn_sample>=level[i]),]))))
				}
			}
 		    sphereplot(rbind(hdr.border.aux[-1,1:3],sample.aux[-1,1:3]),col=c(hdr.border.aux[-1,4],sample.aux[-1,4]))
                legend3d("topright", legend =  as.expression(lapply(tau, function(x) bquote(tau==.(x)))), pch=19,col = col)
		    t<-table(sample.aux[-1,4])
		    sample.in.hdrs=list()
		    sample.in.hdrs[[1]]= apply(sample.aux[-1,-4][1:t[1],],2,as.numeric)
		    for(i in 2:length(tau)){sample.in.hdrs[[i]]=apply(sample.aux[-1,-4][(cumsum(t)[i-1]+1):cumsum(t)[i],],2,as.numeric) }
		    return(sample.in.hdrs)


	}else{
		    stop("argument 'tau' is a vector of probabilities that must take values larger than 0 and smaller than 1")
      }
}

