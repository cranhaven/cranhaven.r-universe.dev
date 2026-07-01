circ.distances<-function(x,y){
	if(!is.numeric(x)|!is.numeric(y)|any(x<0)|any(x>(2*pi))|any(y<0)|any(y>(2*pi))){
		stop("arguments 'x' and 'y' must be two numeric vectors of angles from 0 to 2*pi")
	}else{
		xs1=cbind(cos(x),sin(x))
		ys1=cbind(cos(y),sin(y))
      	Nxs1<-nrow(xs1)
		xys1=rbind(xs1,ys1)
		D<-as.matrix(dist(xys1))[1:Nxs1,(Nxs1+1):nrow(xys1)]
		if((class(D)[1]=="numeric")){D=matrix(D,ncol=length(D)) }
 		return(list(dE=min(apply(D,1,min)),dH=max(max(apply(D,1,min)),max(apply(D,2,min)))) )
	}
}

