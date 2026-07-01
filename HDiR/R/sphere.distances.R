sphere.distances<-function(x,y){
	if(!is.matrix(x)|!is.matrix(y)|(ncol(x)!=3)|(ncol(y)!=3)| any(is.wholenumber(apply((x)**2,1,sum))!=1) |any(is.wholenumber(apply((y)**2,1,sum))!=1)         ){
		stop("arguments x and y must be two matrix of three columns containing points on the unit sphere")
	}else{
       	Nx<-nrow(x)
		xy=rbind(x,y)
		D<-as.matrix(dist(xy))[1:Nx,(Nx+1):nrow(xy)]
		if((class(D)[1]=="numeric")){D=matrix(D,ncol=length(D)) }
 		return(list(dE=min(apply(D,1,min)),dH=max(max(apply(D,1,min)),max(apply(D,2,min)))) )
	}
}

