mwhc <-
function(n=NA,a=NA,b=NA,d=NA,K=NA,cooperation=c(0,1),allocation=c(0,1)){
  
  if (is.na(a)==T|sum(is.na(b)==T)==length(b)|sum(is.na(d)==T)==length(d)|sum(is.na(K)==T)==length(K)){ 
    cat("Values for a, b, d and K are necessary. Please, check them.", sep="\n")
  } else {
    cat("MWHC model", sep="\n")  
    if (sum(sort(d/K)==(d/K))!=n) {
      cat("Warning: see agents, d and K are not in the order indicated by the ratios d/K.", sep="\n")
      d0K0<-d/K
      b<-b[order(d0K0)]
      d<-d[order(d0K0)]
      K<-K[order(d0K0)]
    }
    if (cooperation==0){
      pedido<-sqrt(b*d/(2*a+b*K^2/d))
      costes<-sqrt(b*d*(2*a+b*K^2/d))-b*K
      faltantes<-d/pedido-K
      sol<-list(pedido,costes,faltantes)
      names(sol)<-c("Number of orders per time unit","Costs","Optimal shortages")
      return(sol)
    }
    if (cooperation==1){
      cat("Cooperative case", sep="\n")
	  if (n<=10){
		  matriz0<-coalitions(n)
	  	matriz<-data.frame(as.vector(matriz0[[2]]),rep(0,2^n),rep(0,2^n))
  		matriz0<-matriz0[[1]]
  		matriz1<-matrix(0,ncol=n+1,nrow=2^n)
	  	for (i in 2:nrow(matriz)){
	  		matriz0[i,]->coa
	  		aux<-which(coa==1)
	  		coaux<-which(coa==1)
	  		s=length(coaux)
	  		k=s+1
	  		T<-rep(0,length(coa))
	  		xT=0
	  		SxT=coa

  			while(sum(SxT==T)!=length(coa)){
  				k=k-1;k
  				T[coaux[k]]=1;T
  				aux1<-which(T==1)
  				xT<-sqrt(sum(b[aux1]*d[aux1])/(2*a+sum(b[aux1]*K[aux1]^2/d[aux1])))
  				aux2<-which(xT<d/K)
  				SxT<-rep(0,length(coa));SxT[intersect(which(coa==1),aux2)]=1;SxT
	  		}
		  	matriz[i,2]<-xT
		  	ind<-which(SxT==1)
		  	matriz[i,3]<-sum(b[ind]*d[ind])/xT-sum(b[ind]*K[ind])
		  	suma1<-0
		  	c0<-sqrt((2*a+sum(b[ind]*K[ind]^2/d[ind]))/sum(b[ind]*d[ind]))
		  	if (allocation==1){
			  	for (k in 1:length(ind)){
				    suma1<-suma1+ind[k]*10^(length(ind)-k)
				    matriz1[i,1+ind[k]]<-c0*b[ind[k]]*d[ind[k]]-b[ind[k]]*K[ind[k]]
			    }
			    matriz1[i,1]<-suma1
			    rownames(matriz1)<-rep(" ",2^n)
			    colnames(matriz1)<-c("Coalition",1:n)
		  	}
	  	}
  		colnames(matriz)<-c("Coalitions","Optimal orders","Costs")
  		sol<-matriz
	 }
	 if (n>10){
	    coal<-c()
	    for (i in 1:n){coal[i]<-paste(paste("'{",i,sep=""),"}'",sep="")}
	    coal<-c(coal,"'N'")	   
	    pedido<-sqrt(b*d/(2*a+b*K^2/d))
	    costes<-sqrt(b*d*(2*a+b*K^2/d))-b*K
	 
	    coa<-rep(1,n)
	    aux<-which(coa==1)
	    coaux<-aux
	    s=length(coaux)
	    k=s+1
	    T<-rep(0,length(coa))
	    xT=0
	    SxT=coa
			
		   s=n;k=s+1
		   T<-rep(0,length(coa))
	     xT=0
		   SxT=coa
	     while(sum(SxT==T)!=length(coa)){
			    k=k-1;T[coaux[k]]=1;T
			    aux1<-which(T==1)
			    xT<-sqrt(sum(b[aux1]*d[aux1])/(2*a+sum(b[aux1]*K[aux1]^2/d[aux1])))
			    aux2<-which(xT<d/K)
			    SxT<-rep(0,length(coa));SxT[intersect(which(coa==1),aux2)]=1;SxT
		   }
		   pedido<-c(pedido,xT)
		   ind<-which(SxT==1)
		   costes<-c(costes,sum(b[ind]*d[ind])/xT-sum(b[ind]*K[ind]))
		   suma1<-0
		   c0<-sqrt((2*a+sum(b[ind]*K[ind]^2/d[ind]))/sum(b[ind]*d[ind]))
		   if (allocation==1){
		     matriz1<-rbind(diag(costes[1:n]),rep(0,n))
		     for (k in 1:length(ind)){
			    	matriz1[n+1,ind[k]]<-c0*b[ind[k]]*d[ind[k]]-b[ind[k]]*K[ind[k]]
			   }
		     rownames(matriz1)<-rep(" ",n+1)	
		     colnames(matriz1)<-1:n
		   }
		   coa<-coal
		   matriz<-data.frame(coal,pedido,costes)
		   colnames(matriz)<-c("Coalitions","Optimal orders","Costs")
		   sol<-matriz

   }	 
	 
  
 
  if (allocation==1) {
    sol<-list(matriz,matriz1);names(sol)<-c("Optimal policies","R-rule")}
    
  }
  return(sol)}
  
}




