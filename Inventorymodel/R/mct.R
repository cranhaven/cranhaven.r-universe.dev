mct <-function(n=NA,a=NA,av=NA,d=NA,K=NA,cooperation=c(0,1)){
if (is.na(a)==T|sum(is.na(d)==T)==length(d)|sum(is.na(K)==T)==length(K)|sum(is.na(av)==T)==length(av)){ 
  cat("Values for a, av, d and K are necessary. Please, check them.", sep="\n")
} else {
  cat("MCT model", sep="\n")
  if (cooperation==0){
	  costes<-(a+av)*d/K
	  sol<-costes
	  cat("Individual cost", sep="\n")
  }
  if (cooperation==1){
    cat("Cooperative case", sep="\n")
    if (n<=10){
	    matriz<-data.frame(coalitions(n)[[1]],coalitions(n)[[2]],rep(0,2^n))
	    for (i in 2:nrow(matriz)){
		    aux<-which(matriz[i,1:n]==1)
		    matriz[i,n+2]<-max(a+av[aux])*max(d[aux]/K[aux])
	    }
      colnames(matriz)<-c(1:n,"Coalition","Cost")
      sol<-matriz
    }
    if (n>10){
      coa<-c()
      for (i in 1:n){coa[i]<-paste(paste("'{",i,sep=""),"}'",sep="")}
      coa<-c(coa,"'N'")
      matriz<-rbind(diag(n),rep(1,n))
      
      costes<-max(a+av)*max(d/K)
      costes<-c((a+av)*d/K,costes)
      matriz<-data.frame(matriz,coa,costes)
      
      colnames(matriz)<-c(1:n,"Coalition","Cost")
      sol<-matriz
    }
    
  }
  return(sol)
} 
  
}
