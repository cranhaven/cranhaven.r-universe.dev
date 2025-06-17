EOQcoo <-
function(n=NA,a=NA,d=NA,h=NA,m=NA){
if (n<=10){
  coalicion<-coalitions(n)
  matriz<-as.matrix(coalicion[[1]])
  matriz0<-matriz
}
if (n>10){matriz0<-c();costes<-c()}
costes<-c();costes[1]<-0
Qeoq<-matrix()
if (sum(is.na(h)==T)==length(h)){
  cat("A value for h is necessary to determinate the optimal orders.", sep="\n")
} else {
  if (sum(is.na(m)!=T)==length(m)|sum(is.na(d)!=T)==length(d)){ 
    if (sum(is.na(m)!=T)==length(m)&sum(is.na(d)==T)==length(d)){
      Qeoq<-EOQ(n,a,d=NA,h,m)[[1]] 
      d<-Qeoq*m
    }
    Qeoq<-EOQ(n,a,d,h,m=NA)[[1]]
    if (n<=10){
      for (i in 2:nrow(matriz0)){
        aux<-which(matriz0[i,]!=0)
        for (j in 1:length(aux)){
          matriz0[i,aux[j]]<-sqrt(2*a*d[aux[j]]^2/sum(d[aux]*h[aux]))
        }
        costes[i]<-a*d[aux[1]]/matriz0[i,aux[1]]+sum(h[aux]*matriz0[i,aux]/2)
      }
    
      sol<-data.frame(coalicion[[2]],matriz0,costes)

    }
    if (n>10){
      coa<-c()
      for (i in 1:n){coa[i]<-paste(paste("'{",i,sep=""),"}'",sep="")}
      coa<-c(coa,"'N'")
      Qeoq<-EOQ(n,a,d,h,m=NA)
      
      
      matriz0<-sqrt(2*a*d^2/sum(d*h))
      costes<-a*d/matriz0+sum(h*matriz0/2)
      
      matriz0<-cbind(rbind(diag(Qeoq[[1]]),matriz0),c(Qeoq[[2]],sum(costes)))
      rownames(matriz0)<-rep("",n+1)
      sol<-data.frame(coa,matriz0)
      
    }
    cat("Cooperative case", sep="\n")
    
    cat("Optimal order", sep="\n")
    colnames(sol)<-c("Coalition",1:n,"Coalitional costs")
    
    return(sol)
  } else {
    cat("Values for d or m are necessary to determinate the optimal orders.", sep="\n")
  }
}


}