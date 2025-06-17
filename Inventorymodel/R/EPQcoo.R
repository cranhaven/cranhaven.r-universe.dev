EPQcoo <-
function(n=NA,a=NA,d=NA,h=NA,m=NA,r=NA,b=NA){
  if (n<=10){
    coalicion<-coalitions(n)
    matriz<-as.matrix(coalicion[[1]])
    matriz0<-matriz
    matrizf<-matriz
  }
  if (n>10){
    matriz0<-c()
    matrizf<-c()
    costes<-c()
  }
costes<-c();costes[1]<-0
s<-b
if (sum(is.na(h)==T)==length(h)){
  cat("A value for h is necessary to determinate the optimal orders.", sep="\n")
}  else { 
  if (sum(is.na(m)!=T)==length(m)|sum(is.na(d)!=T)==length(d)){ 
    if (sum(is.na(h)==T)==length(h)|sum(is.na(s)==T)==length(s)|sum(is.na(r)==T)==length(r)){
      cat("Values for r and s are necessary to determinate the optimal orders and shortages.", sep="\n")
    }  else {  
      if (sum(is.na(m)!=T)==length(m)&sum(is.na(d)==T)==length(d)){
        Qepq<-EPQ(n,a,d=NA,h,m,r,s)[[1]]
        d<-Qepq*m
      }
    }
    if (sum(r>d)==length(r)){
      Qepq<-EPQ(n,a,d,h,m=NA,r,s)[[1]]
      if (n<=10){
        for (i in 2:nrow(matriz0)){
          aux<-which(matriz0[i,]!=0)
          for (j in 1:length(aux)){
            matriz0[i,aux[j]]<-sqrt(2*a*d[aux[j]]^2/sum(d[aux]*h[aux]*s[aux]*(1-d[aux]/r[aux])/(h[aux]+s[aux])))
            matrizf[i,aux[j]]<-matriz0[i,aux[j]]*h[aux[j]]*(1-d[aux[j]]/r[aux[j]])/(h[aux[j]]+s[aux[j]])
          }
          costes[i]<-2*a*sqrt(sum((d[aux]/Qepq[aux])^2))
        }
        matriz0<-data.frame(coalicion[[2]],matriz0,costes)
        rownames(matrizf)<-rep("",2^n)
        colnames(matriz0)<-c("Coalition",1:n,"Coalitional costs")
        colnames(matrizf)<-1:n
        sol<-list(matriz0,matrizf)
        names(sol)<-c("Optimal order","Optimal shortages")
      }
      if (n>10){
        coa<-c()
        for (i in 1:n){coa[i]<-paste(paste("'{",i,sep=""),"}'",sep="")}
        coa<-c(coa,"'N'")
        Qepq<-EPQ(n,a,d,h,m=NA,r,s)
        
        QepqN<-sqrt(2*a*d^2/sum(d*h*s*(1-d/r)/(h+s)))
        matriz0<-QepqN
        matrizf<-matriz0*h*(1-d/r)/(h+s)
        matriz0<-rbind(diag(Qepq[[1]]),matriz0)
        matrizf<-rbind(diag(Qepq[[2]]),matrizf)
        costes<-2*a*sqrt(sum((d/QepqN)^2))
        costes<-c(Qepq[[3]],sum(costes))
        rownames(matriz0)<-rep("",n+1);rownames(matrizf)<-rep("",n+1)
        
        solA<-data.frame(coa,matriz0,costes)
        solB<-data.frame(matrizf)
        colnames(solA)<-c("Coalition",1:n,"Coalitional costs")
        colnames(solB)<-c(1:n)
        sol<-list(solA,solB)
        names(sol)<-c("Optimal order","Optimal shortages")
        
      }
      
      cat("Cooperative case", sep="\n")
      
      return(sol)
    } else{
      cat("The values for r_i are not bigger that d_i.", sep="\n")
    }
    
    
  } else {
    cat("Values for d or m are necessary to determinate the optimal orders.", sep="\n")
  }
}

}