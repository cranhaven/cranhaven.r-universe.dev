linerulecoalitional <-
function(n=NA,a=NA,av=NA,d=NA,h=NA,m=NA){ 
  
  if (sum(is.na(h)==T)==length(h)|sum(is.na(av)==T)==length(av)){
    cat("Values for h and av are necessary to determinate the optimal orders.", sep="\n")
  }  else { 
    if (sum(is.na(m)!=TRUE)==length(m)|sum(is.na(d)!=TRUE)==length(d)){
      if (n<=10){
        STI<-STIcoo(n,a,av,d,h,m)[[1]]
        STI<-STI[,n+2]
        coa<-cbind(as.matrix(coalitions(n)[[1]]),STI)
        for (i in 2:nrow(coa)){
          aux<-which(coa[i,1:n]==1)
          aux1<-linerule(length(aux),a,av[aux],d[aux],h[aux],m=NA)
          for (j in 1:length(aux)){coa[i,aux[j]]<-aux1[j]}
        }
        coa<-data.frame(coalitions(n)[[2]],coa)
        colnames(coa)<-c("Coalition",1:n,"Coalitional cost")
        return(coa)
      } else {
        LR<-linerule(n,a,av,d,h,m=NA)
        coa<-data.frame("N",LR,sum(LR))
        colnames(coa)<-c("Coalition",1:n,"Coalitional cost")
        return(coa)
      }
    }
  }
}



