linerule <-
function(n=NA,a=NA,av=NA,d=NA,h=NA,m=NA){
  
  if (sum(is.na(h)==T)==length(h)|sum(is.na(av)==T)==length(av)){
    cat("Values for h and av are necessary to determinate the optimal orders.", sep="\n")
  }  else { 
    if (sum(is.na(m)!=T)==length(m)|sum(is.na(d)!=T)==length(d)){ 
    
      values0AV<-names(table(av));valuesAV<-c()
      agentslistAV<-list()
      permutalistAV<-list()
      for (i in length(values0AV):1){
        valuesAV[(length(values0AV)-i)+1]<-as.numeric(values0AV[i])
        agentslistAV[[(length(values0AV)-i)+1]]<-which(av==valuesAV[(length(values0AV)-i)+1])
        permutalistAV[[(length(values0AV)-i)+1]]<-permutations(length(agentslistAV[[(length(values0AV)-i)+1]]))
      }
      
      
      pAV<-matrix();indicesAV<-c()
      totalAV<-1
      for (i in 1:length(permutalistAV)){
        AUXagentsi<-agentslistAV[[i]]
        AUXpermutai<-permutalistAV[[i]]
        for (j in 1:nrow(AUXpermutai)){AUXpermutai[j,]<-AUXagentsi[AUXpermutai[j,]]}
        permutalistAV[[i]]<-AUXpermutai
        totalAV<-totalAV*nrow(permutalistAV[[i]])
      }
      
      
      
      pAV<-c()
      for (i in 1:length(permutalistAV)){
        copies<-totalAV/nrow(permutalistAV[[i]])
        m0<-permutalistAV[[i]]
        if (copies>=2){for (k in 2:copies){m0<-rbind(m0,permutalistAV[[i]])}}
        pAV<-cbind(pAV,m0)
      }
      
      
      McAV<-matrix(0,ncol=n,nrow=nrow(pAV))
      for (i in 1:nrow(pAV)){
        order<-pAV[i,]
        for (j in 1:n){
          aa<-which(order==j)
          predi<-order[1:aa]
          if (aa>1){pred<-order[1:(aa-1)]}else{pred<-c()}
          cSi<-round(sqrt(2*(a+max(av[predi]))*sum((d*h)[predi])),3)
          if (length(pred)>=1){cS<-round(sqrt(2*(a+max(av[pred]))*sum(d[pred]*h[pred])),3)}else{cS<-0}
          McAV[i,j]<-cSi-cS
        }
      }
      
      
      phi<-apply(McAV,2,mean)
    return(phi)
    } else {
      cat("Values for m or d are necessary. Please, check them.")
    }
  }
}