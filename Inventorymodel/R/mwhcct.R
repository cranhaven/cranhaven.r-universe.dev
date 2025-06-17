mwhcct<-function(n=NA, a=NA, av=NA, d=NA, K=NA,cooperation=c(0,1),allocation=c(0,1)){

  if (is.na(a)==T|sum(is.na(av)==T)==length(av)|sum(is.na(d)==T)==length(d)|sum(is.na(K)==T)==length(K)){ 
    cat("Values for a, b, d and K are necessary. Please, check them.", sep="\n")
  } else {
    cat("MWHC with Transportation Costs model", sep="\n")    
    coa<-coalitions(n)
    
    if (cooperation==0){
      costes<-(a+av[2:(n+1)])*d/K
      sol<-costes
      cat("Individual cost", sep="\n")
    }
    if (cooperation==1){
      cat("Cooperative case", sep="\n")
      if (n<=10){
        cost<-numeric(nrow(coa[[1]]))
        cost1<-numeric(nrow(coa[[1]]))
        cost2<-numeric(nrow(coa[[1]]))

        matriz<-data.frame(coa[[1]],coa[[2]],rep(0,2^n))
        for (i in 2:nrow(matriz)){
          ind<-which(matriz[i,1:n]==1)
          cost1[i]<-a*max(d[ind]/K[ind])
          cost2[i]<-(av[i])*max(d[ind]/K[ind])
          cost[i]<-cost1[i]+cost2[i]
          matriz[i,n+2]<-cost[i]
        }
        colnames(matriz)<-c(1:n,"Coalition","Cost")
        sol<-matriz
      }
      if (n>10){
        coal<-c()
        for (i in 1:n){coal[i]<-paste(paste("'{",i,sep=""),"}'",sep="")}
        coal<-c(coal,"'N'")	
        costes<-(a+av[2:(n+1)])*d/K
        
        matriz<-data.frame(coal,c(costes,(a+av[length(av)])*max(d/K)))
        colnames(matriz)<-c("Coalition","Cost")
        sol<-matriz
      }
    
      if (allocation==1){
        if (n<=10){
          p <- permutations(n)
          aux <- c()
          for (k in 1:nrow(p)) {
            aux0 <- c()
            for (i in 1:n) {
              for (j in 1:n) {
                if (i != j) {
                  if (d[i]/K[i]<d[j]/K[j] & which(p[k, ]==i) <= which(p[k, ]==j)){aux0 <- c(aux0, 1)}
                 }
              }
             }
            if (length(aux0) == 0) {aux <- c(aux, k)}
          }
          p2 <- p[aux, ]
          TL <- marginal_contribution_mean(p2, cost2)
          R<-shapley_mfoc(n,a,d,K)+TL
          sol<-list(sol,R)
          names(sol)<-c("Optimal solution","Allocation R rule")
        }
        if (n>10){
          dK<-d/K
          #library(e1071) #requiere paquete e1071
          values0<-names(table(dK));valuesdK<-c()
          values0dK<-1:length(names(table(dK)))
          agentslistdK<-list()
          permutalistdK<-list()
          for (i in length(values0dK):1){
            valuesdK[(length(values0dK)-i)+1]<-as.numeric(values0[i])
            agentslistdK[[(length(values0dK)-i)+1]]<-which(abs(dK-valuesdK[(length(values0dK)-i)+1])<0.001)
            permutalistdK[[(length(values0dK)-i)+1]]<-permutations(length(agentslistdK[[(length(values0dK)-i)+1]]))
          }
          
          pdK<-matrix();indicesdK<-c()
          totaldK<-1
          for (i in 1:length(permutalistdK)){
            AUXagentsi<-agentslistdK[[i]]
            AUXpermutai<-permutalistdK[[i]]
            for (j in 1:nrow(AUXpermutai)){AUXpermutai[j,]<-AUXagentsi[AUXpermutai[j,]]}
            permutalistdK[[i]]<-AUXpermutai
            totaldK<-totaldK*nrow(permutalistdK[[i]])
          }
          pdK<-c()
          for (i in 1:length(permutalistdK)){
            copies<-totaldK/nrow(permutalistdK[[i]])
            m0<-permutalistdK[[i]]
            if (copies>=2){for (k in 2:copies){m0<-rbind(m0,permutalistdK[[i]])}}
            pdK<-cbind(pdK,m0)
          }
          
          McdK<-rep(0,n)
          for (i in 1:nrow(pdK)){
            order<-pdK[i,]
            for (j in 1:n){
              aa<-which(order==j)
              predi<-order[1:aa]
              if (aa>1){pred<-order[1:(aa-1)]}else{pred<-c()}
              cSi<-(a+max(av[predi]))*max(dK[predi])
              if (length(pred)>=1){cS<-(a+max(av[pred]))*max(dK[pred])}else{cS<-0}
              McdK[j]<-cSi-cS
            }
          }
          P1<-shapley_mfoc(n,a,d,K)
          P2<-1/nrow(pdK)*apply(McdK,2,sum)
          R<-P1+P2
          sol<-list(sol,R)
          names(sol)<-c("Optimal solution","Allocation R rule")
        }
      }
    }
  
  return(sol)}
}