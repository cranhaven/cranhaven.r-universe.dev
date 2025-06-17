twolines <-
function(n=NA,a=NA,av=NA,d=NA,K=NA){
  if (is.na(a)==T|sum(is.na(d)==T)==length(d)|sum(is.na(K)==T)==length(K)|sum(is.na(av)==T)==length(av)){ 
    cat("Values for a, av, d and K are necessary. Please, check them.", sep="\n")
  } else {
    
    dK<-d/K
#library(e1071) #requiere paquete e1071
cat("Two-lines rule", sep="\n")
values0AV<-names(table(av));valuesAV<-c()
agentslistAV<-list()
permutalistAV<-list()
for (i in length(values0AV):1){
  valuesAV[(length(values0AV)-i)+1]<-as.numeric(values0AV[i])
  agentslistAV[[(length(values0AV)-i)+1]]<-which(av==valuesAV[(length(values0AV)-i)+1])
  permutalistAV[[(length(values0AV)-i)+1]]<-permutations(length(agentslistAV[[(length(values0AV)-i)+1]]))
}

values0<-names(table(dK));valuesdK<-c()
values0dK<-1:length(names(table(dK)))
agentslistdK<-list()
permutalistdK<-list()
for (i in length(values0dK):1){
  valuesdK[(length(values0dK)-i)+1]<-as.numeric(values0[i])
  agentslistdK[[(length(values0dK)-i)+1]]<-which(abs(dK-valuesdK[(length(values0dK)-i)+1])<0.001)
  permutalistdK[[(length(values0dK)-i)+1]]<-permutations(length(agentslistdK[[(length(values0dK)-i)+1]]))
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

pdK<-matrix();indicesdK<-c()
totaldK<-1
for (i in 1:length(permutalistdK)){
  AUXagentsi<-agentslistdK[[i]]
  AUXpermutai<-permutalistdK[[i]]
  for (j in 1:nrow(AUXpermutai)){AUXpermutai[j,]<-AUXagentsi[AUXpermutai[j,]]}
  permutalistdK[[i]]<-AUXpermutai
  totaldK<-totaldK*nrow(permutalistdK[[i]])
}

pAV<-c()
for (i in 1:length(permutalistAV)){
  copies<-totalAV/nrow(permutalistAV[[i]])
  m0<-permutalistAV[[i]]
  if (copies>=2){for (k in 2:copies){m0<-rbind(m0,permutalistAV[[i]])}}
  pAV<-cbind(pAV,m0)
}
pdK<-c()
for (i in 1:length(permutalistdK)){
  copies<-totaldK/nrow(permutalistdK[[i]])
  m0<-permutalistdK[[i]]
  if (copies>=2){for (k in 2:copies){m0<-rbind(m0,permutalistdK[[i]])}}
  pdK<-cbind(pdK,m0)
}

McAV<-rep(0,n)
for (i in 1:nrow(pAV)){
  order<-pAV[i,]
  for (j in 1:n){
    aa<-which(order==j)
    predi<-order[1:aa]
    if (aa>1){pred<-order[1:(aa-1)]}else{pred<-c()}
    cSi<-(a+max(av[predi]))*max(dK[predi])
    if (length(pred)>=1){cS<-(a+max(av[pred]))*max(dK[pred])}else{cS<-0}
    McAV[j]<-McAV[j]+cSi-cS
  }
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
    McdK[j]<-McdK[j]+cSi-cS
  }
}

TL<-1/2*1/nrow(pAV)*McAV+1/2*1/nrow(pdK)*McdK
return(TL)}
}
