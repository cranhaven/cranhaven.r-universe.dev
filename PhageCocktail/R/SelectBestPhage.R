# SelectBestPhage

SelectBestPhage<-function(list_phage_cluster,matrix_pb,bacteria_number){
  PhageSet=0
  BestCounter=0
  for (i in list_phage_cluster){
    counter<-0
    for (j in 1:bacteria_number){
      counter<-counter+matrix_pb[j,i]
    }
    if (counter > BestCounter){
      PhageSet=i
      BestCounter<-counter
    }
  }
  if (BestCounter==0){PhageSet<-list_phage_cluster[1]}
  return(PhageSet)
}
