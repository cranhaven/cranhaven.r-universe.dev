# Exhaustive_Phi

ExhaustivePhi<-function(matrix_pb,bacteria_number, phage_number, MaxPhage,MaxBacteria,new_matrix,phage_names,limit,file_name,FUN){

  Temperature=nestedness(matrix_pb, null.models = FALSE, n.nulls = 1000, popsize = 30, n.ind = 7, n.gen = 2000, binmatnestout=FALSE)$statistic
  counter_1=0
  for(i in 1:bacteria_number){
    for(p in 1:phage_number){
      counter_1=counter_1+matrix_pb[i,p]
    }
  }
  Fill=100*counter_1/(bacteria_number*phage_number);

  Phi=as.integer(log2(((bacteria_number*Temperature)/Fill)+2));

  if (MaxPhage < Phi){
    Phi=MaxPhage}
  if(limit<Phi){Phi<-limit}
  if(Phi==0){return(0)}
  ResultSPhi<-Search(Phi, MaxPhage,MaxBacteria,new_matrix,phage_names)
  PhageSet<-ResultSPhi[-length(ResultSPhi)]
  BestBacteria<-ResultSPhi[length(ResultSPhi)]


  if (BestBacteria == MaxBacteria){

    return (c(Phi, TRUE, PhageSet, BestBacteria))}
  if (BestBacteria != MaxBacteria){
    return (c(Phi, FALSE, PhageSet, BestBacteria))
  }
}
