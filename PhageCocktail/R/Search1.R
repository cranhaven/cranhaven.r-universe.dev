# Search1

Search1 <- function(MaxPhage,MaxBacteria,new_matrix,phage_names){
  PhageSet1=0
  BestBacteria1=0
  for (i in 1:MaxPhage){
    BacteriaSet1=0
    for (b in 1:MaxBacteria){
      BacteriaSet1=BacteriaSet1+new_matrix[b,i]
    }
    if (BacteriaSet1 > BestBacteria1) {
      PhageSet1=i;
      BestBacteria1=BacteriaSet1;
      if (BestBacteria1 == MaxBacteria) {
        return(c(phage_names[PhageSet1],BestBacteria1))
      }
    }
  }
  return(c(phage_names[PhageSet1],BestBacteria1))
}
