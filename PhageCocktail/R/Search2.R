# Search2

Search2 <- function(MaxPhage,MaxBacteria,new_matrix,phage_names){

  PhageSet2=0
  BestBacteria2=0
  for (i in 1:(MaxPhage-1)){
    for(j in (i+1):MaxPhage){
      BacteriaSet2=0
      for (b in 1:MaxBacteria){
        if (new_matrix[b,i] | new_matrix[b,j]){
          BacteriaSet2=BacteriaSet2+1
        }
      }
      if (BacteriaSet2 > BestBacteria2){
        PhageSet2=c(i,j)
        BestBacteria2=BacteriaSet2
        if (BestBacteria2 == MaxBacteria){
          return(c(phage_names[PhageSet2],BestBacteria2))
        }
      }
    }

  }
  return(c(phage_names[PhageSet2],BestBacteria2))
}
