# Search3

Search3 <- function(MaxPhage,MaxBacteria,new_matrix,phage_names){

  PhageSet3=0
  BestBacteria3=0
  for (i in 1:(MaxPhage-2)){
    for(j in (i+1):(MaxPhage-1)){
      for(k in (j+1):MaxPhage){
        BacteriaSet3=0
        for (b in 1:MaxBacteria){
          if (new_matrix[b,i] | new_matrix[b,j]| new_matrix[b,k]){
            BacteriaSet3=BacteriaSet3+1
          }
        }
        if (BacteriaSet3 > BestBacteria3){
          PhageSet3=c(i,j,k)
          BestBacteria3=BacteriaSet3
          if (BestBacteria3 == MaxBacteria){
            return(c(phage_names[PhageSet3],BestBacteria3))
          }
        }
      }
    }
  }
  return(c(phage_names[PhageSet3],BestBacteria3))
}
