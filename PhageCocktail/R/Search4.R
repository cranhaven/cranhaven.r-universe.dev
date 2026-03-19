
# Search4

Search4 <- function(MaxPhage,MaxBacteria,new_matrix,phage_names){

  PhageSet4=0
  BestBacteria4=0
  for (i in 1:(MaxPhage-3)){
    for(j in (i+1):(MaxPhage-2)){
      for(k in (j+1):(MaxPhage-1)){
        for(l in (k+1):(MaxPhage)){
          BacteriaSet4=0
          for (b in 1:MaxBacteria){
            if (new_matrix[b,i] | new_matrix[b,j]| new_matrix[b,k]| new_matrix[b,l]){
              BacteriaSet4=BacteriaSet4+1
            }
          }
          if (BacteriaSet4 > BestBacteria4){
            PhageSet4=c(i,j,k,l)
            BestBacteria4=BacteriaSet4
            if (BestBacteria4 == MaxBacteria){
              return(c(phage_names[PhageSet4],BestBacteria4))
            }
          }
        }
      }
    }
  }
  return(c(phage_names[PhageSet4],BestBacteria4))
}
