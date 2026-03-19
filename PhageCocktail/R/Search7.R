# Search7

Search7 <- function(MaxPhage,MaxBacteria,new_matrix,phage_names){

  PhageSet7=0
  BestBacteria7=0
  for (i in 1:(MaxPhage-6)){
    for(j in (i+1):(MaxPhage-5)){
      for(k in (j+1):(MaxPhage-4)){
        for(l in (k+1):(MaxPhage-3)){
          for(m in (l+1):(MaxPhage-2)){
            for(n in (m+1):(MaxPhage-1)){
              for(o in (n+1):(MaxPhage)){
                BacteriaSet7=0
                for (b in 1:MaxBacteria){
                  if (new_matrix[b,i] | new_matrix[b,j]| new_matrix[b,k]| new_matrix[b,l]| new_matrix[b,m]| new_matrix[b,n]| new_matrix[b,o]){
                    BacteriaSet7=BacteriaSet7+1
                  }
                }
                if (BacteriaSet7 > BestBacteria7){
                  PhageSet7=c(i,j,k,l,m,n,o)
                  BestBacteria7=BacteriaSet7
                  if (BestBacteria7 == MaxBacteria) {
                    return(c(phage_names[PhageSet7],BestBacteria7))}
                  }
              }
            }
          }
        }
      }
    }
  }
  return(c(phage_names[PhageSet7],BestBacteria7))
}
