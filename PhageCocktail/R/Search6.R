# Search6

Search6 <- function(MaxPhage,MaxBacteria,new_matrix,phage_names){

  PhageSet6=0
  BestBacteria6=0
  for (i in 1:(MaxPhage-5)){
    for(j in (i+1):(MaxPhage-4)){
      for(k in (j+1):(MaxPhage-3)){
        for(l in (k+1):(MaxPhage-2)){
          for(m in (l+1):(MaxPhage-1)){
            for(n in (m+1):(MaxPhage)){
              BacteriaSet6=0
              for (b in 1:MaxBacteria){
                if (new_matrix[b,i] | new_matrix[b,j]| new_matrix[b,k]| new_matrix[b,l]| new_matrix[b,m]| new_matrix[b,n]){
                  BacteriaSet6=BacteriaSet6+1
                }
              }
              if (BacteriaSet6 > BestBacteria6){
                PhageSet6=c(i,j,k,l,m,n)
                BestBacteria6=BacteriaSet6
                if (BestBacteria6 == MaxBacteria){
                  return(c(phage_names[PhageSet6],BestBacteria6))
                }
              }
            }
          }
        }
      }
    }
  }
  return(c(phage_names[PhageSet6],BestBacteria6))
}
