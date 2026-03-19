# Search5

Search5 <- function(MaxPhage,MaxBacteria,new_matrix,phage_names){

  PhageSet5=0
  BestBacteria5=0
  for (i in 1:(MaxPhage-4)){
    for(j in (i+1):(MaxPhage-3)){
      for(k in (j+1):(MaxPhage-2)){
        for(l in (k+1):(MaxPhage-1)){
          for(m in (l+1):(MaxPhage)){
            BacteriaSet5=0
            for (b in 1:MaxBacteria){
              if (new_matrix[b,i] | new_matrix[b,j]| new_matrix[b,k]| new_matrix[b,l]| new_matrix[b,m]){
                BacteriaSet5=BacteriaSet5+1
              }
            }
            if (BacteriaSet5 > BestBacteria5){
              PhageSet5=c(i,j,k,l,m)
              BestBacteria5=BacteriaSet5
              if (BestBacteria5 == MaxBacteria){
                return(c(phage_names[PhageSet5],BestBacteria5))
              }
            }
          }
        }
      }
    }
  }
  return(c(phage_names[PhageSet5],BestBacteria5))
}
