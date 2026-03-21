
identify_inconsistencies <- function(mat){
  if(nrow(mat) < 2){
    return(c())
  }
  index_is <- c()
  for(r in 2:length(mat[1,])){
    for(c in 1:(r-1)){
      if(mat[r,c] > mat[c,r]){
        index_is <- c(index_is, r, c)
      }
    }  
  }
  index_is <- dimnames(mat)[[1]][index_is]
  return(list(is = unique(index_is), i_count = length(index_is)))
}
