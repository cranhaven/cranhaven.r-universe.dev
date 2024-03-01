
# This function gives a matrix created by multiplying
#(Pointwise multiplication) a Matrix M by each column of Matrix S

Yproduct <- function(S, M){
  product <- matrix(NA, nrow(S), ncol(S)*ncol(M))
  
  for (i in 1:ncol(S)) {
    product[,(1 + ncol(M)*(i-1)):(ncol(M)*i)] <- S[,i]*M
  }
  return(product)
}
