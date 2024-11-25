is.positive.definite <- function(A){
  
  eigs <- eigen(A, symmetric=TRUE, only.values=TRUE)$values
  
  if(any(eigs <= 0)){
    return(FALSE)
  }else{
    return(TRUE)
  }
}