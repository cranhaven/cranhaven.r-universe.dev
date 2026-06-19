

objective.pgrid <- function(x, A) {
  k <- nrow(A)
  m <- ncol(A)
  value <- 0
  for(i in 1:length(x)) {
    if( 0<x[i] ) {
      value <- value + ( x[i] * log(x[i]) )
    } else {
      if( x[i]==0 ) {
        value <- value + 0
      } else {
        value <- value - Inf
      }
    }
  }
  value <- value / (k*m)
  value <- -1*(ll.grid(x=x, y=as.vector(A))) + value
  return(value)
}
