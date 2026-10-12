f.influence <- function(io, i, j){
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  if(length(i) != length(j)) stop("i must be the same length as j")
  n <- dim(io$L)[1]
  if(length(i) >= n) stop("Field of Influence is only defined up to n-1 terms, where n = #sectors * #regions")
  # nth order field of influence
  if(length(i) > 1){
    k <- length(i)
    A <- matrix(0, nrow = n, ncol = n)
    for(s in 1:k){
      for(r in 1:k){
        L = io$L
        Lij <- L[i[s], j[r]]
        A <- 1/(k-1)*(  (-1)^(s+r+1) * Lij * f.influence(io, i[-s], j[-r])) + A
      }
    }
    return(A)
  }
  # 1st order field of influence
  else if(length(i) == 1){
    L.j <- io$L[, j]
    Li. <- io$L[i, ]
    matrix(L.j, ncol = 1) %*% matrix(Li., nrow = 1)
  }
}
