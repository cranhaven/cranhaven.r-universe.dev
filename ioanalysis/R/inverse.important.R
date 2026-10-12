inverse.important <- function(io, i, j, delta.aij){
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  if(length(i) != 1) stop("i (row) should only be one number")
  if(length(j) != 1) stop("j (column) should only be one number")
  L <- io$L
  delta.L <- delta.aij/(1-L[j,i]*delta.aij) * f.influence(io, i, j)
  delta.L
}
