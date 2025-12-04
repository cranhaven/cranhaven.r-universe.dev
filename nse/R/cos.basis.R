cos.basis = function(q,N){
  basis.matrix = matrix(data = NA, nrow = N, ncol = q)
  s = (1:N-0.5)/N
  for(i in 1:q){
    basis.matrix[,i] = sqrt(2)*cos(i*s*pi)
  }
  return(basis.matrix)
}
