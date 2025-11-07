estep.Z.cost <-
function(xx,alpha,beta,mu,a,b,d,Q,k,l,Wmat){
  # Projection of test data in the eigenspace Ei
  nbasis = ncol(xx)
  Qkl = Wmat[[k]][[l]]%*%as.matrix(Q[[k]][[l]])
  Pa = (as.matrix(xx - matrix(1,nrow(xx),1)%*%mu[k,l,]) %*% Qkl) %*% t(Qkl)
  Pb = Pa + as.matrix(matrix(1,nrow(xx),1)%*%mu[k,l,] - xx)

  #Compute cost function
  A = t(1/a[k,l] * rowSums(Pa^2) + (1/b[k,l] * rowSums(Pb^2)) + d[k,l] * log(a[k,l])
        + (nbasis-d[k,l]) * log(b[k,l])) - 2 * log(beta[l])
}
