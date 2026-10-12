ifa <-
function(Psi, B) {
  Psi = as.vector(Psi)
  m = nrow(B)
  nbf = ncol(B)
  beta = B/(sqrt(Psi)%*%t(rep(1,nbf)))
  svdBeta = fast.svd(beta)
  theta = svdBeta$u*(rep(1,m)%*%t(svdBeta$d/sqrt(1+svdBeta$d^2)))
  phi = 1/sqrt(Psi)
  phitheta = theta*(phi%*%t(rep(1,ncol(theta))))
  iS = diag(phi^2)-tcrossprod(phitheta)
  thetabeta = crossprod(theta,beta)
  theta2beta = tcrossprod(theta,t(thetabeta))
  aux = beta-theta2beta
  iSB = (phi%*%t(rep(1,ncol(theta))))*aux
  return(list(iS = iS, iSB = iSB, Phi = phi, Theta = theta))
}
