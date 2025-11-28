Variances <- function(vTau, iTau_star) {
  
  iJ = length(vTau)
  
  mT  = matrix(0, iJ, iJ)
  
  for (j1 in 1:iJ){
    for (j2 in j1:iJ){
      mT[j1, j2] = vTau[j1] * (1.0 - vTau[j2])
      mT[j2, j1] = mT[j1, j2]
    }
  }
  
  vVars = numeric(iJ)
  mOnes = matrix(1, ncol = 1, nrow = iJ)
  
  for (j in 1:iJ) {
    if (j < iTau_star) {
      vVars[j] = t(mOnes[1:j]) %*% mT[1:j, 1:j] %*% mOnes[1:j]
    } else if (j > iTau_star) {
      vVars[j] = t(mOnes[j:iJ]) %*% mT[j:iJ, j:iJ] %*% mOnes[j:iJ]
    } else {
      vVars[j] = t(mOnes) %*% mT %*% mOnes
    }
  }
  
  return(vVars)
  
}
