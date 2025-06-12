"mergemats" <-
function(mat1, mat2){
#
# merge two squared matrices of same dimension
# take upper.tri from one and lower.tri from the other one
  nloci <- ncol(mat1)
  newmat <- matrix(ncol=nloci, nrow=nloci)
  newmat[upper.tri(newmat)] <- mat2[upper.tri(mat2)]
  newmat[lower.tri(newmat)] <- mat1[lower.tri(mat1)]
  for (i in 1:nloci) newmat[i,i] <- NA
  return(newmat)
}

