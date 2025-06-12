"covtocor" <-
function(cov.mat){
  dc <- diag(cov.mat)
  So <- sqrt(dc)
  ds <- diag(1/So)
  Mo <- ds %*% cov.mat %*% ds
  Mo <- Mo - 0.5*(Mo-t(Mo)) # Correct any round-off error
  return(list(mat=Mo,sd=So))
}

