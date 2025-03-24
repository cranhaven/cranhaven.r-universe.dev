reForm <-
function(x, nB, nS, cc., theta, Dfix){
  out <- list()
  out[[1]] <- x[1:nB]
  out[[2]] <- x[(nB + 1):(nB + nS)]
  out[[3]] <- cc.$working.correlation
  tMat <- matrix(0, nrow = nrow(theta[[4]]), ncol = ncol(theta[[4]]))
  tMat[!Dfix] <- x[(nB + nS + 1):length(x)]
  tMat[upper.tri(tMat)] <- t(tMat)[upper.tri(tMat)]
  out[[4]] <- tMat
  return(out)
}
