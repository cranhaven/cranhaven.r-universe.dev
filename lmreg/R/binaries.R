binaries <-
function(x) {
  levels <- unique(x)
  bnryx <- NULL
  for (z in levels) {
    bn <- rep(0,length(x)); bn[x==z] = 1
    bnryx <- cbind(bnryx,bn)
  }
  colnames(bnryx) <- paste0("v.",levels)
  return(bnryx)
}
