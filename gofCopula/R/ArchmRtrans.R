# Rosenblatt type transformation for Archimedean copula as proposed by
# Hering and Hofert (2015)
.ArchmRtrans <- function(copula, x) {
  n <- dim(x)[1]
  U <- c()
  for (i in 1:(copula@dimension - 1)) {
    b <- rowSums(apply(as.matrix(x[, 1:i]), 2, function(a) iPsi(copula, a)))
    d <- rowSums(apply(
      as.matrix(x[, 1:(i + 1)]), 2,
      function(a) iPsi(copula, a)
    ))
    U <- cbind(U, (b / d)^i)
  }
  Cn <- pCopula(x, copula)
  Kn <- F.n(as.matrix(Cn), as.matrix(Cn), offset = 1)
  U <- cbind(U, Kn)
  return(U)
}
