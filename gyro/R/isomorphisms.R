PhiUE <- function(A, s){
  gammaF(A, s) * A
}

PhiEU <- function(A, s){
  betaF(A, s) * A
}

PhiEM <- function(A, s){
  x <- dotprod(A) / s / s
  2 * A / (1 + x)
}

PhiME <- function(A, s){
  gamm <- gammaF(A, s)
  gamm * A / (1 + gamm)
}

.PhiUM <- function(A, s){
  PhiUE(PhiEM(A, s), s)
}
