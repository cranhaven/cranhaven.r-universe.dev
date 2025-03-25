gammaF <- function(A, s){
  1 / sqrt(1 - dotprod(A)/(s*s))
}

betaF <- function(A, s) 1 / sqrt(1 + dotprod(A)/(s*s))

Ugyroadd <- function(A, B, s){
  betaA <- betaF(A, s)
  betaB <- betaF(B, s)
  (1 + betaA/(1+betaA) * dotprod(A, B)/(s*s) + (1-betaB)/betaB) * A + B
}

Mgyroadd <- function(X, Y, s){
  s2 <- s * s
  x <- dotprod(X) / s2
  y <- dotprod(Y) / s2
  xy <- 2 * dotprod(X, Y) / s2
  ((1 + xy + y) * X + (1 - x) * Y) / (1 + xy + x*y)
}

Ugyroscalar <- function(r, A, s){
  h <- sqrt(dotprod(A)) / s
  sinh(r*asinh(h)) * A / h
}

Mgyroscalar <- function(r, X, s){
  Xnorm <- sqrt(dotprod(X))
  s / Xnorm * tanh(r * atanh(Xnorm / s)) * X
}

UgyroABt <- function(A, B, t, s){
  Ugyroadd(A, Ugyroscalar(t, Ugyroadd(-A, B, s), s), s)
}

MgyroABt <- function(A, B, t, s){
  Mgyroadd(A, Mgyroscalar(t, Mgyroadd(-A, B, s), s), s)
}
