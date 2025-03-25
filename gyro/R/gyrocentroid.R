Egyrocentroid <- function(A, B, C, s){
  gA <- gammaF(A, s); gB <- gammaF(B, s); gC <- gammaF(C, s)
  (gA*A + gB*B + gC*C) / (gA + gB + gC)
}

Ugyrocentroid <- function(A, B, C, s){
  PhiUE(Egyrocentroid(PhiEU(A, s), PhiEU(B, s), PhiEU(C, s), s), s)
}

Mgyrocentroid <- function(A, B, C, s){
  s2 <- s * s
  gA2 <- 1 / (1 - dotprod(A)/s2)
  gB2 <- 1 / (1 - dotprod(B)/s2)
  gC2 <- 1 / (1 - dotprod(C)/s2)
  # if(
  #   gA2 < 0 || gB2 < 0 || gC2 < 0 ||
  #   is.infinite(gA2) || is.infinite(gB2) || is.infinite(gC2)
  # ){
  #   stop(
  #     "In the M\u00f6bius gyrovector space, points must be ",
  #     "strictly inside the centered ball of radius `s`.",
  #     call. = FALSE
  #   )
  # }
  Mgyroscalar(0.5, (gA2*A + gB2*B + gC2*C) / (gA2 + gB2 + gC2 - 1.5), s)
}
