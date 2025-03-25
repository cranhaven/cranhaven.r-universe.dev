Egyromidpoint <- function(A, B, s){
  gA <- gammaF(A, s); gB <- gammaF(B, s)
  (gA*A + gB*B) / (gA + gB)
}

Ugyromidpoint <- function(A, B, s){
  PhiUE(Egyromidpoint(PhiEU(A, s), PhiEU(B, s), s), s)
}

Mgyromidpoint <- function(A, B, s){
  PhiME(Egyromidpoint(PhiEM(A, s), PhiEM(B, s), s), s)
}

# Ugyromidpoint <- function(A, B, s){
#   UgyroABt(A, B, 0.5, s)
# }
#
# Mgyromidpoint <- function(A, B, s){
#   MgyroABt(A, B, 0.5, s)
# }
