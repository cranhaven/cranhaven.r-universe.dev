Ugyrosubdiv <- function(A1, A2, A3, s){
  M12 <- Ugyromidpoint(A1, A2, s)
  M13 <- Ugyromidpoint(A1, A3, s)
  M23 <- Ugyromidpoint(A2, A3, s)
  list(
    list(A1, M12, M13),
    list(A2, M23, M12),
    list(A3, M13, M23),
    list(M12, M13, M23)
  )
}

Mgyrosubdiv <- function(A1, A2, A3, s){
  M12 <- Mgyromidpoint(A1, A2, s)
  M13 <- Mgyromidpoint(A1, A3, s)
  M23 <- Mgyromidpoint(A2, A3, s)
  list(
    list(A1, M12, M13),
    list(A2, M23, M12),
    list(A3, M13, M23),
    list(M12, M13, M23)
  )
}
