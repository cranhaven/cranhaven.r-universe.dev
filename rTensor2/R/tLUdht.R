tLUdht <- function (tnsr)
{
  if (tnsr@num_modes != 3)
    stop("T-SVD only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (n1 !=n2)
    stop("LU decomposition only works on tensors with square lateral faces")
  dhtz <- aperm(apply(tnsr@data, MARGIN = 1:2, fht), c(2,3,1))
  L_arr <- array(0, dim = c(n1, n2, n3))
  U_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- LU(dhtz[, , j])
    L_arr[, , j] <- decomp$L
    U_arr[, , j] <- decomp$U
  }
  L <- as.tensor(aperm(apply(L_arr, MARGIN = 1:2, ifht), c(2,3,1)))
  U <- as.tensor(aperm(apply(U_arr, MARGIN = 1:2, ifht), c(2,3,1)))
  invisible(list(L = L, U = U))
}
