tLUdwt <- function (tnsr)
{
  if (tnsr@num_modes != 3)
    stop("T-SVD only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (sum(as.numeric(intToBits(n3))) != 1)
    stop("Mode 3 must be a power of 2 otherwise using 0 padding")
  if (n1 !=n2)
    stop("LU decomposition only works on tensors with square lateral faces")
  dwtz <- tDWT(tnsr)
  L_arr <- array(0, dim = c(n1, n2, n3))
  U_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- LU(dwtz[, , j]@data)
    L_arr[, , j] <- decomp$L
    U_arr[, , j] <- decomp$U
  }
  # Inverse DWT
  L <- tIDWT(as.tensor(L_arr))
  U <- tIDWT(as.tensor(U_arr))
  invisible(list(L = L, U = U))
}
