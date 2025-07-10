tEIGdwt <- function (tnsr)
{
  # Performs a Eigenvalue decomposition of 3-mode tensor
  # using the discrete wavelet transform.

  # Input: tnsr, a 3D tensor
  # Output: A tensor P of eigenvectors and a tensor D
  # eigenvalues so that tnsr = P D P^-1

  if (tnsr@num_modes != 3)
    stop("tEIGdwt only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (sum(as.numeric(intToBits(n3))) != 1)
    stop("Mode 3 must be a power of 2 otherwise using 0 padding")
  dwtz <- tDWT(tnsr)
  P_arr <- array(0, dim = c(n1, n2, n3))
  D_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- eigen(dwtz[, , j]@data)
    decompp <- polar(decomp$vectors,decomp$values)
    P_arr[, , j] <- decompp$P
    D_arr[, , j] <- decompp$D
  }
  # Inverse DWT
  P <- tIDWT(as.tensor(P_arr))
  D <- tIDWT(as.tensor(D_arr))
  invisible(list(P = P,D = D))
}
