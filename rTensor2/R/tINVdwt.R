tINVdwt <- function(tnsr)
{
  # Performs inverse of 3-mode tensor using the
  # discrete wavelet transform.

  # Input: tnsr, a 3D tensor
  # Output: The inverse of tnsr, a 3D tensor

  if (tnsr@num_modes != 3)
    stop("tINVdwt only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (n1 !=n2)
    stop("The inverse is only defined for square lateral faces")
  if (n1 !=n2)
    stop("The eigen value decomposition only works for tensors with square lateral faces")
  dwtz <- tDWT(tnsr)
  T_inv <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    T_inv[, , j] <- solve(dwtz[, , j]@data)
  }
  Tinv <- tIDWT(as.tensor(T_inv))
}
