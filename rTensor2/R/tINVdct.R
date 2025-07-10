tINVdct <- function(tnsr)
{
  # Performs inverse of 3-mode tensor using the
  # discrete cosine transform.

  # Input: tnsr, a 3D tensor
  # Output: The inverse of tnsr, a 3D tensor

  if (tnsr@num_modes != 3)
    stop("tINVdct only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (n1 !=n2)
    stop("The inverse is only defined for square lateral faces")
  dctz <- aperm(apply(tnsr@data, MARGIN = 1:2, dct), c(2,3,1))
  T_inv <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    T_inv[, , j] <- solve(dctz[, , j])
  }
  T_inv <- as.tensor(aperm(apply(T_inv, MARGIN = 1:2, idct), c(2,3,1)))
  return(T_inv)
}
