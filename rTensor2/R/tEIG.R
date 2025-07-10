tEIG <- function(tnsr,tform) {
  # Performs a Eigenvalue decomposition of 3-mode tensor
  # using any discrete transform.

  # Input: tnsr, a 3D tensor
  # Output: A tensor P of eigenvectors and a tensor D
  # eigenvalues so that tnsr = P D P^-1

  if (tnsr@num_modes != 3)
    stop("tEIG only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (n1 !=n2)
    stop("An eigen alue decomposition can only be performed when the lateral faces are square")

  if (tform=="fft") {
    TE = tEIGfft(tnsr)
  } else if (tform=="dwt") {
    TE = tEIGdwt(tnsr)
  } else if (tform=="dct") {
    TE = tEIGdct(tnsr)
  } else if(tform=="dst") {
    TE = tEIGdst(tnsr)
  } else if(tform=="dwht") {
    TE = tEIGdwht(tnsr)
  } else if(tform=="dht") {
    TE = tEIGdht(tnsr)
  } else {
    stop("Transform not supported")
  }
  tEIG <- TE
  return(tEIG)
}
