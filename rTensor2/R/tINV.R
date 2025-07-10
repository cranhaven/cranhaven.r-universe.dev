tINV <- function(tnsr,tform)
{
  # Performs inverse of 3-mode tensor using any
  # discrete transform.

  # Input: tnsr, a 3D tensor
  # Output: The inverse of tnsr, a 3D tensor

  if (tnsr@num_modes != 3)
    stop("tINV only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (n1 !=n2)
    stop("The inverse is only defined for square lateral faces")

  if (tform=="fft") {
    TI = tINVfft(tnsr)
  } else if (tform=="dwt") {
    TI = tINVdwt(tnsr)
  } else if (tform=="dct") {
    TI = tINVdct(tnsr)
  } else if(tform=="dst") {
    TI = tINVdst(tnsr)
  } else if(tform=="dwht") {
    TI = tINVdwht(tnsr)
  } else if(tform=="dht") {
    TI = tINVdht(tnsr)
  } else {
    stop("Transform not supported")
  }
  Tinv <- TI
  return(Tinv)
}
