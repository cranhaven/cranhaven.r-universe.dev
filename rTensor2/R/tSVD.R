tSVD <- function(tnsr,tform)
{
  # Performs a tensor singular value decomposition on any 3-mode
  # tensor using any discrete transform.

  # Input: A, 3-mode tensor
  # Output: Tensors U (left singular value object),
  # V (right singular value object) and
  # S, a diagonal tensor so that A=USV^T.

  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (tnsr@num_modes != 3)
    stop("tSVD only implemented for 3d so far")
  if (tform=="dwt"){
    if (sum(as.numeric(intToBits(n3))) != 1)
    stop("Mode 3 must be a power of 2 otherwise using 0 padding")
  }
  if (tform=="fft") {
    SVD = tSVDfft(tnsr)
  } else if (tform=="dwt") {
    SVD = tSVDdwt(tnsr)
  } else if (tform=="dct") {
    SVD = tSVDdct(tnsr)
  } else if(tform=="dst") {
    SVD = tSVDdst(tnsr)
  } else if(tform=="dwht") {
    SVD = tSVDdwht(tnsr)
  } else if(tform=="dht") {
    SVD = tSVDdht(tnsr)
  } else {
    stop("Transform not supported")
  }
  return(SVD)
}
