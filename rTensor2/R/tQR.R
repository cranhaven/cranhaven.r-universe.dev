tQR <- function(tnsr,tform) {
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (tnsr@num_modes != 3)
    stop("T-SVD only implemented for 3d so far")
  if (tform=="dwt"){
    if (sum(as.numeric(intToBits(n3))) != 1)
      stop("Mode 3 must be a power of 2 otherwise using 0 padding")
  }
  if (n1 !=n2)
    stop("QR decomposition is defined only for square lateral faces")
  if (tform=="fft") {
    QR = tQRfft(tnsr)
  } else if (tform=="dwt") {
    QR = tQRdwt(tnsr)
  } else if (tform=="dct") {
    QR = tQRdct(tnsr)
  } else if(tform=="dst") {
    QR = tQRdst(tnsr)
  } else if(tform=="dwht") {
    QR = tQRdwht(tnsr)
  } else if(tform=="dht") {
    QR = tQRdht(tnsr)
  } else {
    stop("Transform not supported")
  }
  return(list(Q = QR$Q, R = QR$R))
}
