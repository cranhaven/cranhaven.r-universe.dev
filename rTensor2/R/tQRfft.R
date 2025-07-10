tQRfft <- function (tnsr)
{
  # Performs a tensor QR decomposition on any 3-mode tensor
  # using the discrete fast fourier transform.

  # Input: A, 3-mode tensor
  # Output: Tensors Q and R so that A=QR.

  if (tnsr@num_modes != 3)
    stop("tQRfft only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  dftz <- aperm(apply(tnsr@data, MARGIN = 1:2, fft), c(2,3,1))
  Q_arr <- array(0, dim = c(n1, n2, n3))
  R_arr <- array(0, dim = c(n2, n2, n3))
  for (j in 1:n3) {
    decomp <- QR(dftz[, , j])
    Q_arr[, , j] <- decomp$Q
    R_arr[, , j] <- decomp$R
  }
  Q <- as.tensor(aperm(apply(Q_arr, MARGIN = 1:2, ifft), c(2,3,1)))
  R <- as.tensor(aperm(apply(R_arr, MARGIN = 1:2, ifft), c(2,3,1)))
  invisible(list(Q = Q, R = R))
}
