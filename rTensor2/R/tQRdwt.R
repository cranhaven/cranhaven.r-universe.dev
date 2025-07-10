tQRdwt <- function (tnsr)
{
  # Performs a tensor QR decomposition on any 3-mode tensor
  # using the discrete wavelet transform.

  # Input: A, 3-mode tensor
  # Output: Tensors Q and R so that A=QR.

  if (tnsr@num_modes != 3)
    stop("tQRdwt only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (sum(as.numeric(intToBits(n3))) != 1)
    stop("Mode 3 must be a power of 2 otherwise using 0 padding")
  dwtz <- tDWT(tnsr)
  Q_arr <- array(0, dim = c(n1, n2, n3))
  R_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- qr(dwtz[, , j]@data, nu = n1, nv = n2)
    Q_arr[, , j] <- qr.Q(decomp)
    R_arr[, , j] <- qr.R(decomp)
  }
  # Inverse DWT
  Q <- tIDWT(as.tensor(Q_arr))
  R <- tIDWT(as.tensor(R_arr))
  invisible(list(Q = Q, R = R))
}

