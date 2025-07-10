tQRdst <- function (tnsr)
{
  # Performs a tensor QR decomposition on any 3-mode tensor
  # using the discrete sine transform.

  # Input: A, 3-mode tensor
  # Output: Tensors Q and R so that A=QR.

  if (tnsr@num_modes != 3)
    stop("tQRdst only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  dstz <- aperm(apply(tnsr@data, MARGIN = 1:2, dst), c(2,3,1))
  Q_arr <- array(0, dim = c(n1, n2, n3))
  R_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- qr(dstz[, , j], nu = n1, nv = n2)
    Q_arr[, , j] <- qr.Q(decomp)
    R_arr[, , j] <- qr.R(decomp)
  }
  Q <- as.tensor(aperm(apply(Q_arr, MARGIN = 1:2, idst), c(2,3,1)))
  R <- as.tensor(aperm(apply(R_arr, MARGIN = 1:2, idst), c(2,3,1)))
  invisible(list(Q = Q, R = R))
}
