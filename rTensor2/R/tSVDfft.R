tSVDfft <- function (tnsr)
{
  # Performs a tensor singular value decomposition on any 3-mode
  # tensor using the discrete fast fourier transform.

  # Input: A, 3-mode tensor
  # Output: Tensors U (left singular value object),
  # V (right singular value object) and
  # S, a diagonal tensor so that A=USV^T.

  if (tnsr@num_modes != 3)
    stop("tSVDfft  only implemented for 3d so far")
  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  fftz <- aperm(apply(tnsr@data, MARGIN = 1:2, fft), c(2,3,1))
  U_arr <- array(0, dim = c(n1, n1, n3))
  Vt_arr <- array(0, dim = c(n2, n2, n3))
  m <- min(n1, n2)
  S_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- La.svd(fftz[, , j], nu = n1, nv = n2)
    #decomp <- La.svd(fftz[, , j],nu=m,nv=m)
    U_arr[, , j] <- decomp$u
    Vt_arr[, , j] <- decomp$vt
    S_arr[, , j] <- diag(decomp$d, nrow = n1, ncol = n2)
  }
  U <- aperm(apply(U_arr, MARGIN = 1:2, ifft), c(2,3,1))
  Vt <- aperm(apply(Vt_arr, MARGIN = 1:2, ifft), c(2,3,1))
  S <- aperm(apply(S_arr, MARGIN = 1:2, ifft), c(2,3,1))
  U <- as.tensor(U)
  Vt <- as.tensor(Vt)
  V <- t_tpose(Vt,"fft")
  S <- as.tensor(S)
  invisible(list(U = U, V = V, S = S))
}
