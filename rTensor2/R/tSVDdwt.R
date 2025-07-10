tSVDdwt <- function (tnsr)
  {
  # Performs a tensor singular value decomposition on any 3-mode
  # tensor using the discrete wavelet transform.

  # Input: A, 3-mode tensor
  # Output: Tensors U (left singular value object),
  # V (right singular value object) and
  # S, a diagonal tensor so that A=USV^T.

    if (tnsr@num_modes != 3)
      stop("tSVDdwt only implemented for 3d so far")
    modes <- tnsr@modes
    n1 <- modes[1]
    n2 <- modes[2]
    n3 <- modes[3]
    if (sum(as.numeric(intToBits(n3))) != 1)
      stop("Mode 3 must be a power of 2 otherwise using 0 padding")
    dwtz <- tDWT(tnsr)
    U_arr <- array(0, dim = c(n1, n1, n3))
    V_arr <- array(0, dim = c(n2, n2, n3))
    m <- min(n1, n2)
    S_arr <- array(0, dim = c(n1, n2, n3))
    for (j in 1:n3) {
      decomp <- svd(dwtz[, , j]@data, nu = n1, nv = n2)
      U_arr[, , j] <- decomp$u
      V_arr[, , j] <- decomp$v
      S_arr[, , j] <- diag(decomp$d, nrow = n1, ncol = n2)
    }
    # Inverse DWT
    U <- tIDWT(as.tensor(U_arr))
    V <- tIDWT(as.tensor(V_arr))
    S <- tIDWT(as.tensor(S_arr))
    invisible(list(U = U, V = V, S = S))
  }
