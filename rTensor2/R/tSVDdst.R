tSVDdst <- function (tnsr)
  {
  # Performs a tensor singular value decomposition on any 3-mode
  # tensor using the discrete sine transform.

  # Input: A, 3-mode tensor
  # Output: Tensors U (left singular value object),
  # V (right singular value object) and
  # S, a diagonal tensor so that A=USV^T.

    if (tnsr@num_modes != 3)
      stop("tSVDdst only implemented for 3d so far")
    modes <- tnsr@modes
    n1 <- modes[1]
    n2 <- modes[2]
    n3 <- modes[3]
    dstz <- aperm(apply(tnsr@data, MARGIN = 1:2, dst), c(2,3,1))
    U_arr <- array(0, dim = c(n1, n1, n3))
    V_arr <- array(0, dim = c(n2, n2, n3))
    m <- min(n1, n2)
    S_arr <- array(0, dim = c(n1, n2, n3))
    for (j in 1:n3) {
      decomp <- svd(dstz[, , j], nu = n1, nv = n2)
      U_arr[, , j] <- decomp$u
      V_arr[, , j] <- decomp$v
      S_arr[, , j] <- diag(decomp$d, nrow = n1, ncol = n2)
    }
    U <- as.tensor(aperm(apply(U_arr, MARGIN = 1:2,idst), c(2,3,1)))
    V <- as.tensor(aperm(apply(V_arr, MARGIN = 1:2,idst), c(2,3,1)))
    S <- as.tensor(aperm(apply(S_arr, MARGIN = 1:2,idst), c(2,3,1)))
    invisible(list(U = U, V = V, S = S))
  }
