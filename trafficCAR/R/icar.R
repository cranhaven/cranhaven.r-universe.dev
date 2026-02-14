#' Sample ICAR random effects with component-wise sum-to-zero constraints
#'
#' @param A Sparse adjacency matrix (dgCMatrix preferred).
#' @param tau Positive precision multiplier.
#' @param kappa Large penalty enforcing sum-to-zero (default 1e6).
#' @param isolate How to handle isolated nodes: "independent" or "drop".
#' @param isolate_prec Precision for isolated nodes if kept independent.
#'
#'@importFrom stats rnorm
#' @return Numeric vector of length nrow(A).
#' @keywords internal
sample_icar <- function(A,
                        tau = 1,
                        kappa = 1e6,
                        isolate = c("independent", "drop"),
                        isolate_prec = NULL) {

  isolate <- match.arg(isolate)

  if (!inherits(A, "Matrix")) {
    A <- Matrix::Matrix(A, sparse = TRUE)
  }

  if (nrow(A) != ncol(A)) stop("`A` must be square.")

  if (!is.numeric(tau) || length(tau) != 1 || !is.finite(tau) || tau <= 0) {
    stop("`tau` must be a positive finite scalar.")
  }
  if (!is.numeric(kappa) || length(kappa) != 1 || !is.finite(kappa) || kappa <= 0) {
    stop("`kappa` must be a positive finite scalar.")
  }
  if (!is.null(isolate_prec) &&
      (!is.numeric(isolate_prec) || length(isolate_prec) != 1 ||
       !is.finite(isolate_prec) || isolate_prec <= 0)) {
    stop("`isolate_prec` must be a positive finite scalar (or NULL).")
  }

  A <- Matrix::forceSymmetric(A, uplo = "U")
  diag(A) <- 0

  n <- nrow(A)

  # graph structure
  g <- igraph::graph_from_adjacency_matrix(
    as.matrix(A != 0),
    mode = "undirected",
    diag = FALSE
  )
  comps <- igraph::components(g)
  memb <- comps$membership
  deg  <- Matrix::rowSums(A != 0)

  iso_idx <- which(deg == 0)
  noniso  <- which(deg > 0)

  x <- numeric(n)

  # isolates
  if (length(iso_idx) > 0) {
    if (isolate == "drop") {
      x[iso_idx] <- NA_real_
    } else {
      if (is.null(isolate_prec)) isolate_prec <- tau
      x[iso_idx] <- rnorm(length(iso_idx), sd = 1 / sqrt(isolate_prec))
    }
  }

  if (!length(noniso)) return(x)

  # ICAR precision on non-isolates
  A2 <- A[noniso, noniso, drop = FALSE]
  # raw ICAR precision (singular)
  d <- Matrix::rowSums(A2)
  Q <- tau * (Matrix::Diagonal(n = nrow(A2), x = as.numeric(d)) - A2)
  Q <- Matrix::forceSymmetric(Q, uplo = "U")


  memb2 <- memb[noniso]

  # add component-wise constraints
  for (cc in unique(memb2)) {
    idx <- which(memb2 == cc)
    if (length(idx) >= 2) {
      one <- rep(1, length(idx))
      Q[idx, idx] <- Q[idx, idx, drop = FALSE] +
        kappa * (one %*% t(one))
    } else {
      Q[idx, idx] <- Q[idx, idx] + kappa
    }
  }

  # sample
  x2 <- rmvnorm_prec(Q)

  # post-center (numerical cleanup)
  for (cc in unique(memb2)) {
    idx <- which(memb2 == cc)
    if (length(idx) >= 2) {
      x2[idx] <- x2[idx] - mean(x2[idx])
    }
  }

  x[noniso] <- x2
  x
}
