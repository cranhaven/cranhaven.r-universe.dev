#' Distance Metrics for Functional Data
#'
#' Functions for computing various distance metrics between functional data.

#' Compute Distance Metric for Functional Data
#'
#' Unified interface for computing various distance metrics between functional
#' data objects. This function dispatches to the appropriate specialized
#' distance function based on the method parameter.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataref An object of class 'fdata'. If NULL, computes self-distances.
#' @param method Distance method to use. One of:
#'   \itemize{
#'     \item "lp" - Lp metric (default)
#'     \item "hausdorff" - Hausdorff distance
#'     \item "dtw" - Dynamic Time Warping
#'     \item "pca" - Semi-metric based on PCA scores
#'     \item "deriv" - Semi-metric based on derivatives
#'     \item "basis" - Semi-metric based on basis coefficients
#'     \item "fourier" - Semi-metric based on FFT coefficients
#'     \item "hshift" - Semi-metric with horizontal shift
#'     \item "kl" - Symmetric Kullback-Leibler divergence
#'   }
#' @param ... Additional arguments passed to the specific distance function.
#'
#' @return A distance matrix.
#'
#' @details
#' This function provides a convenient unified interface for all distance
#' computations in fdars. The additional arguments in \code{...} are passed
#' to the underlying distance function:
#'
#' \itemize{
#'   \item lp: lp, w
#'   \item hausdorff: (none)
#'   \item dtw: p, w
#'   \item pca: ncomp
#'   \item deriv: nderiv, lp
#'   \item basis: nbasis, basis, nderiv
#'   \item fourier: nfreq
#'   \item hshift: max_shift
#'   \item kl: eps, normalize
#' }
#'
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(200), 20, 10))
#'
#' # Using different distance methods
#' D_lp <- metric(fd, method = "lp")
#' D_hausdorff <- metric(fd, method = "hausdorff")
#' D_pca <- metric(fd, method = "pca", ncomp = 3)
#'
#' # Cross-distances
#' fd2 <- fdata(matrix(rnorm(100), 10, 10))
#' D_cross <- metric(fd, fd2, method = "lp")
metric <- function(fdataobj, fdataref = NULL, method = "lp", ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  method <- match.arg(method, c("lp", "hausdorff", "dtw", "pca", "deriv",
                                 "basis", "fourier", "hshift", "kl"))

  switch(method,
    "lp" = metric.lp(fdataobj, fdataref, ...),
    "hausdorff" = metric.hausdorff(fdataobj, fdataref, ...),
    "dtw" = metric.DTW(fdataobj, fdataref, ...),
    "pca" = semimetric.pca(fdataobj, fdataref, ...),
    "deriv" = semimetric.deriv(fdataobj, fdataref, ...),
    "basis" = semimetric.basis(fdataobj, fdataref, ...),
    "fourier" = semimetric.fourier(fdataobj, fdataref, ...),
    "hshift" = semimetric.hshift(fdataobj, fdataref, ...),
    "kl" = metric.kl(fdataobj, fdataref, ...)
  )
}

#' Lp Metric for Functional Data
#'
#' Computes the Lp distance between functional data objects using
#' numerical integration (Simpson's rule). Works with both regular \code{fdata}
#' and irregular \code{irregFdata} objects.
#'
#' @param x A functional data object (\code{fdata} or \code{irregFdata}).
#' @param y An object of class 'fdata'. If NULL, computes self-distances
#'   for x (more efficient symmetric computation). Only supported for \code{fdata}.
#' @param p The order of the Lp metric (default 2 for L2 distance).
#' @param w Optional weight vector of length equal to number of evaluation
#'   points. Default is uniform weighting. Only supported for \code{fdata}.
#' @param ... Additional arguments passed to methods.
#'
#' @return A distance matrix.
#'
#' @export
#' @examples
#' # Regular fdata
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#' D <- metric.lp(fd)  # Self-distances
#'
#' # Irregular fdata
#' ifd <- sparsify(fd, minObs = 3, maxObs = 7, seed = 42)
#' D_irreg <- metric.lp(ifd)
metric.lp <- function(x, ...) {
  UseMethod("metric.lp")
}

#' @rdname metric.lp
#' @export
metric.lp.fdata <- function(x, y = NULL, p = 2, w = 1, ...) {
  fdata1 <- x
  fdata2 <- y
  lp <- p

  if (fdata1$fdata2d) {
    # 2D functional data (surfaces)
    dims <- fdata1$dims
    m1 <- dims[1]
    m2 <- dims[2]
    m <- m1 * m2

    # Handle weight matrix for 2D
    if (length(w) == 1) {
      w <- matrix(w, m1, m2)
    }
    if (!is.matrix(w) || nrow(w) != m1 || ncol(w) != m2) {
      if (length(w) == m) {
        w <- matrix(w, m1, m2)
      } else {
        stop("Weight must be scalar, vector of length m1*m2, or m1 x m2 matrix")
      }
    }

    argvals_s <- as.numeric(fdata1$argvals[[1]])
    argvals_t <- as.numeric(fdata1$argvals[[2]])

    # Data is already flattened [n, m1*m2]
    if (is.null(fdata2)) {
      D <- .Call("wrap__metric_lp_self_2d", fdata1$data, argvals_s, argvals_t,
                 as.numeric(lp), as.numeric(as.vector(t(w))))
    } else {
      if (!inherits(fdata2, "fdata")) {
        stop("fdata2 must be of class 'fdata'")
      }
      if (!fdata2$fdata2d) {
        stop("Cannot compute distances between 1D and 2D functional data")
      }
      if (!identical(fdata1$dims, fdata2$dims)) {
        stop("fdata1 and fdata2 must have the same grid dimensions")
      }

      D <- .Call("wrap__metric_lp_2d", fdata1$data, fdata2$data, argvals_s, argvals_t,
                 as.numeric(lp), as.numeric(as.vector(t(w))))
    }
  } else {
    # 1D functional data (curves)
    m <- ncol(fdata1$data)

    # Handle weight vector
    if (length(w) == 1) {
      w <- rep(w, m)
    }
    if (length(w) != m) {
      stop("Weight vector must have length equal to number of evaluation points")
    }

    if (is.null(fdata2)) {
      # Self-distances (symmetric, more efficient)
      D <- .Call("wrap__metric_lp_self_1d", fdata1$data, as.numeric(fdata1$argvals), as.numeric(lp), as.numeric(w))
    } else {
      if (!inherits(fdata2, "fdata")) {
        stop("fdata2 must be of class 'fdata'")
      }

      if (fdata2$fdata2d) {
        stop("Cannot compute distances between 1D and 2D functional data")
      }

      if (ncol(fdata1$data) != ncol(fdata2$data)) {
        stop("fdata1 and fdata2 must have the same number of evaluation points")
      }

      D <- .Call("wrap__metric_lp_1d", fdata1$data, fdata2$data, as.numeric(fdata1$argvals), as.numeric(lp), as.numeric(w))
    }
  }

  # Convert to proper matrix with dimnames
  D <- as.matrix(D)

  # Add row and column names if available
  if (!is.null(rownames(fdata1$data))) {
    rownames(D) <- rownames(fdata1$data)
  }

  if (is.null(fdata2)) {
    if (!is.null(rownames(fdata1$data))) {
      colnames(D) <- rownames(fdata1$data)
    }
  } else if (!is.null(rownames(fdata2$data))) {
    colnames(D) <- rownames(fdata2$data)
  }

  D
}

#' Hausdorff Metric for Functional Data
#'
#' Computes the Hausdorff distance between functional data objects.
#' The Hausdorff distance treats each curve as a set of points (t, f(t))
#' in 2D space and computes the maximum of the minimum distances.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataref An object of class 'fdata'. If NULL, uses fdataobj.
#' @param ... Additional arguments (ignored).
#'
#' @return A distance matrix.
#'
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#' D <- metric.hausdorff(fd)
metric.hausdorff <- function(fdataobj, fdataref = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (fdataobj$fdata2d) {
    # 2D functional data (surfaces)
    dims <- fdataobj$dims
    m1 <- dims[1]
    m2 <- dims[2]

    argvals_s <- as.numeric(fdataobj$argvals[[1]])
    argvals_t <- as.numeric(fdataobj$argvals[[2]])

    # Data is already flattened [n, m1*m2]
    if (is.null(fdataref)) {
      # Self-distances (symmetric)
      D <- .Call("wrap__metric_hausdorff_2d", fdataobj$data, argvals_s, argvals_t)
    } else {
      if (!inherits(fdataref, "fdata")) {
        stop("fdataref must be of class 'fdata'")
      }
      if (!fdataref$fdata2d) {
        stop("Cannot compute distances between 1D and 2D functional data")
      }
      if (!identical(fdataobj$dims, fdataref$dims)) {
        stop("fdataobj and fdataref must have the same grid dimensions")
      }

      D <- .Call("wrap__metric_hausdorff_cross_2d", fdataobj$data, fdataref$data, argvals_s, argvals_t)
    }
  } else {
    # 1D functional data (curves)
    if (is.null(fdataref)) {
      # Self-distances (symmetric) - use optimized Rust implementation
      D <- .Call("wrap__metric_hausdorff_1d", fdataobj$data, as.numeric(fdataobj$argvals))
    } else {
      if (!inherits(fdataref, "fdata")) {
        stop("fdataref must be of class 'fdata'")
      }

      if (fdataref$fdata2d) {
        stop("Cannot compute distances between 1D and 2D functional data")
      }

      if (ncol(fdataobj$data) != ncol(fdataref$data)) {
        stop("fdataobj and fdataref must have the same number of evaluation points")
      }

      # Cross-distances - use Rust implementation
      D <- .Call("wrap__metric_hausdorff_cross_1d", fdataobj$data, fdataref$data, as.numeric(fdataobj$argvals))
    }
  }

  # Convert to proper matrix with dimnames
  D <- as.matrix(D)

  # Add row and column names if available
  if (!is.null(rownames(fdataobj$data))) {
    rownames(D) <- rownames(fdataobj$data)
  }

  if (is.null(fdataref)) {
    if (!is.null(rownames(fdataobj$data))) {
      colnames(D) <- rownames(fdataobj$data)
    }
  } else if (!is.null(rownames(fdataref$data))) {
    colnames(D) <- rownames(fdataref$data)
  }

  D
}

#' Dynamic Time Warping for Functional Data
#'
#' Computes the Dynamic Time Warping distance between functional data.
#' DTW allows for non-linear alignment of curves.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataref An object of class 'fdata'. If NULL, computes self-distances.
#' @param p The p in Lp distance (default 2 for L2/Euclidean).
#' @param w Sakoe-Chiba window constraint. Default is min(ncol(fdataobj), ncol(fdataref)).
#'   Use -1 for no window constraint.
#' @param ... Additional arguments (ignored).
#'
#' @return A distance matrix.
#'
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#' D <- metric.DTW(fd)
metric.DTW <- function(fdataobj, fdataref = NULL, p = 2, w = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (fdataobj$fdata2d) {
    stop("metric.DTW not yet implemented for 2D functional data")
  }

  m <- ncol(fdataobj$data)

  if (is.null(fdataref)) {
    # Self-distances - use optimized Rust implementation
    if (is.null(w)) w <- m
    D <- .Call("wrap__metric_dtw_self_1d", fdataobj$data, as.numeric(p), as.integer(w))
  } else {
    if (!inherits(fdataref, "fdata")) {
      stop("fdataref must be of class 'fdata'")
    }

    if (fdataref$fdata2d) {
      stop("metric.DTW not yet implemented for 2D functional data")
    }

    # Cross-distances - use Rust implementation
    m2 <- ncol(fdataref$data)
    if (is.null(w)) w <- min(m, m2)
    D <- .Call("wrap__metric_dtw_cross_1d", fdataobj$data, fdataref$data, as.numeric(p), as.integer(w))
  }

  # Convert to proper matrix with dimnames
  D <- as.matrix(D)

  # Add row and column names if available
  if (!is.null(rownames(fdataobj$data))) {
    rownames(D) <- rownames(fdataobj$data)
  }

  if (is.null(fdataref)) {
    if (!is.null(rownames(fdataobj$data))) {
      colnames(D) <- rownames(fdataobj$data)
    }
  } else if (!is.null(rownames(fdataref$data))) {
    colnames(D) <- rownames(fdataref$data)
  }

  D
}

#' Semi-metric based on Principal Components
#'
#' Computes a semi-metric based on the first ncomp principal component scores.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataref An object of class 'fdata'. If NULL, uses fdataobj.
#' @param ncomp Number of principal components to use.
#' @param ... Additional arguments (ignored).
#'
#' @return A distance matrix based on PC scores.
#'
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(200), 20, 10))
#' D <- semimetric.pca(fd, ncomp = 3)
semimetric.pca <- function(fdataobj, fdataref = NULL, ncomp = 2, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  # Data is already flattened [n, m1*m2] for 2D fdata
  is_2d <- isTRUE(fdataobj$fdata2d)
  data1 <- fdataobj$data
  n1 <- nrow(fdataobj$data)

  # Compute PCA on combined data
  if (is.null(fdataref)) {
    combined <- data1
    n2 <- n1
    same_data <- TRUE
  } else {
    if (!inherits(fdataref, "fdata")) {
      stop("fdataref must be of class 'fdata'")
    }
    if (isTRUE(fdataobj$fdata2d) != isTRUE(fdataref$fdata2d)) {
      stop("Cannot compute distances between 1D and 2D functional data")
    }
    if (is_2d) {
      if (!identical(fdataobj$dims, fdataref$dims)) {
        stop("fdataobj and fdataref must have the same grid dimensions")
      }
    } else {
      if (ncol(fdataobj$data) != ncol(fdataref$data)) {
        stop("fdataobj and fdataref must have the same number of evaluation points")
      }
    }
    data2 <- fdataref$data
    n2 <- nrow(fdataref$data)
    combined <- rbind(data1, data2)
    same_data <- FALSE
  }

  # Center the data
  centered <- scale(combined, center = TRUE, scale = FALSE)

  # Compute SVD
  svd_result <- svd(centered, nu = ncomp, nv = ncomp)

  # Get PC scores
  scores <- centered %*% svd_result$v[, seq_len(ncomp), drop = FALSE]

  # Compute distances
  scores1 <- scores[seq_len(n1), , drop = FALSE]
  if (same_data) {
    scores2 <- scores1
  } else {
    scores2 <- scores[(n1 + 1):(n1 + n2), , drop = FALSE]
  }

  # Euclidean distance in PC space
  D <- matrix(0, n1, n2)
  for (i in seq_len(n1)) {
    for (j in seq_len(n2)) {
      D[i, j] <- sqrt(sum((scores1[i, ] - scores2[j, ])^2))
    }
  }

  D
}

#' Semi-metric based on Derivatives
#'
#' Computes a semi-metric based on the Lp distance of the nderiv-th derivative
#' of functional data.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataref An object of class 'fdata'. If NULL, uses fdataobj.
#' @param nderiv Derivative order (1, 2, ...). Default is 1.
#' @param lp The p in Lp metric. Default is 2 (L2 distance).
#' @param ... Additional arguments passed to deriv.
#'
#' @return A distance matrix based on derivative distances.
#'
#' @export
#' @examples
#' # Create smooth curves
#' t <- seq(0, 2*pi, length.out = 100)
#' X <- matrix(0, 10, 100)
#' for (i in 1:10) X[i, ] <- sin(t + i/5)
#' fd <- fdata(X, argvals = t)
#'
#' # Compute distance based on first derivative
#' D <- semimetric.deriv(fd, nderiv = 1)
semimetric.deriv <- function(fdataobj, fdataref = NULL, nderiv = 1, lp = 2, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  is_2d <- isTRUE(fdataobj$fdata2d)

  if (is_2d) {
    # 2D case: deriv returns a list of derivatives (ds, dt, dsdt)
    # Use the sum of all derivative Lp distances
    fdataobj_derivs <- deriv(fdataobj, nderiv = nderiv, ...)

    if (is.null(fdataref)) {
      # Self-distances - combine derivative distances
      D_ds <- metric.lp(fdataobj_derivs$ds, lp = lp)
      D_dt <- metric.lp(fdataobj_derivs$dt, lp = lp)
      D <- sqrt(D_ds^2 + D_dt^2)
    } else {
      if (!inherits(fdataref, "fdata")) {
        stop("fdataref must be of class 'fdata'")
      }
      if (!isTRUE(fdataref$fdata2d)) {
        stop("Cannot compute distances between 1D and 2D functional data")
      }

      fdataref_derivs <- deriv(fdataref, nderiv = nderiv, ...)
      D_ds <- metric.lp(fdataobj_derivs$ds, fdataref_derivs$ds, lp = lp)
      D_dt <- metric.lp(fdataobj_derivs$dt, fdataref_derivs$dt, lp = lp)
      D <- sqrt(D_ds^2 + D_dt^2)
    }
  } else {
    # 1D case
    # Compute derivative of fdataobj
    fdataobj_deriv <- deriv(fdataobj, nderiv = nderiv, ...)

    if (is.null(fdataref)) {
      # Self-distances
      D <- metric.lp(fdataobj_deriv, lp = lp)
    } else {
      if (!inherits(fdataref, "fdata")) {
        stop("fdataref must be of class 'fdata'")
      }

      if (isTRUE(fdataref$fdata2d)) {
        stop("Cannot compute distances between 1D and 2D functional data")
      }

      # Compute derivative of fdataref
      fdataref_deriv <- deriv(fdataref, nderiv = nderiv, ...)

      D <- metric.lp(fdataobj_deriv, fdataref_deriv, lp = lp)
    }
  }

  D
}

#' Semi-metric based on Basis Expansion
#'
#' Computes a semi-metric based on the L2 distance of basis expansion
#' coefficients. Supports B-spline and Fourier basis.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataref An object of class 'fdata'. If NULL, uses fdataobj.
#' @param nbasis Number of basis functions. Default is 5.
#' @param basis Type of basis: "bspline" (default) or "fourier".
#' @param nderiv Derivative order to compute distance on (default 0).
#' @param ... Additional arguments (ignored).
#'
#' @return A distance matrix based on basis coefficients.
#'
#' @export
#' @examples
#' # Create curves
#' t <- seq(0, 1, length.out = 100)
#' X <- matrix(0, 10, 100)
#' for (i in 1:10) X[i, ] <- sin(2*pi*t + i/5) + rnorm(100, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#'
#' # Compute distance based on B-spline coefficients
#' D <- semimetric.basis(fd, nbasis = 7)
semimetric.basis <- function(fdataobj, fdataref = NULL, nbasis = 5, basis = "bspline",
                             nderiv = 0, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("semimetric.basis not yet implemented for 2D functional data")
  }

  basis <- match.arg(basis, c("bspline", "fourier"))
  argvals <- fdataobj$argvals
  rangeval <- fdataobj$rangeval
  m <- length(argvals)

  # Create basis matrix
  if (basis == "bspline") {
    # B-spline basis using polynomial representation
    degree <- 3
    knots <- seq(rangeval[1], rangeval[2], length.out = nbasis - degree + 1)
    # Use R's built-in splineDesign for B-splines
    B <- splines::bs(argvals, knots = knots[2:(length(knots)-1)],
                     degree = degree, intercept = TRUE, Boundary.knots = rangeval)
    B <- as.matrix(B)
  } else {
    # Fourier basis
    B <- matrix(0, m, nbasis)
    t_scaled <- (argvals - rangeval[1]) / (rangeval[2] - rangeval[1])

    B[, 1] <- 1  # constant term
    k <- 2
    for (freq in 1:((nbasis - 1) %/% 2)) {
      if (k <= nbasis) {
        B[, k] <- sin(2 * pi * freq * t_scaled)
        k <- k + 1
      }
      if (k <= nbasis) {
        B[, k] <- cos(2 * pi * freq * t_scaled)
        k <- k + 1
      }
    }
  }

  # Compute basis coefficients for fdataobj via least squares
  # data[n x m], B[m x nbasis], coef[n x nbasis]
  # coef = data %*% B %*% (B'B)^-1
  BtB_inv <- solve(crossprod(B))
  coef1 <- fdataobj$data %*% B %*% BtB_inv

  if (is.null(fdataref)) {
    coef2 <- coef1
    same_data <- TRUE
  } else {
    if (!inherits(fdataref, "fdata")) {
      stop("fdataref must be of class 'fdata'")
    }
    if (isTRUE(fdataref$fdata2d)) {
      stop("semimetric.basis not yet implemented for 2D functional data")
    }
    coef2 <- fdataref$data %*% B %*% BtB_inv
    same_data <- FALSE
  }

  # Compute L2 distance in coefficient space
  n1 <- nrow(coef1)
  n2 <- nrow(coef2)

  D <- matrix(0, n1, n2)
  for (i in seq_len(n1)) {
    for (j in seq_len(n2)) {
      D[i, j] <- sqrt(sum((coef1[i, ] - coef2[j, ])^2))
    }
  }

  D
}

#' Semi-metric based on Fourier Coefficients (FFT)
#'
#' Computes a semi-metric based on the L2 distance of Fourier coefficients
#' computed via Fast Fourier Transform (FFT). This is more efficient than
#' the Fourier basis option in semimetric.basis for large nfreq.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataref An object of class 'fdata'. If NULL, uses fdataobj.
#' @param nfreq Number of Fourier frequencies to use (excluding DC). Default is 5.
#' @param ... Additional arguments (ignored).
#'
#' @return A distance matrix based on Fourier coefficients.
#'
#' @details
#' The Fourier coefficients are computed using FFT and normalized by the
#' number of points. The distance is the L2 distance between the magnitude
#' of the first nfreq+1 coefficients (DC + nfreq frequencies).
#'
#' This function uses Rust's rustfft library for efficient FFT computation,
#' making it faster than R's base fft for large datasets.
#'
#' @export
#' @examples
#' # Create curves with different frequency content
#' t <- seq(0, 1, length.out = 100)
#' X <- matrix(0, 10, 100)
#' for (i in 1:10) X[i, ] <- sin(2*pi*i*t) + rnorm(100, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#'
#' # Compute distance based on Fourier coefficients
#' D <- semimetric.fourier(fd, nfreq = 10)
semimetric.fourier <- function(fdataobj, fdataref = NULL, nfreq = 5, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("semimetric.fourier not yet implemented for 2D functional data")
  }

  m <- ncol(fdataobj$data)
  nfreq <- min(nfreq, m %/% 2 - 1)  # Can't have more than m/2 meaningful frequencies

  if (is.null(fdataref)) {
    # Self-distances (symmetric)
    D <- .Call("wrap__semimetric_fourier_self_1d", fdataobj$data, as.integer(nfreq))
  } else {
    if (!inherits(fdataref, "fdata")) {
      stop("fdataref must be of class 'fdata'")
    }

    if (isTRUE(fdataref$fdata2d)) {
      stop("semimetric.fourier not yet implemented for 2D functional data")
    }

    D <- .Call("wrap__semimetric_fourier_cross_1d", fdataobj$data, fdataref$data, as.integer(nfreq))
  }

  # Convert to proper matrix with dimnames
  D <- as.matrix(D)

  # Add row and column names if available
  if (!is.null(rownames(fdataobj$data))) {
    rownames(D) <- rownames(fdataobj$data)
  }

  if (is.null(fdataref)) {
    if (!is.null(rownames(fdataobj$data))) {
      colnames(D) <- rownames(fdataobj$data)
    }
  } else if (!is.null(rownames(fdataref$data))) {
    colnames(D) <- rownames(fdataref$data)
  }

  D
}

#' Semi-metric based on Horizontal Shift (Time Warping)
#'
#' Computes a semi-metric based on the minimum L2 distance after optimal
#' horizontal shifting of curves. This is useful for comparing curves that
#' may have phase differences.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataref An object of class 'fdata'. If NULL, uses fdataobj.
#' @param max_shift Maximum shift in number of grid points. Default is m/4
#'   where m is the number of evaluation points. Use -1 for automatic.
#' @param ... Additional arguments (ignored).
#'
#' @return A distance matrix based on minimum L2 distance after shift.
#'
#' @details
#' For each pair of curves, this function computes:
#' \deqn{d(f, g) = \min_{|h| \le h_{max}} ||f(t) - g(t+h)||}
#' where h is the horizontal shift in discrete units.
#'
#' This semi-metric is useful when comparing curves with phase shifts,
#' such as ECG signals with different heart rates or periodic signals
#' with different phases.
#'
#' @export
#' @examples
#' # Create curves with phase shifts
#' t <- seq(0, 2*pi, length.out = 100)
#' X <- matrix(0, 10, 100)
#' for (i in 1:10) X[i, ] <- sin(t + i*0.2) + rnorm(100, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#'
#' # Compute distance accounting for phase shifts
#' D <- semimetric.hshift(fd, max_shift = 10)
semimetric.hshift <- function(fdataobj, fdataref = NULL, max_shift = -1, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("semimetric.hshift not yet implemented for 2D functional data")
  }

  if (is.null(fdataref)) {
    # Self-distances (symmetric)
    D <- .Call("wrap__semimetric_hshift_self_1d", fdataobj$data,
               as.numeric(fdataobj$argvals), as.integer(max_shift))
  } else {
    if (!inherits(fdataref, "fdata")) {
      stop("fdataref must be of class 'fdata'")
    }

    if (isTRUE(fdataref$fdata2d)) {
      stop("semimetric.hshift not yet implemented for 2D functional data")
    }

    if (ncol(fdataobj$data) != ncol(fdataref$data)) {
      stop("fdataobj and fdataref must have the same number of evaluation points")
    }

    D <- .Call("wrap__semimetric_hshift_cross_1d", fdataobj$data, fdataref$data,
               as.numeric(fdataobj$argvals), as.integer(max_shift))
  }

  # Convert to proper matrix with dimnames
  D <- as.matrix(D)

  # Add row and column names if available
  if (!is.null(rownames(fdataobj$data))) {
    rownames(D) <- rownames(fdataobj$data)
  }

  if (is.null(fdataref)) {
    if (!is.null(rownames(fdataobj$data))) {
      colnames(D) <- rownames(fdataobj$data)
    }
  } else if (!is.null(rownames(fdataref$data))) {
    colnames(D) <- rownames(fdataref$data)
  }

  D
}

#' Kullback-Leibler Divergence Metric for Functional Data
#'
#' Computes the symmetric Kullback-Leibler divergence between functional data
#' treated as probability distributions. Curves are first normalized to be
#' valid probability density functions.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataref An object of class 'fdata'. If NULL, computes self-distances.
#' @param eps Small value for numerical stability (default 1e-10).
#' @param normalize Logical. If TRUE (default), curves are shifted to be
#'   non-negative and normalized to integrate to 1.
#' @param ... Additional arguments (ignored).
#'
#' @return A distance matrix based on symmetric KL divergence.
#'
#' @details
#' The symmetric KL divergence is computed as:
#' \deqn{D_{KL}(f, g) = \frac{1}{2}[KL(f||g) + KL(g||f)]}
#' where
#' \deqn{KL(f||g) = \int f(t) \log\frac{f(t)}{g(t)} dt}
#'
#' When \code{normalize = TRUE}, curves are first shifted to be non-negative
#' (by subtracting the minimum and adding eps), then normalized to integrate
#' to 1. This makes them valid probability density functions.
#'
#' The symmetric KL divergence is always non-negative and equals zero only
#' when the two distributions are identical. However, it does not satisfy
#' the triangle inequality.
#'
#' @export
#' @examples
#' # Create curves that look like probability densities
#' t <- seq(0, 1, length.out = 100)
#' X <- matrix(0, 10, 100)
#' for (i in 1:10) {
#'   # Shifted Gaussian-like curves
#'   X[i, ] <- exp(-(t - 0.3 - i/50)^2 / 0.02) + rnorm(100, sd = 0.01)
#' }
#' fd <- fdata(X, argvals = t)
#'
#' # Compute KL divergence
#' D <- metric.kl(fd)
metric.kl <- function(fdataobj, fdataref = NULL, eps = 1e-10, normalize = TRUE, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("metric.kl not yet implemented for 2D functional data")
  }

  if (is.null(fdataref)) {
    # Self-distances (symmetric)
    D <- .Call("wrap__metric_kl_self_1d", fdataobj$data,
               as.numeric(fdataobj$argvals), as.numeric(eps), as.logical(normalize))
  } else {
    if (!inherits(fdataref, "fdata")) {
      stop("fdataref must be of class 'fdata'")
    }

    if (isTRUE(fdataref$fdata2d)) {
      stop("metric.kl not yet implemented for 2D functional data")
    }

    if (ncol(fdataobj$data) != ncol(fdataref$data)) {
      stop("fdataobj and fdataref must have the same number of evaluation points")
    }

    D <- .Call("wrap__metric_kl_cross_1d", fdataobj$data, fdataref$data,
               as.numeric(fdataobj$argvals), as.numeric(eps), as.logical(normalize))
  }

  # Convert to proper matrix with dimnames
  D <- as.matrix(D)

  # Add row and column names if available
  if (!is.null(rownames(fdataobj$data))) {
    rownames(D) <- rownames(fdataobj$data)
  }

  if (is.null(fdataref)) {
    if (!is.null(rownames(fdataobj$data))) {
      colnames(D) <- rownames(fdataobj$data)
    }
  } else if (!is.null(rownames(fdataref$data))) {
    colnames(D) <- rownames(fdataref$data)
  }

  D
}
