#' Depth Functions for Functional Data
#'
#' Functions for computing various depth measures for functional data.

#' Compute Functional Data Depth
#'
#' Unified interface for computing various depth measures for functional data.
#'
#' @param fdataobj An object of class 'fdata' to compute depth for.
#' @param fdataori An object of class 'fdata' as reference sample.
#'   If NULL, uses fdataobj as reference.
#' @param method Depth method to use. One of "FM" (Fraiman-Muniz), "mode" (modal),
#'   "RP" (random projection), "RT" (random Tukey), "BD" (band depth),
#'   "MBD" (modified band depth), "MEI" (modified epigraph index),
#'   "FSD" (functional spatial depth), "KFSD" (kernel functional spatial depth),
#'   or "RPD" (random projection with derivatives). Default is "FM".
#' @param ... Additional arguments passed to the specific depth function.
#'
#' @return A numeric vector of depth values, one per curve in fdataobj.
#'
#' @details
#' Available methods:
#' \describe{
#'   \item{FM}{Fraiman-Muniz depth - integrates univariate depths over domain}
#'   \item{mode}{Modal depth - based on kernel density estimation}
#'   \item{RP}{Random projection depth - projects to random directions}
#'   \item{RT}{Random Tukey depth - halfspace depth via random projections}
#'   \item{BD}{Band depth - proportion of bands containing the curve (1D only)}
#'   \item{MBD}{Modified band depth - allows partial containment (1D only)}
#'   \item{MEI}{Modified epigraph index - proportion of time below other curves (1D only)}
#'   \item{FSD}{Functional spatial depth - based on spatial signs}
#'   \item{KFSD}{Kernel functional spatial depth - smoothed FSD}
#'   \item{RPD}{Random projection with derivatives - includes curve derivatives}
#' }
#'
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#'
#' # Different depth methods
#' depth(fd, method = "FM")
#' depth(fd, method = "mode")
#' depth(fd, method = "RP")
depth <- function(fdataobj, fdataori = NULL, method = c("FM", "mode", "RP", "RT",
                  "BD", "MBD", "MEI", "FSD", "KFSD", "RPD"), ...) {
  method <- match.arg(method)

  switch(method,
    "FM" = .depth.FM(fdataobj, fdataori, ...),
    "mode" = .depth.mode(fdataobj, fdataori, ...),
    "RP" = .depth.RP(fdataobj, fdataori, ...),
    "RT" = .depth.RT(fdataobj, fdataori, ...),
    "BD" = .depth.BD(fdataobj, fdataori, ...),
    "MBD" = .depth.MBD(fdataobj, fdataori, ...),
    "MEI" = .depth.MEI(fdataobj, fdataori, ...),
    "FSD" = .depth.FSD(fdataobj, fdataori, ...),
    "KFSD" = .depth.KFSD(fdataobj, fdataori, ...),
    "RPD" = .depth.RPD(fdataobj, fdataori, ...)
  )
}

# Internal: Fraiman-Muniz Depth implementation
# @noRd
.depth.FM <- function(fdataobj, fdataori = NULL, trim = 0.25, scale = TRUE, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (is.null(fdataori)) {
    fdataori <- fdataobj
  }

  if (!inherits(fdataori, "fdata")) {
    stop("fdataori must be of class 'fdata'")
  }

  # Check for 2D functional data
  is_2d_obj <- isTRUE(fdataobj$fdata2d)
  is_2d_ori <- isTRUE(fdataori$fdata2d)

  if (is_2d_obj != is_2d_ori) {
    stop("Cannot compute depth between 1D and 2D functional data")
  }

  if (is_2d_obj) {
    # 2D functional data (surfaces) - flatten 3D arrays for Rust
    if (!identical(fdataobj$dims, fdataori$dims)) {
      stop("fdataobj and fdataori must have the same grid dimensions")
    }

    m1 <- as.integer(fdataobj$dims[1])
    m2 <- as.integer(fdataobj$dims[2])

    data_obj <- fdataobj$data
    data_ori <- fdataori$data

    return(.Call("wrap__depth_fm_2d", data_obj, data_ori, m1, m2, as.logical(scale)))
  }

  # 1D functional data (curves)
  # Validate dimensions
  if (ncol(fdataobj$data) != ncol(fdataori$data)) {
    stop("fdataobj and fdataori must have the same number of evaluation points")
  }

  .Call("wrap__depth_fm_1d", fdataobj$data, fdataori$data, as.numeric(trim), as.logical(scale))
}

# Internal: Band Depth implementation
# @noRd
.depth.BD <- function(fdataobj, fdataori = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (is.null(fdataori)) {
    fdataori <- fdataobj
  }

  if (!inherits(fdataori, "fdata")) {
    stop("fdataori must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d) || isTRUE(fdataori$fdata2d)) {
    stop("depth.BD not yet implemented for 2D functional data")
  }

  if (ncol(fdataobj$data) != ncol(fdataori$data)) {
    stop("fdataobj and fdataori must have the same number of evaluation points")
  }

  if (nrow(fdataori$data) < 2) {
    stop("fdataori must have at least 2 curves")
  }

  .Call("wrap__depth_bd_1d", fdataobj$data, fdataori$data)
}

# Internal: Modified Band Depth implementation
# @noRd
.depth.MBD <- function(fdataobj, fdataori = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (is.null(fdataori)) {
    fdataori <- fdataobj
  }

  if (!inherits(fdataori, "fdata")) {
    stop("fdataori must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d) || isTRUE(fdataori$fdata2d)) {
    stop("depth.MBD not yet implemented for 2D functional data")
  }

  if (ncol(fdataobj$data) != ncol(fdataori$data)) {
    stop("fdataobj and fdataori must have the same number of evaluation points")
  }

  if (nrow(fdataori$data) < 2) {
    stop("fdataori must have at least 2 curves")
  }

  .Call("wrap__depth_mbd_1d", fdataobj$data, fdataori$data)
}

# Internal: Modified Epigraph Index implementation
# @noRd
.depth.MEI <- function(fdataobj, fdataori = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (is.null(fdataori)) {
    fdataori <- fdataobj
  }

  if (!inherits(fdataori, "fdata")) {
    stop("fdataori must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d) || isTRUE(fdataori$fdata2d)) {
    stop("depth.MEI not yet implemented for 2D functional data")
  }

  if (ncol(fdataobj$data) != ncol(fdataori$data)) {
    stop("fdataobj and fdataori must have the same number of evaluation points")
  }

  .Call("wrap__depth_mei_1d", fdataobj$data, fdataori$data)
}

# Internal: Modal Depth implementation
# @noRd
.depth.mode <- function(fdataobj, fdataori = NULL, h = NULL, metric = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (is.null(fdataori)) {
    fdataori <- fdataobj
  }

  if (!inherits(fdataori, "fdata")) {
    stop("fdataori must be of class 'fdata'")
  }

  # Check for 2D functional data
  is_2d_obj <- isTRUE(fdataobj$fdata2d)
  is_2d_ori <- isTRUE(fdataori$fdata2d)

  if (is_2d_obj != is_2d_ori) {
    stop("Cannot compute depth between 1D and 2D functional data")
  }

  # Auto-compute bandwidth if not provided
  if (is.null(h)) {
    # Use Silverman's rule of thumb adapted for functional data
    n <- nrow(fdataori$data)
    h <- 1.06 * sd(fdataori$data) * n^(-1/5)
    if (h < 1e-10) h <- 0.1  # fallback
  }

  if (is_2d_obj) {
    # 2D functional data (surfaces) - flatten 3D arrays for Rust
    if (!identical(fdataobj$dims, fdataori$dims)) {
      stop("fdataobj and fdataori must have the same grid dimensions")
    }

    m1 <- as.integer(fdataobj$dims[1])
    m2 <- as.integer(fdataobj$dims[2])

    data_obj <- fdataobj$data
    data_ori <- fdataori$data

    return(.Call("wrap__depth_mode_2d", data_obj, data_ori, m1, m2, as.numeric(h)))
  }

  # 1D functional data (curves)
  # Validate dimensions
  if (ncol(fdataobj$data) != ncol(fdataori$data)) {
    stop("fdataobj and fdataori must have the same number of evaluation points")
  }

  .Call("wrap__depth_mode_1d", fdataobj$data, fdataori$data, as.numeric(h))
}

# Internal: Random Projection Depth implementation
# @noRd
.depth.RP <- function(fdataobj, fdataori = NULL, nproj = 50, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (is.null(fdataori)) {
    fdataori <- fdataobj
  }

  if (!inherits(fdataori, "fdata")) {
    stop("fdataori must be of class 'fdata'")
  }

  # Check for 2D functional data
  is_2d_obj <- isTRUE(fdataobj$fdata2d)
  is_2d_ori <- isTRUE(fdataori$fdata2d)

  if (is_2d_obj != is_2d_ori) {
    stop("Cannot compute depth between 1D and 2D functional data")
  }

  if (is_2d_obj) {
    # 2D functional data (surfaces) - flatten 3D arrays for Rust
    if (!identical(fdataobj$dims, fdataori$dims)) {
      stop("fdataobj and fdataori must have the same grid dimensions")
    }

    m1 <- as.integer(fdataobj$dims[1])
    m2 <- as.integer(fdataobj$dims[2])

    data_obj <- fdataobj$data
    data_ori <- fdataori$data

    return(.Call("wrap__depth_rp_2d", data_obj, data_ori, m1, m2, as.integer(nproj)))
  }

  # 1D functional data (curves)
  # Validate dimensions
  if (ncol(fdataobj$data) != ncol(fdataori$data)) {
    stop("fdataobj and fdataori must have the same number of evaluation points")
  }

  .Call("wrap__depth_rp_1d", fdataobj$data, fdataori$data, as.integer(nproj))
}

# Internal: Random Tukey Depth implementation
# @noRd
.depth.RT <- function(fdataobj, fdataori = NULL, nproj = 50, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (is.null(fdataori)) {
    fdataori <- fdataobj
  }

  if (!inherits(fdataori, "fdata")) {
    stop("fdataori must be of class 'fdata'")
  }

  # Check for 2D functional data
  is_2d_obj <- isTRUE(fdataobj$fdata2d)
  is_2d_ori <- isTRUE(fdataori$fdata2d)

  if (is_2d_obj != is_2d_ori) {
    stop("Cannot compute depth between 1D and 2D functional data")
  }

  if (is_2d_obj) {
    # 2D functional data (surfaces) - flatten 3D arrays for Rust
    if (!identical(fdataobj$dims, fdataori$dims)) {
      stop("fdataobj and fdataori must have the same grid dimensions")
    }

    m1 <- as.integer(fdataobj$dims[1])
    m2 <- as.integer(fdataobj$dims[2])

    data_obj <- fdataobj$data
    data_ori <- fdataori$data

    return(.Call("wrap__depth_rt_2d", data_obj, data_ori, m1, m2, as.integer(nproj)))
  }

  # 1D functional data (curves)
  # Validate dimensions
  if (ncol(fdataobj$data) != ncol(fdataori$data)) {
    stop("fdataobj and fdataori must have the same number of evaluation points")
  }

  .Call("wrap__depth_rt_1d", fdataobj$data, fdataori$data, as.integer(nproj))
}

# Internal: Functional Spatial Depth implementation
# @noRd
.depth.FSD <- function(fdataobj, fdataori = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (is.null(fdataori)) {
    fdataori <- fdataobj
  }

  if (!inherits(fdataori, "fdata")) {
    stop("fdataori must be of class 'fdata'")
  }

  # Check for 2D functional data
  is_2d_obj <- isTRUE(fdataobj$fdata2d)
  is_2d_ori <- isTRUE(fdataori$fdata2d)

  if (is_2d_obj != is_2d_ori) {
    stop("Cannot compute depth between 1D and 2D functional data")
  }

  if (is_2d_obj) {
    # 2D functional data (surfaces) - flatten 3D arrays for Rust
    if (!identical(fdataobj$dims, fdataori$dims)) {
      stop("fdataobj and fdataori must have the same grid dimensions")
    }

    m1 <- as.integer(fdataobj$dims[1])
    m2 <- as.integer(fdataobj$dims[2])

    data_obj <- fdataobj$data
    data_ori <- fdataori$data

    return(.Call("wrap__depth_fsd_2d", data_obj, data_ori, m1, m2))
  }

  # 1D functional data (curves)
  # Validate dimensions
  if (ncol(fdataobj$data) != ncol(fdataori$data)) {
    stop("fdataobj and fdataori must have the same number of evaluation points")
  }

  .Call("wrap__depth_fsd_1d", fdataobj$data, fdataori$data)
}

# Internal: Kernel Functional Spatial Depth (KFSD) implementation
# @noRd
.depth.KFSD <- function(fdataobj, fdataori = NULL, trim = 0.25, h = NULL,
                        scale = FALSE, draw = FALSE, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (is.null(fdataori)) {
    fdataori <- fdataobj
  }

  if (!inherits(fdataori, "fdata")) {
    stop("fdataori must be of class 'fdata'")
  }

  # Check for 2D functional data
  is_2d_obj <- isTRUE(fdataobj$fdata2d)
  is_2d_ori <- isTRUE(fdataori$fdata2d)

  if (is_2d_obj != is_2d_ori) {
    stop("Cannot compute depth between 1D and 2D functional data")
  }

  # Auto-compute bandwidth if not provided
  if (is.null(h)) {
    # Use Silverman's rule of thumb adapted for functional data
    n <- nrow(fdataori$data)
    h <- 1.06 * sd(fdataori$data) * n^(-1/5)
    if (h < 1e-10) h <- 0.15  # fallback
  }

  if (is_2d_obj) {
    # 2D functional data (surfaces) - flatten 3D arrays for Rust
    if (!identical(fdataobj$dims, fdataori$dims)) {
      stop("fdataobj and fdataori must have the same grid dimensions")
    }

    m1 <- as.integer(fdataobj$dims[1])
    m2 <- as.integer(fdataobj$dims[2])

    data_obj <- fdataobj$data
    data_ori <- fdataori$data

    return(.Call("wrap__depth_kfsd_2d", data_obj, data_ori, m1, m2, as.numeric(h)))
  }

  # 1D functional data (curves)
  # Validate dimensions
  if (ncol(fdataobj$data) != ncol(fdataori$data)) {
    stop("fdataobj and fdataori must have the same number of evaluation points")
  }

  .Call("wrap__depth_kfsd_1d", fdataobj$data, fdataori$data, as.numeric(fdataori$argvals), as.numeric(h))
}

# Internal: Random Projection Depth with Derivatives (RPD) implementation
# @noRd
.depth.RPD <- function(fdataobj, fdataori = NULL, nproj = 20, deriv = c(0, 1),
                       trim = 0.25, draw = FALSE, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (is.null(fdataori)) {
    fdataori <- fdataobj
  }

  if (!inherits(fdataori, "fdata")) {
    stop("fdataori must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d) || isTRUE(fdataori$fdata2d)) {
    stop("depth.RPD not yet implemented for 2D functional data")
  }

  n <- nrow(fdataobj$data)
  m <- ncol(fdataobj$data)
  n_ori <- nrow(fdataori$data)
  argvals <- fdataobj$argvals

  # Ensure deriv orders are valid
  max_deriv <- max(deriv)
  if (max_deriv >= m) {
    stop("Derivative order must be less than number of evaluation points")
  }

  # Compute derivatives for each order
  derivs_obj <- list()
  derivs_ori <- list()

  for (d in deriv) {
    if (d == 0) {
      derivs_obj[[as.character(d)]] <- fdataobj$data
      derivs_ori[[as.character(d)]] <- fdataori$data
    } else {
      fd_deriv_obj <- deriv(fdataobj, nderiv = d, ...)
      fd_deriv_ori <- deriv(fdataori, nderiv = d, ...)
      derivs_obj[[as.character(d)]] <- fd_deriv_obj$data
      derivs_ori[[as.character(d)]] <- fd_deriv_ori$data
    }
  }

  # For each derivative order, the number of points may differ
  # Use the minimum common dimension
  m_common <- min(sapply(derivs_obj, ncol))

  # Generate random projection vectors (smooth Gaussian processes)
  projections <- matrix(rnorm(nproj * m_common), nproj, m_common)

  # Normalize projections
  for (i in seq_len(nproj)) {
    projections[i, ] <- projections[i, ] / sqrt(sum(projections[i, ]^2))
  }

  # Integration weights for inner product
  argvals_common <- argvals[seq_len(m_common)]
  h <- diff(argvals_common)
  weights <- c(h[1]/2, (h[-length(h)] + h[-1])/2, h[length(h)]/2)

  # Compute inner product function
  inner_prod <- function(curve, proj, w) {
    sum(curve * proj * w)
  }

  # Accumulate depth from all projections
  depths <- rep(0, n)

  for (j in seq_len(nproj)) {
    proj <- projections[j, ]

    # For each derivative order, project curves
    n_derivs <- length(deriv)
    proj_scores_obj <- matrix(0, n, n_derivs)
    proj_scores_ori <- matrix(0, n_ori, n_derivs)

    for (k in seq_along(deriv)) {
      d_char <- as.character(deriv[k])
      data_obj <- derivs_obj[[d_char]][, seq_len(m_common), drop = FALSE]
      data_ori <- derivs_ori[[d_char]][, seq_len(m_common), drop = FALSE]

      for (i in seq_len(n)) {
        proj_scores_obj[i, k] <- inner_prod(data_obj[i, ], proj, weights)
      }
      for (i in seq_len(n_ori)) {
        proj_scores_ori[i, k] <- inner_prod(data_ori[i, ], proj, weights)
      }
    }

    # Compute multivariate halfspace depth for the projected scores
    # Using Tukey (location) depth: minimum fraction in any halfspace
    for (i in seq_len(n)) {
      x <- proj_scores_obj[i, ]

      # Simple univariate depth if n_derivs == 1
      if (n_derivs == 1) {
        vals <- proj_scores_ori[, 1]
        prop_below <- mean(vals <= x[1])
        prop_above <- mean(vals >= x[1])
        d_i <- min(prop_below, prop_above)
      } else {
        # Multivariate halfspace depth approximation
        # Use random directions in the projected space
        d_i <- 1
        n_dirs <- 50
        for (dir_idx in seq_len(n_dirs)) {
          direction <- rnorm(n_derivs)
          direction <- direction / sqrt(sum(direction^2))

          proj_x <- sum(x * direction)
          proj_ref <- proj_scores_ori %*% direction

          prop_below <- mean(proj_ref <= proj_x)
          prop_above <- mean(proj_ref >= proj_x)
          d_i <- min(d_i, min(prop_below, prop_above))
        }
      }

      depths[i] <- depths[i] + d_i
    }
  }

  # Average over projections
  depths / nproj
}

#' Compute Functional Median
#'
#' Returns the curve with maximum depth using the specified depth method.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param method Depth method to use. One of "FM", "mode", "RP", "RT", "BD",
#'   "MBD", "FSD", "KFSD", or "RPD". Default is "FM".
#' @param ... Additional arguments passed to the depth function.
#'
#' @return The curve (as fdata object) with maximum depth.
#'
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#' med <- median(fd)
#' med_mode <- median(fd, method = "mode")
median <- function(fdataobj, method = c("FM", "mode", "RP", "RT", "BD", "MBD",
                   "FSD", "KFSD", "RPD"), ...) {
  if (!inherits(fdataobj, "fdata")) {
    return(stats::median(fdataobj, ...))
  }

  method <- match.arg(method)
  depths <- depth(fdataobj, fdataobj, method = method, ...)
  max_idx <- which.max(depths)

  result <- fdataobj[max_idx, ]
  result$names$main <- paste0("Median (", method, ")")
  result
}

#' Compute Functional Trimmed Mean
#'
#' Computes the trimmed mean by excluding curves with lowest depth.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param trim Proportion of curves to trim (default 0.1).
#' @param method Depth method to use. One of "FM", "mode", "RP", "RT", "BD",
#'   "MBD", "FSD", "KFSD", or "RPD". Default is "FM".
#' @param ... Additional arguments passed to the depth function.
#'
#' @return An fdata object containing the trimmed mean function.
#'
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#' tm <- trimmed(fd, trim = 0.2)
#' tm_mode <- trimmed(fd, trim = 0.2, method = "mode")
trimmed <- function(fdataobj, trim = 0.1, method = c("FM", "mode", "RP", "RT",
                    "BD", "MBD", "FSD", "KFSD", "RPD"), ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  method <- match.arg(method)
  n <- nrow(fdataobj$data)
  n_keep <- ceiling(n * (1 - trim))

  depths <- depth(fdataobj, fdataobj, method = method, ...)
  depth_order <- order(depths, decreasing = TRUE)
  keep_idx <- depth_order[seq_len(n_keep)]

  # Compute mean of kept curves and return as fdata object
  mean_vals <- colMeans(fdataobj$data[keep_idx, , drop = FALSE])
  fdata(matrix(mean_vals, nrow = 1), argvals = fdataobj$argvals,
        names = list(main = paste0("Trimmed Mean (", method, ")"),
                     xlab = fdataobj$names$xlab, ylab = fdataobj$names$ylab))
}

#' Functional Variance
#'
#' Computes the pointwise variance function of functional data.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param ... Additional arguments (currently ignored).
#'
#' @return An fdata object containing the variance function (1D or 2D).
#'
#' @export
#' @examples
#' # 1D functional data
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#' v <- var(fd)
#'
#' # 2D functional data
#' X <- array(rnorm(500), dim = c(5, 10, 10))
#' fd2d <- fdata(X, argvals = list(1:10, 1:10), fdata2d = TRUE)
#' v2d <- var(fd2d)
var <- function(fdataobj, ...) {
  if (!inherits(fdataobj, "fdata")) {
    return(stats::var(fdataobj, ...))
  }
  x <- fdataobj

  var_vals <- apply(x$data, 2, stats::var)

  if (isTRUE(x$fdata2d)) {
    # Return as fdata2d
    result <- list(
      data = matrix(var_vals, nrow = 1),
      argvals = x$argvals,
      rangeval = x$rangeval,
      names = list(main = "Variance", xlab = x$names$xlab,
                   ylab = x$names$ylab, zlab = "Var"),
      fdata2d = TRUE,
      dims = x$dims
    )
    class(result) <- "fdata"
    return(result)
  }

  # 1D case
  fdata(matrix(var_vals, nrow = 1), argvals = x$argvals,
        names = list(main = "Variance", xlab = x$names$xlab,
                     ylab = "Var(X(t))"))
}

#' Functional Standard Deviation
#'
#' Computes the pointwise standard deviation function of functional data.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param ... Additional arguments (currently ignored).
#'
#' @return An fdata object containing the standard deviation function (1D or 2D).
#'
#' @export
#' @examples
#' # 1D functional data
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#' s <- sd(fd)
#'
#' # 2D functional data
#' X <- array(rnorm(500), dim = c(5, 10, 10))
#' fd2d <- fdata(X, argvals = list(1:10, 1:10), fdata2d = TRUE)
#' s2d <- sd(fd2d)
sd <- function(fdataobj, ...) {
  if (!inherits(fdataobj, "fdata")) {
    return(stats::sd(fdataobj, ...))
  }
  x <- fdataobj

  sd_vals <- apply(x$data, 2, stats::sd)

  if (isTRUE(x$fdata2d)) {
    # Return as fdata2d
    result <- list(
      data = matrix(sd_vals, nrow = 1),
      argvals = x$argvals,
      rangeval = x$rangeval,
      names = list(main = "Standard Deviation", xlab = x$names$xlab,
                   ylab = x$names$ylab, zlab = "SD"),
      fdata2d = TRUE,
      dims = x$dims
    )
    class(result) <- "fdata"
    return(result)
  }

  # 1D case
  fdata(matrix(sd_vals, nrow = 1), argvals = x$argvals,
        names = list(main = "Standard Deviation", xlab = x$names$xlab,
                     ylab = "SD(X(t))"))
}

#' Geometric Median of Functional Data
#'
#' Computes the geometric median (L1 median) of functional data using
#' Weiszfeld's iterative algorithm. The geometric median minimizes the
#' sum of L2 distances to all curves/surfaces, making it robust to outliers.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param max.iter Maximum number of iterations (default 100).
#' @param tol Convergence tolerance (default 1e-6).
#'
#' @return An fdata object containing the geometric median function (1D or 2D).
#'
#' @details
#' The geometric median y minimizes:
#' \deqn{\sum_{i=1}^n ||X_i - y||_{L2}}
#'
#' Unlike the mean (L2 center), the geometric median is robust to outliers
#' because extreme values have bounded influence (influence function is bounded).
#'
#' The Weiszfeld algorithm is an iteratively reweighted least squares method
#' that converges to the geometric median.
#'
#' @seealso \code{\link{mean.fdata}} for the (non-robust) mean function,
#'   \code{\link{median}} for depth-based median
#'
#' @export
#' @examples
#' # Create functional data with an outlier
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 20, 50)
#' for (i in 1:19) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.1)
#' X[20, ] <- sin(2*pi*t) + 5  # Large outlier
#' fd <- fdata(X, argvals = t)
#'
#' # Compare mean vs geometric median
#' mean_curve <- mean(fd)
#' gmed_curve <- gmed(fd)
#'
#' # The geometric median is less affected by the outlier
gmed <- function(fdataobj, max.iter = 100, tol = 1e-6) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    # 2D case - data is already flattened [n, m1*m2]
    gmed_vals <- .Call("wrap__geometric_median_2d", fdataobj$data,
                       as.numeric(fdataobj$argvals[[1]]),
                       as.numeric(fdataobj$argvals[[2]]),
                       as.integer(max.iter), as.numeric(tol))

    # Return as flattened matrix [1, m1*m2]
    result <- list(
      data = matrix(gmed_vals, nrow = 1),
      argvals = fdataobj$argvals,
      rangeval = fdataobj$rangeval,
      names = list(main = "Geometric Median", xlab = fdataobj$names$xlab,
                   ylab = fdataobj$names$ylab, zlab = fdataobj$names$zlab),
      fdata2d = TRUE,
      dims = fdataobj$dims
    )
    class(result) <- "fdata"
    return(result)
  }

  # 1D case
  gmed_vals <- .Call("wrap__geometric_median_1d", fdataobj$data,
                     as.numeric(fdataobj$argvals),
                     as.integer(max.iter), as.numeric(tol))

  fdata(matrix(gmed_vals, nrow = 1), argvals = fdataobj$argvals,
        names = list(main = "Geometric Median", xlab = fdataobj$names$xlab,
                     ylab = fdataobj$names$ylab))
}

#' Functional Covariance Function
#'
#' Computes the covariance function (surface) for functional data.
#' For 1D: \code{Cov(s, t) = E[(X(s) - mu(s))(X(t) - mu(t))]}
#' For 2D: Covariance across the flattened domain.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A list with components:
#' \describe{
#'   \item{cov}{The covariance matrix (m x m for 1D, (m1*m2) x (m1*m2) for 2D)}
#'   \item{argvals}{The evaluation points (same as input)}
#'   \item{mean}{The mean function}
#' }
#'
#' @export
#' @examples
#' # 1D functional data
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 20, 50)
#' for (i in 1:20) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.2)
#' fd <- fdata(X, argvals = t)
#' cov_result <- cov(fd)
#' image(cov_result$cov, main = "Covariance Surface")
cov <- function(fdataobj, ...) {
  if (!inherits(fdataobj, "fdata")) {
    return(stats::cov(fdataobj, ...))
  }
  x <- fdataobj

  # Compute mean and centered data
  n <- nrow(x$data)
  m <- ncol(x$data)

  mean_func <- colMeans(x$data)
  centered <- sweep(x$data, 2, mean_func)

  # Compute covariance matrix: (1/(n-1)) * t(X_centered) %*% X_centered
  cov_mat <- crossprod(centered) / (n - 1)

  list(
    cov = cov_mat,
    argvals = x$argvals,
    mean = mean_func,
    fdata2d = isTRUE(x$fdata2d),
    dims = x$dims
  )
}

#' Compute Functional Trimmed Variance
#'
#' Computes the trimmed variance by excluding curves with lowest depth.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param trim Proportion of curves to trim (default 0.1).
#' @param method Depth method to use. One of "FM", "mode", "RP", "RT", "BD",
#'   "MBD", "FSD", "KFSD", or "RPD". Default is "FM".
#' @param ... Additional arguments passed to the depth function.
#'
#' @return An fdata object containing the trimmed variance function.
#'
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#' tv <- trimvar(fd, trim = 0.2)
#' tv_mode <- trimvar(fd, trim = 0.2, method = "mode")
trimvar <- function(fdataobj, trim = 0.1, method = c("FM", "mode", "RP", "RT",
                    "BD", "MBD", "FSD", "KFSD", "RPD"), ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  method <- match.arg(method)
  n <- nrow(fdataobj$data)
  n_keep <- ceiling(n * (1 - trim))

  depths <- depth(fdataobj, fdataobj, method = method, ...)
  depth_order <- order(depths, decreasing = TRUE)
  keep_idx <- depth_order[seq_len(n_keep)]

  var_vals <- apply(fdataobj$data[keep_idx, , drop = FALSE], 2, stats::var)

  fdata(matrix(var_vals, nrow = 1), argvals = fdataobj$argvals,
        names = list(main = paste0("Trimmed Variance (", method, ")"),
                     xlab = fdataobj$names$xlab, ylab = "Var(X(t))"))
}


# =============================================================================
# Depth Function Wrappers (for backward compatibility and use as function args)
# =============================================================================

#' Modified Band Depth
#'
#' Wrapper for \code{depth(method = "MBD")}. Useful as a function argument.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataori Reference sample (default: fdataobj itself).
#' @param ... Additional arguments.
#'
#' @return Numeric vector of depth values.
#' @seealso \code{\link{depth}}
#' @export
depth.MBD <- function(fdataobj, fdataori = NULL, ...) {
  depth(fdataobj, fdataori, method = "MBD", ...)
}

#' Fraiman-Muniz Depth
#'
#' Wrapper for \code{depth(method = "FM")}. Useful as a function argument.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataori Reference sample (default: fdataobj itself).
#' @param ... Additional arguments.
#'
#' @return Numeric vector of depth values.
#' @seealso \code{\link{depth}}
#' @export
depth.FM <- function(fdataobj, fdataori = NULL, ...) {
  depth(fdataobj, fdataori, method = "FM", ...)
}

#' Modal Depth
#'
#' Wrapper for \code{depth(method = "mode")}. Useful as a function argument.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataori Reference sample (default: fdataobj itself).
#' @param ... Additional arguments.
#'
#' @return Numeric vector of depth values.
#' @seealso \code{\link{depth}}
#' @export
depth.mode <- function(fdataobj, fdataori = NULL, ...) {
  depth(fdataobj, fdataori, method = "mode", ...)
}

#' Random Projection Depth
#'
#' Wrapper for \code{depth(method = "RP")}. Useful as a function argument.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataori Reference sample (default: fdataobj itself).
#' @param ... Additional arguments.
#'
#' @return Numeric vector of depth values.
#' @seealso \code{\link{depth}}
#' @export
depth.RP <- function(fdataobj, fdataori = NULL, ...) {
  depth(fdataobj, fdataori, method = "RP", ...)
}

#' Random Tukey Depth
#'
#' Wrapper for \code{depth(method = "RT")}. Useful as a function argument.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataori Reference sample (default: fdataobj itself).
#' @param ... Additional arguments.
#'
#' @return Numeric vector of depth values.
#' @seealso \code{\link{depth}}
#' @export
depth.RT <- function(fdataobj, fdataori = NULL, ...) {
  depth(fdataobj, fdataori, method = "RT", ...)
}

#' Band Depth
#'
#' Wrapper for \code{depth(method = "BD")}. Useful as a function argument.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataori Reference sample (default: fdataobj itself).
#' @param ... Additional arguments.
#'
#' @return Numeric vector of depth values.
#' @seealso \code{\link{depth}}
#' @export
depth.BD <- function(fdataobj, fdataori = NULL, ...) {
  depth(fdataobj, fdataori, method = "BD", ...)
}

#' Modified Epigraph Index
#'
#' Wrapper for \code{depth(method = "MEI")}. Useful as a function argument.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataori Reference sample (default: fdataobj itself).
#' @param ... Additional arguments.
#'
#' @return Numeric vector of depth values.
#' @seealso \code{\link{depth}}
#' @export
depth.MEI <- function(fdataobj, fdataori = NULL, ...) {
  depth(fdataobj, fdataori, method = "MEI", ...)
}

#' Functional Spatial Depth
#'
#' Wrapper for \code{depth(method = "FSD")}. Useful as a function argument.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataori Reference sample (default: fdataobj itself).
#' @param ... Additional arguments.
#'
#' @return Numeric vector of depth values.
#' @seealso \code{\link{depth}}
#' @export
depth.FSD <- function(fdataobj, fdataori = NULL, ...) {
  depth(fdataobj, fdataori, method = "FSD", ...)
}

#' Kernel Functional Spatial Depth
#'
#' Wrapper for \code{depth(method = "KFSD")}. Useful as a function argument.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataori Reference sample (default: fdataobj itself).
#' @param ... Additional arguments.
#'
#' @return Numeric vector of depth values.
#' @seealso \code{\link{depth}}
#' @export
depth.KFSD <- function(fdataobj, fdataori = NULL, ...) {
  depth(fdataobj, fdataori, method = "KFSD", ...)
}

#' Random Projection Depth with Derivatives
#'
#' Wrapper for \code{depth(method = "RPD")}. Useful as a function argument.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param fdataori Reference sample (default: fdataobj itself).
#' @param ... Additional arguments.
#'
#' @return Numeric vector of depth values.
#' @seealso \code{\link{depth}}
#' @export
depth.RPD <- function(fdataobj, fdataori = NULL, ...) {
  depth(fdataobj, fdataori, method = "RPD", ...)
}

