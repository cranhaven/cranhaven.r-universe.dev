# Simulation Functions for Functional Data Analysis
#
# This module provides tools for generating synthetic functional data using
# the Karhunen-Loeve expansion and various eigenfunction/eigenvalue configurations.

# =============================================================================
# Eigenfunction Bases
# =============================================================================

#' Generate Eigenfunction Basis
#'
#' Evaluates orthonormal eigenfunction bases at specified argument values.
#' These eigenfunctions can be used for Karhunen-Loeve simulation.
#'
#' @param argvals Numeric vector of evaluation points in \[0, 1\].
#' @param M Number of eigenfunctions to generate.
#' @param type Character. Type of eigenfunction system:
#'   \describe{
#'     \item{Fourier}{Fourier basis: 1, sqrt(2)*sin(2*pi*k*t), sqrt(2)*cos(2*pi*k*t)}
#'     \item{Poly}{Orthonormal Legendre polynomials of degrees 0, 1, ..., M-1}
#'     \item{PolyHigh}{Orthonormal Legendre polynomials starting at degree 2}
#'     \item{Wiener}{Wiener process eigenfunctions: sqrt(2)*sin((k-0.5)*pi*t)}
#'   }
#'
#' @return A matrix of dimension \code{length(argvals) x M} containing the
#'   eigenfunction values. Each column is an eigenfunction, normalized to
#'   have unit L2 norm on \[0, 1\].
#'
#' @details
#' The eigenfunctions are orthonormal with respect to the L2 inner product:
#' \code{integral(phi_j * phi_k) = 1} if \code{j == k}, \code{0} otherwise.
#'
#' \describe{
#'   \item{Fourier}{Suitable for periodic data. First function is constant.}
#'   \item{Poly}{Orthonormalized Legendre polynomials. Good for smooth data.}
#'   \item{PolyHigh}{Legendre polynomials starting at degree 2, useful when
#'     linear and constant components are handled separately.}
#'   \item{Wiener}{Eigenfunctions of the Brownian motion covariance.
#'     Eigenvalues decay as 1/((k-0.5)*pi)^2.}
#' }
#'
#' @export
#' @seealso \code{\link{eVal}}, \code{\link{simFunData}}
#' @examples
#' t <- seq(0, 1, length.out = 100)
#'
#' # Generate Fourier basis
#' phi_fourier <- eFun(t, M = 5, type = "Fourier")
#' matplot(t, phi_fourier, type = "l", lty = 1,
#'         main = "Fourier Eigenfunctions", ylab = expression(phi(t)))
#'
#' # Generate Wiener eigenfunctions
#' phi_wiener <- eFun(t, M = 5, type = "Wiener")
#' matplot(t, phi_wiener, type = "l", lty = 1,
#'         main = "Wiener Eigenfunctions", ylab = expression(phi(t)))
#'
#' # Check orthonormality (should be identity matrix)
#' dt <- diff(t)[1]
#' gram <- t(phi_fourier) %*% phi_fourier * dt
#' round(gram, 2)
eFun <- function(argvals, M, type = c("Fourier", "Poly", "PolyHigh", "Wiener")) {
  type <- match.arg(type)

  # Map type to integer for Rust
  efun_type <- switch(type,
    Fourier = 0L,
    Poly = 1L,
    PolyHigh = 2L,
    Wiener = 3L
  )

  .Call("wrap__eigenfunctions_1d", as.numeric(argvals), as.integer(M), efun_type)
}

# =============================================================================
# Eigenvalue Sequences
# =============================================================================

#' Generate Eigenvalue Sequence
#'
#' Generates eigenvalue sequences with different decay patterns.
#' These control the variance contribution of each mode in Karhunen-Loeve
#' simulation.
#'
#' @param M Number of eigenvalues to generate.
#' @param type Character. Type of eigenvalue decay:
#'   \describe{
#'     \item{linear}{lambda_k = 1/k for k = 1, ..., M}
#'     \item{exponential}{lambda_k = exp(-k) for k = 1, ..., M}
#'     \item{wiener}{lambda_k = 1/((k - 0.5)*pi)^2, the Wiener process eigenvalues}
#'   }
#'
#' @return A numeric vector of length M containing the eigenvalues
#'   in decreasing order.
#'
#' @details
#' The eigenvalues control how much variance each eigenfunction contributes
#' to the simulated curves:
#' \describe{
#'   \item{linear}{Slow decay, higher modes contribute more variation.
#'     Produces rougher curves.}
#'   \item{exponential}{Fast decay, higher modes contribute very little.
#'     Produces smoother curves.}
#'   \item{wiener}{Specific decay matching Brownian motion. Use with
#'     Wiener eigenfunctions for true Brownian motion simulation.}
#' }
#'
#' @export
#' @seealso \code{\link{eFun}}, \code{\link{simFunData}}
#' @examples
#' # Compare decay patterns
#' lambda_lin <- eVal(20, "linear")
#' lambda_exp <- eVal(20, "exponential")
#' lambda_wie <- eVal(20, "wiener")
#'
#' plot(1:20, lambda_lin, type = "b", log = "y", ylim = c(1e-10, 1),
#'      main = "Eigenvalue Decay Patterns", xlab = "k", ylab = expression(lambda[k]))
#' lines(1:20, lambda_exp, col = "red", type = "b")
#' lines(1:20, lambda_wie, col = "blue", type = "b")
#' legend("topright", c("Linear", "Exponential", "Wiener"),
#'        col = c("black", "red", "blue"), lty = 1, pch = 1)
eVal <- function(M, type = c("linear", "exponential", "wiener")) {
  type <- match.arg(type)

  # Map type to integer for Rust
  eval_type <- switch(type,
    linear = 0L,
    exponential = 1L,
    wiener = 2L
  )

  .Call("wrap__eigenvalues_1d", as.integer(M), eval_type)
}

# =============================================================================
# Karhunen-Loeve Simulation
# =============================================================================

#' Simulate Functional Data via Karhunen-Loeve Expansion
#'
#' Generates functional data samples using a truncated Karhunen-Loeve
#' representation: \code{f_i(t) = mean(t) + sum_{k=1}^M xi_{ik} * phi_k(t)}
#' where \code{xi_{ik} ~ N(0, lambda_k)}.
#'
#' @param n Number of curves to generate.
#' @param argvals Numeric vector of evaluation points.
#' @param M Number of basis functions (eigenfunctions) to use.
#' @param eFun.type Type of eigenfunction basis: \code{"Fourier"},
#'   \code{"Poly"}, \code{"PolyHigh"}, or \code{"Wiener"}.
#' @param eVal.type Type of eigenvalue decay: \code{"linear"},
#'   \code{"exponential"}, or \code{"wiener"}.
#' @param mean Mean function. Can be:
#'   \itemize{
#'     \item \code{NULL} for zero mean
#'     \item A numeric vector of length equal to \code{argvals}
#'     \item A function that takes \code{argvals} as input
#'   }
#' @param seed Optional integer random seed for reproducibility.
#'
#' @return An \code{fdata} object containing the simulated functional data.
#'
#' @details
#' The Karhunen-Loeve expansion provides a natural way to simulate
#' Gaussian functional data with a specified covariance structure.
#' The eigenvalues control the variance contribution of each mode,
#' while the eigenfunctions determine the shape of variation.
#'
#' The theoretical covariance function is:
#' \code{Cov(X(s), X(t)) = sum_{k=1}^M lambda_k * phi_k(s) * phi_k(t)}
#'
#' @seealso \code{\link{eFun}}, \code{\link{eVal}}, \code{\link{addError}},
#'   \code{\link{make.gaussian.process}}
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 100)
#'
#' # Basic simulation with Fourier basis
#' fd <- simFunData(n = 20, argvals = t, M = 5,
#'                  eFun.type = "Fourier", eVal.type = "linear")
#' plot(fd, main = "Simulated Functional Data (Fourier, Linear)")
#'
#' # Smoother curves with exponential decay
#' fd_smooth <- simFunData(n = 20, argvals = t, M = 10,
#'                         eFun.type = "Fourier", eVal.type = "exponential")
#' plot(fd_smooth, main = "Smooth Simulated Data (Exponential Decay)")
#'
#' # Wiener process simulation
#' fd_wiener <- simFunData(n = 20, argvals = t, M = 10,
#'                         eFun.type = "Wiener", eVal.type = "wiener", seed = 42)
#' plot(fd_wiener, main = "Wiener Process Simulation")
#'
#' # With mean function
#' mean_fn <- function(t) sin(2 * pi * t)
#' fd_mean <- simFunData(n = 20, argvals = t, M = 5, mean = mean_fn, seed = 42)
#' plot(fd_mean, main = "Simulated Data with Sinusoidal Mean")
simFunData <- function(n, argvals, M,
                       eFun.type = c("Fourier", "Poly", "PolyHigh", "Wiener"),
                       eVal.type = c("linear", "exponential", "wiener"),
                       mean = NULL,
                       seed = NULL) {
  eFun.type <- match.arg(eFun.type)
  eVal.type <- match.arg(eVal.type)

  # Generate eigenfunctions and eigenvalues
  phi <- eFun(argvals, M, type = eFun.type)
  lambda <- eVal(M, type = eVal.type)

  # Simulate via Rust
  seed_arg <- if (is.null(seed)) NULL else as.integer(seed)
  data_mat <- .Call("wrap__sim_kl_1d", as.integer(n), phi, lambda, seed_arg)

  # Add mean if specified
  if (!is.null(mean)) {
    if (is.function(mean)) {
      mean_vals <- mean(argvals)
    } else {
      mean_vals <- as.numeric(mean)
    }
    if (length(mean_vals) != length(argvals)) {
      stop("mean must have the same length as argvals")
    }
    # Add mean to each row
    data_mat <- sweep(data_mat, 2, mean_vals, "+")
  }

  # Return as fdata object
  fdata(data_mat, argvals = argvals, names = list(
    main = paste0("Simulated Functional Data (", eFun.type, ", ", eVal.type, ")"),
    xlab = "t",
    ylab = "X(t)"
  ))
}

#' Simulate Multivariate Functional Data
#'
#' Generates multivariate (vector-valued) functional data where each
#' component is simulated via Karhunen-Loeve expansion with potentially
#' different eigenfunctions, eigenvalues, and domains.
#'
#' @param n Number of multivariate curves to generate.
#' @param argvals List of numeric vectors, one per component.
#' @param M Integer or integer vector. Number of basis functions per component.
#'   If a single integer, used for all components.
#' @param eFun.type Character or character vector specifying eigenfunction
#'   type for each component. See \code{\link{eFun}} for options.
#' @param eVal.type Character or character vector specifying eigenvalue
#'   decay for each component. See \code{\link{eVal}} for options.
#' @param mean List of mean functions (one per component), or \code{NULL}.
#' @param seed Optional integer random seed.
#'
#' @return A list of class \code{multiFunData} containing:
#'   \describe{
#'     \item{components}{List of \code{fdata} objects, one per component}
#'     \item{n}{Number of observations}
#'     \item{p}{Number of components}
#'   }
#'
#' @export
#' @seealso \code{\link{simFunData}}, \code{\link{eFun}}, \code{\link{eVal}}
#' @examples
#' # Two-component multivariate functional data
#' t1 <- seq(0, 1, length.out = 100)
#' t2 <- seq(0, 0.5, length.out = 50)
#'
#' mfd <- simMultiFunData(
#'   n = 20,
#'   argvals = list(t1, t2),
#'   M = c(5, 3),
#'   eFun.type = c("Fourier", "Wiener"),
#'   eVal.type = c("exponential", "linear")
#' )
#'
#' # Plot first component
#' plot(mfd$components[[1]], main = "Component 1")
simMultiFunData <- function(n, argvals, M,
                            eFun.type = "Fourier",
                            eVal.type = "linear",
                            mean = NULL,
                            seed = NULL) {
  if (!is.list(argvals)) {
    stop("argvals must be a list of numeric vectors")
  }

  p <- length(argvals)

  # Expand scalar/single values to vectors of length p
  if (length(M) == 1) M <- rep(M, p)
  if (length(eFun.type) == 1) eFun.type <- rep(eFun.type, p)
  if (length(eVal.type) == 1) eVal.type <- rep(eVal.type, p)

  if (is.null(mean)) mean <- vector("list", p)

  # Set seed once for reproducibility
  if (!is.null(seed)) set.seed(seed)

  # Simulate each component
  components <- vector("list", p)
  for (j in seq_len(p)) {
    components[[j]] <- simFunData(
      n = n,
      argvals = argvals[[j]],
      M = M[j],
      eFun.type = eFun.type[j],
      eVal.type = eVal.type[j],
      mean = mean[[j]],
      seed = NULL  # seed already set above
    )
  }

  structure(
    list(
      components = components,
      n = n,
      p = p
    ),
    class = "multiFunData"
  )
}

# =============================================================================
# Noise Addition
# =============================================================================

#' Add Measurement Error to Functional Data
#'
#' Adds independent Gaussian noise to functional data observations.
#'
#' @param fdataobj An object of class \code{fdata}.
#' @param sd Standard deviation of the Gaussian noise.
#' @param type Type of noise:
#'   \describe{
#'     \item{pointwise}{(Default) Independent noise at each evaluation point.
#'       Each f_i(t_j) gets independent noise.}
#'     \item{curve}{Common noise level per curve. Each curve gets a single
#'       random value added to all its points.}
#'   }
#' @param seed Optional integer random seed for reproducibility.
#'
#' @return An \code{fdata} object with added noise.
#'
#' @export
#' @seealso \code{\link{simFunData}}, \code{\link{sparsify}}
#' @examples
#' t <- seq(0, 1, length.out = 100)
#' fd_clean <- simFunData(n = 20, argvals = t, M = 5, seed = 42)
#' fd_noisy <- addError(fd_clean, sd = 0.1)
#'
#' oldpar <- par(mfrow = c(1, 2))
#' plot(fd_clean, main = "Clean Data")
#' plot(fd_noisy, main = "With Noise (sd = 0.1)")
#' par(oldpar)
#'
#' # Higher noise level
#' fd_very_noisy <- addError(fd_clean, sd = 0.5)
#' plot(fd_very_noisy, main = "High Noise (sd = 0.5)")
addError <- function(fdataobj, sd = 0.1, type = c("pointwise", "curve"),
                     seed = NULL) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }
  if (isTRUE(fdataobj$fdata2d)) {
    stop("addError for 2D functional data not yet implemented")
  }

  type <- match.arg(type)
  seed_arg <- if (is.null(seed)) NULL else as.integer(seed)

  noisy_data <- if (type == "pointwise") {
    .Call("wrap__add_error_pointwise_1d", fdataobj$data, sd, seed_arg)
  } else {
    .Call("wrap__add_error_curve_1d", fdataobj$data, sd, seed_arg)
  }

  # Return new fdata with noisy data
  fdata(noisy_data, argvals = fdataobj$argvals, rangeval = fdataobj$rangeval,
        names = fdataobj$names, id = fdataobj$id, metadata = fdataobj$metadata)
}

# =============================================================================
# Print/Summary Methods
# =============================================================================

#' Print method for multiFunData objects
#' @param x A multiFunData object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.multiFunData <- function(x, ...) {
  cat("Multivariate Functional Data Object\n")
  cat("===================================\n")
  cat("  Number of observations:", x$n, "\n")
  cat("  Number of components:", x$p, "\n\n")

  for (j in seq_len(x$p)) {
    cat("  Component", j, ":\n")
    cat("    Evaluation points:", length(x$components[[j]]$argvals), "\n")
    cat("    Domain: [", min(x$components[[j]]$argvals), ",",
        max(x$components[[j]]$argvals), "]\n")
  }

  invisible(x)
}
