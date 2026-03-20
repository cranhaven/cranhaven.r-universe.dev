#' Covariance Kernel Functions for Gaussian Processes
#'
#' Parametric covariance functions (kernels) used to define the covariance
#' structure of Gaussian processes. These can be used with \code{make_gaussian_process}
#' to generate synthetic functional data.
#'
#' @return No return value. This page documents the family of kernel functions.
#' @name kernels
NULL

#' Gaussian (Squared Exponential) Covariance Function
#'
#' Computes the Gaussian (RBF/squared exponential) covariance function:
#' \deqn{k(s, t) = \sigma^2 \exp\left(-\frac{(s-t)^2}{2\ell^2}\right)}
#'
#' This kernel produces infinitely differentiable (very smooth) sample paths.
#'
#' @param variance Variance parameter \eqn{\sigma^2} (default 1).
#' @param length_scale Length scale parameter \eqn{\ell} (default 1).
#'
#' @return A covariance function object of class 'kernel_gaussian'.
#'
#' @details
#' The Gaussian covariance function, also known as the squared exponential or
#' radial basis function (RBF) kernel, is one of the most commonly used
#' covariance functions. It produces very smooth sample paths because it is
#' infinitely differentiable.
#'
#' The length scale parameter controls how quickly the correlation decays
#' with distance. Larger values produce smoother, more slowly varying functions.
#'
#' @seealso \code{\link{kernel.exponential}}, \code{\link{kernel.matern}},
#'   \code{\link{make.gaussian.process}}
#'
#' @export
#' @examples
#' # Create a Gaussian covariance function
#' cov_func <- kernel.gaussian(variance = 1, length_scale = 0.2)
#'
#' # Evaluate covariance matrix on a grid
#' t <- seq(0, 1, length.out = 50)
#' K <- cov_func(t)
#' image(K, main = "Gaussian Covariance Matrix")
#'
#' # Generate Gaussian process samples
#' fd <- make.gaussian.process(n = 10, t = t, cov = cov_func)
#' plot(fd)
kernel.gaussian <- function(variance = 1, length_scale = 1) {
  if (variance <= 0) stop("variance must be positive")
  if (length_scale <= 0) stop("length_scale must be positive")

  f <- function(t, s = NULL) {
    if (is.null(s)) s <- t

    # Handle both 1D and 2D cases
    if (is.list(t)) {
      # 2D case: t is list(s_grid, t_grid)
      t_flat <- expand.grid(t[[1]], t[[2]])
      t_vec <- as.matrix(t_flat)
      if (is.list(s)) {
        s_flat <- expand.grid(s[[1]], s[[2]])
        s_vec <- as.matrix(s_flat)
      } else {
        s_vec <- t_vec
      }
    } else {
      t_vec <- as.matrix(t)
      s_vec <- as.matrix(s)
    }

    # Compute squared distances
    n1 <- nrow(t_vec)
    n2 <- nrow(s_vec)
    K <- matrix(0, n1, n2)

    for (i in seq_len(n1)) {
      for (j in seq_len(n2)) {
        d2 <- sum((t_vec[i, ] - s_vec[j, ])^2)
        K[i, j] <- variance * exp(-d2 / (2 * length_scale^2))
      }
    }
    K
  }

  structure(f,
            class = c("kernel_gaussian", "kernel"),
            variance = variance,
            length_scale = length_scale)
}

#' Exponential Covariance Function
#'
#' Computes the exponential covariance function:
#' \deqn{k(s, t) = \sigma^2 \exp\left(-\frac{|s-t|}{\ell}\right)}
#'
#' This is equivalent to the Matern covariance with \eqn{\nu = 0.5}.
#' Sample paths are continuous but not differentiable (rough).
#'
#' @param variance Variance parameter \eqn{\sigma^2} (default 1).
#' @param length_scale Length scale parameter \eqn{\ell} (default 1).
#'
#' @return A covariance function object of class 'kernel_exponential'.
#'
#' @details
#' The exponential covariance function produces sample paths that are
#' continuous but nowhere differentiable, resulting in rough-looking curves.
#' It is a special case of the Matern family with \eqn{\nu = 0.5}.
#'
#' @seealso \code{\link{kernel.gaussian}}, \code{\link{kernel.matern}},
#'   \code{\link{make.gaussian.process}}
#'
#' @export
#' @examples
#' # Create an exponential covariance function
#' cov_func <- kernel.exponential(variance = 1, length_scale = 0.2)
#'
#' # Evaluate covariance matrix
#' t <- seq(0, 1, length.out = 50)
#' K <- cov_func(t)
#'
#' # Generate rough GP samples
#' fd <- make.gaussian.process(n = 10, t = t, cov = cov_func)
#' plot(fd)
kernel.exponential <- function(variance = 1, length_scale = 1) {
  if (variance <= 0) stop("variance must be positive")
  if (length_scale <= 0) stop("length_scale must be positive")

  f <- function(t, s = NULL) {
    if (is.null(s)) s <- t

    if (is.list(t)) {
      t_flat <- expand.grid(t[[1]], t[[2]])
      t_vec <- as.matrix(t_flat)
      if (is.list(s)) {
        s_flat <- expand.grid(s[[1]], s[[2]])
        s_vec <- as.matrix(s_flat)
      } else {
        s_vec <- t_vec
      }
    } else {
      t_vec <- as.matrix(t)
      s_vec <- as.matrix(s)
    }

    n1 <- nrow(t_vec)
    n2 <- nrow(s_vec)
    K <- matrix(0, n1, n2)

    for (i in seq_len(n1)) {
      for (j in seq_len(n2)) {
        d <- sqrt(sum((t_vec[i, ] - s_vec[j, ])^2))
        K[i, j] <- variance * exp(-d / length_scale)
      }
    }
    K
  }

  structure(f,
            class = c("kernel_exponential", "kernel"),
            variance = variance,
            length_scale = length_scale)
}

#' Matern Covariance Function
#'
#' Computes the Matern covariance function with smoothness parameter \eqn{\nu}:
#' \deqn{k(s, t) = \sigma^2 \frac{2^{1-\nu}}{\Gamma(\nu)} \left(\sqrt{2\nu}\frac{|s-t|}{\ell}\right)^\nu K_\nu\left(\sqrt{2\nu}\frac{|s-t|}{\ell}\right)}
#'
#' where \eqn{K_\nu} is the modified Bessel function of the second kind.
#'
#' @param variance Variance parameter \eqn{\sigma^2} (default 1).
#' @param length_scale Length scale parameter \eqn{\ell} (default 1).
#' @param nu Smoothness parameter \eqn{\nu} (default 1.5). Common values:
#'   \itemize{
#'     \item \code{nu = 0.5}: Exponential (continuous, not differentiable)
#'     \item \code{nu = 1.5}: Once differentiable
#'     \item \code{nu = 2.5}: Twice differentiable
#'     \item \code{nu = Inf}: Gaussian/squared exponential (infinitely differentiable)
#'   }
#'
#' @return A covariance function object of class 'kernel_matern'.
#'
#' @details
#' The Matern family of covariance functions provides flexible control over
#' the smoothness of sample paths through the \eqn{\nu} parameter. As \eqn{\nu}
#' increases, sample paths become smoother. The Matern family includes the
#' exponential (\eqn{\nu = 0.5}) and approaches the Gaussian kernel as
#' \eqn{\nu \to \infty}.
#'
#' For computational efficiency, special cases \eqn{\nu \in \{0.5, 1.5, 2.5, \infty\}}
#' use simplified closed-form expressions.
#'
#' @seealso \code{\link{kernel.gaussian}}, \code{\link{kernel.exponential}},
#'   \code{\link{make.gaussian.process}}
#'
#' @export
#' @examples
#' # Create Matern covariance functions with different smoothness
#' cov_rough <- kernel.matern(nu = 0.5)    # Equivalent to exponential
#' cov_smooth <- kernel.matern(nu = 2.5)   # Twice differentiable
#'
#' t <- seq(0, 1, length.out = 50)
#'
#' # Compare sample paths
#' fd_rough <- make.gaussian.process(n = 5, t = t, cov = cov_rough, seed = 42)
#' fd_smooth <- make.gaussian.process(n = 5, t = t, cov = cov_smooth, seed = 42)
kernel.matern <- function(variance = 1, length_scale = 1, nu = 1.5) {
  if (variance <= 0) stop("variance must be positive")
  if (length_scale <= 0) stop("length_scale must be positive")
  if (nu <= 0) stop("nu must be positive")

  f <- function(t, s = NULL) {
    if (is.null(s)) s <- t

    if (is.list(t)) {
      t_flat <- expand.grid(t[[1]], t[[2]])
      t_vec <- as.matrix(t_flat)
      if (is.list(s)) {
        s_flat <- expand.grid(s[[1]], s[[2]])
        s_vec <- as.matrix(s_flat)
      } else {
        s_vec <- t_vec
      }
    } else {
      t_vec <- as.matrix(t)
      s_vec <- as.matrix(s)
    }

    n1 <- nrow(t_vec)
    n2 <- nrow(s_vec)
    K <- matrix(0, n1, n2)

    for (i in seq_len(n1)) {
      for (j in seq_len(n2)) {
        d <- sqrt(sum((t_vec[i, ] - s_vec[j, ])^2))

        if (d < 1e-10) {
          K[i, j] <- variance
        } else if (is.infinite(nu)) {
          # nu = Inf: Gaussian kernel
          K[i, j] <- variance * exp(-d^2 / (2 * length_scale^2))
        } else if (abs(nu - 0.5) < 1e-10) {
          # nu = 0.5: Exponential
          K[i, j] <- variance * exp(-d / length_scale)
        } else if (abs(nu - 1.5) < 1e-10) {
          # nu = 1.5: Matern 3/2
          r <- sqrt(3) * d / length_scale
          K[i, j] <- variance * (1 + r) * exp(-r)
        } else if (abs(nu - 2.5) < 1e-10) {
          # nu = 2.5: Matern 5/2
          r <- sqrt(5) * d / length_scale
          K[i, j] <- variance * (1 + r + r^2 / 3) * exp(-r)
        } else {
          # General case using Bessel function
          r <- sqrt(2 * nu) * d / length_scale
          K[i, j] <- variance * (2^(1 - nu) / gamma(nu)) * r^nu * besselK(r, nu)
        }
      }
    }
    K
  }

  structure(f,
            class = c("kernel_matern", "kernel"),
            variance = variance,
            length_scale = length_scale,
            nu = nu)
}

#' Brownian Motion Covariance Function
#'
#' Computes the Brownian motion (Wiener process) covariance function:
#' \deqn{k(s, t) = \sigma^2 \min(s, t)}
#'
#' @param variance Variance parameter \eqn{\sigma^2} (default 1).
#'
#' @return A covariance function object of class 'kernel_brownian'.
#'
#' @details
#' The Brownian motion covariance produces sample paths that start at 0
#' and have independent increments. The covariance between two points
#' equals the variance times the minimum of their positions.
#'
#' This covariance is only defined for 1D domains starting at 0.
#'
#' @seealso \code{\link{kernel.gaussian}}, \code{\link{make.gaussian.process}}
#'
#' @export
#' @examples
#' # Generate Brownian motion paths
#' cov_func <- kernel.brownian(variance = 1)
#' t <- seq(0, 1, length.out = 100)
#' fd <- make.gaussian.process(n = 10, t = t, cov = cov_func)
#' plot(fd)
kernel.brownian <- function(variance = 1) {
  if (variance <= 0) stop("variance must be positive")

  f <- function(t, s = NULL) {
    if (is.null(s)) s <- t

    if (is.list(t)) {
      stop("Brownian covariance is only defined for 1D domains")
    }

    t_vec <- as.numeric(t)
    s_vec <- as.numeric(s)

    n1 <- length(t_vec)
    n2 <- length(s_vec)
    K <- matrix(0, n1, n2)

    for (i in seq_len(n1)) {
      for (j in seq_len(n2)) {
        K[i, j] <- variance * min(t_vec[i], s_vec[j])
      }
    }
    K
  }

  structure(f,
            class = c("kernel_brownian", "kernel"),
            variance = variance)
}

#' Linear Covariance Function
#'
#' Computes the linear covariance function:
#' \deqn{k(s, t) = \sigma^2 (s - c)(t - c)}
#'
#' @param variance Variance parameter \eqn{\sigma^2} (default 1).
#' @param offset Offset parameter \eqn{c} (default 0).
#'
#' @return A covariance function object of class 'kernel_linear'.
#'
#' @details
#' The linear covariance function produces sample paths that are linear
#' functions. It is useful when the underlying process is expected to
#' have a linear trend.
#'
#' @seealso \code{\link{kernel.polynomial}}, \code{\link{make.gaussian.process}}
#'
#' @export
#' @examples
#' # Generate linear function samples
#' cov_func <- kernel.linear(variance = 1)
#' t <- seq(0, 1, length.out = 50)
#' fd <- make.gaussian.process(n = 10, t = t, cov = cov_func)
#' plot(fd)
kernel.linear <- function(variance = 1, offset = 0) {
  if (variance <= 0) stop("variance must be positive")

  f <- function(t, s = NULL) {
    if (is.null(s)) s <- t

    if (is.list(t)) {
      t_flat <- expand.grid(t[[1]], t[[2]])
      t_vec <- as.matrix(t_flat)
      if (is.list(s)) {
        s_flat <- expand.grid(s[[1]], s[[2]])
        s_vec <- as.matrix(s_flat)
      } else {
        s_vec <- t_vec
      }
    } else {
      t_vec <- as.matrix(t)
      s_vec <- as.matrix(s)
    }

    n1 <- nrow(t_vec)
    n2 <- nrow(s_vec)
    K <- matrix(0, n1, n2)

    for (i in seq_len(n1)) {
      for (j in seq_len(n2)) {
        K[i, j] <- variance * sum((t_vec[i, ] - offset) * (s_vec[j, ] - offset))
      }
    }
    K
  }

  structure(f,
            class = c("kernel_linear", "kernel"),
            variance = variance,
            offset = offset)
}

#' Polynomial Covariance Function
#'
#' Computes the polynomial covariance function:
#' \deqn{k(s, t) = \sigma^2 (s \cdot t + c)^p}
#'
#' @param variance Variance parameter \eqn{\sigma^2} (default 1).
#' @param offset Offset parameter \eqn{c} (default 0).
#' @param degree Polynomial degree \eqn{p} (default 2).
#'
#' @return A covariance function object of class 'kernel_polynomial'.
#'
#' @details
#' The polynomial covariance function produces sample paths that are
#' polynomial functions of degree at most \code{degree}. Setting
#' \code{degree = 1} and \code{offset = 0} gives the linear kernel.
#'
#' @seealso \code{\link{kernel.linear}}, \code{\link{make.gaussian.process}}
#'
#' @export
#' @examples
#' # Generate quadratic function samples
#' cov_func <- kernel.polynomial(degree = 2, offset = 1)
#' t <- seq(0, 1, length.out = 50)
#' fd <- make.gaussian.process(n = 10, t = t, cov = cov_func)
#' plot(fd)
kernel.polynomial <- function(variance = 1, offset = 0, degree = 2) {
  if (variance <= 0) stop("variance must be positive")
  if (degree < 1) stop("degree must be at least 1")

  f <- function(t, s = NULL) {
    if (is.null(s)) s <- t

    if (is.list(t)) {
      t_flat <- expand.grid(t[[1]], t[[2]])
      t_vec <- as.matrix(t_flat)
      if (is.list(s)) {
        s_flat <- expand.grid(s[[1]], s[[2]])
        s_vec <- as.matrix(s_flat)
      } else {
        s_vec <- t_vec
      }
    } else {
      t_vec <- as.matrix(t)
      s_vec <- as.matrix(s)
    }

    n1 <- nrow(t_vec)
    n2 <- nrow(s_vec)
    K <- matrix(0, n1, n2)

    for (i in seq_len(n1)) {
      for (j in seq_len(n2)) {
        inner <- sum(t_vec[i, ] * s_vec[j, ]) + offset
        K[i, j] <- variance * inner^degree
      }
    }
    K
  }

  structure(f,
            class = c("kernel_polynomial", "kernel"),
            variance = variance,
            offset = offset,
            degree = degree)
}

#' White Noise Covariance Function
#'
#' Computes the white noise covariance function:
#' \deqn{k(s, t) = \sigma^2 \mathbf{1}_{s = t}}
#'
#' where \eqn{\mathbf{1}_{s = t}} is 1 if \eqn{s = t} and 0 otherwise.
#'
#' @param variance Variance (noise level) parameter \eqn{\sigma^2} (default 1).
#'
#' @return A covariance function object of class 'kernel_whitenoise'.
#'
#' @details
#' The white noise covariance function represents independent noise at each
#' point. It can be added to other covariance functions to model observation
#' noise.
#'
#' @seealso \code{\link{kernel.gaussian}}, \code{\link{make.gaussian.process}}
#'
#' @export
#' @examples
#' # White noise covariance produces independent samples at each point
#' cov_func <- kernel.whitenoise(variance = 0.1)
#' t <- seq(0, 1, length.out = 50)
#' K <- cov_func(t)
#' # K is diagonal
kernel.whitenoise <- function(variance = 1) {
  if (variance <= 0) stop("variance must be positive")

  f <- function(t, s = NULL) {
    if (is.null(s)) s <- t

    if (is.list(t)) {
      n1 <- length(t[[1]]) * length(t[[2]])
      if (is.list(s)) {
        n2 <- length(s[[1]]) * length(s[[2]])
      } else {
        n2 <- n1
      }
    } else {
      n1 <- length(t)
      n2 <- length(s)
    }

    # White noise: only diagonal if s == t
    if (identical(t, s) || is.null(s)) {
      diag(variance, n1)
    } else {
      matrix(0, n1, n2)
    }
  }

  structure(f,
            class = c("kernel_whitenoise", "kernel"),
            variance = variance)
}

#' Periodic Covariance Function
#'
#' Computes the periodic covariance function:
#' \deqn{k(s, t) = \sigma^2 \exp\left(-\frac{2\sin^2(\pi|s-t|/p)}{\ell^2}\right)}
#'
#' @param variance Variance parameter \eqn{\sigma^2} (default 1).
#' @param length_scale Length scale parameter \eqn{\ell} (default 1).
#' @param period Period parameter \eqn{p} (default 1).
#'
#' @return A covariance function object of class 'kernel_periodic'.
#'
#' @details
#' The periodic covariance function produces sample paths that are periodic
#' with the specified period. It is useful for modeling seasonal or cyclical
#' patterns in functional data.
#'
#' @seealso \code{\link{kernel.gaussian}}, \code{\link{make.gaussian.process}}
#'
#' @export
#' @examples
#' # Generate periodic function samples
#' cov_func <- kernel.periodic(period = 0.5, length_scale = 0.5)
#' t <- seq(0, 2, length.out = 100)
#' fd <- make.gaussian.process(n = 5, t = t, cov = cov_func)
#' plot(fd)
kernel.periodic <- function(variance = 1, length_scale = 1, period = 1) {
  if (variance <= 0) stop("variance must be positive")
  if (length_scale <= 0) stop("length_scale must be positive")
  if (period <= 0) stop("period must be positive")

  f <- function(t, s = NULL) {
    if (is.null(s)) s <- t

    if (is.list(t)) {
      stop("Periodic covariance is only defined for 1D domains")
    }

    t_vec <- as.numeric(t)
    s_vec <- as.numeric(s)

    n1 <- length(t_vec)
    n2 <- length(s_vec)
    K <- matrix(0, n1, n2)

    for (i in seq_len(n1)) {
      for (j in seq_len(n2)) {
        d <- abs(t_vec[i] - s_vec[j])
        K[i, j] <- variance * exp(-2 * sin(pi * d / period)^2 / length_scale^2)
      }
    }
    K
  }

  structure(f,
            class = c("kernel_periodic", "kernel"),
            variance = variance,
            length_scale = length_scale,
            period = period)
}

#' Print Method for Covariance Functions
#'
#' @param x A covariance function object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.kernel <- function(x, ...) {
  kernel_type <- class(x)[1]
  kernel_name <- sub("^kernel_", "", kernel_type)

  cat("Covariance Kernel:", kernel_name, "\n")
  cat("Parameters:\n")

  attrs <- attributes(x)
  param_names <- setdiff(names(attrs), c("class", "srcref"))

  for (param in param_names) {
    val <- attrs[[param]]
    if (inherits(val, "kernel")) {
      cat("  ", param, "= <", class(val)[1], ">\n", sep = "")
    } else {
      cat("  ", param, "=", val, "\n")
    }
  }

  invisible(x)
}

#' Add Covariance Functions
#'
#' Combines two covariance functions by addition.
#'
#' @param kernel1 First covariance function.
#' @param kernel2 Second covariance function.
#'
#' @return A combined covariance function.
#'
#' @export
#' @examples
#' # Combine Gaussian with white noise
#' k_signal <- kernel.gaussian(variance = 1, length_scale = 0.2)
#' k_noise <- kernel.whitenoise(variance = 0.1)
#' k_total <- kernel.add(k_signal, k_noise)
#'
#' t <- seq(0, 1, length.out = 50)
#' fd <- make.gaussian.process(n = 5, t = t, cov = k_total)
kernel.add <- function(kernel1, kernel2) {
  if (!inherits(kernel1, "kernel") || !inherits(kernel2, "kernel")) {
    stop("Both arguments must be kernel functions")
  }

  f <- function(t, s = NULL) {
    kernel1(t, s) + kernel2(t, s)
  }

  structure(f,
            class = c("kernel_sum", "kernel"),
            kernel1 = kernel1,
            kernel2 = kernel2)
}

#' Multiply Covariance Functions
#'
#' Combines two covariance functions by multiplication.
#'
#' @param kernel1 First covariance function.
#' @param kernel2 Second covariance function.
#'
#' @return A combined covariance function.
#'
#' @export
#' @examples
#' # Multiply periodic with Gaussian for locally periodic behavior
#' k_periodic <- kernel.periodic(period = 0.3)
#' k_gaussian <- kernel.gaussian(length_scale = 0.5)
#' k_prod <- kernel.mult(k_periodic, k_gaussian)
kernel.mult <- function(kernel1, kernel2) {
  if (!inherits(kernel1, "kernel") || !inherits(kernel2, "kernel")) {
    stop("Both arguments must be kernel functions")
  }

  f <- function(t, s = NULL) {
    kernel1(t, s) * kernel2(t, s)
  }

  structure(f,
            class = c("kernel_product", "kernel"),
            kernel1 = kernel1,
            kernel2 = kernel2)
}

#' Generate Gaussian Process Samples
#'
#' Generates functional data samples from a Gaussian process with the
#' specified mean and covariance functions.
#'
#' @param n Number of samples to generate.
#' @param t Evaluation points (vector for 1D, list of two vectors for 2D).
#' @param cov Covariance function (from \code{kernel.gaussian}, \code{kernel.matern}, etc.).
#' @param mean Mean function. Can be a scalar (default 0), a vector of length
#'   equal to the number of evaluation points, or a function.
#' @param seed Optional random seed for reproducibility.
#'
#' @return An fdata object containing the generated samples.
#'
#' @details
#' This function generates samples from a Gaussian process with the specified
#' covariance structure. The samples are generated by computing the Cholesky
#' decomposition of the covariance matrix and multiplying by standard normal
#' random variables.
#'
#' For 2D functional data, pass \code{t} as a list of two vectors representing
#' the grid in each dimension.
#'
#' @seealso \code{\link{kernel.gaussian}}, \code{\link{kernel.matern}},
#'   \code{\link{kernel.exponential}}
#'
#' @export
#' @examples
#' # Generate smooth GP samples with Gaussian covariance
#' t <- seq(0, 1, length.out = 100)
#' fd <- make.gaussian.process(n = 20, t = t,
#'                             cov = kernel.gaussian(length_scale = 0.2),
#'                             seed = 42)
#' plot(fd)
#'
#' # Generate rough GP samples with exponential covariance
#' fd_rough <- make.gaussian.process(n = 20, t = t,
#'                                   cov = kernel.exponential(length_scale = 0.1),
#'                                   seed = 42)
#' plot(fd_rough)
#'
#' # Generate 2D GP samples (surfaces)
#' s <- seq(0, 1, length.out = 20)
#' t2 <- seq(0, 1, length.out = 20)
#' fd2d <- make.gaussian.process(n = 5, t = list(s, t2),
#'                               cov = kernel.gaussian(length_scale = 0.3),
#'                               seed = 42)
#' plot(fd2d)
#'
#' # Generate GP with non-zero mean
#' mean_func <- function(t) sin(2 * pi * t)
#' fd_mean <- make.gaussian.process(n = 10, t = t,
#'                                  cov = kernel.gaussian(variance = 0.1),
#'                                  mean = mean_func, seed = 42)
#' plot(fd_mean)
make.gaussian.process <- function(n, t, cov = kernel.gaussian(), mean = 0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  if (!inherits(cov, "kernel")) {
    stop("cov must be a kernel function (e.g., kernel.gaussian(), kernel.matern())")
  }

  # Handle 2D case
  fdata2d <- is.list(t)

  if (fdata2d) {
    m1 <- length(t[[1]])
    m2 <- length(t[[2]])
    m <- m1 * m2
    argvals <- t
  } else {
    m <- length(t)
    argvals <- t
  }

  # Compute covariance matrix
  K <- cov(t)

  # Add small jitter for numerical stability
  K <- K + 1e-8 * diag(nrow(K))

  # Cholesky decomposition
  L <- tryCatch(
    chol(K),
    error = function(e) {
      # If Cholesky fails, use eigendecomposition
      eig <- eigen(K, symmetric = TRUE)
      eig$vectors %*% diag(sqrt(pmax(eig$values, 0))) %*% t(eig$vectors)
    }
  )

  # Generate standard normal samples
  Z <- matrix(rnorm(n * m), n, m)

  # Transform to GP samples
  X <- Z %*% L

  # Add mean
  if (is.function(mean)) {
    if (fdata2d) {
      grid <- expand.grid(t[[1]], t[[2]])
      mean_vals <- mean(grid[, 1], grid[, 2])
    } else {
      mean_vals <- mean(t)
    }
    X <- sweep(X, 2, mean_vals, "+")
  } else if (length(mean) == 1) {
    X <- X + mean
  } else if (length(mean) == m) {
    X <- sweep(X, 2, mean, "+")
  } else {
    stop("mean must be a scalar, vector of length m, or a function")
  }

  # Create fdata object
  if (fdata2d) {
    X_array <- array(0, dim = c(n, m1, m2))
    for (i in seq_len(n)) {
      X_array[i, , ] <- matrix(X[i, ], m1, m2)
    }
    fdata(X_array, argvals = argvals, fdata2d = TRUE)
  } else {
    fdata(X, argvals = argvals)
  }
}
