# Irregular Functional Data Class and Operations
#
# This module provides support for irregularly sampled functional data,
# where each observation can have different evaluation points.

# =============================================================================
# irregFdata Class
# =============================================================================

#' Create an Irregular Functional Data Object
#'
#' Creates an \code{irregFdata} object for functional data where each
#' observation is sampled at potentially different points. This is common
#' in longitudinal studies, sparse sampling, and sensor data with missing
#' values.
#'
#' @param argvals A list of numeric vectors, where \code{argvals[[i]]} contains
#'   the observation times for the i-th curve.
#' @param X A list of numeric vectors, where \code{X[[i]]} contains the observed
#'   values for the i-th curve. Must have the same lengths as corresponding
#'   \code{argvals[[i]]}.
#' @param rangeval Optional numeric vector of length 2 specifying the domain
#'   range. If \code{NULL}, computed from the union of all observation points.
#' @param names List with components \code{main}, \code{xlab}, \code{ylab}
#'   for plot titles.
#' @param id Optional character vector of identifiers for each observation.
#' @param metadata Optional data.frame with additional covariates (one row
#'   per observation).
#'
#' @return An object of class \code{irregFdata} containing:
#'   \describe{
#'     \item{argvals}{List of observation time vectors}
#'     \item{X}{List of value vectors}
#'     \item{n}{Number of observations}
#'     \item{rangeval}{Domain range}
#'     \item{names}{Plot labels}
#'     \item{id}{Observation identifiers}
#'     \item{metadata}{Additional covariates}
#'   }
#'
#' @export
#' @seealso \code{\link{sparsify}}, \code{\link{as.fdata.irregFdata}},
#'   \code{\link{fdata}}
#' @examples
#' # Create irregular functional data directly
#' argvals <- list(
#'   c(0.0, 0.3, 0.7, 1.0),
#'   c(0.0, 0.2, 0.5, 0.8, 1.0),
#'   c(0.1, 0.4, 0.9)
#' )
#' X <- list(
#'   c(0.1, 0.5, 0.3, 0.2),
#'   c(0.0, 0.4, 0.6, 0.4, 0.1),
#'   c(0.3, 0.7, 0.2)
#' )
#' ifd <- irregFdata(argvals, X)
#' print(ifd)
#' plot(ifd)
irregFdata <- function(argvals, X, rangeval = NULL, names = NULL,
                       id = NULL, metadata = NULL) {
  # Validation
  if (!is.list(argvals) || !is.list(X)) {
    stop("argvals and X must be lists")
  }

  n <- length(argvals)
  if (length(X) != n) {
    stop("argvals and X must have the same length")
  }

  # Validate each observation
  for (i in seq_len(n)) {
    if (!is.numeric(argvals[[i]]) || !is.numeric(X[[i]])) {
      stop(paste0("Observation ", i, ": argvals and X must be numeric"))
    }
    if (length(argvals[[i]]) != length(X[[i]])) {
      stop(paste0("Observation ", i, ": argvals and X must have equal length"))
    }
    if (is.unsorted(argvals[[i]], strictly = TRUE) && length(argvals[[i]]) > 1) {
      stop(paste0("Observation ", i, ": argvals must be strictly increasing"))
    }
  }

  # Compute rangeval if not provided
  if (is.null(rangeval)) {
    all_t <- unlist(argvals)
    rangeval <- range(all_t)
  }

  # Default names
  if (is.null(names)) {
    names <- list(main = "", xlab = "t", ylab = "X(t)")
  }

  # Handle id parameter
  if (is.null(id)) {
    id <- paste0("obs_", seq_len(n))
  } else {
    if (length(id) != n) {
      stop("id must have length ", n, " (one per observation)")
    }
    id <- as.character(id)
  }

  # Validate metadata
  if (!is.null(metadata)) {
    if (!is.data.frame(metadata)) {
      stop("metadata must be a data.frame")
    }
    if (nrow(metadata) != n) {
      stop("metadata must have ", n, " rows (one per observation)")
    }
  }

  structure(
    list(
      argvals = argvals,
      X = X,
      n = n,
      rangeval = rangeval,
      names = names,
      id = id,
      metadata = metadata
    ),
    class = "irregFdata"
  )
}

#' Check if an Object is Irregular Functional Data
#'
#' @param x Any R object.
#' @return \code{TRUE} if \code{x} is of class \code{irregFdata}, \code{FALSE}
#'   otherwise.
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#' is.irregular(fd)  # FALSE
#'
#' ifd <- sparsify(fd, minObs = 3, maxObs = 7)
#' is.irregular(ifd)  # TRUE
is.irregular <- function(x) {
  inherits(x, "irregFdata")
}

# =============================================================================
# Print and Summary Methods
# =============================================================================

#' Print method for irregFdata objects
#' @param x An irregFdata object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.irregFdata <- function(x, ...) {
  cat("Irregular Functional Data Object\n")
  cat("=================================\n")
  cat("  Number of observations:", x$n, "\n")

  obs_lengths <- sapply(x$X, length)
  cat("  Points per curve:\n")
  cat("    Min:", min(obs_lengths), "\n")
  cat("    Median:", median(obs_lengths), "\n")
  cat("    Max:", max(obs_lengths), "\n")
  cat("    Total:", sum(obs_lengths), "\n")
  cat("  Domain: [", x$rangeval[1], ",", x$rangeval[2], "]\n")

  if (!is.null(x$metadata)) {
    cat("  Metadata columns:", paste(colnames(x$metadata), collapse = ", "), "\n")
  }

  invisible(x)
}

#' Summary method for irregFdata objects
#' @param object An irregFdata object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object.
#' @export
summary.irregFdata <- function(object, ...) {
  obs_lengths <- sapply(object$X, length)
  all_values <- unlist(object$X)

  cat("Summary of Irregular Functional Data\n")
  cat("=====================================\n\n")
  cat("Number of observations:", object$n, "\n\n")

  cat("Observation points per curve:\n")
  print(summary(obs_lengths))
  cat("\n")

  cat("Value summary:\n")
  print(summary(all_values))
  cat("\n")

  cat("Domain: [", object$rangeval[1], ",", object$rangeval[2], "]\n")

  invisible(object)
}

# =============================================================================
# Subsetting
# =============================================================================

#' Subset method for irregFdata objects
#' @param x An irregFdata object.
#' @param i Indices of observations to select.
#' @param ... Additional arguments (ignored).
#' @return An \code{irregFdata} object containing the selected subset.
#' @export
`[.irregFdata` <- function(x, i, ...) {
  if (missing(i)) {
    return(x)
  }

  # Handle negative indices
  if (is.numeric(i) && any(i < 0)) {
    i <- setdiff(seq_len(x$n), abs(i))
  }

  # Create new irregFdata with subset
  irregFdata(
    argvals = x$argvals[i],
    X = x$X[i],
    rangeval = x$rangeval,
    names = x$names,
    id = x$id[i],
    metadata = if (!is.null(x$metadata)) x$metadata[i, , drop = FALSE] else NULL
  )
}

# =============================================================================
# Conversion Functions
# =============================================================================

#' Convert Irregular Functional Data to Regular Grid
#'
#' Creates a regular \code{fdata} object from an \code{irregFdata} object
#' by interpolating or placing \code{NA} at unobserved points.
#'
#' @param x An object of class \code{irregFdata}.
#' @param argvals Target regular grid. If \code{NULL}, uses the union of
#'   all observation points.
#' @param method Interpolation method:
#'   \describe{
#'     \item{na}{(Default) Only fill exact matches; other points are \code{NA}}
#'     \item{linear}{Linear interpolation between observed points}
#'     \item{nearest}{Nearest neighbor interpolation}
#'   }
#' @param ... Additional arguments (ignored).
#'
#' @return An \code{fdata} object with \code{NA} for unobserved points
#'   (unless interpolated).
#'
#' @export
#' @seealso \code{\link{sparsify}}, \code{\link{irregFdata}}
#' @examples
#' # Create sparse data
#' t <- seq(0, 1, length.out = 100)
#' fd <- simFunData(n = 10, argvals = t, M = 5, seed = 42)
#' ifd <- sparsify(fd, minObs = 10, maxObs = 30, seed = 123)
#'
#' # Convert back to regular grid with NA
#' fd_na <- as.fdata(ifd)
#'
#' # Convert with linear interpolation
#' fd_interp <- as.fdata(ifd, method = "linear")
as.fdata.irregFdata <- function(x, argvals = NULL, method = c("na", "linear", "nearest"), ...) {
  method <- match.arg(method)

  if (is.null(argvals)) {
    # Use union of all observation points
    argvals <- sort(unique(unlist(x$argvals)))
  }

  m <- length(argvals)
  n <- x$n

  if (method == "linear" || method == "nearest") {
    # Use Rust implementation for linear interpolation
    # Convert to flat representation
    offsets <- c(0L, cumsum(sapply(x$argvals, length)))
    flat_argvals <- as.double(unlist(x$argvals))
    flat_values <- as.double(unlist(x$X))

    data_mat <- .Call("wrap__irreg_to_regular", offsets, flat_argvals, flat_values, as.double(argvals))

    if (method == "nearest") {
      # Approximate uses constant for nearest
      for (i in seq_len(n)) {
        data_mat[i, ] <- approx(x$argvals[[i]], x$X[[i]], xout = argvals,
                                 method = "constant", rule = 1)$y
      }
    }
  } else {
    # Only fill exact matches
    data_mat <- matrix(NA_real_, nrow = n, ncol = m)
    for (i in seq_len(n)) {
      idx <- match(x$argvals[[i]], argvals)
      valid <- !is.na(idx)
      data_mat[i, idx[valid]] <- x$X[[i]][valid]
    }
  }

  fdata(data_mat, argvals = argvals, rangeval = x$rangeval,
        names = x$names, id = x$id, metadata = x$metadata)
}

#' @rdname as.fdata.irregFdata
#' @export
as.fdata <- function(x, ...) {
  UseMethod("as.fdata")
}

#' @rdname as.fdata.irregFdata
#' @export
as.fdata.fdata <- function(x, ...) {
  x
}

# =============================================================================
# Sparsify Function
# =============================================================================

#' Convert Regular Functional Data to Irregular by Subsampling
#'
#' Creates an \code{irregFdata} object from regular \code{fdata} by randomly
#' selecting a subset of observation points for each curve.
#'
#' @param fdataobj An object of class \code{fdata}.
#' @param minObs Minimum number of observations to keep per curve.
#' @param maxObs Maximum number of observations to keep per curve.
#'   If \code{NULL}, uses the total number of points.
#' @param prob Sampling probability function. If \code{NULL}, uniform sampling
#'   is used. Otherwise, a function that takes \code{argvals} and returns
#'   sampling weights (not necessarily normalized).
#' @param seed Optional integer random seed for reproducibility.
#'
#' @return An object of class \code{irregFdata}.
#'
#' @details
#' For each curve, the function:
#' \enumerate{
#'   \item Draws a random number of points to keep between \code{minObs} and
#'     \code{maxObs}
#'   \item Samples that many points (without replacement) from the grid
#'   \item If \code{prob} is provided, sampling is weighted accordingly
#' }
#'
#' Common probability functions:
#' \itemize{
#'   \item Uniform: \code{NULL} (default)
#'   \item More points in middle: \code{function(t) dnorm(t, mean = 0.5, sd = 0.2)}
#'   \item More points at ends: \code{function(t) 1 - dnorm(t, mean = 0.5, sd = 0.2)}
#' }
#'
#' @export
#' @seealso \code{\link{irregFdata}}, \code{\link{as.fdata.irregFdata}},
#'   \code{\link{addError}}
#' @examples
#' # Create regular functional data
#' t <- seq(0, 1, length.out = 100)
#' fd <- simFunData(n = 20, argvals = t, M = 5, seed = 42)
#'
#' # Uniform sparsification
#' ifd <- sparsify(fd, minObs = 10, maxObs = 30, seed = 123)
#' print(ifd)
#' plot(ifd)
#'
#' # Non-uniform: more observations in the middle
#' prob_middle <- function(t) dnorm(t, mean = 0.5, sd = 0.2)
#' ifd_middle <- sparsify(fd, minObs = 15, maxObs = 25, prob = prob_middle, seed = 123)
#' plot(ifd_middle, main = "More Observations in Middle")
sparsify <- function(fdataobj, minObs = 5, maxObs = NULL, prob = NULL, seed = NULL) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }
  if (isTRUE(fdataobj$fdata2d)) {
    stop("sparsify not supported for 2D functional data")
  }

  if (!is.null(seed)) set.seed(seed)

  n <- nrow(fdataobj$data)
  m <- ncol(fdataobj$data)
  argvals_full <- fdataobj$argvals

  if (is.null(maxObs)) maxObs <- m
  if (minObs > m) {
    stop("minObs cannot exceed number of evaluation points (", m, ")")
  }
  if (maxObs > m) maxObs <- m
  if (minObs > maxObs) {
    stop("minObs cannot exceed maxObs")
  }

  # Compute sampling probabilities if provided
  if (!is.null(prob)) {
    if (!is.function(prob)) {
      stop("prob must be a function")
    }
    probs <- prob(argvals_full)
    if (any(probs < 0)) {
      stop("prob function returned negative values")
    }
    probs <- probs / sum(probs)  # Normalize
  } else {
    probs <- NULL
  }

  argvals_list <- vector("list", n)
  X_list <- vector("list", n)

  for (i in seq_len(n)) {
    # Random number of points to keep
    n_keep <- sample(minObs:maxObs, 1)

    # Sample indices
    if (is.null(probs)) {
      idx <- sort(sample(m, n_keep))
    } else {
      idx <- sort(sample(m, n_keep, prob = probs))
    }

    argvals_list[[i]] <- argvals_full[idx]
    X_list[[i]] <- fdataobj$data[i, idx]
  }

  irregFdata(argvals_list, X_list, rangeval = fdataobj$rangeval,
             names = fdataobj$names, id = fdataobj$id,
             metadata = fdataobj$metadata)
}

# =============================================================================
# Operations on Irregular Data
# =============================================================================

#' @rdname int.simpson
#' @export
int.simpson.irregFdata <- function(x, ...) {
  # Convert to flat representation
  offsets <- c(0L, cumsum(sapply(x$argvals, length)))
  flat_argvals <- as.double(unlist(x$argvals))
  flat_values <- as.double(unlist(x$X))

  .Call("wrap__irreg_integrate", offsets, flat_argvals, flat_values)
}

#' @rdname norm
#' @export
norm.irregFdata <- function(x, p = 2, ...) {
  offsets <- c(0L, cumsum(sapply(x$argvals, length)))
  flat_argvals <- as.double(unlist(x$argvals))
  flat_values <- as.double(unlist(x$X))

  .Call("wrap__irreg_norm_lp", offsets, flat_argvals, flat_values, as.numeric(p))
}

#' Estimate Mean Function for Irregular Data
#'
#' Estimates the mean function from irregularly sampled functional data.
#'
#' @param x An object of class \code{irregFdata}.
#' @param argvals Target grid for mean estimation. If \code{NULL}, uses
#'   a regular grid of 100 points.
#' @param method Estimation method: \code{"basis"} (default, recommended) fits
#'   basis functions to each curve then averages; \code{"kernel"} uses
#'   Nadaraya-Watson kernel smoothing.
#' @param nbasis Number of basis functions for \code{method = "basis"} (default 15).
#' @param type Basis type for \code{method = "basis"}: \code{"bspline"} (default)
#'   or \code{"fourier"}.
#' @param bandwidth Kernel bandwidth for \code{method = "kernel"}. If \code{NULL},
#'   uses range/10.
#' @param kernel Kernel type for \code{method = "kernel"}: \code{"epanechnikov"}
#'   (default) or \code{"gaussian"}.
#' @param ... Additional arguments (ignored).
#'
#' @return An \code{fdata} object containing the estimated mean function.
#'
#' @details
#' The \code{"basis"} method (default) works by:
#' \enumerate{
#'   \item Fitting basis functions to each curve via least squares
#'   \item Reconstructing each curve on the target grid
#'   \item Averaging the reconstructed curves
#' }
#' This approach preserves the functional structure and typically gives
#' more accurate estimates than kernel smoothing.
#'
#' The \code{"kernel"} method uses Nadaraya-Watson estimation, pooling all
#' observations across curves. This is faster but may be less accurate
#' for structured functional data.
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 100)
#' fd <- simFunData(n = 50, argvals = t, M = 5, seed = 42)
#' ifd <- sparsify(fd, minObs = 10, maxObs = 30, seed = 123)
#'
#' # Recommended: basis method
#' mean_fd <- mean(ifd)
#' plot(mean_fd, main = "Estimated Mean Function")
#'
#' # Alternative: kernel method
#' mean_kernel <- mean(ifd, method = "kernel", bandwidth = 0.1)
mean.irregFdata <- function(x, argvals = NULL, method = c("basis", "kernel"),
                            nbasis = 15, type = c("bspline", "fourier"),
                            bandwidth = NULL, kernel = c("epanechnikov", "gaussian"),
                            ...) {
  if (!inherits(x, "irregFdata")) {
    stop("x must be of class 'irregFdata'")
  }

  method <- match.arg(method)
  type <- match.arg(type)

  if (is.null(argvals)) {
    argvals <- seq(x$rangeval[1], x$rangeval[2], length.out = 100)
  }

  if (method == "basis") {
    # Fit basis to each curve, reconstruct, then average
    coefs <- fdata2basis(x, nbasis = nbasis, type = type)
    fd_recon <- basis2fdata(coefs, argvals = argvals, type = type)
    mean_vals <- colMeans(fd_recon$data)
  } else {
    # Kernel smoothing (original method)
    kernel <- match.arg(kernel)
    kernel_type <- switch(kernel, epanechnikov = 0L, gaussian = 1L)

    if (is.null(bandwidth)) {
      bandwidth <- diff(x$rangeval) / 10
    }

    offsets <- c(0L, cumsum(sapply(x$argvals, length)))
    flat_argvals <- as.double(unlist(x$argvals))
    flat_values <- as.double(unlist(x$X))

    mean_vals <- .Call("wrap__irreg_mean_kernel", offsets, flat_argvals, flat_values,
                       as.double(argvals), as.double(bandwidth), kernel_type)
  }

  fdata(matrix(mean_vals, nrow = 1), argvals = argvals,
        rangeval = x$rangeval, names = list(
          main = "Mean Function",
          xlab = x$names$xlab,
          ylab = x$names$ylab
        ))
}

#' @rdname metric.lp
#' @export
metric.lp.irregFdata <- function(x, p = 2, ...) {
  offsets <- c(0L, cumsum(sapply(x$argvals, length)))
  flat_argvals <- as.double(unlist(x$argvals))
  flat_values <- as.double(unlist(x$X))

  .Call("wrap__irreg_metric_lp", offsets, flat_argvals, flat_values, as.numeric(p))
}

# =============================================================================
# Basis Representation
# =============================================================================

#' @rdname fdata2basis
#' @method fdata2basis irregFdata
#' @export
fdata2basis.irregFdata <- function(x, nbasis = 10, type = c("bspline", "fourier"), ...) {
  type <- match.arg(type)
  basis_type <- if (type == "fourier") 1L else 0L

  # Convert to flat representation with offsets
  offsets <- c(0L, cumsum(sapply(x$argvals, length)))
  flat_argvals <- as.double(unlist(x$argvals))
  flat_values <- as.double(unlist(x$X))

  # Call Rust function for irregular basis fitting
  .Call("wrap__irreg_fdata2basis", offsets, flat_argvals, flat_values,
        as.integer(nbasis), basis_type)
}

# =============================================================================
# Scaling / Normalization
# =============================================================================

#' @rdname standardize
#' @method standardize irregFdata
#' @export
standardize.irregFdata <- function(fdataobj) {
  # Standardize each curve: (x - mean) / sd
  for (i in seq_len(fdataobj$n)) {
    x <- fdataobj$X[[i]]
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    if (s == 0) s <- 1  # Handle constant curves
    fdataobj$X[[i]] <- (x - m) / s
  }
  fdataobj
}

#' @rdname scale_minmax
#' @method scale_minmax irregFdata
#' @export
scale_minmax.irregFdata <- function(fdataobj, min = 0, max = 1) {
  # Scale each curve to [min, max]
  for (i in seq_len(fdataobj$n)) {
    x <- fdataobj$X[[i]]
    x_min <- min(x, na.rm = TRUE)
    x_max <- max(x, na.rm = TRUE)
    x_range <- x_max - x_min
    if (x_range == 0) x_range <- 1  # Handle constant curves

    # Scale to [0, 1] then to [min, max]
    fdataobj$X[[i]] <- ((x - x_min) / x_range) * (max - min) + min
  }
  fdataobj
}

# =============================================================================
# Plotting
# =============================================================================

#' Plot method for irregFdata objects
#' @param x An irregFdata object.
#' @param ... Additional arguments passed to \code{plot}.
#' @param col Colors for curves.
#' @param lty Line type.
#' @param lwd Line width.
#' @param main Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param add Logical. If TRUE, add to existing plot.
#' @param alpha Transparency for many curves.
#' @return Invisibly returns the input object \code{x}.
#' @export
plot.irregFdata <- function(x, ..., col = NULL, lty = 1, lwd = 1,
                             main = NULL, xlab = NULL, ylab = NULL,
                             add = FALSE, alpha = 0.7) {
  if (is.null(main)) main <- x$names$main
  if (is.null(xlab)) xlab <- x$names$xlab
  if (is.null(ylab)) ylab <- x$names$ylab

  n <- x$n
  if (is.null(col)) {
    if (n <= 10) {
      col <- seq_len(n)
    } else {
      # Use semi-transparent gray for many curves
      col <- rgb(0.3, 0.3, 0.3, alpha = alpha)
    }
  }
  col <- rep_len(col, n)

  # Determine plot range
  all_x <- unlist(x$argvals)
  all_y <- unlist(x$X)
  xlim <- range(all_x, na.rm = TRUE)
  ylim <- range(all_y, na.rm = TRUE)

  if (!add) {
    plot(NULL, xlim = xlim, ylim = ylim, main = main,
         xlab = xlab, ylab = ylab, ...)
  }

  for (i in seq_len(n)) {
    lines(x$argvals[[i]], x$X[[i]], col = col[i], lty = lty, lwd = lwd)
    points(x$argvals[[i]], x$X[[i]], col = col[i], pch = 16, cex = 0.5)
  }

  invisible(x)
}

#' Autoplot method for irregFdata objects
#' @param object An irregFdata object.
#' @param ... Additional arguments (ignored).
#' @param alpha Transparency for lines.
#' @return A \code{ggplot} object.
#' @export
autoplot.irregFdata <- function(object, ..., alpha = 0.7) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for autoplot")
  }

  # Create data frame for plotting
  n <- object$n
  df_list <- lapply(seq_len(n), function(i) {
    data.frame(
      t = object$argvals[[i]],
      X = object$X[[i]],
      id = object$id[i],
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, df_list)
  df$id <- factor(df$id, levels = object$id)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = t, y = X, group = id)) +
    ggplot2::geom_line(alpha = alpha) +
    ggplot2::geom_point(size = 0.5, alpha = alpha) +
    ggplot2::labs(
      title = object$names$main,
      x = object$names$xlab,
      y = object$names$ylab
    ) +
    ggplot2::theme_minimal()

  if (n <= 10) {
    p <- p + ggplot2::aes(color = id) +
      ggplot2::guides(color = ggplot2::guide_legend(title = "Observation"))
  }

  p
}
