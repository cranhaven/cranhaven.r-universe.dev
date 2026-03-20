#' Outlier Detection for Functional Data
#'
#' Functions for detecting outliers in functional data using depth measures.

#' Outlier Detection using Weighted Depth
#'
#' Detects outliers based on depth with bootstrap resampling. The threshold
#' for outlier detection can be computed using different methods.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param nb Number of bootstrap samples. Default is 200.
#' @param dfunc Depth function to use. Default is depth.mode.
#' @param threshold_method Method for computing the outlier threshold. Options:
#'   \describe{
#'     \item{"quantile"}{Use quantile of weighted depths (default). Curves with
#'       depth below this quantile are flagged as outliers.}
#'     \item{"mad"}{Use median - k * MAD of weighted depths. More robust to
#'       existing outliers in the data.}
#'     \item{"iqr"}{Use Q1 - k * IQR, similar to boxplot whiskers.}
#'   }
#' @param quan Quantile for outlier cutoff when \code{threshold_method = "quantile"}.
#'   Default is 0.05, meaning curves with depth in the bottom 5% are flagged
#'   (95th percentile threshold). Lower values detect fewer outliers.
#' @param k Multiplier for MAD or IQR methods. Default is 2.5 for MAD and 1.5
#'   for IQR. Higher values detect fewer outliers.
#' @param ... Additional arguments passed to depth function.
#'
#' @return A list of class 'outliers.fdata' with components:
#' \describe{
#'   \item{outliers}{Indices of detected outliers}
#'   \item{depths}{Depth values for all curves}
#'   \item{weighted_depths}{Bootstrap-weighted depth values}
#'   \item{cutoff}{Depth cutoff used}
#'   \item{threshold_method}{Method used for threshold computation}
#'   \item{fdataobj}{Original fdata object}
#' }
#'
#' @details
#' The function first computes depth values for all curves, then uses bootstrap
#' resampling to obtain weighted depths that are more robust to sampling variability.
#'
#' \strong{Threshold Methods:}
#' \itemize{
#'   \item \strong{quantile}: Flags curves with depth below the specified quantile.
#'     With \code{quan = 0.1}, approximately 10% of curves would be flagged under
#'     the null hypothesis of no outliers. Suitable when you expect a specific
#'     proportion of outliers.
#'   \item \strong{mad}: Uses \code{median(depths) - k * MAD(depths)} as threshold.
#'     More robust because MAD is not influenced by extreme values. With k = 2.5,
#'     this corresponds roughly to a 1-2% false positive rate under normality.
#'   \item \strong{iqr}: Uses \code{Q1 - k * IQR} as threshold, similar to boxplot
#'     outlier detection. With k = 1.5, corresponds to the standard boxplot fence.
#' }
#'
#' @export
#' @examples
#' # Create data with outliers
#' set.seed(42)
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 30, 50)
#' for (i in 1:28) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.2)
#' X[29, ] <- sin(2*pi*t) + 3  # outlier
#' X[30, ] <- -sin(2*pi*t)     # outlier
#' fd <- fdata(X, argvals = t)
#'
#' \donttest{
#' # Default: quantile method with 95th percentile (bottom 5%)
#' out1 <- outliers.depth.pond(fd, nb = 50)
#'
#' # More permissive: bottom 10%
#' out1b <- outliers.depth.pond(fd, nb = 50, quan = 0.1)
#'
#' # MAD method (more robust)
#' out2 <- outliers.depth.pond(fd, nb = 50, threshold_method = "mad", k = 2.5)
#'
#' # IQR method (boxplot-like)
#' out3 <- outliers.depth.pond(fd, nb = 50, threshold_method = "iqr", k = 1.5)
#' }
outliers.depth.pond <- function(fdataobj, nb = 200, dfunc = depth.mode,
                                 threshold_method = c("quantile", "mad", "iqr"),
                                 quan = 0.05, k = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  threshold_method <- match.arg(threshold_method)

  # Set default k based on method

  if (is.null(k)) {
    k <- switch(threshold_method,
      "mad" = 2.5,
      "iqr" = 1.5,
      2.5  # default for quantile (not used)
    )
  }

  n <- nrow(fdataobj$data)

  # Compute depths
  depths <- dfunc(fdataobj, fdataobj, ...)

  # Bootstrap to estimate null distribution of depths
  boot_depths <- matrix(0, nb, n)
  for (b in seq_len(nb)) {
    # Resample indices
    idx <- sample(n, n, replace = TRUE)
    fd_boot <- fdataobj[idx, ]

    # Compute depths in bootstrap sample
    boot_depths[b, ] <- dfunc(fdataobj, fd_boot, ...)
  }

  # Compute weighted depth (average over bootstrap samples)
  weighted_depths <- colMeans(boot_depths)

  # Determine cutoff based on threshold method
  cutoff <- switch(threshold_method,
    "quantile" = {
      quantile(weighted_depths, quan)
    },
    "mad" = {
      med <- median(weighted_depths)
      mad_val <- mad(weighted_depths, constant = 1.4826)
      med - k * mad_val
    },
    "iqr" = {
      q1 <- quantile(weighted_depths, 0.25)
      q3 <- quantile(weighted_depths, 0.75)
      iqr_val <- q3 - q1
      q1 - k * iqr_val
    }
  )

  # Identify outliers (curves with depth below cutoff)
  outliers <- which(depths < cutoff)

  structure(
    list(
      outliers = outliers,
      depths = depths,
      weighted_depths = weighted_depths,
      cutoff = unname(cutoff),
      threshold_method = threshold_method,
      fdataobj = fdataobj
    ),
    class = "outliers.fdata"
  )
}

#' Outlier Detection using Trimmed Depth
#'
#' Detects outliers based on depth trimming.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param trim Proportion of curves to consider as potential outliers.
#'   Default is 0.1 (curves with depth in bottom 10%).
#' @param dfunc Depth function to use. Default is depth.mode.
#' @param ... Additional arguments passed to depth function.
#'
#' @return A list of class 'outliers.fdata' with components:
#' \describe{
#'   \item{outliers}{Indices of detected outliers}
#'   \item{depths}{Depth values for all curves}
#'   \item{cutoff}{Depth cutoff used}
#' }
#'
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(200), 20, 10))
#' out <- outliers.depth.trim(fd, trim = 0.1)
outliers.depth.trim <- function(fdataobj, trim = 0.1, dfunc = depth.mode, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (trim <= 0 || trim >= 1) {
    stop("trim must be between 0 and 1")
  }

  n <- nrow(fdataobj$data)

  # Compute depths
  depths <- dfunc(fdataobj, fdataobj, ...)

  # Determine cutoff based on trim proportion
  cutoff <- quantile(depths, trim)

  # Identify outliers
  outliers <- which(depths <= cutoff)

  structure(
    list(
      outliers = outliers,
      depths = depths,
      cutoff = cutoff,
      trim = trim,
      fdataobj = fdataobj
    ),
    class = "outliers.fdata"
  )
}

#' Print method for outliers.fdata objects
#' @param x An outliers.fdata object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.outliers.fdata <- function(x, ...) {
  cat("Functional data outlier detection\n")

  # Handle both depths and distances (LRT uses distances)
  n_obs <- if (!is.null(x$depths)) length(x$depths) else length(x$distances)
  cat("  Number of observations:", n_obs, "\n")
  cat("  Number of outliers:", length(x$outliers), "\n")

  if (length(x$outliers) > 0) {
    cat("  Outlier indices:", head(x$outliers, 10))
    if (length(x$outliers) > 10) cat(" ...")
    cat("\n")
  }

  if (!is.null(x$threshold_method)) {
    cat("  Threshold method:", x$threshold_method, "\n")
  }

  # Handle both cutoff (depth methods) and threshold (LRT)
  if (!is.null(x$cutoff) && !is.na(x$cutoff)) {
    cat("  Depth cutoff:", round(x$cutoff, 4), "\n")
  } else if (!is.null(x$threshold)) {
    percentile_str <- if (!is.null(x$percentile)) {
      paste0(" (", x$percentile * 100, "th percentile)")
    } else {
      ""
    }
    cat("  LRT threshold:", round(x$threshold, 4), percentile_str, "\n")
  }

  invisible(x)
}

#' Plot method for outliers.fdata objects
#'
#' @param x An object of class 'outliers.fdata'.
#' @param col.outliers Color for outlier curves (default "red").
#' @param ... Additional arguments (currently ignored).
#'
#' @return A ggplot object.
#'
#' @export
plot.outliers.fdata <- function(x, col.outliers = "red", ...) {
  fd <- x$fdataobj
  n <- nrow(fd$data)
  m <- ncol(fd$data)

  # Create status factor
  status <- rep("Normal", n)
  status[x$outliers] <- "Outlier"

  # Reshape to long format
  df <- data.frame(
    curve_id = rep(seq_len(n), each = m),
    argval = rep(fd$argvals, n),
    value = as.vector(t(fd$data)),
    status = factor(rep(status, each = m), levels = c("Normal", "Outlier"))
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$argval, y = .data$value,
                                         group = .data$curve_id,
                                         color = .data$status)) +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::scale_color_manual(values = c("Normal" = "gray60",
                                           "Outlier" = col.outliers)) +
    ggplot2::labs(
      x = fd$names$xlab %||% "t",
      y = fd$names$ylab %||% "X(t)",
      title = paste("Outliers detected:", length(x$outliers)),
      color = "Status"
    )

  p
}

#' LRT Outlier Detection Threshold
#'
#' Computes the bootstrap threshold for LRT-based outlier detection.
#' This is a highly parallelized Rust implementation providing significant
#' speedup over pure R implementations.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param nb Number of bootstrap replications (default 200).
#' @param smo Smoothing parameter for bootstrap noise (default 0.05).
#' @param trim Proportion of curves to trim for robust estimation (default 0.1).
#' @param seed Random seed for reproducibility.
#' @param percentile Percentile of bootstrap distribution to use as threshold
#'   (default 0.99, meaning 99th percentile). Lower values make detection
#'   more sensitive (detect more outliers).
#'
#' @return The threshold value at the specified percentile.
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 30, 50)
#' for (i in 1:30) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#' thresh <- outliers.thres.lrt(fd, nb = 100)
#'
#' # More sensitive detection (95th percentile)
#' thresh_sensitive <- outliers.thres.lrt(fd, nb = 100, percentile = 0.95)
outliers.thres.lrt <- function(fdataobj, nb = 200, smo = 0.05, trim = 0.1,
                               seed = NULL, percentile = 0.99) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("outliers.thres.lrt for 2D functional data not yet implemented")
  }

  if (percentile <= 0 || percentile >= 1) {
    stop("percentile must be between 0 and 1 (exclusive)")
  }

  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }

  .Call("wrap__outliers_thres_lrt", fdataobj$data,
        as.numeric(fdataobj$argvals), as.integer(nb),
        as.numeric(smo), as.numeric(trim), as.numeric(seed),
        as.numeric(percentile))
}

#' LRT-based Outlier Detection for Functional Data
#'
#' Detects outliers using the Likelihood Ratio Test approach based on
#' Febrero-Bande et al. Uses bootstrap to estimate a threshold and
#' iteratively removes curves exceeding this threshold.
#' Implemented in Rust for high performance with parallelized bootstrap.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param nb Number of bootstrap replications for threshold estimation (default 200).
#' @param smo Smoothing parameter for bootstrap noise (default 0.05).
#' @param trim Proportion of curves to trim for robust estimation (default 0.1).
#' @param seed Random seed for reproducibility.
#' @param percentile Percentile of bootstrap distribution to use as threshold
#'   (default 0.99, meaning 99th percentile). Lower values make detection
#'   more sensitive (detect more outliers).
#'
#' @return A list of class 'outliers.fdata' with components:
#' \describe{
#'   \item{outliers}{Indices of detected outliers}
#'   \item{distances}{Normalized distances for all curves}
#'   \item{threshold}{Bootstrap threshold used}
#'   \item{percentile}{Percentile used for threshold}
#'   \item{fdataobj}{Original fdata object}
#' }
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 30, 50)
#' for (i in 1:30) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.1)
#' # Add an outlier
#' X[1, ] <- X[1, ] + 3
#' fd <- fdata(X, argvals = t)
#' out <- outliers.lrt(fd, nb = 100)
#'
#' # More sensitive detection
#' out_sensitive <- outliers.lrt(fd, nb = 100, percentile = 0.95)
outliers.lrt <- function(fdataobj, nb = 200, smo = 0.05, trim = 0.1,
                         seed = NULL, percentile = 0.99) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("outliers.lrt for 2D functional data not yet implemented")
  }

  if (percentile <= 0 || percentile >= 1) {
    stop("percentile must be between 0 and 1 (exclusive)")
  }

  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }

  result <- .Call("wrap__outliers_lrt", fdataobj$data,
                  as.numeric(fdataobj$argvals), as.integer(nb),
                  as.numeric(smo), as.numeric(trim), as.numeric(seed),
                  as.numeric(percentile))

  structure(
    list(
      outliers = result$outliers,
      distances = result$distances,
      threshold = result$threshold,
      percentile = percentile,
      fdataobj = fdataobj
    ),
    class = "outliers.fdata"
  )
}

#' Outlier Detection using Functional Boxplot
#'
#' Detects outliers based on the functional boxplot method.
#' Curves that exceed the fence (1.5 times the central envelope width)
#' at any point are flagged as outliers.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param prob Proportion of curves for the central region (default 0.5).
#' @param factor Factor for fence calculation (default 1.5).
#' @param depth.func Depth function to use. Default is depth.MBD.
#' @param ... Additional arguments passed to depth function.
#'
#' @return A list of class 'outliers.fdata' with components:
#' \describe{
#'   \item{outliers}{Indices of detected outliers}
#'   \item{depths}{Depth values for all curves}
#'   \item{cutoff}{Not used (for compatibility)}
#'   \item{fdataobj}{Original fdata object}
#' }
#'
#' @seealso \code{\link{boxplot.fdata}} for functional boxplot visualization
#'
#' @export
#' @examples
#' # Create functional data with outliers
#' set.seed(42)
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 30, 50)
#' for (i in 1:28) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.2)
#' X[29, ] <- sin(2*pi*t) + 2  # Magnitude outlier
#' X[30, ] <- cos(2*pi*t)       # Shape outlier
#' fd <- fdata(X, argvals = t)
#'
#' # Detect outliers
#' out <- outliers.boxplot(fd)
#' print(out)
outliers.boxplot <- function(fdataobj, prob = 0.5, factor = 1.5,
                             depth.func = depth.MBD, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  n <- nrow(fdataobj$data)
  m <- ncol(fdataobj$data)

  # Compute depths
  depths <- depth.func(fdataobj, fdataobj, ...)

  # Order curves by depth
  depth_order <- order(depths, decreasing = TRUE)

  # Central region: top prob proportion of curves
  n_central <- max(1, ceiling(n * prob))
  central_idx <- depth_order[seq_len(n_central)]

  # Compute central envelope
  central_data <- fdataobj$data[central_idx, , drop = FALSE]
  env_min <- apply(central_data, 2, min)
  env_max <- apply(central_data, 2, max)

  # Compute fence
  env_width <- env_max - env_min
  fence_min <- env_min - factor * env_width
  fence_max <- env_max + factor * env_width

  # Identify outliers: curves that exceed the fence at any point
  outlier_idx <- integer(0)
  for (i in seq_len(n)) {
    curve <- fdataobj$data[i, ]
    if (any(curve < fence_min) || any(curve > fence_max)) {
      outlier_idx <- c(outlier_idx, i)
    }
  }

  structure(
    list(
      outliers = outlier_idx,
      depths = depths,
      cutoff = NA,
      envelope = list(min = env_min, max = env_max),
      fence = list(min = fence_min, max = fence_max),
      fdataobj = fdataobj
    ),
    class = "outliers.fdata"
  )
}

#' Magnitude-Shape Outlier Detection for Functional Data
#'
#' Performs Magnitude-Shape (MS) outlier detection for functional data.
#' Each curve is represented as a point in 2D space where the x-axis
#' represents magnitude outlyingness and the y-axis represents shape outlyingness.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param depth.func Depth function to use for computing outlyingness.
#'   Default is depth.MBD.
#' @param cutoff.quantile Quantile for outlier cutoff (default 0.993).
#' @param col.normal Color for normal curves (default "black").
#' @param col.outliers Color for outlier curves (default "red").
#' @param label What to use for labeling outlier points. Options:
#'   \itemize{
#'     \item \code{"index"}: Use numeric indices (default)
#'     \item \code{"id"}: Use observation IDs from the fdata object
#'     \item A column name from the fdata metadata (e.g., \code{"patient_id"})
#'     \item \code{NULL}: No labels
#'   }
#' @param label_all Logical. If TRUE, label all points, not just outliers. Default FALSE.
#' @param ... Additional arguments passed to depth function.
#' @importFrom stats qchisq
#'
#' @return A list of class 'magnitudeshape' with components:
#' \describe{
#'   \item{MO}{Magnitude outlyingness values}
#'   \item{VO}{Shape (variability) outlyingness values}
#'   \item{outliers}{Indices of detected outliers}
#'   \item{cutoff}{Chi-squared cutoff value used}
#'   \item{plot}{The ggplot object}
#' }
#'
#' @details
#' The MS plot (Dai & Genton, 2019) decomposes functional outlyingness into:
#' \itemize{
#'   \item \strong{Magnitude Outlyingness (MO)}: Based on pointwise median of
#'     directional outlyingness - captures shift outliers
#'   \item \strong{Shape Outlyingness (VO)}: Based on variability of directional
#'     outlyingness - captures shape outliers
#' }
#'
#' Outliers are detected using the chi-squared distribution with cutoff at
#' the specified quantile.
#'
#' @references
#' Dai, W. and Genton, M.G. (2019). Directional outlyingness for multivariate
#' functional data. \emph{Computational Statistics & Data Analysis}, 131, 50-65.
#'
#' @export
#' @examples
#' # Create functional data with outliers
#' set.seed(42)
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 30, 50)
#' for (i in 1:28) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.2)
#' X[29, ] <- sin(2*pi*t) + 2  # Magnitude outlier
#' X[30, ] <- sin(4*pi*t)       # Shape outlier
#' fd <- fdata(X, argvals = t)
#'
#' # Create MS plot
#' ms <- magnitudeshape(fd)
#'
#' # With IDs and metadata
#' fd <- fdata(X, argvals = t, id = paste0("curve_", 1:30))
#' ms <- magnitudeshape(fd, label = "id")
magnitudeshape <- function(fdataobj, depth.func = depth.MBD,
                           cutoff.quantile = 0.993,
                           col.normal = "black", col.outliers = "red",
                           label = "index", label_all = FALSE, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("magnitudeshape not yet implemented for 2D functional data")
  }

  n <- nrow(fdataobj$data)
  m <- ncol(fdataobj$data)
  argvals <- fdataobj$argvals

  # Compute median curve using depth
  depths <- depth.func(fdataobj, fdataobj, ...)
  median_idx <- which.max(depths)
  median_curve <- fdataobj$data[median_idx, ]

  # Compute pointwise MAD for robust scale
  centered <- sweep(fdataobj$data, 2, median_curve)
  pointwise_mad <- apply(abs(centered), 2, median) * 1.4826  # scale factor for consistency

  # Avoid division by zero
  pointwise_mad[pointwise_mad < 1e-10] <- 1e-10

  # Compute directional outlyingness at each time point
  # O(t) = (X(t) - median(t)) / MAD(t)
  outlyingness <- sweep(centered, 2, pointwise_mad, "/")

  # Magnitude Outlyingness (MO): pointwise median of outlyingness for each curve
  MO <- apply(outlyingness, 1, median)

  # Variability Outlyingness (VO): MAD of outlyingness for each curve
  # This captures shape deviation
  VO <- apply(outlyingness, 1, function(x) median(abs(x - median(x))) * 1.4826)

  # Chi-squared cutoff for outlier detection
  # Using 2 degrees of freedom for (MO, VO)
  cutoff <- qchisq(cutoff.quantile, df = 2)

  # Compute squared Mahalanobis-like distance
  # Using robust estimates of center and scale
  MO_center <- median(MO)
  VO_center <- median(VO)
  MO_scale <- median(abs(MO - MO_center)) * 1.4826
  VO_scale <- median(abs(VO - VO_center)) * 1.4826

  # Avoid division by zero
  if (MO_scale < 1e-10) MO_scale <- 1
  if (VO_scale < 1e-10) VO_scale <- 1

  # Squared standardized distance
  dist_sq <- ((MO - MO_center) / MO_scale)^2 + ((VO - VO_center) / VO_scale)^2

  # Identify outliers
  outliers <- which(dist_sq > cutoff)

  # Create status for plotting
  status <- rep("Normal", n)
  status[outliers] <- "Outlier"

  # Determine labels based on label parameter
  if (is.null(label)) {
    labels <- as.character(seq_len(n))
    show_labels <- FALSE
  } else if (label == "index") {
    labels <- as.character(seq_len(n))
    show_labels <- TRUE
  } else if (label == "id") {
    if (!is.null(fdataobj$id)) {
      labels <- fdataobj$id
    } else {
      labels <- as.character(seq_len(n))
    }
    show_labels <- TRUE
  } else {
    # Try to get from metadata
    if (!is.null(fdataobj$metadata) && label %in% colnames(fdataobj$metadata)) {
      labels <- as.character(fdataobj$metadata[[label]])
    } else {
      warning("Label column '", label, "' not found in metadata. Using index.")
      labels <- as.character(seq_len(n))
    }
    show_labels <- TRUE
  }

  # Create data frame for plotting
  df <- data.frame(
    MO = MO,
    VO = VO,
    status = factor(status, levels = c("Normal", "Outlier")),
    label = labels
  )

  # Create ggplot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$MO, y = .data$VO,
                                         color = .data$status)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_manual(values = c("Normal" = col.normal,
                                           "Outlier" = col.outliers)) +
    ggplot2::labs(
      x = "Magnitude Outlyingness (MO)",
      y = "Shape Outlyingness (VO)",
      title = "Magnitude-Shape Plot",
      color = "Status"
    )

  # Add chi-squared contour at cutoff
  theta <- seq(0, 2*pi, length.out = 100)
  r <- sqrt(cutoff)
  ellipse_df <- data.frame(
    x = MO_center + r * MO_scale * cos(theta),
    y = VO_center + r * VO_scale * sin(theta)
  )
  p <- p + ggplot2::geom_path(data = ellipse_df,
                              ggplot2::aes(x = .data$x, y = .data$y),
                              color = "blue", linetype = "dashed",
                              inherit.aes = FALSE)

  # Add labels
  if (show_labels) {
    if (label_all) {
      p <- p + ggplot2::geom_text(
        data = df,
        ggplot2::aes(label = .data$label),
        nudge_y = 0.05,
        size = 3,
        show.legend = FALSE
      )
    } else if (length(outliers) > 0) {
      outlier_df <- df[outliers, ]
      p <- p + ggplot2::geom_text(
        data = outlier_df,
        ggplot2::aes(label = .data$label),
        nudge_y = 0.05,
        size = 3,
        color = col.outliers
      )
    }
  }

  # Return result
  result <- structure(
    list(
      MO = MO,
      VO = VO,
      outliers = outliers,
      cutoff = cutoff,
      dist_sq = dist_sq,
      fdataobj = fdataobj,
      plot = p
    ),
    class = "magnitudeshape"
  )

  invisible(result)
}

#' Print Method for magnitudeshape Objects
#'
#' @param x An object of class 'magnitudeshape'.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.magnitudeshape <- function(x, ...) {
  cat("Magnitude-Shape Outlier Detection\n")
  cat("==================================\n")
  cat("Number of curves:", length(x$MO), "\n")
  cat("Outliers detected:", length(x$outliers), "\n")
  if (length(x$outliers) > 0) {
    cat("Outlier indices:", paste(x$outliers, collapse = ", "), "\n")
  }
  cat("Chi-squared cutoff:", round(x$cutoff, 3), "\n")
  invisible(x)
}

#' Plot Method for magnitudeshape Objects
#'
#' @param x An object of class 'magnitudeshape'.
#' @param ... Additional arguments (ignored).
#'
#' @return The ggplot object (invisibly).
#'
#' @export
plot.magnitudeshape <- function(x, ...) {
  print(x$plot)
  invisible(x$plot)
}

#' Outliergram for Functional Data
#'
#' Creates an outliergram plot that displays MEI (Modified Epigraph Index) versus
#' MBD (Modified Band Depth) for outlier detection. Points below the parabolic
#' boundary are identified as outliers, and each outlier is classified by type.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param factor Factor to adjust the outlier detection threshold. Higher values
#'   make detection less sensitive. Default is 1.5.
#' @param mei_threshold Deprecated and ignored. Kept for backwards compatibility.
#' @param ... Additional arguments (currently ignored).
#'
#' @return An object of class 'outliergram' with components:
#' \describe{
#'   \item{fdataobj}{The input functional data}
#'   \item{mei}{MEI values for each curve}
#'   \item{mbd}{MBD values for each curve}
#'   \item{outliers}{Indices of detected outliers}
#'   \item{outlier_type}{Character vector of outlier types ("shape") for each
#'     detected outlier}
#'   \item{n_outliers}{Number of outliers detected}
#'   \item{factor}{The factor used for threshold adjustment}
#'   \item{parabola}{Coefficients of the parabolic boundary (a0, a1, a2)}
#'   \item{threshold}{The boxplot-fence threshold for distance below the parabola}
#'   \item{dist_to_parabola}{Vertical distance below the parabola for each curve
#'     (positive values indicate the point is below the parabola)}
#' }
#'
#' @details
#' The outliergram plots MEI on the x-axis versus MBD on the y-axis. For a
#' sample of size \eqn{n}, the theoretical relationship is bounded by the
#' finite-sample parabola (Arribas-Gil & Romo, 2014, Proposition 1):
#' \deqn{MBD \le a_0 + a_1 \cdot MEI + a_2 \cdot MEI^2}
#' where \eqn{a_0 = -2/(n(n-1))}, \eqn{a_1 = 2(n+1)/(n-1)},
#' \eqn{a_2 = -2(n+1)/(n-1)}.
#'
#' Shape outliers are detected using a boxplot fence on the vertical distances
#' below the parabola: a curve is flagged when its distance exceeds
#' \eqn{Q_3 + \mathrm{factor} \times IQR}.
#'
#' @references
#' Arribas-Gil, A. and Romo, J. (2014). Shape outlier detection and visualization
#' for functional data: the outliergram. \emph{Biostatistics}, 15(4), 603-619.
#'
#' @seealso \code{\link{depth}} for depth computation, \code{\link{magnitudeshape}} for
#'   an alternative outlier visualization.
#'
#' @export
#' @examples
#' # Create functional data with different outlier types
#' set.seed(42)
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 32, 50)
#' for (i in 1:29) X[i, ] <- sin(2 * pi * t) + rnorm(50, sd = 0.2)
#' X[30, ] <- sin(2 * pi * t) + 2       # magnitude outlier (high)
#' X[31, ] <- sin(2 * pi * t) - 2       # magnitude outlier (low)
#' X[32, ] <- sin(4 * pi * t)           # shape outlier
#' fd <- fdata(X, argvals = t)
#'
#' # Create outliergram
#' og <- outliergram(fd)
#' print(og)
#' plot(og, color_by_type = TRUE)
outliergram <- function(fdataobj, factor = 1.5, mei_threshold = 0.25, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("outliergram not yet implemented for 2D functional data")
  }

  n <- nrow(fdataobj$data)

  # Compute MEI and MBD
  mei <- depth(fdataobj, method = "MEI")
  mbd <- depth(fdataobj, method = "MBD")

  # Finite-sample parabola coefficients (Arribas-Gil & Romo, 2014, Prop. 1)
  # MBD = a0 + a1*MEI + a2*MEI^2 is the theoretical upper bound relating
  # MBD and MEI for a sample of size n.
  a0 <- -2 / (n * (n - 1))
  a1 <- 2 * (n + 1) / (n - 1)
  a2 <- -2 * (n + 1) / (n - 1)

  # Compute expected MBD based on MEI
  mbd_expected <- a0 + a1 * mei + a2 * mei^2

  # Compute vertical distance below the parabola (positive = below)
  # d_i > 0 means the point is below the parabola (potential shape outlier)
  dist_to_parabola <- mbd_expected - mbd

  # Outlier detection using boxplot fence (Arribas-Gil & Romo, 2014)
  q1 <- stats::quantile(dist_to_parabola, 0.25)
  q3 <- stats::quantile(dist_to_parabola, 0.75)
  iqr <- q3 - q1
  threshold <- q3 + factor * iqr
  shape_outliers <- which(dist_to_parabola > threshold)

  # All outliers are shape outliers (magnitude detection via MEI threshold
  # is not part of the original outliergram method)
  all_outliers <- sort(shape_outliers)

  # Classify outlier types
  outlier_type <- rep("shape", length(all_outliers))

  outliers <- all_outliers

  structure(
    list(
      fdataobj = fdataobj,
      mei = mei,
      mbd = mbd,
      outliers = outliers,
      outlier_type = outlier_type,
      n_outliers = length(outliers),
      factor = factor,
      mei_threshold = mei_threshold,
      parabola = c(a0 = a0, a1 = a1, a2 = a2),
      threshold = threshold,
      dist_to_parabola = dist_to_parabola
    ),
    class = "outliergram"
  )
}

#' Plot Method for Outliergram Objects
#'
#' Creates a scatter plot of MEI vs MBD with the parabolic boundary and
#' identified outliers highlighted.
#'
#' @param x An object of class 'outliergram'.
#' @param col_normal Color for normal observations. Default is "gray60".
#' @param col_outlier Color for outliers (used when \code{color_by_type = FALSE}).
#'   Default is "red".
#' @param color_by_type Logical. If TRUE, color outliers by their type.
#'   Default is FALSE.
#' @param show_parabola Logical. If TRUE, draw the theoretical parabola. Default TRUE.
#' @param show_threshold Logical. If TRUE, draw the adjusted threshold parabola. Default TRUE.
#' @param label What to use for labeling outlier points. Options:
#'   \itemize{
#'     \item \code{"index"}: Use numeric indices (default)
#'     \item \code{"id"}: Use observation IDs from the fdata object
#'     \item A column name from the fdata metadata (e.g., \code{"patient_id"})
#'   }
#' @param label_all Logical. If TRUE, label all points, not just outliers. Default FALSE.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return A \code{ggplot} object (invisibly).
#' @export
plot.outliergram <- function(x, col_normal = "gray60", col_outlier = "red",
                              color_by_type = FALSE,
                              show_parabola = TRUE, show_threshold = TRUE,
                              label = "index", label_all = FALSE, ...) {
  n <- length(x$mei)

  # Determine labels based on label parameter
  if (label == "index") {
    labels <- as.character(seq_len(n))
  } else if (label == "id") {
    if (!is.null(x$fdataobj$id)) {
      labels <- x$fdataobj$id
    } else {
      labels <- as.character(seq_len(n))
    }
  } else {
    # Try to get from metadata
    if (!is.null(x$fdataobj$metadata) && label %in% colnames(x$fdataobj$metadata)) {
      labels <- as.character(x$fdataobj$metadata[[label]])
    } else {
      warning("Label column '", label, "' not found in metadata. Using index.")
      labels <- as.character(seq_len(n))
    }
  }

  # Create status/type factor for coloring
  if (color_by_type && length(x$outliers) > 0) {
    status <- rep("Normal", n)
    status[x$outliers] <- x$outlier_type
    status <- factor(status, levels = c("Normal", "shape", "magnitude_high",
                                         "magnitude_low", "mixed"))
    color_values <- c("Normal" = col_normal,
                      "shape" = "#E69F00",        # orange
                      "magnitude_high" = "#D55E00", # red-orange
                      "magnitude_low" = "#0072B2",  # blue
                      "mixed" = "#CC79A7")          # pink
  } else {
    status <- rep("Normal", n)
    if (length(x$outliers) > 0) {
      status[x$outliers] <- "Outlier"
    }
    status <- factor(status, levels = c("Normal", "Outlier"))
    color_values <- c("Normal" = col_normal, "Outlier" = col_outlier)
  }

  # Create data frame for plotting
  df <- data.frame(
    mei = x$mei,
    mbd = x$mbd,
    status = status,
    label = labels
  )

  # Create parabola data for plotting
  mei_seq <- seq(0, 1, length.out = 100)
  mbd_parabola <- x$parabola["a0"] + x$parabola["a1"] * mei_seq +
                  x$parabola["a2"] * mei_seq^2
  parabola_df <- data.frame(
    mei = mei_seq,
    mbd_theoretical = mbd_parabola,
    mbd_threshold = mbd_parabola - x$threshold
  )

  # Build ggplot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = mei, y = mbd, color = status)) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::scale_color_manual(values = color_values, drop = FALSE) +
    ggplot2::labs(
      title = "Outliergram",
      x = "MEI (Modified Epigraph Index)",
      y = "MBD (Modified Band Depth)",
      color = ""
    ) +
    ggplot2::theme(legend.position = "bottom")

  # Add theoretical parabola
  if (show_parabola) {
    p <- p + ggplot2::geom_line(
      data = parabola_df,
      ggplot2::aes(x = mei, y = mbd_theoretical),
      inherit.aes = FALSE,
      color = "blue",
      linetype = "dashed",
      linewidth = 0.8
    )
  }

  # Add threshold parabola
  if (show_threshold) {
    p <- p + ggplot2::geom_line(
      data = parabola_df,
      ggplot2::aes(x = mei, y = mbd_threshold),
      inherit.aes = FALSE,
      color = "darkred",
      linetype = "dotted",
      linewidth = 0.8
    )
  }

  # Add labels
  if (label_all) {
    p <- p + ggplot2::geom_text(
      data = df,
      ggplot2::aes(label = label),
      nudge_y = 0.02,
      size = 3,
      show.legend = FALSE
    )
  } else if (length(x$outliers) > 0) {
    outlier_df <- df[x$outliers, ]
    p <- p + ggplot2::geom_text(
      data = outlier_df,
      ggplot2::aes(label = label),
      nudge_y = 0.02,
      size = 3,
      show.legend = FALSE
    )
  }

  p
}

#' Print Method for Outliergram Objects
#'
#' @param x An object of class 'outliergram'.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.outliergram <- function(x, ...) {
  cat("Outliergram\n")
  cat("===========\n")
  cat("Number of curves:", length(x$mei), "\n")
  cat("Outliers detected:", x$n_outliers, "\n")

  if (x$n_outliers > 0) {
    # Count by type
    type_counts <- table(x$outlier_type)
    cat("\nOutlier types:\n")
    if ("shape" %in% names(type_counts)) {
      cat("  Shape:          ", type_counts["shape"], "\n")
    }
    if ("magnitude_high" %in% names(type_counts)) {
      cat("  Magnitude (high):", type_counts["magnitude_high"], "\n")
    }
    if ("magnitude_low" %in% names(type_counts)) {
      cat("  Magnitude (low): ", type_counts["magnitude_low"], "\n")
    }
    if ("mixed" %in% names(type_counts)) {
      cat("  Mixed:          ", type_counts["mixed"], "\n")
    }

    cat("\nOutlier details:\n")
    for (i in seq_along(x$outliers)) {
      cat("  Index", x$outliers[i], ":", x$outlier_type[i], "\n")
    }
  }

  cat("\nParameters:\n")
  cat("  Factor:", x$factor, "\n")
  cat("  MEI threshold:", x$mei_threshold, "\n")
  invisible(x)
}
