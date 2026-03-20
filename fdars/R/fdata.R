# Null-coalescing operator (use left if not NULL, otherwise right)
# Internal helper function - not exported
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Create a functional data object
#'
#' Creates an fdata object for 1D functional data (curves) or 2D functional
#' data (surfaces). For 2D data, the internal storage uses a flattened matrix
#' format \code{[n, m1*m2]} where each row represents a surface stored in
#' row-major order.
#'
#' @param mdata Input data. Can be:
#'   \itemize{
#'     \item For 1D: A matrix \code{[n, m]} where n is number of curves, m is
#'       number of evaluation points
#'     \item For 2D: A 3D array \code{[n, m1, m2]} where n is number of surfaces,
#'       m1 x m2 is the grid size. Automatically detected and converted to
#'       flattened storage.
#'     \item For 2D: A matrix \code{[n, m1*m2]} (already flattened) with argvals
#'       specifying grid dimensions
#'     \item For 2D: A single surface matrix \code{[m1, m2]} with argvals
#'       specifying grid dimensions
#'   }
#' @param argvals Evaluation points. For 1D: a numeric vector.
#'   For 2D: a list with two numeric vectors specifying the s and t coordinates.
#' @param rangeval Range of the argument values. For 1D: a numeric vector of
#'   length 2. For 2D: a list with two numeric vectors of length 2.
#' @param names List with components 'main', 'xlab', 'ylab' for plot titles.
#'   For 2D, also 'zlab' for the surface value label.
#' @param fdata2d Logical. If TRUE, create 2D functional data (surface).
#'   Automatically set to TRUE if mdata is a 3D array.
#' @param id Optional character vector of identifiers for each observation.
#'   If NULL, uses row names of mdata or generates "obs_1", "obs_2", etc.
#' @param metadata Optional data.frame with additional covariates (one row per
#'   observation). If metadata has an "id" column or non-default row names,
#'   they must match the \code{id} parameter.
#'
#' @return An object of class 'fdata' containing:
#' \describe{
#'   \item{data}{The data matrix. For 2D: flattened \code{[n, m1*m2]} format}
#'   \item{argvals}{Evaluation points}
#'   \item{rangeval}{Range of arguments}
#'   \item{names}{Plot labels}
#'   \item{fdata2d}{Logical indicating if 2D}
#'   \item{dims}{For 2D only: \code{c(m1, m2)} grid dimensions}
#'   \item{id}{Character vector of observation identifiers}
#'   \item{metadata}{Data frame of additional covariates (or NULL)}
#' }
#'
#' @details
#' For 2D functional data, surfaces are stored internally as a flattened matrix
#' where each row is a surface in row-major order. To extract a single surface
#' as a matrix, use subsetting with \code{drop = TRUE} or reshape manually:
#' \preformatted{
#' # Extract surface i as matrix
#' surface_i <- fd[i, drop = TRUE]
#' # Or manually:
#' surface_i <- matrix(fd$data[i, ], nrow = fd$dims[1], ncol = fd$dims[2])
#' }
#'
#' @export
#' @examples
#' # Create 1D functional data (curves)
#' x <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' fd <- fdata(x, argvals = seq(0, 1, length.out = 10))
#'
#' # Create with identifiers and metadata
#' meta <- data.frame(group = rep(c("A", "B"), 5), endpoint = rnorm(10))
#' fd <- fdata(x, id = paste0("patient_", 1:10), metadata = meta)
#'
#' # Access metadata
#' fd$id
#' fd$metadata$group
#'
#' # Create 2D functional data from 3D array [n, m1, m2]
#' surfaces <- array(rnorm(500), dim = c(5, 10, 10))
#' fd2d <- fdata(surfaces)
#'
#' # Access individual surface as matrix
#' surface_1 <- fd2d[1, drop = TRUE]
fdata <- function(mdata, argvals = NULL, rangeval = NULL,
                  names = NULL, fdata2d = FALSE, id = NULL, metadata = NULL) {

  # Detect 2D data from input
  if (is.array(mdata) && length(dim(mdata)) == 3) {
    fdata2d <- TRUE
  }
  # Also detect 2D if argvals is a list with two components
  if (is.list(argvals) && length(argvals) == 2) {
    fdata2d <- TRUE
  }

  if (fdata2d) {
    return(.fdata2d(mdata, argvals, rangeval, names, id, metadata))
  } else {
    return(.fdata1d(mdata, argvals, rangeval, names, id, metadata))
  }
}

#' Internal: Create 1D functional data
#' @noRd
.fdata1d <- function(mdata, argvals = NULL, rangeval = NULL, names = NULL,
                     id = NULL, metadata = NULL) {

  # Convert vector to matrix
  if (is.vector(mdata)) {
    mdata <- matrix(mdata, nrow = 1)
  }

  if (!is.matrix(mdata)) {
    mdata <- as.matrix(mdata)
  }

  n <- nrow(mdata)
  m <- ncol(mdata)

  # Set default argvals
  if (is.null(argvals)) {
    argvals <- seq_len(m)
  }

  if (length(argvals) != m) {
    stop("Length of argvals must equal number of columns in mdata")
  }

  # Set default rangeval
  if (is.null(rangeval)) {
    rangeval <- range(argvals)
  }

  # Set default names
  if (is.null(names)) {
    names <- list(main = "", xlab = "t", ylab = "X(t)")
  }

  # Handle id parameter
  if (is.null(id)) {
    # Use row names if they exist and are not default
    if (!is.null(rownames(mdata)) &&
        !identical(rownames(mdata), as.character(seq_len(n)))) {
      id <- rownames(mdata)
    } else {
      id <- paste0("obs_", seq_len(n))
    }
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

    # Check ID matching if metadata has IDs
    meta_ids <- NULL
    if ("id" %in% colnames(metadata)) {
      meta_ids <- as.character(metadata$id)
    } else if (!is.null(rownames(metadata)) &&
               !identical(rownames(metadata), as.character(seq_len(nrow(metadata))))) {
      meta_ids <- rownames(metadata)
    }

    if (!is.null(meta_ids)) {
      if (!identical(id, meta_ids)) {
        stop("IDs in metadata do not match fdata identifiers")
      }
    }
  }

  structure(
    list(
      data = mdata,
      argvals = argvals,
      rangeval = rangeval,
      names = names,
      fdata2d = FALSE,
      id = id,
      metadata = metadata
    ),
    class = "fdata"
  )
}

#' Internal: Create 2D functional data (surfaces)
#'
#' Data is stored internally as a flattened matrix (n x m1*m2) where each row
#' is a surface stored in row-major order. Use dims to reshape for visualization.
#'
#' @noRd
.fdata2d <- function(mdata, argvals = NULL, rangeval = NULL, names = NULL,
                     id = NULL, metadata = NULL) {

  if (is.array(mdata) && length(dim(mdata)) == 3) {
    # Input is 3D array [n, m1, m2] - flatten to [n, m1*m2]
    dims <- dim(mdata)
    n <- dims[1]
    m1 <- dims[2]
    m2 <- dims[3]
    # Flatten each surface: row i becomes mdata[i,,] as vector
    data_mat <- matrix(0, nrow = n, ncol = m1 * m2)
    for (i in seq_len(n)) {
      data_mat[i, ] <- as.vector(mdata[i, , ])
    }
  } else if (is.matrix(mdata)) {
    # Matrix input: assume already flattened or single surface
    if (is.null(argvals)) {
      stop("argvals must be provided for 2D fdata from matrix input")
    }
    m1 <- length(argvals[[1]])
    m2 <- length(argvals[[2]])
    if (ncol(mdata) == m1 * m2) {
      # Already flattened format [n, m1*m2]
      n <- nrow(mdata)
      data_mat <- mdata
    } else if (nrow(mdata) == m1 && ncol(mdata) == m2) {
      # Single surface as matrix [m1, m2] - flatten to [1, m1*m2]
      n <- 1
      data_mat <- matrix(as.vector(mdata), nrow = 1)
    } else {
      stop("Matrix dimensions do not match argvals for 2D fdata")
    }
  } else {
    stop("For 2D fdata, mdata must be a 3D array or matrix")
  }

  # Set default argvals
  if (is.null(argvals)) {
    argvals <- list(
      s = seq_len(m1),
      t = seq_len(m2)
    )
  }

  if (!is.list(argvals) || length(argvals) != 2) {
    stop("argvals must be a list with two components for 2D fdata")
  }

  # Set default rangeval
  if (is.null(rangeval)) {
    rangeval <- list(
      s = range(argvals[[1]]),
      t = range(argvals[[2]])
    )
  }

  # Set default names
  if (is.null(names)) {
    names <- list(main = "", xlab = "s", ylab = "t", zlab = "X(s,t)")
  }

  # Handle id parameter
  if (is.null(id)) {
    # Use row names if they exist and are not default
    if (!is.null(rownames(data_mat)) &&
        !identical(rownames(data_mat), as.character(seq_len(n)))) {
      id <- rownames(data_mat)
    } else {
      id <- paste0("obs_", seq_len(n))
    }
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

    # Check ID matching if metadata has IDs
    meta_ids <- NULL
    if ("id" %in% colnames(metadata)) {
      meta_ids <- as.character(metadata$id)
    } else if (!is.null(rownames(metadata)) &&
               !identical(rownames(metadata), as.character(seq_len(nrow(metadata))))) {
      meta_ids <- rownames(metadata)
    }

    if (!is.null(meta_ids)) {
      if (!identical(id, meta_ids)) {
        stop("IDs in metadata do not match fdata identifiers")
      }
    }
  }

  structure(
    list(
      data = data_mat,
      argvals = argvals,
      rangeval = rangeval,
      names = names,
      fdata2d = TRUE,
      dims = c(m1, m2),
      id = id,
      metadata = metadata
    ),
    class = "fdata"
  )
}

#' Convert DataFrame to 2D functional data
#'
#' Converts a data frame in long format to a 2D fdata object (surfaces).
#' The expected format is: one identifier column, one column for the s-dimension
#' index, and multiple columns for the t-dimension values.
#'
#' @param df A data frame with the structure described below.
#' @param id_col Name or index of the identifier column (default: 1).
#' @param s_col Name or index of the s-dimension column (default: 2).
#' @param t_cols Names or indices of the t-dimension value columns. If NULL
#'   (default), uses all columns after \code{s_col}.
#' @param names Optional list with 'main', 'xlab', 'ylab', 'zlab' for labels.
#' @param metadata Optional data.frame with additional covariates (one row per
#'   surface). If metadata has an "id" column or non-default row names,
#'   they must match the surface identifiers from \code{id_col}.
#'
#' @return An object of class 'fdata' with 2D functional data.
#'
#' @details
#' The expected data frame structure is:
#' \itemize{
#'   \item Column 1 (id_col): Surface identifier (e.g., "surface_1", "surface_2")
#'   \item Column 2 (s_col): Index for the s-dimension (row index of surface)
#'   \item Columns 3+ (t_cols): Values for each t-dimension point (columns of surface)
#' }
#'
#' Each unique identifier represents one surface. For each surface, there should
#' be m1 rows (one per s-value), and m2 t-columns, resulting in an m1 x m2 surface.
#'
#' @export
#' @examples
#' # Create example data frame
#' df <- data.frame(
#'   id = rep(c("surf1", "surf2"), each = 5),
#'   s = rep(1:5, 2),
#'   t1 = rnorm(10),
#'   t2 = rnorm(10),
#'   t3 = rnorm(10)
#' )
#' fd <- df_to_fdata2d(df)
#' print(fd)
#'
#' # With metadata
#' meta <- data.frame(group = c("A", "B"), value = c(1.5, 2.3))
#' fd <- df_to_fdata2d(df, metadata = meta)
df_to_fdata2d <- function(df, id_col = 1, s_col = 2, t_cols = NULL, names = NULL,
                          metadata = NULL) {
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  # Convert column names to indices if needed
  if (is.character(id_col)) {
    id_col <- which(colnames(df) == id_col)
  }
  if (is.character(s_col)) {
    s_col <- which(colnames(df) == s_col)
  }

  # Determine t_cols if not specified
  if (is.null(t_cols)) {
    all_cols <- seq_len(ncol(df))
    t_cols <- setdiff(all_cols, c(id_col, s_col))
  } else if (is.character(t_cols)) {
    t_cols <- which(colnames(df) %in% t_cols)
  }

  # Extract components
  ids <- df[[id_col]]
  s_vals <- df[[s_col]]
  t_data <- as.matrix(df[, t_cols, drop = FALSE])

  # Get unique identifiers and s values

  unique_ids <- unique(ids)
  unique_s <- sort(unique(s_vals))
  n <- length(unique_ids)
  m1 <- length(unique_s)
  m2 <- length(t_cols)

  # Build flattened matrix [n, m1*m2]
  data_mat <- matrix(0, nrow = n, ncol = m1 * m2)

  for (i in seq_len(n)) {
    id <- unique_ids[i]
    rows <- which(ids == id)

    # Sort by s-value to ensure correct order
    row_order <- order(s_vals[rows])
    sorted_rows <- rows[row_order]

    # Each surface row corresponds to one s-value
    # t_data[sorted_rows, ] is [m1, m2] matrix
    surface_mat <- t_data[sorted_rows, , drop = FALSE]

    # Flatten to row-major order
    data_mat[i, ] <- as.vector(surface_mat)
  }

  # Set row names to identifiers
  id <- as.character(unique_ids)
  rownames(data_mat) <- id

  # Validate metadata if provided
  if (!is.null(metadata)) {
    if (!is.data.frame(metadata)) {
      stop("metadata must be a data.frame")
    }
    if (nrow(metadata) != n) {
      stop("metadata must have ", n, " rows (one per surface), got ", nrow(metadata))
    }

    # Check ID matching if metadata has IDs
    meta_ids <- NULL
    if ("id" %in% colnames(metadata)) {
      meta_ids <- as.character(metadata$id)
    } else if (!is.null(rownames(metadata)) &&
               !identical(rownames(metadata), as.character(seq_len(nrow(metadata))))) {
      meta_ids <- rownames(metadata)
    }

    if (!is.null(meta_ids)) {
      if (!identical(id, meta_ids)) {
        stop("IDs in metadata do not match surface identifiers from id_col")
      }
    }
  }

  # Get t values from column names if numeric, otherwise use indices
  t_vals <- suppressWarnings(as.numeric(colnames(df)[t_cols]))
  if (any(is.na(t_vals))) {
    t_vals <- seq_len(m2)
  }

  # Create argvals
  argvals <- list(s = unique_s, t = t_vals)

  # Create fdata object
  fdata(data_mat, argvals = argvals, names = names, id = id, metadata = metadata)
}

#' Center functional data
#'
#' Subtract the mean function from each curve.
#'
#' @param fdataobj An object of class 'fdata'.
#'
#' @return A centered 'fdata' object.
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#' fd_centered <- fdata.cen(fd)
fdata.cen <- function(fdataobj) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (fdataobj$fdata2d) {
    stop("fdata.cen not yet implemented for 2D functional data")
  }

  centered_data <- .Call("wrap__fdata_center_1d", fdataobj$data)

  fdataobj$data <- centered_data
  fdataobj
}

#' Arithmetic Operations for Functional Data
#'
#' Perform elementwise arithmetic on \code{fdata} objects.
#' Supports addition, subtraction, multiplication, division, and exponentiation
#' between two \code{fdata} objects or between an \code{fdata} object and a
#' numeric scalar.
#'
#' @param e1,e2 Objects of class \code{fdata} or numeric scalars.
#'   At least one must be of class \code{fdata}.
#'
#' @return An \code{fdata} object with the result.
#' @importFrom methods callGeneric
#' @export
#' @examples
#' fd1 <- fdata(matrix(1:20, 4, 5))
#' fd2 <- fdata(matrix(21:40, 4, 5))
#' fd1 + fd2
#' fd1 * 2
#' 3 - fd1
Ops.fdata <- function(e1, e2) {
  op <- .Generic

  if (inherits(e1, "fdata") && inherits(e2, "fdata")) {
    if (!identical(e1$argvals, e2$argvals)) {
      stop("argvals of both fdata objects must be identical")
    }
    if (!identical(dim(e1$data), dim(e2$data))) {
      stop("data dimensions must match")
    }
    result <- e1
    result$data <- callGeneric(e1$data, e2$data)
  } else if (inherits(e1, "fdata")) {
    result <- e1
    result$data <- callGeneric(e1$data, e2)
  } else {
    result <- e2
    result$data <- callGeneric(e1, e2$data)
  }

  result
}

#' Compute functional mean
#'
#' Computes the pointwise mean function across all observations.
#' This is an S3 method for the generic \code{mean} function.
#'
#' @param x An object of class 'fdata'.
#' @param ... Additional arguments (currently ignored).
#'
#' @return For 1D fdata: a numeric vector containing the mean function values.
#'   For 2D fdata: an fdata object containing the mean surface.
#' @export
#' @examples
#' # 1D functional data
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#' fm <- mean(fd)
#'
#' # 2D functional data
#' X <- array(rnorm(500), dim = c(5, 10, 10))
#' fd2d <- fdata(X, argvals = list(1:10, 1:10), fdata2d = TRUE)
#' fm2d <- mean(fd2d)
mean.fdata <- function(x, ...) {
  if (!inherits(x, "fdata")) {
    stop("x must be of class 'fdata'")
  }

  if (isTRUE(x$fdata2d)) {
    # 2D case - data is already flattened [n, m1*m2]
    mean_vals <- .Call("wrap__fdata_mean_2d", x$data)
    # Return as fdata2d object with flattened matrix [1, m1*m2]
    result <- list(
      data = matrix(mean_vals, nrow = 1),
      argvals = x$argvals,
      rangeval = x$rangeval,
      names = list(
        main = "Mean surface",
        xlab = x$names$xlab,
        ylab = x$names$ylab,
        zlab = x$names$zlab
      ),
      fdata2d = TRUE,
      dims = x$dims
    )
    class(result) <- "fdata"
    return(result)
  }

  # 1D case
  mean_vals <- .Call("wrap__fdata_mean_1d", x$data)
  result <- list(
    data = matrix(mean_vals, nrow = 1),
    argvals = x$argvals,
    rangeval = x$rangeval,
    names = list(
      main = "Mean curve",
      xlab = x$names$xlab,
      ylab = x$names$ylab
    ),
    fdata2d = FALSE
  )
  class(result) <- "fdata"
  result
}

#' Compute Lp Norm of Functional Data
#'
#' Generic function to compute Lp norms for functional data objects.
#' Works with both regular \code{fdata} and irregular \code{irregFdata} objects.
#'
#' @param x A functional data object (\code{fdata} or \code{irregFdata}).
#' @param p The order of the norm (default 2 for L2 norm).
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric vector of norms, one per curve.
#' @export
#' @examples
#' # Regular fdata
#' fd <- fdata(matrix(rnorm(100), 10, 10))
#' norms <- norm(fd)
#'
#' # Irregular fdata
#' ifd <- sparsify(fd, minObs = 3, maxObs = 7, seed = 42)
#' norms_irreg <- norm(ifd)
norm <- function(x, p = 2, ...) {

  UseMethod("norm")
}

#' @rdname norm
#' @export
norm.fdata <- function(x, p = 2, ...) {
  if (x$fdata2d) {
    stop("norm not yet implemented for 2D functional data")
  }

  .Call("wrap__fdata_norm_lp_1d", x$data, as.numeric(x$argvals), as.numeric(p))
}

#' Normalize functional data
#'
#' Scales each curve to have Lp norm equal to 1.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param p The order of the norm (default 2 for L2 norm).
#'
#' @return A normalized 'fdata' object where each curve has unit norm.
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(100), 10, 10), argvals = seq(0, 1, length.out = 10))
#' fd_norm <- normalize(fd)
#' norm(fd_norm)  # All values should be 1
normalize <- function(fdataobj, p = 2) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (fdataobj$fdata2d) {
    stop("normalize not yet implemented for 2D functional data")
  }

  # Compute norms
  norms <- norm(fdataobj, p = p)

  # Handle zero norms (avoid division by zero)
  norms[norms == 0] <- 1

  # Divide each row by its norm
  fdataobj$data <- fdataobj$data / norms

  fdataobj
}

#' Standardize functional data (z-score normalization)
#'
#' Transforms each curve to have mean 0 and standard deviation 1.
#' This is useful for comparing curve shapes regardless of their level or scale.
#'
#' @param fdataobj An object of class 'fdata'.
#'
#' @return A standardized 'fdata' object where each curve has mean 0 and sd 1.
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(100) * 10 + 50, 10, 10), argvals = seq(0, 1, length.out = 10))
#' fd_std <- standardize(fd)
#' # Check: each curve now has mean ~0 and sd ~1
#' rowMeans(fd_std$data)
#' apply(fd_std$data, 1, sd)
standardize <- function(fdataobj) {
  UseMethod("standardize")
}

#' @rdname standardize
#' @method standardize fdata
#' @export
standardize.fdata <- function(fdataobj) {
  if (fdataobj$fdata2d) {
    stop("standardize not yet implemented for 2D functional data")
  }

  # Compute mean and sd for each curve
  means <- rowMeans(fdataobj$data, na.rm = TRUE)
  sds <- apply(fdataobj$data, 1, sd, na.rm = TRUE)

  # Handle zero sd (constant curves)
  sds[sds == 0] <- 1

  # Standardize: (x - mean) / sd
  fdataobj$data <- (fdataobj$data - means) / sds

  fdataobj
}

#' Min-Max scaling for functional data
#'
#' Scales each curve to the range \eqn{[0, 1]} (or custom range).
#' This preserves the shape while normalizing the range.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param min Target minimum value (default 0).
#' @param max Target maximum value (default 1).
#'
#' @return A scaled 'fdata' object where each curve is in the specified range.
#' @export
#' @examples
#' fd <- fdata(matrix(rnorm(100) * 10 + 50, 10, 10), argvals = seq(0, 1, length.out = 10))
#' fd_scaled <- scale_minmax(fd)
#' # Check: each curve now in [0, 1]
#' apply(fd_scaled$data, 1, range)
scale_minmax <- function(fdataobj, min = 0, max = 1) {
  UseMethod("scale_minmax")
}

#' @rdname scale_minmax
#' @method scale_minmax fdata
#' @export
scale_minmax.fdata <- function(fdataobj, min = 0, max = 1) {
  if (fdataobj$fdata2d) {
    stop("scale_minmax not yet implemented for 2D functional data")
  }

  # Compute min and max for each curve
  row_mins <- apply(fdataobj$data, 1, min, na.rm = TRUE)
  row_maxs <- apply(fdataobj$data, 1, max, na.rm = TRUE)
  row_range <- row_maxs - row_mins

  # Handle zero range (constant curves)
  row_range[row_range == 0] <- 1

  # Scale to [0, 1] first, then to [min, max]
  fdataobj$data <- (fdataobj$data - row_mins) / row_range
  fdataobj$data <- fdataobj$data * (max - min) + min

  fdataobj
}

#' Print method for fdata objects
#'
#' @param x An object of class 'fdata'.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.fdata <- function(x, ...) {
  cat("Functional data object\n")
  cat("  Type:", if (isTRUE(x$fdata2d)) "2D (surface)" else "1D (curve)", "\n")
  cat("  Number of observations:", nrow(x$data), "\n")

  if (isTRUE(x$fdata2d)) {
    cat("  Grid dimensions:", x$dims[1], "x", x$dims[2], "\n")
    cat("  Range s:", x$rangeval$s[1], "-", x$rangeval$s[2], "\n")
    cat("  Range t:", x$rangeval$t[1], "-", x$rangeval$t[2], "\n")
  } else {
    cat("  Number of points:", ncol(x$data), "\n")
    cat("  Range:", x$rangeval[1], "-", x$rangeval[2], "\n")
  }

  # Show metadata info if present
  if (!is.null(x$metadata)) {
    cat("  Metadata columns:", paste(colnames(x$metadata), collapse = ", "), "\n")
  }

  invisible(x)
}

#' Summary method for fdata objects
#'
#' @param object An object of class 'fdata'.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns a summary list with descriptive statistics.
#' @export
summary.fdata <- function(object, ...) {
  cat("Functional data summary\n")
  cat("=======================\n")
  cat("Type:", if (isTRUE(object$fdata2d)) "2D (surface)" else "1D (curve)", "\n")
  cat("Number of observations:", nrow(object$data), "\n")

  if (isTRUE(object$fdata2d)) {
    cat("Grid dimensions:", object$dims[1], "x", object$dims[2], "\n")
    cat("Total evaluation points:", prod(object$dims), "\n")
  } else {
    cat("Number of evaluation points:", ncol(object$data), "\n")
  }

  cat("\nData range:\n")
  cat("  Min:", min(object$data), "\n")
  cat("  Max:", max(object$data), "\n")
  cat("  Mean:", base::mean(object$data), "\n")
  cat("  SD:", stats::sd(object$data), "\n")

  # Show metadata summary if present
  if (!is.null(object$metadata)) {
    cat("\nMetadata:\n")
    cat("  Columns:", paste(colnames(object$metadata), collapse = ", "), "\n")
    for (col in colnames(object$metadata)) {
      val <- object$metadata[[col]]
      if (is.numeric(val)) {
        cat("  ", col, ": numeric, range [", min(val), ", ", max(val), "]\n", sep = "")
      } else if (is.factor(val)) {
        lvls <- levels(val)
        cat("  ", col, ": factor with ", length(lvls), " levels (",
            paste(utils::head(lvls, 3), collapse = ", "),
            if (length(lvls) > 3) ", ..." else "", ")\n", sep = "")
      } else {
        uniq <- unique(val)
        cat("  ", col, ": ", class(val)[1], " with ", length(uniq), " unique values\n", sep = "")
      }
    }
  }

  invisible(object)
}

#' Create a ggplot for fdata objects
#'
#' For 1D functional data, plots curves as lines with optional coloring by
#' external variables. For 2D functional data, plots surfaces as heatmaps
#' with contour lines.
#'
#' Use \code{autoplot()} to get the ggplot object without displaying it.
#' Use \code{plot()} to display the plot (returns invisibly).
#'
#' @param object An object of class 'fdata'.
#' @param color Optional vector for coloring curves. Can be:
#'   \itemize{
#'     \item Numeric vector: curves colored by continuous scale (viridis)
#'     \item Factor/character: curves colored by discrete groups
#'   }
#'   Must have length equal to number of curves.
#' @param alpha Transparency of individual curve lines. Default is 0.7 for
#'   basic plots, but automatically reduced to 0.3 when \code{show.mean = TRUE}
#'   or \code{show.ci = TRUE} to reduce visual clutter and allow mean curves
#'   to stand out. Can be explicitly set to override the default.
#' @param show.mean Logical. If TRUE and color is categorical, overlay group
#'   mean curves with thicker lines (default FALSE).
#' @param show.ci Logical. If TRUE and color is categorical, show pointwise
#'   confidence interval ribbons per group (default FALSE).
#' @param ci.level Confidence level for CI ribbons (default 0.90 for 90 percent).
#' @param palette Optional named vector of colors for categorical coloring,
#'   e.g., c("A" = "blue", "B" = "red").
#' @param ... Additional arguments (currently ignored).
#'
#' @return A ggplot object.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal geom_tile geom_contour scale_fill_viridis_c scale_color_viridis_c facet_wrap geom_ribbon scale_color_manual scale_fill_manual geom_text coord_equal autoplot
#' @examples
#' library(ggplot2)
#' # Get ggplot object without displaying
#' fd <- fdata(matrix(rnorm(200), 20, 10))
#' p <- autoplot(fd)
#'
#' # Customize the plot
#' p + theme_minimal()
#'
#' # Color by numeric variable
#' y <- rnorm(20)
#' autoplot(fd, color = y)
#'
#' # Color by category with mean and CI
#' groups <- factor(rep(c("A", "B"), each = 10))
#' autoplot(fd, color = groups, show.mean = TRUE, show.ci = TRUE)
autoplot.fdata <- function(object, color = NULL, alpha = NULL, show.mean = FALSE,
                           show.ci = FALSE, ci.level = 0.90, palette = NULL, ...) {
  x <- object
  # Set default alpha based on whether means are shown

  # Lower alpha when showing means to reduce visual clutter
  if (is.null(alpha)) {
    alpha <- if (show.mean || show.ci) 0.3 else 0.7
  }
  if (isTRUE(x$fdata2d)) {
    # 2D surface plotting (color parameters not supported for 2D)
    n <- nrow(x$data)  # Number of surfaces
    m1 <- x$dims[1]
    m2 <- x$dims[2]
    s <- x$argvals[[1]]
    t <- x$argvals[[2]]

    # Compute tile dimensions to avoid white lines between tiles
    tile_width <- if (length(s) > 1) mean(diff(s)) else 1
    tile_height <- if (length(t) > 1) mean(diff(t)) else 1

    # Create grid for plotting
    grid <- expand.grid(s = s, t = t)

    # Build long-format data frame from flattened matrix
    df_list <- lapply(seq_len(n), function(i) {
      # x$data[i, ] is flattened surface, reshape for plotting
      data.frame(
        surface_id = i,
        s = grid$s,
        t = grid$t,
        value = x$data[i, ]
      )
    })
    df <- do.call(rbind, df_list)

    # Plot with facets if multiple surfaces
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$s, y = .data$t)) +
      ggplot2::geom_tile(ggplot2::aes(fill = .data$value),
                         width = tile_width, height = tile_height) +
      ggplot2::geom_contour(ggplot2::aes(z = .data$value), color = "black", alpha = 0.5) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(
        x = x$names$xlab %||% "s",
        y = x$names$ylab %||% "t",
        fill = x$names$zlab %||% "value",
        title = x$names$main
      )

    if (n > 1) {
      p <- p + ggplot2::facet_wrap(~ surface_id)
    }

    return(p)
  }

  # 1D curve plotting
  n <- nrow(x$data)
  m <- ncol(x$data)

  # Validate color parameter
 if (!is.null(color)) {
    if (length(color) != n) {
      stop("length(color) must equal the number of curves (", n, ")")
    }
  }

  # Reshape to long format
  df <- data.frame(
    curve_id = rep(seq_len(n), each = m),
    argval = rep(x$argvals, n),
    value = as.vector(t(x$data))
  )

  # Determine coloring type
  is_categorical <- !is.null(color) && (is.factor(color) || is.character(color))
  is_numeric <- !is.null(color) && is.numeric(color)

  if (is_categorical) {
    # Categorical coloring
    df$group <- factor(rep(color, each = m))

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$argval, y = .data$value,
                                           group = .data$curve_id,
                                           color = .data$group)) +
      ggplot2::geom_line(alpha = alpha)

    # Add confidence interval ribbons if requested
    if (show.ci) {
      ci_df <- .compute_group_ci(df, ci.level)
      p <- p + ggplot2::geom_ribbon(
        data = ci_df,
        ggplot2::aes(x = .data$argval, ymin = .data$lower, ymax = .data$upper,
                     fill = .data$group, group = .data$group),
        alpha = 0.2, inherit.aes = FALSE
      )
    }

    # Add group means if requested
    if (show.mean) {
      mean_df <- .compute_group_mean(df)
      p <- p + ggplot2::geom_line(
        data = mean_df,
        ggplot2::aes(x = .data$argval, y = .data$mean_val, color = .data$group,
                     group = .data$group),
        linewidth = 1.2, inherit.aes = FALSE
      )
    }

    # Apply custom palette if provided
    if (!is.null(palette)) {
      p <- p + ggplot2::scale_color_manual(values = palette)
      if (show.ci) {
        p <- p + ggplot2::scale_fill_manual(values = palette)
      }
    }

  } else if (is_numeric) {
    # Numeric coloring with continuous scale
    df$color_var <- rep(color, each = m)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$argval, y = .data$value,
                                           group = .data$curve_id,
                                           color = .data$color_var)) +
      ggplot2::geom_line(alpha = alpha) +
      ggplot2::scale_color_viridis_c()

  } else {
    # No coloring (default behavior)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$argval, y = .data$value,
                                           group = .data$curve_id)) +
      ggplot2::geom_line(alpha = alpha)
  }

  p <- p +
    ggplot2::labs(
      x = x$names$xlab %||% "t",
      y = x$names$ylab %||% "X(t)",
      title = x$names$main
    )

  p
}

#' Plot method for fdata objects
#'
#' Displays a plot of functional data. For 1D functional data, plots curves
#' as lines with optional coloring. For 2D functional data, plots surfaces
#' as heatmaps with contour lines.
#'
#' This function displays the plot immediately. To get the ggplot object
#' without displaying (e.g., for customization), use \code{\link{autoplot.fdata}}.
#'
#' @inheritParams autoplot.fdata
#' @param x An object of class 'fdata'.
#'
#' @return The ggplot object (invisibly).
#'
#' @export
#' @examples
#' library(ggplot2)
#' # Display plot immediately
#' fd <- fdata(matrix(rnorm(200), 20, 10))
#' plot(fd)
#'
#' # To get ggplot object without displaying, use autoplot:
#' p <- autoplot(fd)
plot.fdata <- function(x, color = NULL, alpha = NULL, show.mean = FALSE,
                       show.ci = FALSE, ci.level = 0.90, palette = NULL, ...) {
  p <- autoplot.fdata(x, color = color, alpha = alpha, show.mean = show.mean,
                      show.ci = show.ci, ci.level = ci.level, palette = palette, ...)
  # Return the ggplot - it will auto-print at top level
  p
}

# Helper function to compute pointwise group means
.compute_group_mean <- function(df) {
  groups <- unique(df$group)
  argvals <- unique(df$argval)

  result_list <- lapply(groups, function(g) {
    group_data <- df[df$group == g, ]
    means <- tapply(group_data$value, group_data$argval, mean, na.rm = TRUE)
    data.frame(
      group = g,
      argval = as.numeric(names(means)),
      mean_val = as.numeric(means)
    )
  })
  do.call(rbind, result_list)
}

# Helper function to compute pointwise group confidence intervals
.compute_group_ci <- function(df, ci.level) {
  groups <- unique(df$group)
  argvals <- unique(df$argval)

  result_list <- lapply(groups, function(g) {
    group_data <- df[df$group == g, ]

    # Compute stats for each argval
    stats <- tapply(seq_len(nrow(group_data)), group_data$argval, function(idx) {
      vals <- group_data$value[idx]
      n_obs <- sum(!is.na(vals))
      if (n_obs < 2) {
        return(c(mean = mean(vals, na.rm = TRUE), lower = NA, upper = NA))
      }
      m <- mean(vals, na.rm = TRUE)
      se <- sd(vals, na.rm = TRUE) / sqrt(n_obs)
      t_crit <- qt((1 + ci.level) / 2, n_obs - 1)
      c(mean = m, lower = m - t_crit * se, upper = m + t_crit * se)
    })

    data.frame(
      group = g,
      argval = as.numeric(names(stats)),
      mean_val = sapply(stats, function(s) s["mean"]),
      lower = sapply(stats, function(s) s["lower"]),
      upper = sapply(stats, function(s) s["upper"])
    )
  })
  do.call(rbind, result_list)
}

#' Functional Boxplot
#'
#' Creates a functional boxplot for visualizing the distribution of functional
#' data. The boxplot shows the median curve, central 50 percent envelope, fence
#' (equivalent to whiskers), and outliers.
#'
#' @param x An object of class 'fdata'.
#' @importFrom graphics boxplot
#' @param prob Proportion of curves for the central region (default 0.5 for 50 percent).
#' @param factor Factor for fence calculation (default 1.5, as in standard boxplots).
#' @param depth.func Depth function to use. Default is depth.MBD.
#' @param show.outliers Logical. If TRUE (default), show outlier curves.
#' @param col.median Color for median curve (default "black").
#' @param col.envelope Color for central envelope (default "magenta").
#' @param col.fence Color for fence region (default "pink").
#' @param col.outliers Color for outlier curves (default "red").
#' @param ... Additional arguments passed to depth function.
#'
#' @return A list of class 'fbplot' with components:
#' \describe{
#'   \item{median}{Index of the median curve}
#'   \item{central}{Indices of curves in the central region}
#'   \item{outliers}{Indices of outlier curves}
#'   \item{depth}{Depth values for all curves}
#'   \item{plot}{The ggplot object}
#' }
#'
#' @details
#' The functional boxplot (Sun & Genton, 2011) generalizes the standard boxplot
#' to functional data using depth ordering:
#'
#' \itemize{
#'   \item \strong{Median}: The curve with maximum depth
#'   \item \strong{Central region}: Envelope of curves with top 50 percent depth
#'   \item \strong{Fence}: 1.5 times the envelope width beyond the central region
#'   \item \strong{Outliers}: Curves that exceed the fence at any point
#' }
#'
#' @references
#' Sun, Y. and Genton, M.G. (2011). Functional boxplots.
#' \emph{Journal of Computational and Graphical Statistics}, 20(2), 316-334.
#'
#' @seealso \code{\link{depth.MBD}} for the default depth function,
#'   \code{\link{outliers.boxplot}} for outlier detection using functional boxplots
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
#' # Create functional boxplot
#' fbp <- boxplot(fd)
boxplot.fdata <- function(x, prob = 0.5, factor = 1.5,
                          depth.func = depth.MBD,
                          show.outliers = TRUE,
                          col.median = "black",
                          col.envelope = "magenta",
                          col.fence = "pink",
                          col.outliers = "red", ...) {
  if (!inherits(x, "fdata")) {
    stop("x must be of class 'fdata'")
  }

  if (isTRUE(x$fdata2d)) {
    stop("boxplot.fdata not yet implemented for 2D functional data")
  }

  n <- nrow(x$data)
  m <- ncol(x$data)
  argvals <- x$argvals

  # Compute depths
  depths <- depth.func(x, x, ...)

  # Order curves by depth
  depth_order <- order(depths, decreasing = TRUE)

  # Median: curve with maximum depth
  median_idx <- depth_order[1]

  # Central region: top prob proportion of curves
  n_central <- max(1, ceiling(n * prob))
  central_idx <- depth_order[seq_len(n_central)]

  # Compute central envelope (pointwise min/max of central curves)
  central_data <- x$data[central_idx, , drop = FALSE]
  env_min <- apply(central_data, 2, min)
  env_max <- apply(central_data, 2, max)

  # Compute fence: envelope expanded by factor * envelope width
  env_width <- env_max - env_min
  fence_min <- env_min - factor * env_width
  fence_max <- env_max + factor * env_width

  # Identify outliers: curves that exceed the fence at any point
  outlier_idx <- integer(0)
  for (i in seq_len(n)) {
    curve <- x$data[i, ]
    if (any(curve < fence_min) || any(curve > fence_max)) {
      outlier_idx <- c(outlier_idx, i)
    }
  }

  # Non-outlier curves
  normal_idx <- setdiff(seq_len(n), outlier_idx)

  # Create ggplot visualization
  # Build data frames for plotting

  # Fence region (ribbon)
  df_fence <- data.frame(
    argval = argvals,
    ymin = fence_min,
    ymax = fence_max
  )

  # Central envelope
  df_envelope <- data.frame(
    argval = argvals,
    ymin = env_min,
    ymax = env_max
  )

  # Median curve
  df_median <- data.frame(
    argval = argvals,
    value = x$data[median_idx, ]
  )

  # Start building plot
  p <- ggplot2::ggplot()

  # Add fence region
  p <- p + ggplot2::geom_ribbon(
    data = df_fence,
    ggplot2::aes(x = .data$argval, ymin = .data$ymin, ymax = .data$ymax),
    fill = col.fence, alpha = 0.5
  )

  # Add central envelope
  p <- p + ggplot2::geom_ribbon(
    data = df_envelope,
    ggplot2::aes(x = .data$argval, ymin = .data$ymin, ymax = .data$ymax),
    fill = col.envelope, alpha = 0.5
  )

  # Add outlier curves if requested
  if (show.outliers && length(outlier_idx) > 0) {
    df_outliers <- data.frame(
      curve_id = rep(outlier_idx, each = m),
      argval = rep(argvals, length(outlier_idx)),
      value = as.vector(t(x$data[outlier_idx, , drop = FALSE]))
    )
    p <- p + ggplot2::geom_line(
      data = df_outliers,
      ggplot2::aes(x = .data$argval, y = .data$value, group = .data$curve_id),
      color = col.outliers, alpha = 0.7
    )
  }

  # Add median curve
  p <- p + ggplot2::geom_line(
    data = df_median,
    ggplot2::aes(x = .data$argval, y = .data$value),
    color = col.median, linewidth = 1.2
  )

  # Add labels and theme
  p <- p + ggplot2::labs(
    x = x$names$xlab %||% "t",
    y = x$names$ylab %||% "X(t)",
    title = "Functional Boxplot"
  )

  # Return result invisibly
  result <- structure(
    list(
      median = median_idx,
      central = central_idx,
      outliers = outlier_idx,
      depth = depths,
      envelope = list(min = env_min, max = env_max),
      fence = list(min = fence_min, max = fence_max),
      fdataobj = x,
      plot = p
    ),
    class = "fbplot"
  )

  invisible(result)
}

#' Print Method for fbplot Objects
#'
#' @param x An object of class 'fbplot'.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.fbplot <- function(x, ...) {
  cat("Functional Boxplot\n")
  cat("==================\n")
  cat("Number of curves:", nrow(x$fdataobj$data), "\n")
  cat("Median curve index:", x$median, "\n")
  cat("Central region curves:", length(x$central), "\n")
  cat("Outliers detected:", length(x$outliers), "\n")
  if (length(x$outliers) > 0) {
    cat("Outlier indices:", paste(x$outliers, collapse = ", "), "\n")
  }
  invisible(x)
}

#' Curve Registration (Alignment)
#'
#' Aligns functional data by horizontal shifting to a target curve.
#' This reduces phase variation in the sample.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param target Target curve to align to. If NULL (default), uses the mean.
#' @param max.shift Maximum allowed shift as proportion of domain (default 0.2).
#'
#' @return A list of class 'register.fd' with components:
#' \describe{
#'   \item{registered}{An fdata object with registered (aligned) curves.}
#'   \item{shifts}{Numeric vector of shift amounts for each curve.}
#'   \item{target}{The target curve used for alignment.}
#'   \item{fdataobj}{Original (unregistered) functional data.}
#' }
#'
#' @details
#' Shift registration finds the horizontal translation that maximizes the
#' cross-correlation between each curve and the target. This is appropriate
#' when curves have similar shapes but differ mainly in timing.
#'
#' For more complex warping, consider DTW-based methods.
#'
#' @seealso \code{\link{metric.DTW}} for dynamic time warping distance
#'
#' @export
#' @examples
#' # Create phase-shifted curves
#' set.seed(42)
#' t <- seq(0, 1, length.out = 100)
#' X <- matrix(0, 20, 100)
#' for (i in 1:20) {
#'   phase <- runif(1, -0.1, 0.1)
#'   X[i, ] <- sin(2*pi*(t + phase)) + rnorm(100, sd = 0.1)
#' }
#' fd <- fdata(X, argvals = t)
#'
#' # Register curves
#' reg <- register.fd(fd)
#' print(reg)
#'
#' # Compare original vs registered
#' oldpar <- par(mfrow = c(1, 2))
#' plot(fd)
#' plot(reg$registered)
#' par(oldpar)
register.fd <- function(fdataobj, target = NULL, max.shift = 0.2) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("register.fd not yet implemented for 2D functional data")
  }

  n <- nrow(fdataobj$data)
  m <- ncol(fdataobj$data)
  argvals <- fdataobj$argvals

  # Determine target curve
  if (is.null(target)) {
    target <- colMeans(fdataobj$data)
  } else if (inherits(target, "fdata")) {
    if (ncol(target$data) != m) {
      stop("target must have same number of evaluation points as fdataobj")
    }
    target <- as.vector(target$data[1, ])
  } else if (!is.numeric(target) || length(target) != m) {
    stop("target must be NULL, an fdata object, or a numeric vector of length m")
  }

  # Compute max shift in domain units
  domain_range <- max(argvals) - min(argvals)
  max_shift_val <- max.shift * domain_range

  # Call Rust function
  result <- .Call("wrap__register_shift_1d", fdataobj$data,
                  as.numeric(target), as.numeric(argvals), as.numeric(max_shift_val))

  # Create registered fdata object
  registered <- fdata(result$registered, argvals = argvals,
                      names = list(main = "Registered Curves",
                                   xlab = fdataobj$names$xlab,
                                   ylab = fdataobj$names$ylab))

  structure(
    list(
      registered = registered,
      shifts = result$shifts,
      target = target,
      fdataobj = fdataobj
    ),
    class = "register.fd"
  )
}

#' Print Method for register.fd Objects
#'
#' @param x An object of class 'register.fd'.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.register.fd <- function(x, ...) {
  cat("Curve Registration\n")
  cat("==================\n")
  cat("Number of curves:", nrow(x$fdataobj$data), "\n")
  cat("Shift statistics:\n")
  cat("  Min:", round(min(x$shifts), 4), "\n")
  cat("  Max:", round(max(x$shifts), 4), "\n")
  cat("  Mean:", round(mean(x$shifts), 4), "\n")
  cat("  SD:", round(sd(x$shifts), 4), "\n")
  invisible(x)
}

#' Plot Method for register.fd Objects
#'
#' @param x An object of class 'register.fd'.
#' @param type Type of plot: "registered" (default), "original", or "both".
#' @param ... Additional arguments (currently ignored).
#'
#' @return A ggplot object.
#'
#' @export
plot.register.fd <- function(x, type = c("registered", "original", "both"), ...) {
  type <- match.arg(type)

  if (type == "original") {
    plot(x$fdataobj)
  } else if (type == "registered") {
    plot(x$registered)
  } else {
    # Side-by-side comparison
    fd_orig <- x$fdataobj
    fd_reg <- x$registered
    n <- nrow(fd_orig$data)
    m <- ncol(fd_orig$data)

    df <- data.frame(
      curve_id = rep(rep(seq_len(n), each = m), 2),
      argval = rep(fd_orig$argvals, n * 2),
      value = c(as.vector(t(fd_orig$data)), as.vector(t(fd_reg$data))),
      type = factor(rep(c("Original", "Registered"), each = n * m),
                    levels = c("Original", "Registered"))
    )

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$argval, y = .data$value,
                                           group = .data$curve_id)) +
      ggplot2::geom_line(alpha = 0.5) +
      ggplot2::facet_wrap(~ .data$type) +
      ggplot2::labs(
        x = fd_orig$names$xlab %||% "t",
        y = fd_orig$names$ylab %||% "X(t)",
        title = "Curve Registration: Before vs After"
      )

    p
  }
}

#' Local Averages Feature Extraction
#'
#' Extracts features from functional data by computing local averages over
#' specified intervals. This is a simple but effective dimension reduction
#' technique for functional data.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param n.intervals Number of equal-width intervals (default 10).
#' @param intervals Optional matrix of custom intervals (2 columns: start, end).
#'   If provided, \code{n.intervals} is ignored.
#'
#' @return A matrix with n rows (curves) and one column per interval, containing
#'   the local average for each curve in each interval.
#'
#' @details
#' Local averages provide a simple way to convert functional data to
#' multivariate data while preserving local structure. Each curve is
#' summarized by its average value over each interval.
#'
#' This can be useful as a preprocessing step for classification or
#' clustering methods that require fixed-dimensional input.
#'
#' @export
#' @examples
#' # Create functional data
#' t <- seq(0, 1, length.out = 100)
#' X <- matrix(0, 20, 100)
#' for (i in 1:20) X[i, ] <- sin(2*pi*t) + rnorm(100, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#'
#' # Extract 5 local average features
#' features <- localavg.fdata(fd, n.intervals = 5)
#' dim(features)  # 20 x 5
#'
#' # Use custom intervals
#' intervals <- cbind(c(0, 0.25, 0.5), c(0.25, 0.5, 1))
#' features2 <- localavg.fdata(fd, intervals = intervals)
localavg.fdata <- function(fdataobj, n.intervals = 10, intervals = NULL) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("localavg.fdata not yet implemented for 2D functional data")
  }

  n <- nrow(fdataobj$data)
  m <- ncol(fdataobj$data)
  argvals <- fdataobj$argvals
  range_val <- range(argvals)

  # Create intervals if not provided
  if (is.null(intervals)) {
    breaks <- seq(range_val[1], range_val[2], length.out = n.intervals + 1)
    intervals <- cbind(breaks[-length(breaks)], breaks[-1])
  } else {
    intervals <- as.matrix(intervals)
    if (ncol(intervals) != 2) {
      stop("intervals must be a matrix with 2 columns (start, end)")
    }
  }

  n_int <- nrow(intervals)

  # Compute local averages for each curve and interval
  features <- matrix(0, n, n_int)

  for (k in seq_len(n_int)) {
    int_start <- intervals[k, 1]
    int_end <- intervals[k, 2]

    # Find indices within this interval
    idx <- which(argvals >= int_start & argvals <= int_end)

    if (length(idx) > 0) {
      # Compute mean within interval for each curve
      features[, k] <- rowMeans(fdataobj$data[, idx, drop = FALSE])
    }
  }

  # Add column names
  colnames(features) <- paste0("int_", seq_len(n_int))

  features
}

#' Compute functional derivative
#'
#' Compute the numerical derivative of functional data. Uses finite differences
#' for fast computation via Rust.
#'
#' For 1D functional data (curves), computes the nth derivative.
#' For 2D functional data (surfaces), computes partial derivatives:
#' \itemize{
#'   \item \code{ds}: partial derivative with respect to s (first argument)
#'   \item \code{dt}: partial derivative with respect to t (second argument)
#'   \item \code{dsdt}: mixed partial derivative
#' }
#'
#' @param fdataobj An object of class 'fdata'.
#' @param nderiv Derivative order (1, 2, ...). Default is 1. For 2D data,
#'   only first-order derivatives are currently supported.
#' @param method Method for computing derivatives. Currently only "diff"
#'   (finite differences) is supported.
#' @param class.out Output class, either "fdata" or "fd". Default is "fdata".
#' @param nbasis Not used (for compatibility with fda.usc).
#' @param ... Additional arguments (ignored).
#'
#' @return For 1D data: an 'fdata' object containing the derivative values.
#'   For 2D data: a list with components \code{ds}, \code{dt}, and \code{dsdt},
#'   each an 'fdata' object containing the respective partial derivative.
#'
#' @export
#' @examples
#' # Create smooth curves
#' t <- seq(0, 2*pi, length.out = 100)
#' X <- matrix(0, 10, 100)
#' for (i in 1:10) X[i, ] <- sin(t + i/5)
#' fd <- fdata(X, argvals = t)
#'
#' # First derivative (should be approximately cos)
#' fd_deriv <- deriv(fd, nderiv = 1)
deriv <- function(fdataobj, nderiv = 1, method = "diff",
                        class.out = "fdata", nbasis = NULL, ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (nderiv < 1) {
    return(fdataobj)
  }

  if (method != "diff") {
    warning("Only method='diff' is currently supported, using finite differences")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    # 2D functional data: compute partial derivatives
    if (nderiv > 1) {
      warning("For 2D data, only first-order derivatives are currently supported")
    }

    m1 <- fdataobj$dims[1]
    m2 <- fdataobj$dims[2]
    argvals_s <- fdataobj$argvals[[1]]
    argvals_t <- fdataobj$argvals[[2]]

    # Data is already flattened [n, m1*m2]
    # Call Rust implementation for 2D derivatives
    result <- .Call("wrap__fdata_deriv_2d", fdataobj$data,
                    as.numeric(argvals_s), as.numeric(argvals_t),
                    as.integer(m1), as.integer(m2))

    # Create fdata objects for each derivative type
    # Result is already in flattened format [n, m1*m2]
    make_deriv_fdata <- function(deriv_data, deriv_name) {
      new_names <- fdataobj$names
      if (!is.null(new_names$zlab)) {
        new_names$zlab <- paste0(deriv_name, "[", new_names$zlab, "]")
      }
      res <- structure(
        list(
          data = deriv_data,
          argvals = fdataobj$argvals,
          rangeval = fdataobj$rangeval,
          names = new_names,
          fdata2d = TRUE,
          dims = fdataobj$dims
        ),
        class = "fdata"
      )
      # Preserve id and metadata
      if (!is.null(fdataobj$id)) res$id <- fdataobj$id
      if (!is.null(fdataobj$metadata)) res$metadata <- fdataobj$metadata
      res
    }

    return(list(
      ds = make_deriv_fdata(result$ds, "d/ds"),
      dt = make_deriv_fdata(result$dt, "d/dt"),
      dsdt = make_deriv_fdata(result$dsdt, "d2/dsdt")
    ))
  }

  # 1D functional data
  m <- ncol(fdataobj$data)
  if (nderiv >= m) {
    stop("nderiv must be less than the number of evaluation points")
  }

  # Call Rust implementation
  deriv_data <- .Call("wrap__fdata_deriv_1d", fdataobj$data,
                      as.numeric(fdataobj$argvals), as.integer(nderiv))

  # Update argvals - derivative reduces number of points
  # For central differences, we keep interior points
  new_argvals <- fdataobj$argvals

  # Update names
  deriv_suffix <- if (nderiv == 1) "'" else paste0("^(", nderiv, ")")
  new_names <- fdataobj$names
  if (!is.null(new_names$ylab)) {
    new_names$ylab <- paste0("D", nderiv, "[", new_names$ylab, "]")
  }
  if (!is.null(new_names$main) && nchar(new_names$main) > 0) {
    new_names$main <- paste0(new_names$main, deriv_suffix)
  }

  result <- structure(
    list(
      data = deriv_data,
      argvals = new_argvals,
      rangeval = fdataobj$rangeval,
      names = new_names,
      fdata2d = FALSE
    ),
    class = "fdata"
  )

  # Preserve id and metadata from original

  if (!is.null(fdataobj$id)) {
    result$id <- fdataobj$id
  }
  if (!is.null(fdataobj$metadata)) {
    result$metadata <- fdataobj$metadata
  }

  result
}

#' Subset method for fdata objects
#'
#' @param x An object of class 'fdata'.
#' @param i Row indices (which curves to keep).
#' @param j Column indices (which time points to keep).
#' @param drop Logical. If TRUE and only one curve selected, return vector.
#'
#' @return An \code{fdata} object containing the selected subset.
#' @export
`[.fdata` <- function(x, i, j, drop = FALSE) {
  # Check for 2D fdata - handle NULL or missing fdata2d (e.g., from fda.usc objects)
  is_2d <- isTRUE(x$fdata2d)

  if (is_2d) {
    # 2D fdata: data is flattened matrix [n, m1*m2]
    n <- nrow(x$data)
    if (missing(i)) i <- seq_len(n)

    # Column subsetting not supported for 2D
    if (!missing(j)) {
      stop("Column subsetting is not supported for 2D fdata. Use i to select surfaces.")
    }

    # Subset surfaces (rows of flattened matrix)
    new_data <- x$data[i, , drop = FALSE]
    new_argvals <- x$argvals
    new_rangeval <- x$rangeval
    new_dims <- x$dims

    if (drop && nrow(new_data) == 1) {
      # Return single surface as matrix [m1, m2]
      return(matrix(new_data[1, ], nrow = x$dims[1], ncol = x$dims[2]))
    }
  } else {
    # 1D fdata: data is 2D matrix [n, m]
    if (missing(i)) i <- seq_len(nrow(x$data))
    if (missing(j)) j <- seq_len(ncol(x$data))

    new_data <- x$data[i, j, drop = FALSE]
    new_argvals <- x$argvals[j]
    new_rangeval <- range(new_argvals)
    new_dims <- NULL

    if (drop && nrow(new_data) == 1) {
      return(as.vector(new_data))
    }
  }

  # Subset id and metadata
  new_id <- if (!is.null(x$id)) x$id[i] else NULL
  new_metadata <- if (!is.null(x$metadata)) x$metadata[i, , drop = FALSE] else NULL

  structure(
    list(
      data = new_data,
      argvals = new_argvals,
      rangeval = new_rangeval,
      names = x$names,
      fdata2d = is_2d,
      dims = new_dims,
      id = new_id,
      metadata = new_metadata
    ),
    class = "fdata"
  )
}

#' Bootstrap Functional Data
#'
#' Generate bootstrap samples from functional data. Supports naive bootstrap
#' (resampling curves with replacement) and smooth bootstrap (adding noise
#' based on estimated covariance structure).
#'
#' @param fdataobj An object of class 'fdata'.
#' @param n.boot Number of bootstrap replications (default 200).
#' @param method Bootstrap method: "naive" for resampling with replacement,
#'   "smooth" for adding Gaussian noise (default "naive").
#' @param variance For method="smooth", the variance of the added noise.
#'   If NULL, estimated from the data.
#' @param seed Optional seed for reproducibility.
#'
#' @return A list of class 'fdata.bootstrap' with components:
#' \describe{
#'   \item{boot.samples}{List of n.boot fdata objects, each a bootstrap sample}
#'   \item{original}{The original fdata object}
#'   \item{method}{The bootstrap method used}
#'   \item{n.boot}{Number of bootstrap replications}
#' }
#'
#' @export
#' @examples
#' # Create functional data
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 20, 50)
#' for (i in 1:20) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#'
#' # Naive bootstrap
#' boot_naive <- fdata.bootstrap(fd, n.boot = 100, method = "naive")
#'
#' # Smooth bootstrap
#' boot_smooth <- fdata.bootstrap(fd, n.boot = 100, method = "smooth")
fdata.bootstrap <- function(fdataobj, n.boot = 200, method = c("naive", "smooth"),
                            variance = NULL, seed = NULL) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("fdata.bootstrap for 2D functional data not yet implemented")
  }

  method <- match.arg(method)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- nrow(fdataobj$data)
  m <- ncol(fdataobj$data)

  boot.samples <- vector("list", n.boot)

  if (method == "naive") {
    # Naive bootstrap: resample curves with replacement
    for (b in seq_len(n.boot)) {
      idx <- sample(n, n, replace = TRUE)
      boot_data <- fdataobj$data[idx, , drop = FALSE]

      boot.samples[[b]] <- structure(
        list(
          data = boot_data,
          argvals = fdataobj$argvals,
          rangeval = fdataobj$rangeval,
          names = fdataobj$names,
          fdata2d = FALSE
        ),
        class = "fdata"
      )
    }
  } else if (method == "smooth") {
    # Smooth bootstrap: add noise based on estimated covariance
    # Estimate pointwise variance if not provided
    if (is.null(variance)) {
      # Use pooled residual variance from mean function
      mean_func <- colMeans(fdataobj$data)
      residuals <- sweep(fdataobj$data, 2, mean_func)
      variance <- mean(residuals^2)
    }

    for (b in seq_len(n.boot)) {
      # Resample with replacement
      idx <- sample(n, n, replace = TRUE)
      boot_data <- fdataobj$data[idx, , drop = FALSE]

      # Add Gaussian noise
      noise <- matrix(rnorm(n * m, mean = 0, sd = sqrt(variance)), n, m)
      boot_data <- boot_data + noise

      boot.samples[[b]] <- structure(
        list(
          data = boot_data,
          argvals = fdataobj$argvals,
          rangeval = fdataobj$rangeval,
          names = fdataobj$names,
          fdata2d = FALSE
        ),
        class = "fdata"
      )
    }
  }

  structure(
    list(
      boot.samples = boot.samples,
      original = fdataobj,
      method = method,
      n.boot = n.boot
    ),
    class = "fdata.bootstrap"
  )
}

#' Bootstrap Confidence Intervals for Functional Statistics
#'
#' Compute bootstrap confidence intervals for functional statistics such as
#' the mean function, depth values, or regression coefficients.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param statistic A function that computes the statistic of interest.
#'   Must take an fdata object and return a numeric vector.
#' @param n.boot Number of bootstrap replications (default 200).
#' @param alpha Significance level for confidence intervals (default 0.05
#'   for 95 percent CI).
#' @param method CI method: "percentile" for simple percentile method,
#'   "basic" for basic bootstrap, "normal" for normal approximation
#'   (default "percentile").
#' @param seed Optional seed for reproducibility.
#'
#' @return A list of class 'fdata.bootstrap.ci' with components:
#' \describe{
#'   \item{estimate}{The statistic computed on the original data}
#'   \item{ci.lower}{Lower confidence bound}
#'   \item{ci.upper}{Upper confidence bound}
#'   \item{boot.stats}{Matrix of bootstrap statistics (n.boot x length(statistic))}
#'   \item{alpha}{The significance level used}
#'   \item{method}{The CI method used}
#' }
#'
#' @export
#' @examples
#' # Create functional data
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 20, 50)
#' for (i in 1:20) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#'
#' # Bootstrap CI for the mean function (returns numeric vector)
#' ci_mean <- fdata.bootstrap.ci(fd,
#'   statistic = function(x) as.numeric(mean(x)$data),
#'   n.boot = 100)
#'
#' # Bootstrap CI for depth values
#' ci_depth <- fdata.bootstrap.ci(fd,
#'   statistic = function(x) depth.FM(x),
#'   n.boot = 100)
fdata.bootstrap.ci <- function(fdataobj, statistic, n.boot = 200,
                               alpha = 0.05,
                               method = c("percentile", "basic", "normal"),
                               seed = NULL) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (!is.function(statistic)) {
    stop("statistic must be a function")
  }

  method <- match.arg(method)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Compute statistic on original data
  estimate <- statistic(fdataobj)
  n_stat <- length(estimate)

  # Generate bootstrap samples and compute statistics
  boot_obj <- fdata.bootstrap(fdataobj, n.boot = n.boot, method = "naive")

  boot.stats <- matrix(NA, n.boot, n_stat)
  for (b in seq_len(n.boot)) {
    boot.stats[b, ] <- statistic(boot_obj$boot.samples[[b]])
  }

  # Compute confidence intervals
  if (method == "percentile") {
    # Simple percentile method
    ci.lower <- apply(boot.stats, 2, quantile, probs = alpha / 2, na.rm = TRUE)
    ci.upper <- apply(boot.stats, 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)

  } else if (method == "basic") {
    # Basic bootstrap: 2*theta_hat - theta*_(1-alpha/2), 2*theta_hat - theta*_(alpha/2)
    q_lower <- apply(boot.stats, 2, quantile, probs = alpha / 2, na.rm = TRUE)
    q_upper <- apply(boot.stats, 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
    ci.lower <- 2 * estimate - q_upper
    ci.upper <- 2 * estimate - q_lower

  } else if (method == "normal") {
    # Normal approximation
    boot.se <- apply(boot.stats, 2, sd, na.rm = TRUE)
    z <- qnorm(1 - alpha / 2)
    ci.lower <- estimate - z * boot.se
    ci.upper <- estimate + z * boot.se
  }

  structure(
    list(
      estimate = estimate,
      ci.lower = ci.lower,
      ci.upper = ci.upper,
      boot.stats = boot.stats,
      alpha = alpha,
      method = method
    ),
    class = "fdata.bootstrap.ci"
  )
}

#' Print method for bootstrap CI
#' @param x A fdata.bootstrap.ci object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.fdata.bootstrap.ci <- function(x, ...) {
  cat("Bootstrap Confidence Intervals\n")
  cat("==============================\n")
  cat("Method:", x$method, "\n")
  cat("Confidence level:", (1 - x$alpha) * 100, "%\n")
  cat("Number of bootstrap replications:", nrow(x$boot.stats), "\n\n")

  n_show <- min(10, length(x$estimate))
  cat("First", n_show, "values:\n")
  df <- data.frame(
    Estimate = x$estimate[1:n_show],
    Lower = x$ci.lower[1:n_show],
    Upper = x$ci.upper[1:n_show]
  )
  print(df, digits = 4)

  if (length(x$estimate) > n_show) {
    cat("... (", length(x$estimate) - n_show, " more values)\n")
  }

  invisible(x)
}

#' Convert Functional Data to Principal Component Scores
#'
#' Performs functional PCA and returns principal component scores for
#' functional data. Uses SVD on centered data.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param ncomp Number of principal components to extract (default 2).
#' @param lambda Regularization parameter (default 0, not currently used).
#' @param norm Logical. If TRUE (default), normalize the scores.
#'
#' @return A list with components:
#' \describe{
#'   \item{d}{Singular values (proportional to sqrt of eigenvalues)}
#'   \item{rotation}{fdata object containing PC loadings}
#'   \item{x}{Matrix of PC scores (n x ncomp)}
#'   \item{mean}{Mean function (numeric vector)}
#'   \item{fdataobj.cen}{Centered fdata object}
#'   \item{call}{The function call}
#' }
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 20, 50)
#' for (i in 1:20) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#' pc <- fdata2pc(fd, ncomp = 3)
fdata2pc <- function(fdataobj, ncomp = 2, lambda = 0, norm = TRUE) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("fdata2pc for 2D functional data not yet implemented")
  }

  result <- .Call("wrap__fdata2pc_1d", fdataobj$data,
                  as.integer(ncomp), as.numeric(lambda))

  # Construct fdata for rotation (loadings)
  # Rotation from Rust is m x ncomp, transpose to ncomp x m for fdata
  rotation <- fdata(t(result$rotation), argvals = fdataobj$argvals,
                    names = list(main = "PC Loadings",
                                 xlab = fdataobj$names$xlab,
                                 ylab = "Loading"))

  # Centered fdata
  fdataobj.cen <- fdata(result$centered, argvals = fdataobj$argvals,
                        names = list(main = "Centered Data",
                                     xlab = fdataobj$names$xlab,
                                     ylab = fdataobj$names$ylab))

  structure(
    list(
      d = result$d,
      rotation = rotation,
      x = result$scores,
      mean = result$mean,
      fdataobj.cen = fdataobj.cen,
      argvals = fdataobj$argvals,
      call = match.call()
    ),
    class = "fdata2pc"
  )
}

#' Plot FPCA Results
#'
#' Visualize functional principal component analysis results with multiple
#' plot types: component perturbation plots, variance explained (scree plot),
#' or score plots.
#'
#' @param x An object of class 'fdata2pc' from \code{\link{fdata2pc}}.
#' @param type Type of plot: "components" (default) shows mean +/- scaled PC loadings,
#'   "variance" shows a scree plot of variance explained, "scores" shows PC1 vs PC2
#'   scatter plot of observations.
#' @param ncomp Number of components to display (default 3 or fewer if not available).
#' @param multiple Factor for scaling PC perturbations. Default is 2 (shows +/- 2*sqrt(eigenvalue)*PC).
#' @param show_both_directions Logical. If TRUE (default), show both positive and
#'   negative perturbations (mean + PC and mean - PC). If FALSE, only show positive
#'   perturbation. All curves are solid lines differentiated by color.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return A ggplot object (invisibly).
#'
#' @details
#' The "components" plot shows the mean function (black) with perturbations
#' in the direction of each principal component. The perturbation is computed as:
#' mean +/- multiple * sqrt(variance_explained) * PC_loading. All lines are solid
#' and differentiated by color only.
#'
#' The "variance" plot shows a scree plot with the proportion of variance
#' explained by each component as a bar chart.
#'
#' The "scores" plot shows a scatter plot of observations in PC space,
#' typically PC1 vs PC2.
#'
#' @seealso \code{\link{fdata2pc}} for computing FPCA.
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 30, 50)
#' for (i in 1:30) X[i, ] <- sin(2*pi*t + runif(1, 0, pi)) + rnorm(50, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#' pc <- fdata2pc(fd, ncomp = 3)
#'
#' # Plot PC components (mean +/- perturbations)
#' plot(pc, type = "components")
#'
#' # Scree plot
#' plot(pc, type = "variance")
#'
#' # Score plot
#' plot(pc, type = "scores")
plot.fdata2pc <- function(x, type = c("components", "variance", "scores"),
                          ncomp = 3, multiple = 2,
                          show_both_directions = TRUE, ...) {
  type <- match.arg(type)

  ncomp <- min(ncomp, length(x$d), ncol(x$x))

  switch(type,
    "components" = .plot_fpca_components(x, ncomp, multiple, show_both_directions),
    "variance" = .plot_fpca_variance(x, ncomp),
    "scores" = .plot_fpca_scores(x, ncomp)
  )
}

# Internal: Plot FPCA components (mean +/- perturbations)
# @noRd
.plot_fpca_components <- function(x, ncomp, multiple, show_both_directions = TRUE) {
  m <- length(x$argvals)

  # Compute variance explained (proportional to d^2)
  var_explained <- x$d^2
  total_var <- sum(var_explained)
  prop_var <- var_explained / total_var

  # Define curve type labels upfront
  label_mean <- "Mean"
  label_plus <- paste0("Mean + ", multiple, "*sqrt(lambda)*PC")
  label_minus <- paste0("Mean - ", multiple, "*sqrt(lambda)*PC")

  # Build data frame for plotting - each component gets its own facet
  plot_data <- list()

  for (k in seq_len(ncomp)) {
    loading <- x$rotation$data[k, ]
    scale_factor <- multiple * sqrt(var_explained[k])
    facet_label <- paste0("PC", k, " (", round(100 * prop_var[k], 1), "%)")

    # Mean function
    plot_data[[length(plot_data) + 1]] <- data.frame(
      t = x$argvals,
      value = x$mean,
      component = facet_label,
      curve_type = label_mean
    )

    # Plus direction
    plot_data[[length(plot_data) + 1]] <- data.frame(
      t = x$argvals,
      value = x$mean + scale_factor * loading,
      component = facet_label,
      curve_type = label_plus
    )

    # Minus direction (only if show_both_directions is TRUE)
    if (show_both_directions) {
      plot_data[[length(plot_data) + 1]] <- data.frame(
        t = x$argvals,
        value = x$mean - scale_factor * loading,
        component = facet_label,
        curve_type = label_minus
      )
    }
  }

  df <- do.call(rbind, plot_data)

  # Set factor levels for consistent ordering
  pc_labels <- paste0("PC", seq_len(ncomp), " (",
                      round(100 * prop_var[seq_len(ncomp)], 1), "%)")
  df$component <- factor(df$component, levels = pc_labels)

  # Define colors and labels based on whether both directions are shown
  if (show_both_directions) {
    curve_labels <- c(label_mean, label_plus, label_minus)
    color_values <- c("black", "steelblue", "coral")
  } else {
    curve_labels <- c(label_mean, label_plus)
    color_values <- c("black", "steelblue")
  }
  names(color_values) <- curve_labels
  df$curve_type <- factor(df$curve_type, levels = curve_labels)

  # Create faceted plot - all solid lines, differentiated by color only
  p <- ggplot2::ggplot(df, ggplot2::aes(x = t, y = value, color = curve_type)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~component, scales = "free_y") +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::labs(
      title = "FPCA: Principal Component Perturbations",
      subtitle = if (show_both_directions) {
        paste0("Mean +/- ", multiple, " * sqrt(eigenvalue) * PC")
      } else {
        paste0("Mean + ", multiple, " * sqrt(eigenvalue) * PC")
      },
      x = "t",
      y = "X(t)",
      color = ""
    ) +
    ggplot2::theme(legend.position = "bottom")

  p
}

# Internal: Plot FPCA variance explained (scree plot)
# @noRd
.plot_fpca_variance <- function(x, ncomp) {
  # Compute variance explained
  var_explained <- x$d^2
  total_var <- sum(var_explained)
  prop_var <- var_explained / total_var
  cum_var <- cumsum(prop_var)

  # Limit to ncomp
  ncomp <- min(ncomp, length(var_explained))

  df <- data.frame(
    component = factor(seq_len(ncomp), levels = seq_len(ncomp)),
    prop = prop_var[seq_len(ncomp)] * 100,
    cumulative = cum_var[seq_len(ncomp)] * 100
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = component, y = prop)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = cumulative, group = 1),
                        color = "darkred", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(y = cumulative), color = "darkred", size = 2) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(prop, 1), "%")),
                       vjust = -0.5, size = 3) +
    ggplot2::scale_y_continuous(
      name = "Variance Explained (%)",
      sec.axis = ggplot2::sec_axis(~., name = "Cumulative (%)")
    ) +
    ggplot2::labs(
      title = "FPCA: Variance Explained (Scree Plot)",
      x = "Principal Component"
    )

  p
}

# Internal: Plot FPCA scores
# @noRd
.plot_fpca_scores <- function(x, ncomp) {
  scores <- x$x
  n <- nrow(scores)

  # Compute variance explained for axis labels
  var_explained <- x$d^2
  total_var <- sum(var_explained)
  prop_var <- var_explained / total_var * 100

  if (ncol(scores) >= 2) {
    # 2D scatter plot: PC1 vs PC2 with ID labels
    df <- data.frame(
      PC1 = scores[, 1],
      PC2 = scores[, 2],
      id = seq_len(n)
    )

    p <- ggplot2::ggplot(df, ggplot2::aes(x = PC1, y = PC2, label = id)) +
      ggplot2::geom_point(color = "steelblue", size = 2, alpha = 0.7) +
      ggplot2::geom_text(hjust = -0.2, vjust = 0.5, size = 2.5, color = "gray30") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::labs(
        title = "FPCA: Score Plot",
        subtitle = "Labels show observation ID for curve identification",
        x = paste0("PC1 (", round(prop_var[1], 1), "%)"),
        y = paste0("PC2 (", round(prop_var[2], 1), "%)")
      )
  } else {
    # Only 1 PC: plot scores as bar chart with IDs
    df <- data.frame(
      id = factor(seq_len(n)),
      PC1 = scores[, 1]
    )

    p <- ggplot2::ggplot(df, ggplot2::aes(x = id, y = PC1)) +
      ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
      ggplot2::labs(
        title = "FPCA: Score Plot",
        subtitle = "X-axis shows observation ID",
        x = "Observation ID",
        y = paste0("PC1 (", round(prop_var[1], 1), "%)")
      )
  }

  p
}

#' Print Method for FPCA Results
#'
#' @param x An object of class 'fdata2pc'.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.fdata2pc <- function(x, ...) {
  cat("Functional Principal Component Analysis\n")
  cat("========================================\n")
  cat("Number of observations:", nrow(x$x), "\n")
  cat("Number of components:", length(x$d), "\n\n")

  # Compute variance explained
  var_explained <- x$d^2
  total_var <- sum(var_explained)
  prop_var <- var_explained / total_var * 100
  cum_var <- cumsum(prop_var)

  cat("Variance explained:\n")
  for (k in seq_along(x$d)) {
    cat(sprintf("  PC%d: %.1f%% (cumulative: %.1f%%)\n",
                k, prop_var[k], cum_var[k]))
  }
  invisible(x)
}

#' Convert Functional Data to PLS Scores
#'
#' Performs Partial Least Squares regression and returns component scores
#' for functional data using the NIPALS algorithm.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param y Response vector (numeric).
#' @param ncomp Number of PLS components to extract (default 2).
#' @param lambda Regularization parameter (default 0, not currently used).
#' @param norm Logical. If TRUE (default), normalize the scores.
#'
#' @return A list with components:
#' \describe{
#'   \item{weights}{Matrix of PLS weights (m x ncomp)}
#'   \item{scores}{Matrix of PLS scores (n x ncomp)}
#'   \item{loadings}{Matrix of PLS loadings (m x ncomp)}
#'   \item{call}{The function call}
#' }
#'
#' @export
#' @examples
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 20, 50)
#' for (i in 1:20) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.1)
#' y <- rowMeans(X) + rnorm(20, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#' pls <- fdata2pls(fd, y, ncomp = 3)
fdata2pls <- function(fdataobj, y, ncomp = 2, lambda = 0, norm = TRUE) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  if (isTRUE(fdataobj$fdata2d)) {
    stop("fdata2pls for 2D functional data not yet implemented")
  }

  if (length(y) != nrow(fdataobj$data)) {
    stop("Length of y must equal number of curves")
  }

  result <- .Call("wrap__fdata2pls_1d", fdataobj$data,
                  as.numeric(y), as.integer(ncomp), as.numeric(lambda))

  list(
    rotation = result$weights,
    x = result$scores,
    loadings = result$loadings,
    call = match.call()
  )
}

#' Convert Functional Data to Basis Coefficients
#'
#' Project functional data onto a basis system and return coefficients.
#' Supports B-spline and Fourier basis. Works with both regular \code{fdata}
#' and irregular \code{irregFdata} objects.
#'
#' @param x An object of class 'fdata' or 'irregFdata'.
#' @param nbasis Number of basis functions (default 10).
#' @param type Type of basis: "bspline" (default) or "fourier".
#' @param ... Additional arguments (currently unused).
#'
#' @return A matrix of coefficients (n x nbasis).
#'
#' @details
#' For regular \code{fdata} objects, all curves are projected onto the same
#' basis evaluated at the common grid points.
#'
#' For irregular \code{irregFdata} objects, each curve is individually
#' fitted to the basis using least squares at its own observation points.
#' This is the preferred approach for sparse/irregularly sampled data as it
#' avoids interpolation artifacts.
#'
#' @export
#' @examples
#' # Regular fdata
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 20, 50)
#' for (i in 1:20) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#' coefs <- fdata2basis(fd, nbasis = 10, type = "bspline")
#'
#' # Irregular fdata (sparsified)
#' ifd <- sparsify(fd, minObs = 10, maxObs = 20, seed = 42)
#' coefs_irreg <- fdata2basis(ifd, nbasis = 10, type = "bspline")
fdata2basis <- function(x, nbasis = 10, type = c("bspline", "fourier"), ...) {
  UseMethod("fdata2basis")
}

#' @rdname fdata2basis
#' @method fdata2basis fdata
#' @export
fdata2basis.fdata <- function(x, nbasis = 10, type = c("bspline", "fourier"), ...) {
  fdataobj <- x

  if (isTRUE(fdataobj$fdata2d)) {
    stop("fdata2basis for 2D functional data not yet implemented")
  }

  type <- match.arg(type)
  basis_type <- if (type == "fourier") 1L else 0L

  .Call("wrap__fdata2basis_1d", fdataobj$data,
        as.numeric(fdataobj$argvals), as.integer(nbasis), basis_type)
}

#' Convert Functional Data to fd class
#'
#' Converts an fdata object to an fd object from the fda package.
#' Requires the fda package to be installed.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param nbasis Number of basis functions (default 10).
#' @param type Type of basis: "bspline" (default) or "fourier".
#'
#' @return An object of class 'fd' from the fda package.
#'
#' @export
#' @examplesIf requireNamespace("fda", quietly = TRUE)
#' t <- seq(0, 1, length.out = 50)
#' X <- matrix(0, 20, 50)
#' for (i in 1:20) X[i, ] <- sin(2*pi*t) + rnorm(50, sd = 0.1)
#' fd <- fdata(X, argvals = t)
#' fd_obj <- fdata2fd(fd, nbasis = 10)
fdata2fd <- function(fdataobj, nbasis = 10, type = c("bspline", "fourier")) {
  if (!requireNamespace("fda", quietly = TRUE)) {
    stop("Package 'fda' is required for fdata2fd. Please install it.")
  }

  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  type <- match.arg(type)

  # Get coefficients from our implementation
  coefs <- fdata2basis(fdataobj, nbasis = nbasis, type = type)

  # Create basis object
  rangeval <- fdataobj$rangeval
  if (type == "fourier") {
    basis <- fda::create.fourier.basis(rangeval = rangeval, nbasis = nbasis)
  } else {
    basis <- fda::create.bspline.basis(rangeval = rangeval, nbasis = nbasis)
  }

  # Create fd object
  # Note: fda::fd expects coefs as (nbasis x n), we have (n x nbasis)
  fda::fd(coef = t(coefs), basisobj = basis)
}

#' Compute Distance/Similarity Between Groups of Functional Data
#'
#' Computes various distance and similarity measures between pre-defined groups
#' of functional curves.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param groups A factor or character vector specifying group membership for each curve.
#'   Must have length equal to the number of curves.
#' @param method Distance/similarity method:
#'   \itemize{
#'     \item "centroid": L2 distance between group mean curves
#'     \item "hausdorff": Hausdorff-style distance between groups
#'     \item "depth": Depth-based overlap (similarity, not distance)
#'     \item "all": Compute all methods
#'   }
#' @param metric Distance metric for centroid method (default "lp").
#' @param p Power for Lp metric (default 2 for L2).
#' @param depth.method Depth method for depth-based overlap (default "FM").
#' @param ... Additional arguments passed to metric functions.
#'
#' @return An object of class 'group.distance' containing:
#' \describe{
#'   \item{centroid}{Centroid distance matrix (if method includes centroid)}
#'   \item{hausdorff}{Hausdorff distance matrix (if method includes hausdorff)}
#'   \item{depth}{Depth-based similarity matrix (if method includes depth)}
#'   \item{groups}{Unique group labels}
#'   \item{group.sizes}{Number of curves per group}
#'   \item{method}{Methods used}
#' }
#'
#' @export
#' @examples
#' # Create grouped functional data
#' set.seed(42)
#' n <- 30
#' m <- 50
#' t_grid <- seq(0, 1, length.out = m)
#' X <- matrix(0, n, m)
#' for (i in 1:15) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
#' for (i in 16:30) X[i, ] <- cos(2 * pi * t_grid) + rnorm(m, sd = 0.1)
#' fd <- fdata(X, argvals = t_grid)
#' groups <- factor(rep(c("A", "B"), each = 15))
#'
#' # Compute all distance measures
#' gd <- group.distance(fd, groups, method = "all")
#' print(gd)
group.distance <- function(fdataobj, groups,
                           method = c("centroid", "hausdorff", "depth", "all"),
                           metric = "lp", p = 2, depth.method = "FM", ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  n <- nrow(fdataobj$data)
  if (length(groups) != n) {
    stop("length(groups) must equal the number of curves (", n, ")")
  }

  groups <- as.factor(groups)
  group_levels <- levels(groups)
  n_groups <- length(group_levels)

  if (n_groups < 2) {
    stop("Need at least 2 groups to compute distances")
  }

  method <- match.arg(method)
  compute_all <- method == "all"

  result <- list(
    groups = group_levels,
    group.sizes = table(groups),
    method = if (compute_all) c("centroid", "hausdorff", "depth") else method
  )

  # Compute centroid distances
  if (method == "centroid" || compute_all) {
    result$centroid <- .group_centroid_distance(fdataobj, groups, group_levels, metric, p, ...)
  }

  # Compute Hausdorff distances
  if (method == "hausdorff" || compute_all) {
    result$hausdorff <- .group_hausdorff_distance(fdataobj, groups, group_levels, metric, p, ...)
  }

  # Compute depth-based overlap
  if (method == "depth" || compute_all) {
    result$depth <- .group_depth_overlap(fdataobj, groups, group_levels, depth.method)
  }

  class(result) <- "group.distance"
  result
}

# Internal: Compute centroid (mean curve) distances between groups
.group_centroid_distance <- function(fdataobj, groups, group_levels, metric, p, ...) {
  n_groups <- length(group_levels)

  # Compute group means
  group_means <- lapply(group_levels, function(g) {
    idx <- which(groups == g)
    mean(fdataobj[idx])
  })

  # Compute pairwise distances between means
  dist_mat <- matrix(0, n_groups, n_groups)
  rownames(dist_mat) <- colnames(dist_mat) <- group_levels

  for (i in seq_len(n_groups)) {
    for (j in seq_len(n_groups)) {
      if (i < j) {
        # Combine means into single fdata for distance calculation
        combined <- fdata(
          rbind(group_means[[i]]$data, group_means[[j]]$data),
          argvals = fdataobj$argvals
        )
        d <- metric.lp(combined, p = p, ...)[1, 2]
        dist_mat[i, j] <- d
        dist_mat[j, i] <- d
      }
    }
  }

  dist_mat
}

# Internal: Compute Hausdorff-style distances between groups
.group_hausdorff_distance <- function(fdataobj, groups, group_levels, metric, p, ...) {
  n_groups <- length(group_levels)

  # Pre-compute full distance matrix
  full_dist <- metric.lp(fdataobj, p = p, ...)

  dist_mat <- matrix(0, n_groups, n_groups)
  rownames(dist_mat) <- colnames(dist_mat) <- group_levels

  for (i in seq_len(n_groups)) {
    for (j in seq_len(n_groups)) {
      if (i < j) {
        idx_i <- which(groups == group_levels[i])
        idx_j <- which(groups == group_levels[j])

        # Hausdorff: max(max_a min_b d(a,b), max_b min_a d(a,b))
        # For each curve in group i, find minimum distance to group j
        min_dists_i_to_j <- apply(full_dist[idx_i, idx_j, drop = FALSE], 1, min)
        # For each curve in group j, find minimum distance to group i
        min_dists_j_to_i <- apply(full_dist[idx_j, idx_i, drop = FALSE], 1, min)

        hausdorff_dist <- max(max(min_dists_i_to_j), max(min_dists_j_to_i))
        dist_mat[i, j] <- hausdorff_dist
        dist_mat[j, i] <- hausdorff_dist
      }
    }
  }

  dist_mat
}

# Internal: Compute depth-based overlap (similarity) between groups
.group_depth_overlap <- function(fdataobj, groups, group_levels, depth.method) {
  n_groups <- length(group_levels)

  sim_mat <- matrix(0, n_groups, n_groups)
  rownames(sim_mat) <- colnames(sim_mat) <- group_levels

  for (i in seq_len(n_groups)) {
    for (j in seq_len(n_groups)) {
      idx_i <- which(groups == group_levels[i])
      idx_j <- which(groups == group_levels[j])

      if (i == j) {
        # Self-overlap is 1 (curves in group have depth w.r.t. themselves)
        sim_mat[i, j] <- 1
      } else {
        # Compute mean depth of curves in group i w.r.t. group j
        depth_i_in_j <- depth(fdataobj[idx_i], fdataobj[idx_j], method = depth.method)
        sim_mat[i, j] <- mean(depth_i_in_j)
      }
    }
  }

  sim_mat
}

#' Print method for group.distance
#' @param x A group.distance object.
#' @param digits Number of digits for printing (default 3).
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.group.distance <- function(x, digits = 3, ...) {
  cat("Group Distance Analysis\n")
  cat("=======================\n")
  cat("Groups:", paste(x$groups, collapse = ", "), "\n")
  cat("Group sizes:", paste(paste0(names(x$group.sizes), "=", x$group.sizes), collapse = ", "), "\n\n")

  if (!is.null(x$centroid)) {
    cat("Centroid Distance (L2 between group means):\n")
    print(round(x$centroid, digits))
    cat("\n")
  }

  if (!is.null(x$hausdorff)) {
    cat("Hausdorff Distance (worst-case between groups):\n")
    print(round(x$hausdorff, digits))
    cat("\n")
  }

  if (!is.null(x$depth)) {
    cat("Depth Overlap (similarity, higher = more similar):\n")
    print(round(x$depth, digits))
    cat("\n")
  }

  invisible(x)
}

#' Plot method for group.distance
#'
#' @param x An object of class 'group.distance'.
#' @param type Plot type: "heatmap" or "dendrogram".
#' @param which Which distance matrix to plot. If NULL (default), uses the first
#'   available matrix from the group.distance object.
#' @param ... Additional arguments.
#'
#' @return A ggplot object (for heatmap) or NULL (for dendrogram, uses base graphics).
#' @export
plot.group.distance <- function(x, type = c("heatmap", "dendrogram"),
                                which = NULL, ...) {
  type <- match.arg(type)

  # Auto-detect which matrix to use if not specified
  available <- c("centroid", "hausdorff", "depth")
  available <- available[vapply(available, function(m) !is.null(x[[m]]), logical(1))]


  if (length(available) == 0) {
    stop("No distance matrices available in the group.distance object")
  }

  if (is.null(which)) {
    which <- available[1]
  } else {
    which <- match.arg(which, choices = c("centroid", "hausdorff", "depth"))
    if (!(which %in% available)) {
      stop("Distance matrix '", which, "' not available. ",
           "Available: ", paste(available, collapse = ", "), ". ",
           "Run group.distance with method='all' or method='", which, "'")
    }
  }

  mat <- x[[which]]

  if (type == "heatmap") {
    # Convert to long format for ggplot
    n <- nrow(mat)
    df <- data.frame(
      group1 = rep(rownames(mat), n),
      group2 = rep(colnames(mat), each = n),
      value = as.vector(mat)
    )
    df$group1 <- factor(df$group1, levels = rownames(mat))
    df$group2 <- factor(df$group2, levels = colnames(mat))

    title <- switch(which,
      centroid = "Centroid Distance",
      hausdorff = "Hausdorff Distance",
      depth = "Depth Overlap (Similarity)"
    )

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$group1, y = .data$group2, fill = .data$value)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = round(.data$value, 2)), color = "white", size = 4) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(x = "Group", y = "Group", fill = "Value", title = title) +
      ggplot2::coord_equal()

    p

  } else {
    # Dendrogram using base R
    if (which == "depth") {
      # Convert similarity to distance for clustering
      dist_mat <- 1 - mat
    } else {
      dist_mat <- mat
    }

    hc <- hclust(as.dist(dist_mat), method = "complete")
    plot(hc, main = paste("Hierarchical Clustering -", which),
         xlab = "Group", ylab = "Distance")
    invisible(NULL)
  }
}

#' Permutation Test for Group Differences
#'
#' Tests whether groups of functional data are significantly different using
#' permutation testing.
#'
#' @details
#' **Null Hypothesis (H0):** All groups come from the same distribution. That is,
#' the group labels are exchangeable and there is no systematic difference between
#' the functional curves in different groups.
#'
#' **Alternative Hypothesis (H1):** At least one group differs from the others in
#' terms of location (mean function) or dispersion.
#'
#' The test works by:
#' 1. Computing a test statistic on the observed data
#' 2. Repeatedly permuting the group labels and recomputing the statistic
#' 3. Calculating the p-value as the proportion of permuted statistics >= observed
#'
#' Two test statistics are available:
#' \itemize{
#'   \item \code{"centroid"}: Sum of pairwise L2 distances between group mean
#'     functions. Sensitive to differences in group locations (means).
#'   \item \code{"ratio"}: Ratio of between-group to within-group variance,
#'     similar to an F-statistic. Sensitive to both location and dispersion.
#' }
#'
#' A small p-value (e.g., < 0.05) indicates evidence against H0, suggesting
#' that the groups are significantly different.
#'
#' @param fdataobj An object of class 'fdata'.
#' @param groups A factor or character vector specifying group membership.
#' @param n.perm Number of permutations (default 1000).
#' @param statistic Test statistic: "centroid" (distance between group means) or
#'   "ratio" (between/within group variance ratio).
#' @param ... Additional arguments passed to distance functions.
#'
#' @return An object of class 'group.test' containing:
#' \describe{
#'   \item{statistic}{Observed test statistic}
#'   \item{p.value}{Permutation p-value}
#'   \item{perm.dist}{Permutation distribution of test statistic}
#'   \item{n.perm}{Number of permutations used}
#' }
#'
#' @export
#' @examples
#' \donttest{
#' set.seed(42)
#' n <- 30
#' m <- 50
#' t_grid <- seq(0, 1, length.out = m)
#' X <- matrix(0, n, m)
#' for (i in 1:15) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
#' for (i in 16:30) X[i, ] <- cos(2 * pi * t_grid) + rnorm(m, sd = 0.1)
#' fd <- fdata(X, argvals = t_grid)
#' groups <- factor(rep(c("A", "B"), each = 15))
#'
#' # Test for significant difference
#' gt <- group.test(fd, groups, n.perm = 500)
#' print(gt)
#' }
group.test <- function(fdataobj, groups, n.perm = 1000,
                       statistic = c("centroid", "ratio"), ...) {
  if (!inherits(fdataobj, "fdata")) {
    stop("fdataobj must be of class 'fdata'")
  }

  n <- nrow(fdataobj$data)
  if (length(groups) != n) {
    stop("length(groups) must equal the number of curves (", n, ")")
  }

  groups <- as.factor(groups)
  statistic <- match.arg(statistic)

  # Compute observed test statistic
  obs_stat <- .compute_group_stat(fdataobj, groups, statistic, ...)

  # Permutation distribution
  perm_stats <- numeric(n.perm)
  for (i in seq_len(n.perm)) {
    perm_groups <- sample(groups)
    perm_stats[i] <- .compute_group_stat(fdataobj, perm_groups, statistic, ...)
  }

  # Compute p-value (proportion of permuted stats >= observed)
  p_value <- mean(perm_stats >= obs_stat)

  result <- list(
    statistic = obs_stat,
    p.value = p_value,
    perm.dist = perm_stats,
    n.perm = n.perm,
    stat.type = statistic
  )
  class(result) <- "group.test"
  result
}

# Internal: Compute test statistic for group comparison
.compute_group_stat <- function(fdataobj, groups, statistic, ...) {
  group_levels <- levels(groups)

  if (statistic == "centroid") {
    # Sum of pairwise centroid distances
    gd <- group.distance(fdataobj, groups, method = "centroid", ...)
    # Return sum of upper triangle (total between-group distance)
    return(sum(gd$centroid[upper.tri(gd$centroid)]))

  } else {
    # Between/within variance ratio
    # Between: sum of distances from group means to overall mean
    # Within: sum of distances from curves to their group means

    overall_mean <- mean(fdataobj)
    n_groups <- length(group_levels)

    between_var <- 0
    within_var <- 0

    for (g in group_levels) {
      idx <- which(groups == g)
      n_g <- length(idx)
      group_data <- fdataobj[idx]
      group_mean <- mean(group_data)

      # Between: distance from group mean to overall mean, weighted by group size
      combined <- fdata(rbind(group_mean$data, overall_mean$data), argvals = fdataobj$argvals)
      between_var <- between_var + n_g * metric.lp(combined, p = 2)[1, 2]^2

      # Within: distances from curves to group mean
      for (i in seq_len(n_g)) {
        curve_mean <- fdata(rbind(group_data$data[i, ], group_mean$data), argvals = fdataobj$argvals)
        within_var <- within_var + metric.lp(curve_mean, p = 2)[1, 2]^2
      }
    }

    # F-like ratio (higher = more separation)
    return(between_var / max(within_var, 1e-10))
  }
}

#' Print method for group.test
#' @param x A group.test object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.group.test <- function(x, ...) {
  cat("Permutation Test for Group Differences\n")
  cat("======================================\n")
  cat("Test statistic type:", x$stat.type, "\n")
  cat("Observed statistic:", round(x$statistic, 4), "\n")
  cat("Number of permutations:", x$n.perm, "\n")
  cat("P-value:", format.pval(x$p.value, digits = 3))

  if (x$p.value < 0.001) {
    cat(" ***\n")
  } else if (x$p.value < 0.01) {
    cat(" **\n")
  } else if (x$p.value < 0.05) {
    cat(" *\n")
  } else {
    cat("\n")
  }

  invisible(x)
}
