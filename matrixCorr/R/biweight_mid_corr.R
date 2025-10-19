#' Biweight mid-correlation (bicor)
#'
#' @description
#' Use biweight mid-correlatio when you want a Pearson-like measure that is
#' robust to outliers and
#' heavy-tailed noise. Bicor down-weights extreme observations via Tukey’s
#' biweight while preserving location/scale invariance, making it well suited
#' to high-throughput data (e.g., gene expression) where occasional gross errors
#' or platform artefacts occur. Prefer Spearman/Kendall for purely ordinal
#' structure or strongly non-linear monotone relations.
#'
#' @param data A numeric matrix or a data frame containing numeric columns.
#'   Factors, logicals and common time classes are dropped in the data-frame
#'   path. Missing values are not allowed unless \code{na_method = "pairwise"}.
#' @param c_const Positive numeric. Tukey biweight tuning constant applied to the
#'   \emph{raw} MAD; default \code{9} (Langfelder & Horvath’s convention).
#' @param max_p_outliers Numeric in \code{(0, 1]}. Optional cap on the maximum
#'   proportion of outliers \emph{on each side}; if \code{< 1}, side-specific
#'   rescaling maps those quantiles to \code{|u|=1}. Use \code{1} to disable.
#' @param pearson_fallback Character scalar indicating the fallback policy.
#'   One of:
#'   \itemize{
#'     \item \code{"hybrid"} (default): if a column has MAD = 0, that column
#'       uses Pearson standardisation, yielding a hybrid correlation.
#'     \item \code{"none"}: return \code{NA} if a column has MAD = 0 or becomes
#'       degenerate after weighting.
#'     \item \code{"all"}: force ordinary Pearson for all columns.
#'   }
#' @param na_method One of \code{"error"} (default, fastest) or \code{"pairwise"}.
#'   With \code{"pairwise"}, each \eqn{(j,k)} correlation is computed on the
#'   intersection of non-missing rows for the pair.
#' @param mad_consistent Logical; if \code{TRUE}, use the normal-consistent MAD
#'   (\code{MAD_raw * 1.4826}) in the bicor weights. Default \code{FALSE} to
#'   match Langfelder & Horvath (2012).
#' @param w Optional non-negative numeric vector of length \code{nrow(data)}
#'   giving \emph{row weights}. When supplied, weighted medians/MADs are used
#'   and Tukey weights are multiplied by \code{w} before normalisation.
#' @param sparse_threshold Optional numeric \eqn{\geq 0}. If supplied, sets
#'   entries with \code{|r| < sparse_threshold} to 0 and returns a sparse
#'   \code{"ddiMatrix"} (requires \pkg{Matrix}).
#' @param n_threads Integer \eqn{\geq 1}. Number of OpenMP threads. Defaults to
#'   \code{getOption("matrixCorr.threads", 1L)}.
#'
#' @return A symmetric correlation matrix with class \code{biweight_mid_corr}
#'   (or a \code{dgCMatrix} if \code{sparse_threshold} is used), with attributes:
#'   \code{method = "biweight_mid_correlation"}, \code{description},
#'   and \code{package = "matrixCorr"}.
#'
#' @details
#'
#' For a column \eqn{x = (x_a)_{a=1}^m}, let \eqn{\mathrm{med}(x)} be the median and
#' \eqn{\mathrm{MAD}(x) = \mathrm{med}(|x - \mathrm{med}(x)|)} the (raw) median
#' absolute deviation. If \code{mad_consistent = TRUE}, the consistent scale
#' \eqn{\mathrm{MAD}^\star(x) = 1.4826\,\mathrm{MAD}(x)} is used. With tuning constant
#' \eqn{c>0}, define
#' \deqn{u_a = \frac{x_a - \mathrm{med}(x)}{c\,\mathrm{MAD}^{(\star)}(x)}.}
#' The Tukey biweight gives per-observation weights
#' \deqn{w_a = (1 - u_a^2)^2\,\mathbf{1}\{|u_a| < 1\}.}
#' Robust standardisation of a column is
#' \deqn{\tilde x_a =
#' \frac{(x_a - \mathrm{med}(x))\,w_a}{
#'       \sqrt{\sum_{b=1}^m \big[(x_b - \mathrm{med}(x))\,w_b\big]^2}}.}
#' For two columns \eqn{x,y}, the biweight mid-correlation is
#' \deqn{\mathrm{bicor}(x,y) = \sum_{a=1}^m \tilde x_a\,\tilde y_a \in [-1,1].}
#'
#' \strong{Capping the maximum proportion of outliers (\code{max_p_outliers}).}
#' If \code{max_p_outliers < 1}, let \eqn{q_L = Q_x(\text{max\_p\_outliers})} and
#' \eqn{q_U = Q_x(1-\text{max\_p\_outliers})} be the lower/upper quantiles of \eqn{x}.
#' If the corresponding \eqn{|u|} at either quantile exceeds 1, \eqn{u} is rescaled
#' \emph{separately} on the negative and positive sides so that those quantiles land at
#' \eqn{|u|=1}. This guarantees that all observations between the two quantiles receive
#' positive weight. Note the bound applies per side, so up to \eqn{2\,\text{max\_p\_outliers}}
#' of observations can be treated as outliers overall.
#'
#' \strong{Fallback when for zero MAD / degeneracy (\code{pearson_fallback}).}
#' If a column has \eqn{\mathrm{MAD}=0} or the robust denominator becomes zero,
#' the following rules apply:
#' \itemize{
#'   \item \code{"none"} when correlations involving that column are \code{NA} (diagonal
#'         remains 1).
#'   \item \code{"hybrid"} when only the affected column switches to Pearson standardisation
#'         \eqn{\bar x_a = (x_a - \overline{x}) / \sqrt{\sum_b (x_b - \overline{x})^2}},
#'         yielding the hybrid correlation
#'         \deqn{\mathrm{bicor}_{\mathrm{hyb}}(x,y) = \sum_a \bar x_a\,\tilde y_a,}
#'         with the other column still robust-standardised.
#'   \item \code{"all"} when all columns use ordinary Pearson standardisation; the result
#'         equals \code{stats::cor(…, method="pearson")} when the NA policy matches.
#' }
#'
#' \strong{Handling missing values (\code{na_method}).}
#' \itemize{
#'   \item \code{"error"} (default): inputs must be finite; this yields a symmetric,
#'         positive semidefinite (PSD) matrix since \eqn{R = \tilde X^\top \tilde X}.
#'   \item \code{"pairwise"}: each \eqn{R_{jk}} is computed on the intersection of
#'         rows where both columns are finite. Pairs with fewer than 5 overlapping
#'         rows return \code{NA} (guarding against instability). Pairwise deletion can
#'         break PSD, as in the Pearson case.
#' }
#'
#' \strong{Row weights (\code{w}).}
#' When \code{w} is supplied (non-negative, length \eqn{m}), the weighted median
#' \eqn{\mathrm{med}_w(x)} and weighted MAD
#' \eqn{\mathrm{MAD}_w(x) = \mathrm{med}_w(|x - \mathrm{med}_w(x)|)} are used to form
#' \eqn{u}. The Tukey weights are then multiplied by the observation weights prior
#' to normalisation:
#' \deqn{\tilde x_a =
#' \frac{(x_a - \mathrm{med}_w(x))\,w_a\,w^{(\mathrm{obs})}_a}{
#'       \sqrt{\sum_b \big[(x_b - \mathrm{med}_w(x))\,w_b\,w^{(\mathrm{obs})}_b\big]^2}},}
#' where \eqn{w^{(\mathrm{obs})}_a \ge 0} are the user-supplied row weights and \eqn{w_a}
#' are the Tukey biweights built from the weighted median/MAD. Weighted pairwise
#' behaves analogously on each column pair's overlap.
#'
#' \strong{MAD choice (\code{mad_consistent}).}
#' Setting \code{mad_consistent = TRUE} multiplies the raw MAD by 1.4826 inside
#' \eqn{u}. Equivalently, it uses an effective tuning constant
#' \eqn{c^\star = c \times 1.4826}. The default \code{FALSE} reproduces the convention
#' in Langfelder & Horvath (2012).
#'
#' \strong{Optional sparsification (\code{sparse_threshold}).}
#' If provided, entries with \eqn{|r| < \text{sparse\_threshold}} are set to 0 and the
#' result is returned as a \code{"ddiMatrix"} (diagonal is forced to 1). This is a
#' post-processing step that does not alter the per-pair estimates.
#'
#' \strong{Computation and threads.}
#' Columns are robust-standardised in parallel and the matrix is formed as
#' \eqn{R = \tilde X^\top \tilde X}. \code{n_threads} selects the number of OpenMP
#' threads; by default it uses \code{getOption("matrixCorr.threads", 1L)}.
#'
#' \strong{Basic properties.}
#' \eqn{\mathrm{bicor}(a x + b,\; c y + d) = \mathrm{sign}(ac)\,\mathrm{bicor}(x,y)}.
#' With no missing data (and with per-column hybrid/robust standardisation), the
#' output is symmetric and PSD. As with Pearson, affine equivariance does not hold
#' for the associated biweight midcovariance.
#'
#' @references
#' Langfelder, P. & Horvath, S. (2012).
#' Fast R Functions for Robust Correlations and Hierarchical Clustering.
#' Journal of Statistical Software, 46(11), 1–17. \doi{10.18637/jss.v046.i11}
#'
#' @importFrom Matrix Matrix
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(2000 * 40), 2000, 40)
#' R <- biweight_mid_corr(X, c_const = 9, max_p_outliers = 1,
#'                        pearson_fallback = "hybrid")
#' print(attr(R, "method"))
#'
#' @author Thiago de Paula Oliveira
#' @export
biweight_mid_corr <- function(
    data,
    c_const          = 9,
    max_p_outliers   = 1,
    pearson_fallback = c("hybrid", "none", "all"),
    na_method        = c("error", "pairwise"),
    mad_consistent   = FALSE,
    w                = NULL,
    sparse_threshold = NULL,
    n_threads        = getOption("matrixCorr.threads", 1L)
) {
  pf <- match.arg(pearson_fallback)
  pf_int <- switch(pf, "none" = 0L, "hybrid" = 1L, "all" = 2L)
  na_method <- match.arg(na_method)

  # --- checks
  if (!is.numeric(c_const) || length(c_const) != 1L || !(c_const > 0))
    stop("`c_const` must be a single positive numeric.", call. = FALSE)
  if (!is.numeric(max_p_outliers) || length(max_p_outliers) != 1L ||
      !(max_p_outliers > 0 && max_p_outliers <= 1))
    stop("`max_p_outliers` must be a single numeric in (0, 1].", call. = FALSE)
  if (!is.numeric(n_threads) || length(n_threads) != 1L || n_threads < 1)
    n_threads <- 1L
  n_threads <- as.integer(n_threads)

  if (!is.null(w)) {
    w <- as.numeric(w)
    if (anyNA(w) || any(w < 0))
      stop("`w` must be a non-negative numeric vector with no missing values.", call. = FALSE)
  }

  if (!is.null(sparse_threshold)) {
    if (!is.numeric(sparse_threshold) || length(sparse_threshold) != 1L ||
        sparse_threshold < 0)
      stop("`sparse_threshold` must be a single numeric >= 0.", call. = FALSE)
  }

  # --- validate/coerce input (allow NA only in pairwise mode)
  numeric_data <- if (na_method == "error") {
    validate_corr_input(data)
  } else {
    validate_corr_input(data, check_na = FALSE)
  }
  colnames_data <- colnames(numeric_data)

  # --- MAD consistency via effective c
  c_eff <- if (isTRUE(mad_consistent)) c_const * 1.4826 else c_const

  # --- choose backend
  if (is.null(w) && na_method == "error") {
    res <- bicor_matrix_cpp(
      numeric_data,
      c_const          = c_eff,
      maxPOutliers     = max_p_outliers,
      pearson_fallback = pf_int,
      n_threads        = n_threads
    )
  } else if (is.null(w) && na_method == "pairwise") {
    res <- bicor_matrix_pairwise_cpp(
      numeric_data,
      c_const          = c_eff,
      maxPOutliers     = max_p_outliers,
      pearson_fallback = pf_int,
      min_n            = 5L,
      n_threads        = n_threads
    )
  } else if (!is.null(w) && na_method == "error") {
    res <- bicor_matrix_weighted_cpp(
      numeric_data, w,
      c_const          = c_eff,
      maxPOutliers     = max_p_outliers,
      pearson_fallback = pf_int,
      n_threads        = n_threads
    )
  } else { # weighted + pairwise
    res <- bicor_matrix_weighted_pairwise_cpp(
      numeric_data, w,
      c_const          = c_eff,
      maxPOutliers     = max_p_outliers,
      pearson_fallback = pf_int,
      min_n            = 5L,
      n_threads        = n_threads
    )
  }

  # --- names & metadata
  colnames(res) <- rownames(res) <- colnames_data
  attr(res, "method")      <- "biweight_mid_correlation"
  attr(res, "description") <- paste0(
    "Median/MAD-based biweight mid-correlation (bicor); max_p_outliers = ", max_p_outliers,
    ", MAD = ", if (mad_consistent) "normal-consistent (1.4826 * raw)" else "raw",
    "; fallback = ", pf, "; NA mode = ", na_method, "."
  )
  attr(res, "package")     <- "matrixCorr"

  # --- default: dense matrix with S3 class (original behaviour)
  if (is.null(sparse_threshold)) {
    class(res) <- c("biweight_mid_corr", "matrix")
    return(res)
  }

  # --- optional sparse thresholding (S4 Matrix, no S3 class mutation)
  # zero-out small entries safely, keeping NAs intact
  idx <- !is.na(res) & (abs(res) < sparse_threshold)
  if (any(idx)) res[idx] <- 0
  diag(res) <- 1

  res_sparse <- Matrix::Matrix(res, sparse = TRUE)
  # carry metadata; do not overwrite S4 class
  attr(res_sparse, "method")      <- attr(res, "method")
  attr(res_sparse, "description") <- paste0(attr(res, "description"),
                                            " Sparse threshold = ", sparse_threshold, ".")
  attr(res_sparse, "package")     <- "matrixCorr"
  # Return S4 dsCMatrix with attrs; no S3 class assignment here
  res_sparse
}



#' @rdname biweight_mid_corr
#' @method print biweight_mid_corr
#' @title Print Method for \code{biweight_mid_corr} Objects
#'
#' @description
#' Prints a matrix with a compact header,
#' optional truncation for large matrices, and a small summary of
#' off-diagonal values.
#'
#' @param x An object of class \code{biweight_mid_corr}.
#' @param digits Integer; number of decimal places used for the matrix.
#' @param max_rows Optional integer; maximum number of rows to display
#'   (default shows all).
#' @param max_cols Optional integer; maximum number of columns to display
#'   (default shows all).
#' @param width Integer; target console width for wrapping header text.
#' @param na_print Character; how to display missing values.
#' @param ... Additional arguments passed to \code{print()}.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.biweight_mid_corr <- function(x,
                                    digits   = 4,
                                    max_rows = NULL,
                                    max_cols = NULL,
                                    width    = getOption("width", 80L),
                                    na_print = "NA",
                                    ...) {
  if (!inherits(x, "biweight_mid_corr"))
    stop("x must be of class 'biweight_mid_corr'.")

  # ---- header ----
  cat("Biweight mid-correlation matrix (bicor):\n")

  method <- attr(x, "method")
  if (!is.null(method))
    cat("  method: ", method, "\n", sep = "")

  desc <- attr(x, "description")
  if (!is.null(desc)) {
    wrap <- strwrap(desc, width = max(1, width - 2), exdent = 2)
    cat("  ", paste(wrap, collapse = "\n"), "\n", sep = "")
  }

  pkg <- attr(x, "package")
  if (!is.null(pkg))
    cat("  package: ", pkg, "\n", sep = "")

  nr <- nrow(x); nc <- ncol(x)
  cat(sprintf("  dimensions: %d x %d%s\n",
              nr, nc,
              if (inherits(x, "dsCMatrix")) " (sparse)" else ""))

  # ---- small numeric summary (off-diagonals only) ----
  as_dense <- if (inherits(x, "dsCMatrix")) Matrix::as.matrix(x) else as.matrix(x)
  if (nr >= 2L && nc >= 2L) {
    off <- as_dense
    diag(off) <- NA_real_
    rng <- range(off, na.rm = TRUE)
    na_prop <- mean(!is.finite(off))
    if (is.finite(rng[1]) && is.finite(rng[2])) {
      cat(sprintf("  off-diagonal range: [%.4f, %.4f]; missing: %.1f%%\n",
                  rng[1], rng[2], 100*na_prop))
    } else {
      cat(sprintf("  off-diagonal range: <all missing>; missing: %.1f%%\n",
                  100*na_prop))
    }
  }

  # ---- body ----
  m <- as_dense
  attributes(m) <- attributes(m)[c("dim", "dimnames")]  # strip attrs

  # truncation logic
  if (!is.null(max_rows) || !is.null(max_cols)) {
    r <- if (is.null(max_rows)) nr else min(nr, as.integer(max_rows))
    c <- if (is.null(max_cols)) nc else min(nc, as.integer(max_cols))
    m2 <- m[seq_len(r), seq_len(c), drop = FALSE]
    m2 <- round(m2, digits)
    print(m2, na.print = na_print, ...)
    if (nr > r || nc > c) {
      cat(sprintf("... omitted: %d rows, %d cols\n", nr - r, nc - c))
    }
  } else {
    print(round(m, digits), na.print = na_print, ...)
  }

  invisible(x)
}

#' @rdname biweight_mid_corr
#' @method plot biweight_mid_corr
#' @title Plot Method for \code{biweight_mid_corr} Objects
#'
#' @description Produces a \pkg{ggplot2} heatmap of the biweight
#' mid-correlation matrix. Optionally reorders variables via hierarchical
#' clustering on \eqn{1 - r_{\text{bicor}}}, and can show only a triangle.
#'
#' @param x An object of class \code{biweight_mid_corr}.
#' @param title Plot title. Default is \code{"Biweight mid-correlation heatmap"}.
#' @param reorder Character; one of \code{"none"} (default) or \code{"hclust"}.
#'   If \code{"hclust"}, variables are reordered by complete-linkage clustering
#'   on the distance \eqn{d = 1 - r}, after replacing \code{NA} by 0 for
#'   clustering purposes only.
#' @param triangle One of \code{"full"} (default), \code{"lower"}, or \code{"upper"}
#'   to display the full matrix or a single triangle.
#' @param low_color,mid_color,high_color Colours for the gradient in
#'   \code{scale_fill_gradient2}. Defaults are \code{"indianred1"},
#'   \code{"white"}, \code{"steelblue1"}.
#' @param value_text_size Numeric; font size for cell labels. Set to \code{NULL}
#'   to suppress labels (recommended for large matrices).
#' @param na_fill Fill colour for \code{NA} cells. Default \code{"grey90"}.
#' @param ... Additional arguments passed to \code{ggplot2::theme()} or other layers.
#'
#' @return A \code{ggplot} object.
#' @import ggplot2
#' @importFrom stats as.dist hclust
#' @export
plot.biweight_mid_corr <- function(
    x,
    title = "Biweight mid-correlation heatmap",
    reorder  = c("none", "hclust"),
    triangle = c("full", "lower", "upper"),
    low_color = "indianred1",
    mid_color = "white",
    high_color = "steelblue1",
    value_text_size = 3,
    na_fill = "grey90",
    ...
) {
  if (!inherits(x, "biweight_mid_corr"))
    stop("x must be of class 'biweight_mid_corr'.")

  reorder  <- match.arg(reorder)
  triangle <- match.arg(triangle)

  mat <- as.matrix(x)

  # Optional reordering via robust-inspired clustering on 1 - r
  if (reorder == "hclust" && ncol(mat) > 1) {
    R_fill <- mat
    R_fill[is.na(R_fill)] <- 0   # for clustering only
    R_fill[R_fill > 1] <- 1; R_fill[R_fill < -1] <- -1
    d <- stats::as.dist(1 - R_fill)
    ord <- stats::hclust(d, method = "complete")$order
    mat <- mat[ord, ord, drop = FALSE]
  }

  # Prepare long format with indices for triangle filtering
  rn <- rownames(mat); cn <- colnames(mat)
  if (is.null(rn)) rn <- seq_len(nrow(mat))
  if (is.null(cn)) cn <- seq_len(ncol(mat))
  df <- as.data.frame(as.table(mat))
  names(df) <- c("Var1", "Var2", "bicor")
  df$Var1 <- factor(df$Var1, levels = rev(rn))
  df$Var2 <- factor(df$Var2, levels = cn)

  # Triangle filtering
  if (triangle != "full") {
    # map factors back to integer indices
    i <- as.integer(df$Var1)         # note: Var1 is reversed
    j <- as.integer(df$Var2)
    # Because Var1 is reversed, convert to row indices in original order
    n <- length(levels(df$Var1))
    i_true <- n - i + 1
    keep <- switch(triangle,
                   lower = i_true >= j,
                   upper = i_true <= j)
    df <- df[keep, , drop = FALSE]
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(Var2, Var1, fill = bicor)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(
      low = low_color, mid = mid_color, high = high_color,
      midpoint = 0, limits = c(-1, 1), na.value = na_fill, name = "bicor"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank(),
      ...
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title, x = NULL, y = NULL)

  if (!is.null(value_text_size) && is.finite(value_text_size)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = ifelse(is.na(bicor), "NA", sprintf("%.2f", bicor))),
      size = value_text_size, color = "black"
    )
  }

  return(p)
}

