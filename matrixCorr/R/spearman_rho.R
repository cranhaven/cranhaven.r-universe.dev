#' @title Pairwise Spearman's rank correlation
#'
#' @description
#' Computes all pairwise Spearman's rank correlation coefficients for the
#' numeric columns of a matrix or data frame using a high-performance
#' 'C++' backend.
#'
#' This function ranks the data and computes Pearson correlation on ranks,
#' which is equivalent to Spearman’s rho. It supports large datasets and
#' is optimized in 'C++' for performance.
#'
#' @param data A numeric matrix or a data frame with at least two numeric
#' columns. All non-numeric columns will be excluded. Each column must have
#' at least two non-missing values and contain no NAs.
#'
#' @return A symmetric numeric matrix where the \code{(i, j)}-th element is
#' the Spearman correlation between the \code{i}-th and \code{j}-th
#' numeric columns of the input.
#'
#' @details
#' For each column \eqn{j=1,\ldots,p}, let
#' \eqn{R_{\cdot j} \in \{1,\ldots,n\}^n} denote the (mid-)ranks of
#' \eqn{X_{\cdot j}}, assigning average ranks to ties. The mean rank is
#' \eqn{\bar R_j = (n+1)/2} regardless of ties. Define the centred rank
#' vectors \eqn{\tilde R_{\cdot j} = R_{\cdot j} - \bar R_j \mathbf{1}},
#' where \eqn{\mathbf{1}\in\mathbb{R}^n} is the all-ones vector. The
#' Spearman correlation between columns \eqn{i} and \eqn{j} is the Pearson
#' correlation of their rank vectors:
#' \deqn{
#' \rho_S(i,j) \;=\;
#' \frac{\sum_{k=1}^n (R_{ki}-\bar R_i)(R_{kj}-\bar R_j)}
#'      {\sqrt{\sum_{k=1}^n (R_{ki}-\bar R_i)^2}\;
#'       \sqrt{\sum_{k=1}^n (R_{kj}-\bar R_j)^2}}.
#' }
#' In matrix form, with \eqn{R=[R_{\cdot 1},\ldots,R_{\cdot p}]},
#' \eqn{\mu=(n+1)\mathbf{1}_p/2} for \eqn{\mathbf{1}_p\in\mathbb{R}^p}, and
#' \eqn{S_R=\bigl(R-\mathbf{1}\mu^\top\bigr)^\top
#'            \bigl(R-\mathbf{1}\mu^\top\bigr)/(n-1)},
#' the Spearman correlation matrix is
#' \deqn{
#' \widehat{\rho}_S \;=\; D^{-1/2} S_R D^{-1/2}, \qquad
#' D \;=\; \mathrm{diag}(\mathrm{diag}(S_R)).
#' }
#' When there are no ties, the familiar rank-difference formula obtains
#' \deqn{
#' \rho_S(i,j) \;=\; 1 - \frac{6}{n(n^2-1)} \sum_{k=1}^n d_k^2,
#' \quad d_k \;=\; R_{ki}-R_{kj},
#' }
#' but this expression does \emph{not} hold under ties; computing Pearson on
#' mid-ranks (as above) is the standard tie-robust approach. Without ties,
#' \eqn{\mathrm{Var}(R_{\cdot j})=(n^2-1)/12}; with ties, the variance is
#' smaller.
#'
#' \eqn{\rho_S(i,j) \in [-1,1]} and \eqn{\widehat{\rho}_S} is symmetric
#' positive semi-definite by construction (up to floating-point error). The
#' implementation symmetrises the result to remove round-off asymmetry.
#' Spearman’s correlation is invariant to strictly monotone transformations
#' applied separately to each variable.
#'
#' \strong{Computation.} Each column is ranked (mid-ranks) to form \eqn{R}.
#' The product \eqn{R^\top R} is computed via a 'BLAS' symmetric rank update
#' ('SYRK'), and centred using
#' \deqn{
#' (R-\mathbf{1}\mu^\top)^\top (R-\mathbf{1}\mu^\top)
#' \;=\; R^\top R \;-\; n\,\mu\mu^\top,
#' }
#' avoiding an explicit centred copy. Division by \eqn{n-1} yields the sample
#' covariance of ranks; standardising by \eqn{D^{-1/2}} gives \eqn{\widehat{\rho}_S}.
#' Columns with zero rank variance (all values equal) are returned as \code{NA}
#' along their row/column; the corresponding diagonal entry is also \code{NA}.
#'
#' Ranking costs
#' \eqn{O\!\bigl(p\,n\log n\bigr)}; forming and normalising
#' \eqn{R^\top R} costs \eqn{O\!\bigl(n p^2\bigr)} with \eqn{O(p^2)} additional
#' memory. 'OpenMP' parallelism is used across columns for ranking, and a 'BLAS'
#' 'SYRK' kernel is used for the matrix product when available.
#'
#' @note Missing values are not allowed. Columns with fewer than two
#' observations are excluded.
#'
#' @references
#' Spearman, C. (1904). The proof and measurement of association between
#' two things. International Journal of Epidemiology, 39(5), 1137-1150.
#'
#' @examples
#' ## Monotone transformation invariance (Spearman is rank-based)
#' set.seed(123)
#' n <- 400; p <- 6; rho <- 0.6
#' # AR(1) correlation
#' Sigma <- rho^abs(outer(seq_len(p), seq_len(p), "-"))
#' L <- chol(Sigma)
#' X <- matrix(rnorm(n * p), n, p) %*% L
#' colnames(X) <- paste0("V", seq_len(p))
#'
#' # Monotone transforms to some columns
#' X_mono <- X
#' # exponential
#' X_mono[, 1] <- exp(X_mono[, 1])
#' # softplus
#' X_mono[, 2] <- log1p(exp(X_mono[, 2]))
#' # odd monotone polynomial
#' X_mono[, 3] <- X_mono[, 3]^3
#'
#' sp_X <- spearman_rho(X)
#' sp_m <- spearman_rho(X_mono)
#'
#' # Spearman should be (nearly) unchanged under monotone transformations
#' round(max(abs(sp_X - sp_m)), 3)
#' # heatmap of Spearman correlations
#' plot(sp_X)
#'
#' ## Ties handled via mid-ranks
#' tied <- cbind(
#'   # many ties
#'   a = rep(1:5, each = 20),
#'   # noisy reverse order
#'   b = rep(5:1, each = 20) + rnorm(100, sd = 0.1),
#'   # ordinal with ties
#'   c = as.numeric(gl(10, 10))
#' )
#' sp_tied <- spearman_rho(tied)
#' print(sp_tied, digits = 2)
#'
#' ## Bivariate normal, theoretical Spearman's rho
#' ## For BVN with Pearson correlation r, rho_S = (6/pi) * asin(r/2).
#' r_target <- c(-0.8, -0.4, 0, 0.4, 0.8)
#' n2 <- 200
#' est <- true_corr <- numeric(length(r_target))
#' for (i in seq_along(r_target)) {
#'   R2 <- matrix(c(1, r_target[i], r_target[i], 1), 2, 2)
#'   Z  <- matrix(rnorm(n2 * 2), n2, 2) %*% chol(R2)
#'   s  <- spearman_rho(Z)
#'   est[i]  <- s[1, 2]
#'   true_corr[i] <- (6 / pi) * asin(r_target[i] / 2)
#' }
#' cbind(r_target, est = round(est, 3), theory = round(true_corr, 3))
#'
#' @useDynLib matrixCorr, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @seealso \code{\link{print.spearman_rho}}, \code{\link{plot.spearman_rho}}
#' @author Thiago de Paula Oliveira
#' @export
spearman_rho <- function(data) {
  numeric_data <- validate_corr_input(data)
  colnames_data <- colnames(numeric_data)
  result <- spearman_matrix_cpp(numeric_data)
  colnames(result) <- rownames(result) <- colnames_data
  attr(result, "method") <- "spearman"
  attr(result, "description") <- "Pairwise Spearman's rank correlation matrix"
  attr(result, "package") <- "matrixCorr"
  class(result) <- c("spearman_rho", "matrix")
  return(result)
}

#' @rdname spearman_rho
#' @method print spearman_rho
#' @title Print Method for \code{spearman_rho} Objects
#'
#' @description Prints a summary of the Spearman's correlation matrix,
#' including description and method metadata.
#'
#' @param x An object of class \code{spearman_rho}.
#' @param digits Integer; number of decimal places to print.
#' @param max_rows Optional integer; maximum number of rows to display.
#'  If \code{NULL}, all rows are shown.
#' @param max_cols Optional integer; maximum number of columns to display.
#' If \code{NULL}, all columns are shown.
#' @param ... Additional arguments passed to \code{print}.
#'
#' @return Invisibly returns the \code{spearman_rho} object.
#' @export
print.spearman_rho <- function(x, digits = 4, max_rows = NULL, max_cols = NULL, ...) {
  cat("Spearman correlation matrix:\n")

  # Strip non-essential attributes
  m <- as.matrix(x)
  attributes(m) <- attributes(m)[c("dim", "dimnames")]

  # Optional truncation for large matrices
  if (!is.null(max_rows) || !is.null(max_cols)) {
    nr <- nrow(m); nc <- ncol(m)
    r  <- if (is.null(max_rows)) nr else min(nr, max_rows)
    c  <- if (is.null(max_cols)) nc else min(nc, max_cols)
    mm <- round(m[seq_len(r), seq_len(c), drop = FALSE], digits)
    print(mm, ...)
    if (nr > r || nc > c) {
      cat(sprintf("... omitted: %d rows, %d cols\n", nr - r, nc - c))
    }
  } else {
    print(round(m, digits), ...)
  }

  invisible(x)
}


#' @rdname spearman_rho
#' @method plot spearman_rho
#' @title Plot Method for \code{spearman_rho} Objects
#'
#' @description Generates a ggplot2-based heatmap of the Spearman's rank
#' correlation matrix.
#'
#' @param x An object of class \code{spearman_rho}.
#' @param title Plot title. Default is \code{"Spearman's rank correlation
#' heatmap"}.
#' @param low_color Color for the minimum rho value. Default is
#'  \code{"indianred1"}.
#' @param high_color Color for the maximum rho value. Default is
#' \code{"steelblue1"}.
#' @param mid_color Color for zero correlation. Default is \code{"white"}.
#' @param value_text_size Font size for displaying correlation values. Default
#' is \code{4}.
#' @param ... Additional arguments passed to \code{ggplot2::theme()} or other
#' \code{ggplot2} layers.
#'
#' @return A \code{ggplot} object representing the heatmap.
#' @import ggplot2
#' @export
plot.spearman_rho <-
  function(x, title = "Spearman's rank correlation heatmap",
           low_color = "indianred1", high_color = "steelblue1",
           mid_color = "white", value_text_size = 4, ...) {

  if (!inherits(x, "spearman_rho")) {
    stop("x must be of class 'spearman_rho'.")
  }

  mat <- as.matrix(x)
  df <- as.data.frame(as.table(mat))
  colnames(df) <- c("Var1", "Var2", "Rho")

  df$Var1 <- factor(df$Var1, levels = rev(unique(df$Var1)))

  p <- ggplot2::ggplot(df, ggplot2::aes(Var2, Var1, fill = Rho)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Rho)),
                       size = value_text_size, color = "black") +
    ggplot2::scale_fill_gradient2(
      low = low_color, high = high_color, mid = mid_color,
      midpoint = 0, limit = c(-1, 1), name = "Rho"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank(),
      ...
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title, x = NULL, y = NULL)

  return(p)
}
