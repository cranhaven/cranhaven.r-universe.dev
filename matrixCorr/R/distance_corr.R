#' @title Pairwise Distance Correlation (dCor)
#'
#' @description
#' Computes all pairwise \emph{distance correlations} using the unbiased
#' U-statistic estimator for the numeric columns of a matrix or data frame,
#' via a high-performance 'C++' backend ('OpenMP'-parallelised).
#' Distance correlation detects general (including non-linear and non-monotonic)
#' dependence between variables; unlike Pearson or Spearman, it is zero
#' (in population) if and only if the variables are independent.
#'
#' @param data A numeric matrix or a data frame with at least two numeric
#' columns. All non-numeric columns are dropped. Columns must be numeric
#' and contain no \code{NA}s.
#'
#' @return A symmetric numeric matrix where the \code{(i, j)} entry is the
#' unbiased distance correlation between the \code{i}-th and \code{j}-th
#' numeric columns. The object has class \code{distance_corr} with attributes
#' \code{method = "distance_correlation"}, \code{description}, and
#' \code{package = "matrixCorr"}.
#'
#' @details
#' Let \eqn{x \in \mathbb{R}^n} and \eqn{D^{(x)}} be the pairwise distance matrix
#' with zero diagonal: \eqn{D^{(x)}_{ii} = 0}, \eqn{D^{(x)}_{ij} = |x_i - x_j|} for
#' \eqn{i \neq j}. Define row sums \eqn{r^{(x)}_i = \sum_{k \neq i} D^{(x)}_{ik}} and
#' grand sum \eqn{S^{(x)} = \sum_{i \neq k} D^{(x)}_{ik}}. The U-centred matrix is
#' \deqn{A^{(x)}_{ij} =
#'       \begin{cases}
#'         D^{(x)}_{ij} - \dfrac{r^{(x)}_i + r^{(x)}_j}{n - 2}
#'         + \dfrac{S^{(x)}}{(n - 1)(n - 2)}, & i \neq j,\\[6pt]
#'         0, & i = j~.
#'       \end{cases}}
#' For two variables \eqn{x,y}, the unbiased distance covariance and variances are
#' \deqn{\widehat{\mathrm{dCov}}^2_u(x,y) = \frac{2}{n(n-3)} \sum_{i<j} A^{(x)}_{ij} A^{(y)}_{ij}
#' \;=\; \frac{1}{n(n-3)} \sum_{i \neq j} A^{(x)}_{ij} A^{(y)}_{ij},}
#' with \eqn{\widehat{\mathrm{dVar}}^2_u(x)} defined analogously from \eqn{A^{(x)}}.
#' The unbiased distance correlation is
#' \deqn{\widehat{\mathrm{dCor}}_u(x,y) =
#'       \frac{\widehat{\mathrm{dCov}}_u(x,y)}
#'            {\sqrt{\widehat{\mathrm{dVar}}_u(x)\,\widehat{\mathrm{dVar}}_u(y)}} \in [0,1].}
#'
#' @note Requires \eqn{n \ge 4}. Columns with (near) zero unbiased distance
#' variance yield \code{NA} in their row/column. Computation is \eqn{O(n^2)} per
#' pair.
#'
#' @references
#' Székely, G. J., Rizzo, M. L., & Bakirov, N. K. (2007).
#' Measuring and testing dependence by correlation of distances.
#' \emph{Annals of Statistics}, 35(6), 2769–2794.
#'
#' Székely, G. J., & Rizzo, M. L. (2013).
#' The distance correlation t-test of independence.
#' \emph{Journal of Multivariate Analysis}, 117, 193-213.
#'
#' @examples
#' ##Independent variables -> dCor ~ 0
#' set.seed(1)
#' X <- cbind(a = rnorm(200), b = rnorm(200))
#' D <- distance_corr(X)
#' print(D, digits = 3)
#'
#' ## Non-linear dependence: Pearson ~ 0, but unbiased dCor > 0
#' set.seed(42)
#' n <- 200
#' x <- rnorm(n)
#' y <- x^2 + rnorm(n, sd = 0.2)
#' XY <- cbind(x = x, y = y)
#' D2 <- distance_corr(XY)
#' # Compare Pearson vs unbiased distance correlation
#' round(c(pearson = cor(XY)[1, 2], dcor = D2["x", "y"]), 3)
#' plot(D2, title = "Unbiased distance correlation (non-linear example)")
#'
#' ## Small AR(1) multivariate normal example
#' set.seed(7)
#' p <- 5; n <- 150; rho <- 0.6
#' Sigma <- rho^abs(outer(seq_len(p), seq_len(p), "-"))
#' X3 <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
#' colnames(X3) <- paste0("V", seq_len(p))
#' D3 <- distance_corr(X3)
#' print(D3[1:3, 1:3], digits = 2)
#'
#' @author Thiago de paula Oliveira
#'
#' @export
distance_corr <- function(data) {
  numeric_data <- validate_corr_input(data)
  colnames_data <- colnames(numeric_data)

  dcor_matrix <- ustat_dcor_matrix_cpp(numeric_data)
  colnames(dcor_matrix) <- rownames(dcor_matrix) <- colnames_data

  attr(dcor_matrix, "method") <- "distance_correlation"
  attr(dcor_matrix, "description") <- "Pairwise distance correlation matrix (unbiased)"
  attr(dcor_matrix, "package") <- "matrixCorr"
  class(dcor_matrix) <- c("distance_corr", "matrix")
  dcor_matrix
}

#' @rdname distance_corr
#' @method print distance_corr
#' @title Print Method for \code{distance_corr} Objects
#'
#' @description Prints a summary of the distance correlation matrix with
#' optional truncation for large objects.
#'
#' @param x An object of class \code{distance_corr}.
#' @param digits Integer; number of decimal places to print.
#' @param max_rows Optional integer; maximum number of rows to display.
#' If \code{NULL}, all rows are shown.
#' @param max_cols Optional integer; maximum number of columns to display.
#' If \code{NULL}, all columns are shown.
#' @param ... Additional arguments passed to \code{print}.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.distance_corr <- function(x, digits = 4, max_rows = NULL,
                            max_cols = NULL, ...) {
  cat("Distance correlation (dCor) matrix:\n")
  m <- as.matrix(x)
  attributes(m) <- attributes(m)[c("dim", "dimnames")]

  if (!is.null(max_rows) || !is.null(max_cols)) {
    nr <- nrow(m); nc <- ncol(m)
    r  <- if (is.null(max_rows)) nr else min(nr, max_rows)
    c  <- if (is.null(max_cols)) nc else min(nc, max_cols)
    m2 <- round(m[seq_len(r), seq_len(c), drop = FALSE], digits)
    print(m2, ...)
    if (nr > r || nc > c) {
      cat(sprintf("... omitted: %d rows, %d cols\n", nr - r, nc - c))
    }
  } else {
    print(round(m, digits), ...)
  }
  invisible(x)
}

#' @rdname distance_corr
#' @method plot distance_corr
#' @title Plot Method for \code{distance_corr} Objects
#'
#' @description Generates a ggplot2 heatmap of the distance correlation matrix.
#' Distance correlation is non-negative; the fill scale spans \code{[0, 1]}.
#'
#' @param x An object of class \code{distance_corr}.
#' @param title Plot title. Default is \code{"Distance correlation heatmap"}.
#' @param low_color Colour for zero correlation. Default is \code{"white"}.
#' @param high_color Colour for strong correlation. Default is \code{"steelblue1"}.
#' @param value_text_size Font size for displaying values. Default is \code{4}.
#' @param ... Additional arguments passed to \code{ggplot2::theme()} or other
#' \code{ggplot2} layers.
#'
#' @return A \code{ggplot} object representing the heatmap.
#' @import ggplot2
#' @export
plot.distance_corr <-
  function(x, title = "Distance correlation heatmap",
           low_color = "white", high_color = "steelblue1",
           value_text_size = 4, ...) {

    if (!inherits(x, "distance_corr")) {
      stop("x must be of class 'distance_corr'.")
    }
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Package 'ggplot2' is required for plotting.")
    }

    mat <- as.matrix(x)
    df <- as.data.frame(as.table(mat))
    colnames(df) <- c("Var1", "Var2", "dCor")

    df$Var1 <- factor(df$Var1, levels = rev(unique(df$Var1)))

    p <- ggplot2::ggplot(df, ggplot2::aes(Var2, Var1, fill = dCor)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", dCor)),
                         size = value_text_size, color = "black") +
      ggplot2::scale_fill_gradient(
        low = low_color, high = high_color,
        limits = c(0, 1), name = "dCor"
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
