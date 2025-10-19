#' @title Partial correlation matrix (sample / ridge / OAS)
#'
#' @description
#' Computes the Gaussian partial correlation matrix from a numeric data frame
#' or matrix. The covariance matrix can be estimated using:
#' \itemize{
#'   \item \strong{Unbiased sample covariance}: the standard empirical
#'   covariance estimator.
#'   \item \strong{Ridge-regularised covariance}: adds a positive ridge
#'   penalty to improve stability when the covariance matrix is near-singular.
#'   \item \strong{OAS shrinkage to a scaled identity}: recommended when
#'   \eqn{p \gg n}, as it reduces estimation error by shrinking towards a
#'   scaled identity matrix.
#' }
#'
#' The method uses a high-performance 'C++' backend.
#'
#' @param data A numeric matrix or data frame with at least two numeric columns.
#'   Non-numeric columns are ignored.
#' @param method Character; one of \code{"oas"}, \code{"ridge"}, \code{"sample"}.
#'   Default \code{"oas"}.
#' @param lambda Numeric \eqn{\ge 0}; ridge penalty added to the covariance
#'   diagonal when \code{method = "ridge"}. Ignored otherwise. Default \code{1e-3}.
#' @param return_cov_precision Logical; if \code{TRUE}, also return the
#'   covariance (\code{cov}) and precision (\code{precision}) matrices used to
#'   form the partial correlations. Default to \code{FALSE}
#'
#' @return An object of class \code{"partial_corr"} (a list) with elements:
#'   \itemize{
#'     \item \code{pcor}: \eqn{p \times p} partial correlation matrix.
#'     \item \code{cov} (if requested): covariance matrix used.
#'     \item \code{precision} (if requested): precision matrix \eqn{\Theta}.
#'     \item \code{method}: the estimator used (\code{"oas"}, \code{"ridge"},
#'     or \code{"sample"}).
#'     \item \code{lambda}: ridge penalty (or \code{NA_real_}).
#'     \item \code{rho}: OAS shrinkage weight in \eqn{[0,1]} (or \code{NA_real_}).
#'     \item \code{jitter}: diagonal jitter added (if any) to ensure positive
#'     definiteness.
#'   }
#'
#' @details
#' \strong{Statistical overview.} Given an \eqn{n \times p} data matrix \eqn{X}
#' (rows are observations, columns are variables), the routine estimates a
#' \emph{partial correlation} matrix via the precision (inverse covariance)
#' matrix. Let \eqn{\mu} be the vector of column means and
#' \deqn{S = (X - \mathbf{1}\mu)^\top (X - \mathbf{1}\mu)}
#' be the centred cross-product matrix computed without forming a centred copy
#' of \eqn{X}. Two conventional covariance scalings are formed:
#' \deqn{\hat\Sigma_{\mathrm{MLE}} = S/n, \qquad
#'       \hat\Sigma_{\mathrm{unb}} = S/(n-1).}
#'
#' \itemize{
#'   \item \emph{Sample:} \eqn{\Sigma = \hat\Sigma_{\mathrm{unb}}}.
#'   \item \emph{Ridge:} \eqn{\Sigma = \hat\Sigma_{\mathrm{unb}} + \lambda I_p}
#'         with user-supplied \eqn{\lambda \ge 0} (diagonal inflation).
#'   \item \emph{OAS (Oracle Approximating Shrinkage):}
#'         shrink \eqn{\hat\Sigma_{\mathrm{MLE}}} towards a scaled identity
#'         target \eqn{\mu_I I_p}, where \eqn{\mu_I = \mathrm{tr}(\hat\Sigma_{\mathrm{MLE}})/p}.
#'         The data-driven weight \eqn{\rho \in [0,1]} is
#'         \deqn{\rho = \min\!\left\{1,\max\!\left(0,\;
#'         \frac{(1-\tfrac{2}{p})\,\mathrm{tr}(\hat\Sigma_{\mathrm{MLE}}^2)
#'               + \mathrm{tr}(\hat\Sigma_{\mathrm{MLE}})^2}
#'              {(n + 1 - \tfrac{2}{p})
#'               \left[\mathrm{tr}(\hat\Sigma_{\mathrm{MLE}}^2)
#'               - \tfrac{\mathrm{tr}(\hat\Sigma_{\mathrm{MLE}})^2}{p}\right]}
#'         \right)\right\},}
#'         and
#'         \deqn{\Sigma = (1-\rho)\,\hat\Sigma_{\mathrm{MLE}} + \rho\,\mu_I I_p.}
#' }
#'
#' The method then ensures positive definiteness of \eqn{\Sigma} (adding a very
#' small diagonal \emph{jitter} only if necessary) and computes the precision
#' matrix \eqn{\Theta = \Sigma^{-1}}. Partial correlations are obtained by
#' standardising the off-diagonals of \eqn{\Theta}:
#' \deqn{\mathrm{pcor}_{ij} \;=\;
#'       -\,\frac{\theta_{ij}}{\sqrt{\theta_{ii}\,\theta_{jj}}}, \qquad
#'       \mathrm{pcor}_{ii}=1.}
#'
#' \strong{Interpretation.} For Gaussian data, \eqn{\mathrm{pcor}_{ij}} equals
#' the correlation between residuals from regressing variable \eqn{i} and
#' variable \eqn{j} on all the remaining variables; equivalently, it encodes
#' conditional dependence in a Gaussian graphical model, where
#' \eqn{\mathrm{pcor}_{ij}=0} if variables \eqn{i} and \eqn{j} are
#' conditionally independent given the others. Partial correlations are
#' invariant to separate rescalings of each
#' variable; in particular, multiplying \eqn{\Sigma} by any positive scalar
#' leaves the partial correlations unchanged.
#'
#' \strong{Why shrinkage/regularisation?} When \eqn{p \ge n}, the sample
#' covariance is singular and inversion is ill-posed. Ridge and OAS both yield
#' well-conditioned \eqn{\Sigma}. Ridge adds a fixed \eqn{\lambda} on the
#' diagonal, whereas OAS shrinks adaptively towards \eqn{\mu_I I_p} with a
#' weight chosen to minimise (approximately) the Frobenius risk under a
#' Gaussian model, often improving mean–square accuracy in high dimension.
#'
#' \strong{Computational notes.} The implementation forms \eqn{S} using 'BLAS'
#' \code{syrk} when available and constructs partial correlations by traversing
#' only the upper triangle with 'OpenMP' parallelism. Positive definiteness is
#' verified via a Cholesky factorisation; if it fails, a tiny diagonal jitter is
#' increased geometrically up to a small cap, at which point the routine
#' signals an error.
#'
#' @examples
#' ## Structured MVN with known partial correlations
#' set.seed(42)
#' p <- 12; n <- 1000
#'
#' ## Build a tri-diagonal precision (Omega) so the true partial correlations
#' ## are sparse
#' phi <- 0.35
#' Omega <- diag(p)
#' for (j in 1:(p - 1)) {
#'   Omega[j, j + 1] <- Omega[j + 1, j] <- -phi
#' }
#' ## Strict diagonal dominance
#' diag(Omega) <- 1 + 2 * abs(phi) + 0.05
#' Sigma <- solve(Omega)
#'
#' ## Upper Cholesky
#' L <- chol(Sigma)
#' Z <- matrix(rnorm(n * p), n, p)
#' X <- Z %*% L
#' colnames(X) <- sprintf("V%02d", seq_len(p))
#'
#' pc <- partial_correlation(X, method = "oas")
#'
#' ## True partial correlation from Omega
#' pcor_true <- -Omega / sqrt(diag(Omega) %o% diag(Omega))
#' diag(pcor_true) <- 1
#'
#' ## Quick visual check (first 5x5 block)
#' round(pc$pcor[1:5, 1:5], 2)
#' round(pcor_true[1:5, 1:5], 2)
#'
#' ## Plot method
#' plot(pc)
#'
#' ## High-dimensional case p >> n
#' set.seed(7)
#' n <- 60; p <- 120
#'
#' ar_block <- function(m, rho = 0.6) rho^abs(outer(seq_len(m), seq_len(m), "-"))
#'
#' ## Two AR(1) blocks on the diagonal
#' if (requireNamespace("Matrix", quietly = TRUE)) {
#'   Sigma_hd <- as.matrix(Matrix::bdiag(ar_block(60, 0.6), ar_block(60, 0.6)))
#' } else {
#'   Sigma_hd <- rbind(
#'     cbind(ar_block(60, 0.6), matrix(0, 60, 60)),
#'     cbind(matrix(0, 60, 60), ar_block(60, 0.6))
#'   )
#' }
#'
#' L <- chol(Sigma_hd)
#' X_hd <- matrix(rnorm(n * p), n, p) %*% L
#' colnames(X_hd) <- paste0("G", seq_len(p))
#'
#' pc_oas   <-
#'  partial_correlation(X_hd, method = "oas",   return_cov_precision = TRUE)
#' pc_ridge <-
#'  partial_correlation(X_hd, method = "ridge", lambda = 1e-2,
#'                      return_cov_precision = TRUE)
#' pc_samp  <-
#'  partial_correlation(X_hd, method = "sample", return_cov_precision = TRUE)
#'
#' ## Show how much diagonal regularisation was used
#' c(oas_jitter = pc_oas$jitter,
#'   ridge_lambda = pc_ridge$lambda,
#'   sample_jitter = pc_samp$jitter)
#'
#' ## Compare conditioning of the estimated covariance matrices
#' c(kappa_oas = kappa(pc_oas$cov),
#'   kappa_ridge = kappa(pc_ridge$cov),
#'   kappa_sample = kappa(pc_samp$cov))
#'
#' ## Simple conditional-dependence graph from partial correlations
#' pcor <- pc_oas$pcor
#' vals <- abs(pcor[upper.tri(pcor, diag = FALSE)])
#' thresh <- quantile(vals, 0.98)  # top 2%
#' edges  <- which(abs(pcor) > thresh & upper.tri(pcor), arr.ind = TRUE)
#' head(data.frame(i = colnames(pcor)[edges[,1]],
#'                 j = colnames(pcor)[edges[,2]],
#'                 pcor = round(pcor[edges], 2)))
#'
#' @references
#' Chen, Y., Wiesel, A., & Hero, A. O. III (2011).
#' Robust Shrinkage Estimation of High-dimensional Covariance Matrices.
#' IEEE Transactions on Signal Processing.
#'
#' @references
#' Ledoit, O., & Wolf, M. (2004).
#' A well-conditioned estimator for large-dimensional covariance matrices.
#' Journal of Multivariate Analysis, 88(2), 365–411.
#'
#' @references
#' Schafer, J., & Strimmer, K. (2005).
#' A shrinkage approach to large-scale covariance matrix estimation and
#' implications for functional genomics.
#' Statistical Applications in Genetics and Molecular Biology, 4(1), Article 32.
#'
#' @export
partial_correlation <- function(data, method = c("oas","ridge","sample"),
                                lambda = 1e-3, return_cov_precision = FALSE) {
  method <- match.arg(method)

  numeric_data <-
    if (is.matrix(data) && is.double(data) && all(is.finite(data))) {
      data
    } else {
      # drops non-numeric
      validate_corr_input(data)
    }

  res <- partial_correlation_cpp(numeric_data, method, lambda, return_cov_precision)

  # set dimnames (cheap; attributes only)
  dn <- list(colnames(numeric_data), colnames(numeric_data))
  if (!is.null(res$pcor)) {
    dimnames(res$pcor) <- dn
    if (!is.null(res$cov))       dimnames(res$cov)       <- dn
    if (!is.null(res$precision)) dimnames(res$precision) <- dn
  } else {
    pcor <- res[[1]]; dimnames(pcor) <- dn; res <- list(pcor = pcor)
  }

  res$method <- method
  res$lambda <- if (identical(method, "ridge")) lambda else NA_real_
  res$rho    <- if (identical(method, "oas"))   res$rho %||% NA_real_ else NA_real_
  res$jitter <- res$jitter %||% NA_real_
  class(res) <- c("partial_corr", "list")
  res
}


# small helper for older R versions without %||%
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' @rdname partial_correlation
#' @title Print method for \code{partial_corr}
#' @description Prints only the partial correlation matrix (no attribute spam),
#'   with an optional one-line header stating the estimator used.
#'
#' @param x An object of class \code{partial_corr}.
#' @param digits Integer; number of decimal places for display (default 3).
#' @param show_method Logical; print a one-line header with \code{method}
#'   (and \code{lambda}/\code{rho} if available). Default \code{TRUE}.
#' @param max_rows,max_cols Optional integer limits for display; if provided,
#'   the printed matrix is truncated with a note about omitted rows/cols.
#' @param ... Further arguments passed to \code{print.matrix()}.
#' @return Invisibly returns \code{x}.
#' @method print partial_correlation
#' @export
print.partial_correlation <- function(
    x,
    digits = 3,
    show_method = TRUE,
    max_rows = NULL,
    max_cols = NULL,
    ...
) {
  stopifnot(inherits(x, "partial_corr"))
  M <- x$pcor
  if (!is.matrix(M)) stop("`x$pcor` must be a matrix.")

  if (isTRUE(show_method)) {
    meth <- if (!is.null(x$method)) as.character(x$method) else NA_character_
    hdr <- switch(
      tolower(meth),
      "oas"   = {
        rho <- if (!is.null(x$rho) && is.finite(x$rho)) sprintf(", OAS rho=%.3f", x$rho) else ""
        paste0("Partial correlation (OAS", rho, ")")
      },
      "ridge" = {
        lam <- if (!is.null(x$lambda) && is.finite(x$lambda)) sprintf(", lambda=%.3g", x$lambda) else ""
        paste0("Partial correlation (ridge", lam, ")")
      },
      "sample" = "Partial correlation (sample covariance)",
      "Partial correlation"
    )
    cat(hdr, "\n")
  } else {
    cat("Partial correlation matrix:\n")
  }

  # keep only dim + dimnames attributes to print like a plain matrix
  attributes(M) <- attributes(M)[c("dim", "dimnames")]

  if (!is.null(max_rows) || !is.null(max_cols)) {
    nr <- nrow(M); nc <- ncol(M)
    r  <- if (is.null(max_rows)) nr else min(nr, max_rows)
    c  <- if (is.null(max_cols)) nc else min(nc, max_cols)
    mm <- round(M[seq_len(r), seq_len(c), drop = FALSE], digits)
    print(mm, ...)  # safe: ... is a formal
    if (nr > r || nc > c) {
      cat(sprintf("... omitted: %d rows, %d cols\n", nr - r, nc - c))
    }
  } else {
    print(round(M, digits), ...)
  }

  invisible(x)
}


#' @rdname partial_correlation
#' @method plot partial_corr
#' @title Plot Method for \code{partial_corr} Objects
#'
#' @description
#' Produces a \pkg{ggplot2}-based heatmap of the partial correlation matrix
#' stored in \code{x$pcor}. Optionally masks the diagonal and/or reorders
#' variables via hierarchical clustering of \eqn{1 - |pcor|}.
#'
#' @param x An object of class \code{partial_corr}.
#' @param title Plot title. By default, constructed from the estimator in
#' \code{x$method}.
#' @param low_color Colour for low (negative) values. Default
#' \code{"indianred1"}.
#' @param high_color Colour for high (positive) values. Default
#' \code{"steelblue1"}.
#' @param mid_color Colour for zero. Default \code{"white"}.
#' @param value_text_size Font size for cell labels. Default \code{4}.
#' @param mask_diag Logical; if \code{TRUE}, the diagonal is masked
#' (set to \code{NA}) and not labelled. Default \code{TRUE}.
#' @param reorder Logical; if \code{TRUE}, variables are reordered by
#' hierarchical clustering of \eqn{1 - |pcor|}. Default \code{FALSE}.
#' @param ... Additional arguments passed to \code{ggplot2::theme()} or other
#'   \pkg{ggplot2} layers.
#'
#' @return A \code{ggplot} object.
#' @import ggplot2
#' @importFrom stats as.dist hclust
#' @export
plot.partial_corr <- function(
    x,
    title = NULL,
    low_color  = "indianred1",
    high_color = "steelblue1",
    mid_color  = "white",
    value_text_size = 4,
    mask_diag = TRUE,
    reorder   = FALSE,
    ...
) {
  if (!inherits(x, "partial_corr")) stop("x must be of class 'partial_corr'.")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for plotting.")

  M <- x$pcor
  if (!is.matrix(M)) stop("`x$pcor` must be a matrix.")

  # Ensure dimnames for labelling
  if (is.null(colnames(M))) colnames(M) <- paste0("V", seq_len(ncol(M)))
  if (is.null(rownames(M))) rownames(M) <- colnames(M)

  # Optional reordering by hierarchical clustering of 1 - |pcor|
  if (isTRUE(reorder) && nrow(M) >= 2L) {
    D  <- stats::as.dist(1 - pmin(1, abs(M)))
    hc <- stats::hclust(D, method = "average")
    ord <- hc$order
    M <- M[ord, ord, drop = FALSE]
  }

  # Default title constructed from x$method (+ tuning, if present)
  if (is.null(title)) {
    method <- tolower(as.character(x$method %||% ""))
    extra <- switch(
      method,
      "oas"   = if (is.finite(x$rho %||% NA_real_)) sprintf(" (OAS, rho=%.3f)", x$rho) else " (OAS)",
      "ridge" = if (is.finite(x$lambda %||% NA_real_)) sprintf(" (ridge, lambda=%.3g)", x$lambda) else " (ridge)",
      "sample" = " (sample)",
      ""
    )
    title <- paste0("Partial correlation heatmap", extra)
  }

  # Build plotting frame
  df <- as.data.frame(as.table(M))
  names(df) <- c("Var1", "Var2", "PCor")

  # Mask diagonal if requested
  if (isTRUE(mask_diag)) {
    df$PCor[df$Var1 == df$Var2] <- NA_real_
  }

  # Reverse y-axis order for a tidy heatmap
  df$Var1 <- factor(df$Var1, levels = rev(unique(df$Var1)))

  ggplot2::ggplot(df, ggplot2::aes(x = Var2, y = Var1, fill = PCor)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(is.na(PCor), "", sprintf("%.2f", PCor))),
      size = value_text_size, colour = "black"
    ) +
    ggplot2::scale_fill_gradient2(
      low = low_color, high = high_color, mid = mid_color,
      midpoint = 0, limits = c(-1, 1), name = "Partial r",
      na.value = "grey95"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid  = ggplot2::element_blank(),
      ...
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title, x = NULL, y = NULL)
}
