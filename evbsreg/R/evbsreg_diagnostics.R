#' Conformal Normal Curvature Local Influence Diagnostics
#'
#' Computes conformal normal curvature (CNC) local influence diagnostics
#' from a fitted EVBS regression model. The CNC approach of Poon and Poon
#' (1999) produces a scale-invariant influence measure bounded in
#' \eqn{[0,1]}; the aggregate contribution statistic of Zhu and Lee (2001)
#' attributes curvature to individual observations and is compared against
#' interpretable reference thresholds.
#'
#' @param fit A fitted model object returned by \code{\link{evbsreg.fit}}.
#'   The function uses the influence matrix \code{fit$B}.
#'
#' @details
#' The function performs the symmetric eigendecomposition of the influence
#' matrix \eqn{B}, normalizes the eigenvalues to unit norm, and for each
#' threshold \eqn{q = 1, \ldots, 7} identifies the \eqn{q}-influential
#' eigenvectors (those whose normalized eigenvalue exceeds
#' \eqn{q/\sqrt{n}}). The aggregate contribution of observation \eqn{j} at
#' level \eqn{q}, \eqn{B_j(q)}, is the sum of the normalized eigenvalues
#' weighted by the squared eigenvector coordinates of observation \eqn{j}.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{eigenvalues}}{Numeric vector: the raw eigenvalues of
#'     \eqn{B}.}
#'   \item{\code{eigenvalues_norm}}{Numeric vector: the absolute normalized
#'     eigenvalues \eqn{|\lambda_i^*|}.}
#'   \item{\code{eigenvectors}}{Matrix: the eigenvectors of \eqn{B}
#'     (columns).}
#'   \item{\code{thresholds}}{Numeric vector of length 7: the reference
#'     thresholds \eqn{q/\sqrt{n}} for \eqn{q = 1, \ldots, 7}.}
#'   \item{\code{Bj}}{Matrix of dimension \eqn{8 \times n}: rows 1--7 hold
#'     the aggregate contributions \eqn{B_j(q)} for \eqn{q=1,\ldots,7};
#'     row 8 holds the global aggregate over all eigenvectors.}
#'   \item{\code{bq}}{Numeric vector of length 8: the reference values
#'     \eqn{b(q)} for flagging influential observations at each level.}
#'   \item{\code{n}}{Integer: the sample size.}
#' }
#'
#' @references
#' Poon, W.-Y. and Poon, Y. S. (1999). Conformal normal curvature and
#' assessment of local influence. \emph{Journal of the Royal Statistical
#' Society, Series B}, 61, 51--61.
#'
#' Zhu, H. and Lee, S. (2001). Local influence for incomplete-data models.
#' \emph{Journal of the Royal Statistical Society, Series B}, 63, 111--126.
#'
#' Ospina, R., Lima, J. I. C., Barros, M., and Macedo, A. M. S. (2026).
#' Local influence diagnostics for the extreme-value Birnbaum-Saunders
#' regression model. \emph{Submitted}.
#'
#' @seealso \code{\link{evbsreg.fit}}, \code{\link{plot_cnc}}.
#'
#' @examples
#' data(itajai)
#' X <- cbind(1, itajai$pressure)
#' fit <- evbsreg.fit(X, itajai$wind)
#' diag <- cnc_diagnostics(fit)
#'
#' ## Top normalized eigenvalues
#' head(diag$eigenvalues_norm, 4)
#'
#' ## Observations flagged at q = 7
#' which(diag$Bj[7, ] > diag$bq[7])
#'
#' @export
cnc_diagnostics <- function(fit) {

  F  <- fit$B
  n  <- nrow(F)

  # Eigendecomposition
  eg     <- eigen(F, symmetric = TRUE)
  autoval      <- eg$values
  autoval_norm <- autoval / ((sum(autoval^2))^(1/2))
  mi           <- abs(autoval_norm)
  dim_F        <- length(mi)

  # Thresholds q/sqrt(n), q = 1..7
  lim   <- (1:7) / sqrt(dim_F)
  dim_q <- length(lim)

  # q-influential eigenvalues (mask)
  mi_influ <- matrix(0, dim_q, dim_F)
  for (i in seq_len(dim_q)) {
    mi_influ[i, ] <- ifelse(mi > lim[i], mi, 0)
  }

  # Normalized eigenvectors (row-wise)
  autovec      <- eg$vectors
  autovec_norm <- matrix(0, n, n)
  for (i in seq_len(n)) {
    autovec_norm[i, ] <- autovec[i, ] / ((sum(autovec[i, ]^2))^(1/2))
  }

  NOR        <- sqrt(sum(diag(F %*% F)))
  mq_barra0  <- sum(diag(F)) / (dim_F * NOR)  # reference b
  mq_barra   <- matrix(0, dim_q, 1)
  for (i in seq_len(dim_q)) {
    mq_barra[i, ] <- sum(mi_influ[i, ] / dim_F)
  }

  # Critical values
  valor_critico0 <- mq_barra0
  valor_critico  <- as.matrix(mq_barra)
  vcritico       <- t(as.matrix(c(valor_critico, valor_critico0)))

  # Aggregate contributions B_j(q)
  autovec2 <- autovec_norm * autovec_norm
  mj_q     <- matrix(0, dim_q, dim_F)
  for (j in seq_len(dim_F)) {
    for (i in seq_len(dim_q)) {
      mj_q[i, j] <- sum(mi_influ[i, ] * autovec2[j, ])
    }
  }
  mj_q0 <- matrix(0, 1, dim_F)
  for (j in seq_len(dim_F)) {
    mj_q0[, j] <- sum(mi * autovec2[j, ])
  }
  mq_j <- rbind(mj_q, mj_q0)  # 8 x n

  list(
    eigenvalues      = autoval,
    eigenvalues_norm = mi,
    eigenvectors     = autovec,
    thresholds       = lim,
    Bj               = mq_j,
    bq               = as.vector(vcritico),
    n                = n
  )
}


#' Plot Normalized Eigenvalues of the Influence Matrix
#'
#' Produces panel (a) of the diagnostic figure: the normalized eigenvalues
#' of the influence matrix, with horizontal reference thresholds for
#' \eqn{q = 1, \ldots, 7}.
#'
#' @param diag A list returned by \code{\link{cnc_diagnostics}}.
#' @param pch Plotting character (default \code{16}).
#' @param cex Point size expansion (default \code{1}).
#' @param main Plot title (default empty).
#'
#' @return Called for its side effect (a base-graphics plot). Returns
#'   \code{NULL} invisibly.
#'
#' @seealso \code{\link{plot_cnc}}, \code{\link{cnc_diagnostics}}.
#'
#' @examples
#' data(itajai)
#' fit <- evbsreg.fit(cbind(1, itajai$pressure), itajai$wind)
#' diag <- cnc_diagnostics(fit)
#' plot_normalized_eigenvalues(diag, main = "(a)")
#'
#' @importFrom graphics plot abline text
#' @export
plot_normalized_eigenvalues <- function(diag,
                                         pch = 16, cex = 1,
                                         main = "") {
  mi  <- diag$eigenvalues_norm
  lim <- diag$thresholds
  n   <- diag$n
  plot(mi, pch = pch, cex = cex,
       ylim = c(0, max(mi, max(lim)) * 1.05),
       ylab = "Normalized eigenvalue", xlab = "Index",
       main = main)
  for (i in seq_along(lim)) {
    abline(h = lim[i], lty = 2, col = "grey60")
    text(n - 5, lim[i] + 0.03, paste0("q=", i), cex = 0.85)
  }
}


#' Plot Aggregate Contributions B_j(q)
#'
#' Produces panel (b) of the diagnostic figure: the aggregate contributions
#' \eqn{B_j(q)} of each observation, with a horizontal reference line at
#' \eqn{b(q)} and automatic labelling of the most influential points.
#'
#' @param diag A list returned by \code{\link{cnc_diagnostics}}.
#' @param q Integer influence threshold in \eqn{1, \ldots, 7}. The paper
#'   uses \code{q = 7}.
#' @param label.flagged Number of largest observations to label.
#' @param pch Plotting character (default \code{16}).
#' @param cex Point size expansion (default \code{0.7}).
#' @param main Plot title (default empty).
#'
#' @return The indices of the labelled observations, returned invisibly.
#'   Also produces a base-graphics plot as a side effect.
#'
#' @seealso \code{\link{plot_cnc}}, \code{\link{cnc_diagnostics}}.
#'
#' @examples
#' data(itajai)
#' fit <- evbsreg.fit(cbind(1, itajai$pressure), itajai$wind)
#' diag <- cnc_diagnostics(fit)
#' plot_aggregate_contributions(diag, q = 7, main = "(b)")
#'
#' @importFrom graphics plot abline text
#' @export
plot_aggregate_contributions <- function(diag, q = 7,
                                         label.flagged = 5,
                                         pch = 16, cex = 0.7,
                                         main = "") {
  Bj_q   <- diag$Bj[q, ]
  vc     <- diag$bq[q]
  n      <- diag$n
  plot(Bj_q, type = "p", pch = pch, cex = cex,
       ylim = c(0, max(Bj_q, vc) * 1.1),
       ylab = bquote(B[j]*"("*.(q)*")"),
       xlab = "Index", main = main)
  abline(h = vc, lty = 2, col = "red")
  ord <- order(Bj_q, decreasing = TRUE)[seq_len(label.flagged)]
  ypos <- .declutter_above(ord, Bj_q[ord], n = n,
                            step = max(Bj_q, vc) * 0.05)
  text(ord, ypos, labels = ord, cex = 0.85, col = "blue")
  invisible(ord)
}

#' Vertical offsets that keep index labels from overlapping
#'
#' Flagged observations that are close together on the index axis (as
#' happens, e.g., with two adjacent months, or several early cases in a
#' short series) receive text labels that collide when placed at a fixed
#' height above each point. This helper staggers the height of labels
#' whose x-position falls within the closeness threshold of the previous
#' one (in sorted order, so a chain of nearby points all stack rather
#' than only immediate pairs), so that close-together labels stack
#' instead of overlapping; isolated points keep the original spacing.
#'
#' @param x Integer index positions of the labelled points.
#' @param y Their heights on the plotted quantity.
#' @param n Sample size, used to scale the closeness threshold.
#' @param step Vertical spacing added per stacked label.
#' @return Numeric vector of y-coordinates to pass to \code{text()}.
#' @noRd
.declutter_above <- function(x, y, n, step) {
  o <- order(x)
  gap <- 0
  level <- integer(length(x))
  thresh <- max(3, 0.06 * n)
  for (i in seq_along(o)) {
    if (i > 1 && (x[o[i]] - x[o[i - 1]]) < thresh) {
      gap <- gap + 1
    } else {
      gap <- 0
    }
    level[o[i]] <- gap
  }
  y + step * (1 + level)
}


#' Combined Conformal Normal Curvature Diagnostic Plot
#'
#' Produces the two-panel diagnostic figure of the paper: normalized
#' eigenvalues (left) and aggregate contributions \eqn{B_j(q)} (right),
#' side by side.
#'
#' @param diag A list returned by \code{\link{cnc_diagnostics}}.
#' @param q Integer influence threshold in \eqn{1, \ldots, 7}
#'   (default \code{7}).
#' @param label.flagged Number of largest observations to label in the
#'   right panel (default \code{5}).
#'
#' @return Called for its side effect (a two-panel base-graphics figure).
#'   Returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{cnc_diagnostics}},
#'   \code{\link{plot_normalized_eigenvalues}},
#'   \code{\link{plot_aggregate_contributions}}.
#'
#' @examples
#' data(itajai)
#' fit <- evbsreg.fit(cbind(1, itajai$pressure), itajai$wind)
#' diag <- cnc_diagnostics(fit)
#' plot_cnc(diag, q = 7)
#'
#' @importFrom graphics par
#' @export
plot_cnc <- function(diag, q = 7, label.flagged = 5) {
  op <- par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.5, 1.5))
  on.exit(par(op))
  plot_normalized_eigenvalues(diag, main = "(a) Normalized eigenvalues")
  plot_aggregate_contributions(diag, q = q,
                                label.flagged = label.flagged,
                                main = sprintf("(b) Aggregate contributions, q=%d", q))
}
