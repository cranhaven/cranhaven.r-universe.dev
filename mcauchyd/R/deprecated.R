#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [dmcd()] instead of `mvdcd()`.
#'
#' @importFrom lifecycle deprecate_soft
#' @export
#' @keywords internal
#' @name deprecated

mvdcd <- function(x, mu, Sigma, tol = 1e-6) {
  deprecate_soft("1.2.0", "mvdcd()", "dmcd()")
  
  dmcd(x, mu, Sigma, tol)
}

#' @description
#' Use [rmcd()] instead of `mvrcd()`.
#'
#' @importFrom lifecycle deprecate_soft
#' @export
#' @keywords internal
#' @rdname deprecated
mvrcd <- function(n, mu, Sigma, tol = 1e-6) {
  deprecate_soft("1.2.0", "mvrcd()", "rmcd()")
  
  rmcd(n, mu, Sigma, tol)
}

#' @description
#' Use [plotmcd()] instead of `plotmvcd()`.
#' Use [contourmcd()] instead of `contourmvcd()`.
#'
#' @importFrom lifecycle deprecate_soft
#' @export
#' @keywords internal
#' @rdname deprecated
plotmvcd <- function(mu, Sigma, xlim = c(mu[1] + c(-10, 10)*Sigma[1, 1]),
                     ylim = c(mu[2] + c(-10, 10)*Sigma[2, 2]), n = 101,
                     xvals = NULL, yvals = NULL, xlab = "x", ylab = "y",
                     zlab = "f(x,y)", col = "gray", tol = 1e-6, ...) {
  deprecate_soft("1.2.0", "plotmvcd()", "plotmcd()")
  
  plotmcd(mu, Sigma, xlim, ylim, n, xvals, yvals, xlab, ylab, zlab,
          col, tol, ...)
}

#' @description
#' Use [contourmcd()] instead of `contourmvcd()`.
#'
#' @importFrom lifecycle deprecate_soft
#' @export
#' @keywords internal
#' @rdname deprecated
contourmvcd <- function(mu, Sigma, beta,
                         xlim = c(mu[1] + c(-10, 10)*Sigma[1, 1]),
                         ylim = c(mu[2] + c(-10, 10)*Sigma[2, 2]),
                         zlim = NULL, npt = 30, nx = npt, ny = npt,
                         main = "Multivariate generalised Gaussian density",
                         sub = NULL, nlevels = 10,
                         levels = pretty(zlim, nlevels),
                         tol = 1e-6, ...) {
  deprecate_soft("1.2.0", "contourmvcd()", "contourmcd()")
  
  contourmcd(mu, Sigma, beta, xlim, ylim, zlim, npt, nx, ny,
              main, sub, nlevels, levels, tol = 1e-6, ...)
}
