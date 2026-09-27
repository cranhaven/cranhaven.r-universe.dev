#' @title Robust Concordance Correlation Coefficient (rCCC)
#'
#' @description
#' Computes a robust concordance correlation coefficient using
#' Minimum Covariance Determinant (MCD) estimates.
#'
#' @details
#' The rCCC replaces means and (co)variances in Lin's CCC with their
#' MCD counterparts: \eqn{\rho_c = \frac{2\sigma_{xy}}{\sigma_x^2+\sigma_y^2+(\mu_x-\mu_y)^2}}.
#'
#' @param x Numeric vector; first variable.
#' @param y Numeric vector; second variable.
#' @param alpha Numeric in (0.5, 1]; MCD subset size proportion. Default 0.75.
#'
#' @return A list with one element:
#' \item{coef}{Robust concordance correlation coefficient}
#'
#' @references Bulut, H. (2025). A Robust Concordance Correlation Coefficient. (Unpublished)
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#'
#' @examples
#' if (requireNamespace("robustbase", quietly = TRUE)) {
#'   set.seed(1)
#'   x <- rnorm(50)
#'   y <- 2 + 3*x + rnorm(50, mean = 3)
#'   rccc(x, y)
#' }
#'
#' @export
rccc <- function(x, y, alpha = 0.75) {
  # basic checks
  if (!is.numeric(x) || !is.numeric(y))
    stop("'x' and 'y' must be numeric vectors.", call. = FALSE)
  if (length(x) != length(y))
    stop("'x' and 'y' must have the same length.", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0.5 || alpha > 1)
    stop("'alpha' must be a single numeric value in (0.5, 1].", call. = FALSE)
  
  # dependency check (Suggests)
  if (!requireNamespace("robustbase", quietly = TRUE)) {
    stop("Package 'robustbase' is required for rCCC (covMcd). Please install it.",
         call. = FALSE)
  }
  
  # remove NAs pairwise
  ok <- stats::complete.cases(x, y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3L)
    stop("Not enough complete observations after removing NAs.", call. = FALSE)
  
  # MCD estimates (robustbase returns a LIST with $center and $cov)
  data <- cbind(x, y)
  mcd  <- robustbase::covMcd(data, alpha = alpha)
  
  mu_x <- mcd$center[1]
  mu_y <- mcd$center[2]
  sxx  <- mcd$cov[1, 1]
  syy  <- mcd$cov[2, 2]
  sxy  <- mcd$cov[1, 2]
  
  coef <- (2 * sxy) / (sxx + syy + (mu_x - mu_y)^2)
  return(list(coef = unname(coef)))
}
