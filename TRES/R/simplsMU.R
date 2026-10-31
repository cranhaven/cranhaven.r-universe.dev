#' SIMPLS-type algorithm for estimating the envelope subspace
#'
#' This algorithm is a generalization of the SIMPLS algorithm in De Jong, S. (1993). See Cook (2018) Section 6.5 for more details of this generalized moment-based envelope algorithm; see Cook, Helland, and Su (2013) for a connection between SIMPLS and the predictor envelope in linear model.
#'
#'
#' @param M The \eqn{p}-by-\eqn{p} positive definite matrix \eqn{M} in the envelope objective function.
#' @param U The \eqn{p}-by-\eqn{p} positive semi-definite matrix \eqn{U} in the envelope objective function.
#' @param u An integer between 0 and \eqn{n} representing the envelope dimension.
#'
#' @return Returns the estimated orthogonal basis of the envelope subspace.
#'
#' @references
#' De Jong, S., 1993. SIMPLS: an alternative approach to partial least squares regression. Chemometrics and intelligent laboratory systems, 18(3), pp.251-263.
#'
#' Cook, R.D., Helland, I.S. and Su, Z., 2013. Envelopes and partial least squares regression. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 75(5), pp.851-877.
#'
#' Cook, R.D., 2018. An introduction to envelopes: dimension reduction for efficient estimation in multivariate statistics (Vol. 401). John Wiley & Sons.
#'
#'
#' @examples
#' ##simulate two matrices M and U with an envelope structure#
#' data <- MenvU_sim(p = 20, u = 5, wishart = TRUE, n = 200)
#' M <- data$M
#' U <- data$U
#' G <- data$Gamma
#' Gamma_pls <- simplsMU(M, U, u=5)
#' subspace(Gamma_pls, G)
#'
#' @export
simplsMU <- function(M, U, u) {
  dimM <- dim(M)
  dimU <- dim(U)
  p <- dimM[1]

  if (dimM[1] != dimM[2] & dimU[1] != dimU[2]) stop("M and U should be square matrices.")
  if (dimM[1] != dimU[1]) stop("M and U should have the same dimension.")
  if (qr(M)$rank < p) stop("M should be positive definite.")
  if (u > p & u < 0) stop("u should be between 0 and p.")
  if (u == p) {return(Gamma = diag(p))}else {
    W <- matrix(0, p, (u+1))
    for (k in 1:u) {
      Wk <- W[, 1:k]
      Ek <- M %*% Wk
      temp <- crossprod(Ek)
      QEK <- diag(p) - Ek %*% MASS::ginv(temp) %*% t(Ek)
      W[, (k+1)] <- Re(eigen(QEK %*% U %*% QEK)$vectors[, 1])
    }
    Gamma <- qr.Q(qr(W[, 2:(u+1)]))
    Gamma
  }
}
