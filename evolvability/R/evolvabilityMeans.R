#' Calculate average evolvability parameters of a G-matrix
#'
#' \code{evolvabilityMeans} calculates the average (unconditional) evolvability
#' (e), respondability (r), conditional evolvability (c), autonomy (a) and
#' integration (i) of a additive-genetic variance matrix using the approximation
#' formulas described in Hansen and Houle (2008, 2009).
#'
#' @param G A variance matrix (must be symmetric and positive definite).
#' @param means An optional vector of trait means, for mean standardization.
#' @details The equations for calculating the evolvability parameters are
#'   approximations, except for the minimum, maximum and unconditional
#'   evolvability which are exact. The bias of the approximations depends on the
#'   dimensionality of the G-matrix, with higher bias for few dimensions (see
#'   Hansen and Houle 2008). For low dimensional G-matrices, we recommend
#'   estimating the averages of the evolvability parameters using
#'   \code{evolavbilityBetaMCMC} over many random selection gradients (
#'   \code{randomBeta}). The maximum and minimum evolvability, which
#'   are also the maximum and minimum respondability and conditional
#'   evolvability, equals the largest and smallest eigenvalue of the G-matrix,
#'   respectively.
#' @return A vector with the following components:
#' \tabular{llllll}{
#' \code{e_mean} \tab\tab\tab\tab The average (unconditional) evolvability. \cr
#' \code{e_min} \tab\tab\tab\tab The minimum evolvability. \cr
#' \code{e_max} \tab\tab\tab\tab The maximum evolvability. \cr
#' \code{r_mean} \tab\tab\tab\tab The average respondability. \cr
#' \code{c_mean} \tab\tab\tab\tab The average conditional evolvability. \cr
#' \code{a_mean} \tab\tab\tab\tab The average autonomy. \cr
#' \code{i_mean} \tab\tab\tab\tab The average integration.
#' }
#' @references
#' Hansen, T. F. & Houle, D. (2008) Measuring and comparing evolvability and
#'  constraint in multivariate characters. J. Evol. Biol. 21:1201-1219. \cr
#' Hansen, T. F. & Houle, D. (2009) Corrigendum. J. Evol. Biol. 22:913-915.
#' @author Geir H. Bolstad
#' @examples
#' G <- matrix(c(1, 1, 0, 1, 2, 1, 0, 1, 2), ncol = 3)
#' evolvabilityMeans(G)
#' @keywords array algebra
#' @importFrom stats var
#' @export
evolvabilityMeans <- function(G, means = 1) {
  if (any(G[upper.tri(G)] != G[t(lower.tri(G))])) {
    stop("G is not symmetric.")
  }
  if (means[1] == 1) {
    means <- rep(1, nrow(G))
  }
  G <- G / (means %*% t(means))
  e_mean <- mean(eigen(G)$values)
  e_max <- max(eigen(G)$values)
  e_min <- min(eigen(G)$values)
  VAR <- function(x) sum((x - mean(x))^2)/length(x)
  Heig <- 1 / mean(1 / eigen(G)$values)
  Ieig <- VAR(eigen(G)$values) / (mean(eigen(G)$values))^2
  Ieig2 <- VAR(eigen(G)$values^2) / (mean(eigen(G)$values^2))^2
  Iinveig <- VAR(1 / eigen(G)$values) / (mean(1 / eigen(G)$values))^2
  k <- nrow(G)
  c_mean <- Heig * (1 + (2 * Iinveig) / (k + 2))
  r_mean <- sqrt(mean(eigen(G)$values^2)) * (1 - (Ieig2 / (4 * (k + 2))))
  a_mean <- (Heig / e_mean) * 
      (1 + 2 * (Ieig + Iinveig - 1 + (Heig / e_mean) + 
           2 * Ieig * Iinveig / (k + 2)) / (k + 2)
       )
  i_mean <- 1 - a_mean
  return(c(e_mean = e_mean, e_min = e_min, e_max = e_max, c_mean = c_mean, 
           r_mean = r_mean, a_mean = a_mean, i_mean = i_mean))
}
