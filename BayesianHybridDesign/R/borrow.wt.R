#' Calculate Borrowing Weights from Historical Data
#'
#' Calculates borrowing weights for a hybrid control arm using one of several
#' dynamic borrowing methods.
#'
#' @details
#' The function implements the following methods:
#' \itemize{
#'   \item{"Empirical Bayes"}: The weight is determined by maximizing the
#'     marginal likelihood of the current data.
#'   \item{"Bayesian p"}: Similarity is measured by a Bayesian p-value
#'     comparing the posterior distributions.
#'   \item{"Generalized BC"}: Uses the Generalized Bhattacharyya Coefficient.
#'     The standard BC is a special case with theta = 0.5.
#'   \item{"JSD"}: Uses the Jensen-Shannon Divergence to measure similarity.
#' }
#'
#' @param Yc A scalar integer. The number of responders in the current control arm.
#' @param nc A scalar integer. The number of subjects in the current control arm.
#' @param Ych A scalar integer. The number of responders in the historical control arm.
#' @param nch A scalar integer. The number of subjects in the historical control arm.
#' @param nche A scalar. The maximum number of subjects that can be borrowed,
#'   used to calculate the global weight `a`.
#' @param a0c,b0c Numerics. Hyperparameters for the Beta(a0c, b0c) prior on
#'   the control response rate. Default to 0.001.
#' @param delta_threshold A numeric threshold. Borrowing is only triggered if the
#'   absolute difference in observed response rates is less than this value.
#'   Default is 0.1.
#' @param method A string specifying the dynamic borrowing method. Options
#'   include "Empirical Bayes", "Bayesian p", "Generalized BC", "JSD".
#' @param theta A numeric scalar in (0, 1), applicable to the "Generalized BC"
#'   method. Default is 0.5.
#' @param eta A numeric scalar, applicable to the "Bayesian p", "Generalized
#'   BC", and "JSD" methods. Default is 1.
#'
#' @return A list containing three values:
#' \itemize{
#'   \item a: The global borrowing weight, calculated as nche / nch.
#'   \item wd: The dynamic borrowing weight, calculated based on the chosen method.
#'   \item w: The final overall borrowing weight, which is a product of `a`,
#'     `wd`, and an indicator for whether the response rates are sufficiently similar.
#' }
#'
#' @examples
#' \donttest{
#' borrow.wt(Yc=12, nc=40, Ych=54, nch=180, nche=40, a0c=0.001,
#' b0c=0.001, delta_threshold=0.1, eta=1)
#' }
#'
#' @export
#'
borrow.wt <- function(Yc, nc,
                      Ych, nch, nche,
                      a0c = 0.001, b0c = 0.001,
                      delta_threshold = 0.1,
                      method = "Empirical Bayes", theta = 0.5, eta = 1) {

  # Global weight parameter for borrowing
  a <- nche / nch

  # Target function to optimize weight for Empirical Bayes method
  logwfunction <- function(w, a0c, b0c, Yc, nc, Ych, nch) {
    # a0c, b0c: beta hyper prior parameters beta(a0c, b0c)
    # Yc, nc: current study number of responders and size in control
    # Ych, nch: historical control for number of responders and size
    lbeta(a0c + Yc + w * Ych, b0c + (nc - Yc) + w * (nch - Ych)) -
      lbeta(a0c + w * Ych, b0c + w * (nch - Ych))
  }

  ac <- a0c + Yc
  bc <- b0c + (nc - Yc)
  ach <- a0c + a * Ych
  bch <- b0c + a * (nch - Ych)

  if (method == "Empirical Bayes") {
    # Empirical weight
    wd <- optimize(logwfunction, c(0, 1),
                   a0c = a0c,
                   b0c = b0c,
                   Yc = Yc,
                   nc = nc,
                   Ych = Ych,
                   nch = nch,
                   maximum = TRUE)$maximum
  } else if (method == "Bayesian p") {
    # Heterogeneity
    # P(p_c > p_ch)
    P_c_p_ch <- function(y, ac, bc, ach, bch) {
      p <- pbeta(y, ac, bc, lower.tail = FALSE) * dbeta(y, ach, bch)
      Idx <- is.infinite(p)
      p[Idx] <- sign(p[Idx]) * 1e10
      return(p)
    }
    # P(p_ch > p_c)
    P_ch_p_c <- function(y, ac, bc, ach, bch) {
      p <- pbeta(y, ach, bch, lower.tail = FALSE) * dbeta(y, ac, bc)
      Idx <- is.infinite(p)
      p[Idx] <- sign(p[Idx]) * 1e10
      return(p)
    }
    xi1 <- integrate(P_c_p_ch, lower = 0.0001, upper = 0.9999,
                     ac = ac, bc = bc, ach = ach, bch = bch)$value
    xi2 <- integrate(P_ch_p_c, lower = 0.0001, upper = 0.9999,
                     ac = ac, bc = bc, ach = ach, bch = bch)$value
    wd <- (2 * min(xi1, xi2))^eta
  } else if (method == "Generalized BC") {
    # \int_0^1 \sqrt{f_1(x)f_2(x)}dx
    f.den <- function(y) {
      L1 <- exp(theta * log(dbeta(y, ach, bch)) + (1 - theta) * log(dbeta(y, ac, bc)))
      L2 <- exp(theta * log(dbeta(y, ac, bc)) + (1 - theta) * log(dbeta(y, ach, bch)))
      L <- (L1 + L2) / 2
      Idx <- is.infinite(L)
      L[Idx] <- sign(L[Idx]) * 1e10
      return(L)
    }
    wd <- integrate(f.den, lower = 0.0001, upper = 0.9999)$value
    wd <- wd^eta
  } else if (method == "JSD") {
    f.den <- function(y) {
      logfc <- dbeta(y, ac, bc, log = TRUE)
      logfch <- dbeta(y, ach, bch, log = TRUE)
      logfbar <- log((exp(logfc) + exp(logfch)) / 2)
      
      ans <- (logfc - logfbar) * exp(logfc) + (logfch - logfbar) * exp(logfch)
      
      Idx <- is.infinite(ans)
      ans[Idx] <- sign(ans[Idx]) * 1e10
      Idx <- is.na(ans)
      ans[Idx] <- mean(ans[!Idx])
      #
      return(ans)
    }
    
    wd <- (1 - 0.5 * integrate(f.den, lower = 0.0001, upper = 0.9999)$value)^eta
    
  } else {
    stop(sprintf("Method '%s' is not supported. Please choose from the available options.", method))
  }
  
  # Overall weight
  w <- wd * a * (abs(Yc / nc - Ych / nch) < delta_threshold)
  
  return(list(a = a, wd = wd, w = w))
}
