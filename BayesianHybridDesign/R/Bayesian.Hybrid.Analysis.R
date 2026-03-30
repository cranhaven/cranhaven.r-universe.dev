#' Statistical Analysis for a Bayesian Hybrid Design
#'
#' This function performs the statistical analysis for a Bayesian Hybrid Design
#' using a dynamic power prior approach.
#'
#' @param Yt Number of responses in the experimental arm in the current study.
#' @param nt Number of patients in the experimental arm in the current study.
#' @param Yc Number of responses in the control arm in the current study.
#' @param nc Number of patients in the control arm in the current study.
#' @param nche Equivalent number of patients borrowed from the historical study.
#' @param nch Total number of patients in the historical control.
#' @param Ych Number of responses in the control treatment in the historical study.
#' @param sig Significance boundary. The hypothesis is considered significant if
#'   the posterior probability \eqn{P(p_t > p_c | data) > sig}.
#' @param credlev Credible interval level (e.g., 0.95 for 95 percent CI).
#' @param a0c Prior alpha parameter for control response rate, \eqn{Beta(a_{0c}, b_{0c})}.
#' @param b0c Prior beta parameter for control response rate, \eqn{Beta(a_{0c}, b_{0c})}.
#' @param a0t Prior alpha parameter for experimental response rate, \eqn{Beta(a_{0t}, b_{0t})}.
#' @param b0t Prior beta parameter for experimental response rate, \eqn{Beta(a_{0t}, b_{0t})}.
#' @param delta_threshold Borrowing threshold. Borrowing occurs when
#'   \eqn{|p_{c,trial} - p_{c,hist}| \le} \code{delta_threshold}.
#'
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{prob.pt.gt.pc}: Probability of experimental arm having a better posterior response rate than control.
#'   \item \code{median_hca}: Posterior median response rate for hybrid control.
#'   \item \code{CI_hca}: Credible interval for median response rate for hybrid control.
#'   \item \code{median_c}: Posterior median response rate for current study control.
#'   \item \code{CI_c}: Credible interval for median response rate for current study control.
#'   \item \code{median_t}: Posterior median response rate for current study experimental arm.
#'   \item \code{CI_t}: Credible interval for median response rate for current experimental arm.
#'   \item \code{delta.m}: Posterior median response rate difference (experimental - control) based on hybrid design.
#'   \item \code{delta.CI}: Credible interval for response rate difference based on hybrid design.
#'   \item \code{delta.m_trial}: Posterior median response rate difference (experimental - control) based on current study only.
#'   \item \code{delta.CI_trial}: Credible interval for response rate difference based on current study only.
#'   \item \code{conclusion}: Statistical inference conclusion string.
#' }
#'
#' @importFrom stats dbeta pbeta qbeta integrate uniroot
#' @export
#'
#' @examples
#' \donttest{
#' # Note: This example relies on the internal package function 'borrow.wt'
#' Bayesian.Hybrid.Analysis(Yt=18, nt=40, Yc=13, nc=40, Ych=73,
#'                          nche=40, nch=234)
#' }
Bayesian.Hybrid.Analysis <- function(Yt=20, nt=40, Yc=12, nc=40, Ych=73,
                                     nche=40, nch=234, sig=0.90, credlev = 0.8,
                                     a0c=0.001, b0c=0.001, a0t=0.001, b0t=0.001,
                                     delta_threshold=0.1) {

  # Dynamic borrowing parameter
  # Note: borrow.wt must be defined in the package namespace
  wt <- borrow.wt(Yc=Yc, nc=nc, Ych=Ych, nch=nch, nche=nche,
                  a0c=a0c, b0c=b0c, delta_threshold=delta_threshold)
  w <- wt$w

  # Posterior distributions
  apost_c_trial <- a0c + Yc
  bpost_c_trial <- b0c + (nc - Yc)

  apost_c_hca <- apost_c_trial + w * Ych
  bpost_c_hca <- bpost_c_trial + w * (nch - Ych)

  apost_t <- a0t + Yt
  bpost_t <- b0t + (nt - Yt)

  # --- 1. Probability Experimental > Control ---
  P_ORRt_upper_Times_p_ORRc <- function(y, ac, bc, at, bt) {
    pbeta(y, at, bt, lower.tail = FALSE) * dbeta(y, ac, bc)
  }

  # Wrap integration in tryCatch to handle numerical failures safely
  phat_res <- tryCatch({
    integrate(P_ORRt_upper_Times_p_ORRc,
              lower = 0, upper = 1,
              ac = apost_c_hca, bc = bpost_c_hca,
              at = apost_t, bt = bpost_t)
  }, error = function(e) list(value = NA_real_))

  phat_pt_larger_pc <- phat_res$value

  # Robust conclusion logic:
  # 1. Initialize as NA
  conclusion <- NA_character_

  # 2. Only compare if phat_pt_larger_pc is a valid number
  if (!is.na(phat_pt_larger_pc) && !is.nan(phat_pt_larger_pc)) {
    if (phat_pt_larger_pc >= sig) {
      conclusion <- "Statistically significant"
    } else {
      conclusion <- "Not Statistically significant"
    }
  } else {
    warning("Integration failed for posterior probability; conclusion is NA.")
  }

  # --- 2. Individual Credible Intervals ---
  # Calculate tail probabilities based on user input 'credlev'
  alpha_tail <- (1 - credlev) / 2

  median_hca <- qbeta(0.5, apost_c_hca, bpost_c_hca)
  CI_hca <- c(qbeta(alpha_tail, apost_c_hca, bpost_c_hca),
              qbeta(1 - alpha_tail, apost_c_hca, bpost_c_hca))

  median_c <- qbeta(0.5, apost_c_trial, bpost_c_trial)
  CI_c <- c(qbeta(alpha_tail, apost_c_trial, bpost_c_trial),
            qbeta(1 - alpha_tail, apost_c_trial, bpost_c_trial))

  median_t <- qbeta(0.5, apost_t, bpost_t)
  CI_t <- c(qbeta(alpha_tail, apost_t, bpost_t),
            qbeta(1 - alpha_tail, apost_t, bpost_t))

  # --- 3. Posterior median of difference (Hybrid) ---
  # P(pt - pc < m) = 0.5
  mfun <- function(m) {
    yfun <- function(y, ac, bc, at, bt) {
      pbeta(m + y, at, bt) * dbeta(y, ac, bc)
    }
    ans <- integrate(yfun,
                     lower = 0, upper = 1,
                     ac = apost_c_hca, bc = bpost_c_hca,
                     at = apost_t, bt = bpost_t)$value - 0.5
    return(ans)
  }

  # Fixed interval: Difference in rates is between -1 and 1
  delta.m <- tryCatch({
    uniroot(f = mfun, interval = c(-1, 1))$root
  }, error = function(e) NA_real_)

  # --- 4. Posterior median of difference (Trial Only) ---
  mfun_trial <- function(m) {
    yfun <- function(y, ac, bc, at, bt) {
      pbeta(m + y, at, bt) * dbeta(y, ac, bc)
    }
    ans <- integrate(yfun,
                     lower = 0, upper = 1,
                     ac = apost_c_trial, bc = bpost_c_trial,
                     at = apost_t, bt = bpost_t)$value - 0.5
    return(ans)
  }

  delta.m_trial <- tryCatch({
    uniroot(f = mfun_trial, interval = c(-1, 1))$root
  }, error = function(e) NA_real_)


  # --- 5. Credible Interval of Difference (Hybrid) ---
  # Solves for 'd' such that P(median - d < pt - pc < median + d) = credlev

  delta.CI <- c(NA, NA)

  if (!is.na(delta.m)) {
    dfun <- function(d) {
      yfun <- function(y, ac, bc, at, bt) {
        (pbeta(delta.m + y + d, at, bt) - pbeta(delta.m + y - d, at, bt)) * dbeta(y, ac, bc)
      }
      ans <- integrate(yfun,
                       lower = 0, upper = 1,
                       ac = apost_c_hca, bc = bpost_c_hca,
                       at = apost_t, bt = bpost_t)$value - credlev
      return(ans)
    }

    delta.d <- tryCatch({
      uniroot(f = dfun, interval = c(0, 1))$root
    }, error = function(e) NA)

    if (!is.na(delta.d)) {
      delta.CI <- c(delta.m - delta.d, delta.m + delta.d)
    }
  }

  # --- 6. Credible Interval of Difference (Trial Only) ---

  delta.CI_trial <- c(NA, NA)

  if (!is.na(delta.m_trial)) {
    dfun_trial <- function(d) {
      yfun <- function(y, ac, bc, at, bt) {
        # Corrected: Using delta.m_trial
        (pbeta(delta.m_trial + y + d, at, bt) - pbeta(delta.m_trial + y - d, at, bt)) * dbeta(y, ac, bc)
      }
      ans <- integrate(yfun,
                       lower = 0, upper = 1,
                       ac = apost_c_trial, bc = bpost_c_trial,
                       at = apost_t, bt = bpost_t)$value - credlev
      return(ans)
    }

    delta.d_trial <- tryCatch({
      uniroot(f = dfun_trial, interval = c(0, 1))$root
    }, error = function(e) NA)

    if (!is.na(delta.d_trial)) {
      delta.CI_trial <- c(delta.m_trial - delta.d_trial, delta.m_trial + delta.d_trial)
    }
  }

  return(list(prob.pt.gt.pc = phat_pt_larger_pc,
              conclusion = conclusion,
              median_hca = median_hca, CI_hca = CI_hca,
              median_c = median_c, CI_c = CI_c,
              median_t = median_t, CI_t = CI_t,
              delta.m = delta.m, delta.CI = delta.CI,
              delta.m_trial = delta.m_trial, delta.CI_trial = delta.CI_trial))
}
