#' Obtain noncentrality parameter of a chisquare distribution
#'
#' Calculate the noncentrality parameter as well as the model missipecification epsilon_t given its lower-tail critical value.
#'
#' @param T A chi-square statistic
#' @param df Degrees of freedom
#' @param N Total sample size of all groups
#' @param m Number of groups
#' @param alpha Significance level. Default at 0.05.
#' @return The noncentrality parameter \code{ncp}, the minimum tolerable size \code{epsilon_t}, and \code{RMSEA_t} under equivalence testing.
#' @details This function is to compute the noncentrality parameter \code{ncp}, the model missipecification \code{epsilon_t}, and its corresponding \code{RMSEA_t}. With equivalence testing, the model missipecification is also the minimum tolerable size that a researcher needs to tolerate if one wishes to proceed with further restricted tests. The formula from Venables (1975) is used for obtaining the noncentrality parameter of a non-central chi-square distribution given its lower-tail critical value.
#' @references Yuan, K. H., & Chan, W. (2016). Measurement invariance via multigroup SEM: Issues and solutions with chi-square-difference tests. Psychological methods, 21(3), 405-426.
#' @importFrom stats qnorm
#' @export
#' @examples
#' alpha <- .05
#' n_1 <- 200
#' n_2 <- 200
#' N <- n_1 + n_2
#' m <- 2
#' # A made-up likelihood-ratio statistic
#' T_ml <- 8.824
#' df <- 6
#' ncp <- eqMI.ncp(T = T_ml, df = df, N = N, m = m, alpha = alpha)
#'
eqMI.ncp <- function(T, df, N, m, alpha = 0.05){
  z <- qnorm(1-alpha)
  z2 <- z*z
  z3 <- z2*z
  z4 <- z3*z
  z5 <- z4*z
  sig2 <- 2*(2*T-df+2)
  if(!is.na(sig2) & sig2<0){
    delta <- 0
  } else {
    sig <- sqrt(sig2)
    sig3 <- sig*sig2
    sig4 <- sig2*sig2
    sig5 <- sig4*sig
    sig6 <- sig2*sig4

    delta <- T-df+2+sig*
      (
        z+(z2-1)/sig-z/sig2 + 2*(df-1)*(z2-1)/(3*sig3)
        +( -(df-1)*(4*z3-z)/6+(df-2)*z/2 )/sig4
        +4*(df-1)*(3*z4+2*z2-11)/(15*sig5)
        +(
          -(df-1)*(96*z5+164*z3-767*z)/90-4*(df-1)*(df-2)*(2*z3-5*z)/9
          +(df-2)*z/2
        )/sig6
      )
    delta <- max(delta,0)
  }
  epsilon_t <- delta/(N-m)
  RMSEA_t <- sqrt(m*epsilon_t/df)

  #message("noncentrality parameter ncp = ", delta, "\n")
  #message("minimum tolerable size = ", epsilon_t, "\n")
  #message("RMSEA_t under equivalence testing = ", RMSEA_t, "\n")

  return(list(epsilon_t = epsilon_t, RMSEA_t = RMSEA_t))
}
