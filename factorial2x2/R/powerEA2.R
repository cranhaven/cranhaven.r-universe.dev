#' Power of the Equal Allocation 2 procedure
#'
#' Computes the Equal Allocation 2's procedure power to
#' detect the simple A effect and the simple AB effect, respectively.
#'
#' @param  n  total subjects with n/4 subjects in each of the C, A, B, and AB groups
#' @param  hrA  group A to group C hazard ratio; \code{hrA} < 1 corresponds to group A superiority
#' @param  hrAB  group AB to group C hazard ratio; \code{hrAB} < 1 corresponds to group AB superiority
#' @param  probA_C  event probability averaged across the A and C groups
#' @param  probAB_C  event probability averaged across the AB and C groups
#' @param  crit12  logrank statistic critical value for both the simple A and simple AB effects
#' @return \item{powerEA2simpleA }{power to detect the simple A effect}
#' @return \item{powerEA2simpleAB }{power to detect the simple AB effect}
#' @details   For a 2-by-2 factorial design, this function computes
#' the probability that either the simple A, respectively, simple AB logrank statistics
#' reject their null hypotheses using a Dunnett-corrected \code{crit12} critical value.
#' When the two-sided familywise type I error is 0.05, we may use
#' \code{\link{crit2x2}} to compute \code{crit12} = -2.22 which corresponds
#' to a 0.0264 two-sided significance level.  This is described in
#' Leifer, Troendle, et al. (2020).
#' @references Leifer, E.S., Troendle, J.F., Kolecki, A., Follmann, D.
#' Joint testing of overall and simple effect for the two-by-two factorial design. (2020). Submitted.
#' @references Lin, D-Y., Gong, J., Gallo, P., et al. Simultaneous inference on treatment effects
#' in survival studies with factorial designs. Biometrics. 2016; 72: 1078-1085.
#' @references Slud, E.V. Analysis of factorial survival experiments. Biometrics. 1994; 50: 25-38.
#' @export powerEA2
#'
#' @seealso \code{crit2x2}, \code{lgrkPower}
#'
#' @examples
#' # Corresponds to scenario 4 in Table 2 from Leifer, Troendle, et al. (2020).
#' rateC <- 0.0445  # one-year C group event rate
#' hrA <- 0.80
#' hrB <- 0.80
#' hrAB <- 0.72
#' mincens <- 4.0
#' maxcens <- 8.4
#' evtprob <- eventProb(rateC, hrA, hrB, hrAB, mincens, maxcens)
#' probA_C <- evtprob$probA_C
#' probAB_C <- evtprob$probAB_C
#' corAa  <- 1/sqrt(2)
#' corAab <- 1/sqrt(2)
#' coraab <- 1/2
#' dig <- 2
#' alpha <- 0.05
#' critEA2 <- crit2x2(corAa, corAab, coraab, dig, alpha)$critEA2
#' n <- 4600
#' powerEA2(n, hrA, hrAB, probA_C, probAB_C, critEA2)
#'
#' # $powerEA2simpleA
#' # [1] 0.6203837
#'
#' # $powerEA2simpleAB
#' # [1] 0.9226679
#'
powerEA2 <- function(n, hrA, hrAB, probA_C, probAB_C, crit12)
{
  alphaA <- 2 * pnorm(crit12)
  alphaAB <- 2 * pnorm(crit12)
  # compute power to detect the simple A effect
  powerA <- lgrkPower(hrA, (n/2) * probA_C, alpha = alphaA)$power
  # compute power to detect the simple AB effect
  powerAB <- lgrkPower(hrAB, (n/2) * probAB_C, alpha = alphaAB)$power
  list(powerEA2simpleA = powerA, powerEA2simpleAB = powerAB)
}


