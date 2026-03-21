
#' Power of the Equal Allocation 3 procedure
#'
#' Computes the Equal Allocation 3 procedure's power to
#' detect the overall A effect, the simple A effect, or the simple AB effect, respectively.
#'
#' @param  n  total subjects with n/4 subjects in each of the C, A, B, and AB groups
#' @param  hrA  group A to group C hazard ratio; \code{hrA} < 1 corresponds to group A superiority
#' @param  hrB  group B to group C hazard ratio; \code{hrA} < 1 corresponds to group A superiority
#' @param  hrAB  group AB to group C hazard ratio; \code{hrAB} < 1 corresponds to group AB superiority
#' @param  avgprob  event probability averaged across the C, A, B, and AB groups
#' @param  probA_C  event probability averaged across the A and C groups
#' @param  probAB_C  event probability averaged across the AB and C groups
#' @param  critEA3  rejection critical value for the overall A, simple A, and simple AB logrank statistics
#' @param  dig number of decimal places to \code{\link{roundDown}} the critical value to
#' @param  cormat12  asymptotic correlation matrix for the overall A and simple A, respectively, simple AB logrank statistics
#' @param  niter  number of times we call \code{pmvnorm} to average out its randomness
#' @param  abseps  \code{abseps} setting in the \code{pmvnorm} call
#' @return \item{powerEA3overallA }{power to detect the overall A effect}
#' @return \item{powerEA3simpleA }{power to detect the simple A effect}
#' @return \item{powerEA3simpleAB }{power to detect the simple AB effect}
#' @return \item{powerEA3anyA }{power to detect either the overall A or simple A effects}
#' @import mvtnorm
#' @details   For a 2-by-2 factorial design, this function computes
#' the probability that either the overall A
#' or the simple A or the simple AB logrank statistics
#' reject their null hypotheses at the Dunnet-corrected
#' \code{critEA3} critical value.  As described in Leifer, Troendle, et al. (2019),
#' the \code{critEA3} = -2.32 critical value
#' corresponds to controlling the famiywise error of the Equal Allocation 3 procedure at the
#' two-sided 0.05 significance level.
#' The critical value -2.32 may be computed using the \code{crit2x2} function.
#' The \code{pmvnorm} function from the \code{mvtnorm} package is used to calculate
#' the power for simultaneously detecting the overall and simple A effects.
#' This is used to compute the power for detecting the overall A and/or simple A effects,
#' which is computed as the sum of the powers for each of the effects minus the
#' power for simultaneously detecting both effects.
#' Since the power for simultaneously detecting both effects involves bivariate
#' normal integration over an unbounded region in R^2, \code{pmvnorm}
#' uses a random seed for these computations.  Note that cRAN suggested
#' we not include the random seed as an argument in this function.  To smooth out the
#' randomness, \code{pmvnorm} is called \code{niter} times and
#' the average value over the \code{niter} calls is taken to be those powers.
#' @references Leifer, E.S., Troendle, J.F., Kolecki, A., Follmann, D.
#' Joint testing of overall and simple effect for the two-by-two factorial design. (2020). Submitted.
#' @references Lin, D-Y., Gong, J., Gallo, P., et al. Simultaneous inference on treatment effects
#' in survival studies with factorial designs. Biometrics. 2016; 72: 1078-1085.
#' @references Slud, E.V. Analysis of factorial survival experiments. Biometrics. 1994; 50: 25-38.
#' @export powerEA3
#' @seealso \code{\link{crit2x2}}, \code{lgrkPower}, \code{strLgrkPower}, \code{pmvnorm}
#' @examples
#' # Corresponds to scenario 5 in Table 2 from Leifer, Troendle, et al. (2020).
#' rateC <- 0.0445
#' hrA <- 0.80
#' hrB <- 0.80
#' hrAB <- 0.72
#' mincens <- 4.0
#' maxcens <- 8.4
#' evtprob <- eventProb(rateC, hrA, hrB, hrAB, mincens, maxcens)
#' avgprob <- evtprob$avgprob
#' probAB_C <- evtprob$probAB_C
#' probA_C <- evtprob$probA_C
#' dig <- 2
#' alpha <- 0.05
#' corAa  <- 1/sqrt(2)
#' corAab <- 1/sqrt(2)
#' coraab <- 1/2
#' critEA3 <- crit2x2(corAa, corAab, coraab, dig, alpha)$critEA3
#' n <- 4600
#' powerEA3(n, hrA, hrB, hrAB, avgprob, probA_C, probAB_C,
#'   critEA3, dig, cormat12 = matrix(c(1, sqrt(0.5), sqrt(0.5), 1), byrow = TRUE,
#'   nrow = 2), niter = 1, abseps = 1e-03)
#'
#' # $powerEA3overallA
#' # [1] 0.5861992
#'
#' # $powerEA3simpleA
#' # [1] 0.5817954
#'
#' # $powerAB
#' # [1] 0.9071236
#'
#' # $powerEA3anyA
#' # [1] 0.7060777

powerEA3 <- function(n, hrA, hrB, hrAB, avgprob, probA_C, probAB_C,
                          critEA3, dig,
                          cormat12 = matrix(c(1, sqrt(0.5),
                                              sqrt(0.5), 1), byrow = T, nrow = 2),
                                niter = 5, abseps = 1e-03)
{
  alpha <- 2 * pnorm(critEA3)
  muoverA <- (log(hrA) + 0.5 * log(hrAB/(hrA*hrB)))* sqrt((n/4) * avgprob)
  muA <- log(hrA) * sqrt((n/8) * probA_C)
  muAB <- log(hrAB) * sqrt((n/8) * probAB_C)
  # Compute power for overall A effect
  powerEA3overallA <- strLgrkPower(n, hrA, hrB, hrAB, avgprob, dig, alpha)$power
  # Compute power for simple A effect
  powerEA3simpleA <- lgrkPower(hrA, (n/2) * probA_C, alpha)$power
  # Compute power for simple AB effect
  powerEA3simpleAB <- lgrkPower(hrAB, (n/2) * probAB_C, alpha)$power
  # Compute the power that both the overall A and simple A effects are detected.
  # Use pmvnorm to compute the power to simultaenously detect overall A and simple A effects.
  # Do this niter times to average out the randomness in pmvnorm.
  # Previous versions of crit2x2 set a random seed here
  # to be used in conjunction with the pmvnorm call.  CRAN
  # suggested that this be omitted.
  # set.seed(rseed)
  powermat <- matrix(rep(0, niter), nrow = niter)
  for(i in 1:niter){
    powermat[i, 1] <- pmvnorm(lower=-Inf, upper=c(critEA3, critEA3), mean=c(muoverA, muA),
                          corr=cormat12, sigma=NULL, maxpts = 25000, abseps = abseps, releps = 0)
  }
  poweraux <- apply(powermat, 2, mean)
  powerinter12 <- poweraux[1]
  list(powerEA3overallA = powerEA3overallA, powerEA3simpleA = powerEA3simpleA,
       powerEA3simpleAB = powerEA3simpleAB,
## NEW on 3/13/2020 is the next line
       powerEA3anyA = powerEA3overallA + powerEA3simpleA - powerinter12
       )
}



