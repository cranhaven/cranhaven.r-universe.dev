#' Power for the Equal Allocation 3, Proportional Allocation 2, and
#' Equal Allocation 2 procedures.
#'
#' @param n  total sample size
#' @param rateC group C one year event rate
#' @param hrA   group A to group C hazard ratio
#' @param hrB  group B to group C hazard ratio
#' @param hrAB  group AB to group C hazard ratio
#' @param mincens  minimum censoring time
#' @param maxcens  maximum censoring time
#' @param dig number of decimal places to \code{\link{roundDown}} the critical value to
#' @param alpha two-sided significance level
#' @param corAa	correlation between the overall A and simple A log hazard ratio estimates
#' @param corAab correlation between the overall A and simple AB log hazard ratio estimates
#' @param coraab	correlation between the simple A and simple AB log hazard ratio estimates
#' @param  niter  number of times we call \code{pmvnorm} to average out its randomness
#' @param  abseps  \code{abseps} setting in the \code{pmvnorm} call
#' @return \item{events}{expected number of events}
#' @return \item{evtprob}{event probabilities for the C, A, B, and AB groups, respectively}
#' @return \item{powerEA3overallA }{Equal Allocation 3's power to detect the overall A effect}
#' @return \item{powerEA3simpleA }{Equal Allocation 3's power to detect the simple A effect}
#' @return \item{powerEA3simpleAB }{Equal Allocation 3's power to detect the simple AB effect}
#' @return \item{powerEA3anyA }{Equal Allocation 3's power to detect the simple A or AB effects}
#' @return \item{powerPA2overallA }{Proportional Allocation 2's power to detect the overall A effect}
#' @return \item{powerPA2simpleAB }{Proportional Allocation 2's power to detect the simple AB effect}
#' @return \item{powerEA2simpleA }{Equal Allocation 2's power to detect the simple A effect}
#' @return \item{powerEA2simpleAB }{Equal Allocation 2's power to detect the simple AB effect}
#' @return \item{powerA}{power to detect the overall A effect at the two-sided \code{alpha} level}
#' @return \item{powerB}{power to detect the overall B effect at the two-sided \code{alpha} level}
#' @seealso  \code{\link{eventProb}}, \code{\link{crit2x2}}, \code{\link{lgrkPower}}
#' \code{\link{strLgrkPower}}, \code{\link{powerEA3}}, \code{\link{powerPA2}},
#' \code{\link{powerEA2}}
#' @references Leifer, E.S., Troendle, J.F., Kolecki, A., Follmann, D.
#' Joint testing of overall and simple effect for the two-by-two factorial design. (2019). Submitted.
#' @references Slud, E.V. Analysis of factorial survival experiments. Biometrics. 1994; 50: 25-38.
#' @export fac2x2design
#' @examples
#' # Corresponds to scenario 4 in Table 2 from Leifer, Troendle, et al. (2019).
#' n <- 4600
#' rateC <- 0.0445
#' hrA <- 0.80
#' hrB <- 0.80
#' hrAB <- 0.72
#' mincens <- 4.0
#' maxcens <- 8.4
#'
#' fac2x2design(n, rateC, hrA, hrB, hrAB, mincens, maxcens, dig = 2, alpha = 0.05, niter = 1)
#' # $events
#' # [1] 954.8738
#'
#' # $evtprob
#' # probC     probA     probB    probAB
#' # 0.2446365 0.2012540 0.2012540 0.1831806
#'
#' # $powerEA3overallA
#' # [1] 0.5861992
#'
#' # $powerEA3simpleA
#' # [1] 0.5817954
#'
#' # $powerEA3simplAB
#' # [1] 0.9071236
#'
#' # $powerEA3anyA
#' # [1] 0.7060777
#
#' # $powerPA2overallA
#' # [1] 0.6582819
#'
#' # $powerPA2simpleAB
#' # [1] 0.9197286
#'
#' # $powerEA2simpleA
#' # [1] 0.6203837
#' #
#' # $powerEA2simpleAB
#' # [1] 0.9226679
#'
#' # $powerA
#' # [1] 0.7182932
#'
#' # $powerB
#' # [1] 0.7182932
#'
fac2x2design <- function(n, rateC, hrA, hrB, hrAB, mincens, maxcens, dig = 2, alpha = 0.05,
                         niter = 5, abseps = 1e-03,
                         corAa = 1/sqrt(2), corAab = 1/sqrt(2), coraab = 1/2){
  evtprob <- eventProb(rateC, hrA, hrB, hrAB, mincens, maxcens)
  avgprob <- evtprob$avgprob
  probA_C <- evtprob$probA_C
  probAB_C <- evtprob$probAB_C
  critvals <- crit2x2(corAa, corAab, coraab, dig, alpha)
  critPA2A <- critvals$critPA2A
  critPA2ab <- critvals$critPA2ab
  critEA3 <- critvals$critEA3
  critEA2 <- critvals$critEA2

  # compute power for the overall A test
  powerA <- strLgrkPower(n, hrA, hrB, hrAB, avgprob, dig, alpha)$power

  # compute the power for the overall B test be reversing the roles of
  # of hrA and hrB in the previous line
  powerB <- strLgrkPower(n, hrB, hrA, hrAB, avgprob, dig, alpha)$power

  auxPA2 <- powerPA2(n, hrA, hrB, hrAB, avgprob, probAB_C,
    critPA2A, critPA2ab, dig)

  auxEA3 <- powerEA3(n, hrA, hrB, hrAB, avgprob, probA_C,
    probAB_C, critEA3, dig, cormat12 = matrix(c(1, sqrt(0.5),
    sqrt(0.5), 1), byrow = TRUE, nrow = 2), niter, abseps)

  auxEA2 <- powerEA2(n, hrA, hrAB, probA_C, probAB_C,
    critEA2)

  list(events = n * avgprob, evtprob = c(unlist(evtprob))[2:5],
       powerEA3overallA = auxEA3$powerEA3overallA, powerEA3simpleA = auxEA3$powerEA3simpleA,
       powerEA3simplAB = auxEA3$powerEA3simpleAB, powerEA3anyA = auxEA3$powerEA3anyA,
       powerPA2overallA = auxPA2$powerPA2overallA,
       powerPA2simpleAB = auxPA2$powerPA2simpleAB,
       powerEA2simpleA = auxEA2$powerEA2simpleA,
       powerEA2simpleAB = auxEA2$powerEA2simpleAB,
       powerA = powerA, powerB = powerB)
}
