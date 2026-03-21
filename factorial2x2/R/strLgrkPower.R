#'  Stratified (overall) logrank power
#'
#'  Computes the power for the overall treatment A effect based on the stratified logrank test
#'  based on Slud (1994).
#'
#' @param n  total subjects with n/4 subjects in the C, A, B, and AB groups
#' @param hrA  group A to group C hazard ratio
#' @param hrB  group B to group C hazard ratio
#' @param hrAB  group AB to group C hazard ratio
#' @param avgprob  average event probability across the four groups as
#'    calculated by the function eventProb
#' @param dig number of decimal places to which we \code{\link{roundDown}} the critical value
#' corresponding to \code{alpha}
#' @param alpha  two-sided significance level
#' @return \item{mean}{logrank mean value}
#' @return \item{power}{logrank power}
#' @return \item{nevent}{expected number of events}
#' @details The \code{roundDown} function is used in conjunction with the \code{dig} argument
#' to insure that any rounding of the (negative) critical values will be done conservatively to control
#' the familywise type I error at the desired level.
#' @seealso \code{\link{roundDown}},  \code{\link{eventProb}}
#' @author Eric Leifer, James Troendle
#' @references Leifer, E.S., Troendle, J.F., Kolecki, A., Follmann, D. Joint testing of overall and
#' simple effect for the two-by-two factorial design. (2019). Submitted.
#' @references Slud, E.V. Analysis of factorial survival experiments. Biometrics. 1994; 50: 25-38.
#' @export strLgrkPower
#' @examples
#' rateC <- 0.0445
#' hrA <- 0.80
#' hrB <- 0.80
#' hrAB <- 0.72
#' mincens <- 4.0
#' maxcens <- 8.4
#' avgprob <- eventProb(rateC, hrA, hrB, hrAB, mincens, maxcens)$avgprob
#' n <- 4600
#' strLgrkPower(n, hrA, hrB, hrAB, avgprob, dig = 2, alpha = 0.05)
#' # $mean
#' # [1] -2.537779
#'
#' # $power
#' # [1] 0.7182932
#'
#' # $nevent
#' # [1] 954.8738

strLgrkPower <- function(n, hrA, hrB, hrAB, avgprob, dig = 2, alpha = 0.05){

  lhrA <- log(hrA)
  lhrB <- log(hrB)
  lhrAB <- log(hrAB)
  mean <- sqrt(n) * (lhrA + 0.5 * (lhrAB - lhrA - lhrB)) * sqrt(0.25 * avgprob)
  critval <- roundDown(qnorm(alpha/2), dig)
  power <- pnorm(critval - mean)
  nevent <- n * avgprob
  list(mean = mean, power = power, nevent = nevent)
}


