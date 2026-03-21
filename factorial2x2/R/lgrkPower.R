#' Unstratified (ordinary) logrank power
#'
#' Computes the power for the unstratified (ordinary) logrank statistic
#' for two group comparison.
#'
#' @param hr  hazard ratio
#' @param nevent  expected number of events
#' @param alpha  two-sided significance level
#' @param rprob  randomization probability
#'
#' @details Uses the formula at the bottom of p.317 from Schoenfeld (Biometrika, 1981)
#' where the beta should be 1 - beta.  The formula is modified
#' to assume that values of the hazard ratio
#' less than 1 correspond to treatment efficacy.  We do this because we only want
#' to include the probability of rejecting the null in favor of efficacy, not inferiority
#' as well.
#' @return  \item{power }{logrank power}
#' @references Schoenfeld, D. The asymptotic properties of nonparametric tests for comparing
#'   survival distributions. Biometrika. 1981; 68: 316-319.
#' @author Eric Leifer, James Troendle
#' @export lgrkPower
#' @examples
#'  hr <- 0.5
#'  nevent <- 98
#'  lgrkPower(hr, nevent, alpha = 0.05,  rprob = 0.5)
#'  # $power
#'  # [1] 0.9293463
#'

lgrkPower <- function(hr, nevent, alpha = 0.05, rprob = 0.5){

  lhr <- log(hr)
  critval <- qnorm(1 - alpha/2)
  power <- pnorm( -sqrt(nevent * rprob * (1 - rprob)) * lhr - critval)
  # we used to compute power as in the below line, but that includes the probability
  # of rejecting in the inferiority direction, which we do not want to include.
 # power <- pnorm( sqrt(nevent * rprob * (1 - rprob)) * abs(lhr) - critval)
  list(power = power)
}

