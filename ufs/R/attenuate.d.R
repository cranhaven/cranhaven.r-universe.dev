#' Attenuate a Cohen's d estimate for unreliability in the continuous variable
#'
#' Measurement error (i.e. the complement of reliability) results in a downward bias
#' of observed effect sizes. This attenuation can be emulated by this function.
#'
#' @param d The value of Cohen's d (that would be obtained with perfect measurements)
#' @param reliability The reliability of the measurements of the continuous variable
#'
#' @return The attenuated value of Cohen's d
#' @export
#' @author Gjalt-Jorn Peters & Stefan Gruijters
#' @references Bobko, P., Roth, P. L., & Bobko, C. (2001). Correcting
#' the Effect Size of d for Range Restriction and Unreliability.
#' *Organizational Research Methods, 4*(1), 46–61.
#' \doi{10.1177/109442810141003}
#'
#' @examples
#' attenuate.d(.5, .8);
attenuate.d <- function(d, reliability) {
  return(d * sqrt(reliability));
}
