#' Disattenuate a Cohen's d estimate for unreliability in the continuous variable
#'
#' Measurement error (i.e. the complement of reliability) results in a downward bias
#' of observed effect sizes. This attenuation can be reversed by disattenuation.
#'
#' @param d The (attenuated) value of Cohen's d (i.e. the value as observed in the sample,
#' and therefore attenuated (decreased) by measurement error in the continuous variable).
#' @param reliability The reliability of the measurements of the continuous variable
#'
#' @return The disattenuated value of Cohen's d
#' @export
#' @author Gjalt-Jorn Peters & Stefan Gruijters
#' @references Bobko, P., Roth, P. L., & Bobko, C. (2001). Correcting
#' the Effect Size of d for Range Restriction and Unreliability.
#' *Organizational Research Methods, 4*(1), 46–61.
#' \doi{10.1177/109442810141003}
#'
#' @examples
#' disattenuate.d(.5, .8);
disattenuate.d <- function(d, reliability) {
  return(d / sqrt(reliability));
}
