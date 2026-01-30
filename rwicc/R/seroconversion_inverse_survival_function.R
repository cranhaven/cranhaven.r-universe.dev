#' Inverse survival function for time-to-event variable with linear hazard function
#'
#' This function determines the seroconversion date corresponding to a provided
#' probability of survival. See \doi{10.1111/biom.13472},
#' Supporting Information, Section A.4.
#'
#' @references
#'
#' Morrison, Laeyendecker, and Brookmeyer (2021).
#' "Regression with interval-censored covariates: Application to cross-sectional incidence estimation".
#' Biometrics, \doi{10.1111/biom.13472}.

#' @param u a vector of seroconversion survival probabilities
#' @param e a vector of time differences between study start and enrollment (in years)
#' @param hazard_alpha the instantaneous hazard of seroconversion on the study start date
#' @param hazard_beta the change in hazard per year after study start date

#' @return numeric vector of time differences between study start and seroconversion (in years)
###############################################################################

seroconversion_inverse_survival_function <- function(u,
                                                     e,
                                                     hazard_alpha,
                                                     hazard_beta) {
  if (hazard_beta != 0) {
    a <- hazard_beta / 2
    b <- hazard_alpha
    c <- log(u) - ((a * (e^2)) + (b * e))

    # this is the quadratic formula:
    root <- (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)

    return(root)
  } else {
    value <- e + (-log(u) / hazard_alpha)
    # equivalent: value = e + qexp(p = u, rate = hazard_alpha, lower.tail = FALSE)
    return(value)
  }
}
