#'Generating a probability of DLT function based on the EWOC classical model
#
#'@param rho a numerical value indicating the true value of the parameter rho.
#'@param mtd a numerical value indicating the true value of the parameter mtd.
#'@param theta a numerical value defining the proportion of expected patients
#'to experience a medically unacceptable, dose-limiting toxicity (DLT) if
#'administered the MTD.
#'@param min_dose a numerical value defining the lower bound of the support of
#'the MTD.
#'@param max_dose a numerical value defining the upper bound of the support of
#'the MTD.
#'
#'@return A function with dose as an input and a probability based on the
#'logistic regression and parameters as an output.
#'
#'@examples
#'pdlt <- pdlt_d1classical(rho = 0.05, mtd = 60, theta = 0.33,
#'                       min_dose = 20, max_dose = 100)
#'
#'pdlt(20)
#'
#'
#'@export
pdlt_d1classical <- function(rho, mtd, theta, min_dose, max_dose){

  gamma <- standard_dose(dose = mtd,
                         min_dose = min_dose,
                         max_dose = max_dose)

  pdlt <- function(dose){

    dose <- standard_dose(dose = dose,
                          min_dose = min_dose,
                          max_dose = max_dose)

    beta <- rep(NA, 2)
    beta[1] <- logit(rho)
    beta[2] <- (logit(theta) - logit(rho))/gamma

    design_matrix <- cbind(1, dose)
    lp <- design_matrix %*% beta
    out <- as.numeric(plogis(lp))
    return(out)
  }

  return(pdlt)
}

#'Generating a probability of DLT function based on the EWOC extended model
#
#'@param rho a numerical vector indicating the true value of the parameters
#'rho_0 and rho_1.
#'@param min_dose a numerical value defining the lower bound of the support of
#'the MTD.
#'@param max_dose a numerical value defining the upper bound of the support of
#'the MTD.
#'
#'@return A function with dose as an input and a probability based on the
#'logistic regression and parameters as an output.
#'
#'@examples
#'pdlt <- pdlt_d1extended(rho = c(0.05, 0.5),
#'                        min_dose = 10, max_dose = 50)
#'pdlt(20)
#'
#'@export
pdlt_d1extended <- function(rho, min_dose, max_dose){

  pdlt <- function(dose){

    dose <- standard_dose(dose = dose,
                          min_dose = min_dose,
                          max_dose = max_dose)

    beta <- rep(NA, 2)
    beta[1] <- logit(rho[1])
    beta[2] <- logit(rho[2]) - logit(rho[1])

    design_matrix <- cbind(1, dose)
    lp <- design_matrix%*%beta
    out <- as.numeric(plogis(lp))
    return(out)
  }

  return(pdlt)
}

#'Generating a probability of DLT function based on the EWOC Proportional Hazards model
#
#'@param rho a numerical value indicating the true value of the parameter rho.
#'@param mtd a numerical value indicating the true value of the parameter mtd.
#'@param theta a numerical value defining the proportion of expected patients
#'to experience a medically unacceptable, dose-limiting toxicity (DLT) if
#'administered the MTD.
#'@param min_dose a numerical value defining the lower bound of the support of
#'the MTD.
#'@param max_dose a numerical value defining the upper bound of the support of
#'the MTD.
#'@param tau a numerical value defining the period of time for a possible
#'toxicity be observed.
#'@param distribution a character establishing the distribution for the time of
#'events. It can be defined as 'exponential' or 'weibull'.
#'@param shape a numerical value indicating the true value of the parameter shape.
#'It is only necessary if 'distribution' = "weibull".
#'
#'@return A function with dose as an input and a probability based on the
#'logistic regression and parameters as an output.
#'
#'@examples
#'pdlt <- pdlt_d1ph(rho = 0.05, mtd = 40, theta = 0.33,
#'                  min_dose = 30, max_dose = 50,
#'                  tau = 10, distribution = "exponential")
#'pdlt(40)
#'
#'@export
pdlt_d1ph <- function(rho, mtd, shape = NULL, theta, min_dose, max_dose,
                      tau, distribution) {

  gamma <- standard_dose(dose = mtd,
                         min_dose = min_dose,
                         max_dose = max_dose)

  pdlt <- function(dose){

    dose <- standard_dose(dose = dose,
                          min_dose = min_dose,
                          max_dose = max_dose)

    if (distribution != "weibull")
      shape <- 1

    beta <- rep(NA, 2)
    beta[1] <- log(-log(1 - rho[1])/(tau^shape))
    beta[2] <- log(log(1 - theta)/log(1 - rho[1]))/gamma

    design_matrix <- cbind(1, dose)

    out <- 1 - exp(-(exp(design_matrix%*%beta)*tau)^shape)

    return(out)
  }

  return(pdlt)
}

