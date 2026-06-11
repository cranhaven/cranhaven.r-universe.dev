#'Generating a binary response function based on the EWOC classical model
#'
#'@import stats
#'
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
#'@return A function with dose as an input and a Binomial variable based on the
#'parameters as an output.
#'
#'@examples
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'response_sim(20)
#'
#'@export
response_d1classical <- function(rho, mtd, theta, min_dose, max_dose) {

  gamma <- standard_dose(dose = mtd,
                         min_dose = min_dose,
                         max_dose = max_dose)

  response_sim <- function(dose){

    dose <- standard_dose(dose = dose,
                          min_dose = min_dose,
                          max_dose = max_dose)

    beta <- rep(NA, 2)
    beta[1] <- logit(rho)
    beta[2] <- (logit(theta) - logit(rho))/gamma

    design_matrix <- cbind(1, dose)
    lp <- design_matrix %*% beta
    p <- as.numeric(plogis(lp))
    out <- rbinom(n = length(dose), size = 1, prob = p)
    return(out)
  }

  out <- response_sim
  return(out)
}


#'Generating a binary response function based on the EWOC extended model
#'
#'@import stats
#'
#'@param rho a numerical vector indicating the true value of the parameters
#'rho_0 and rho_1.
#'@param min_dose a numerical value defining the lower bound of the support of
#'the MTD.
#'@param max_dose a numerical value defining the upper bound of the support of
#'the MTD.
#'
#'@return A function with dose as an input and a Binomial variable based on the
#'parameters as an output.
#'
#'@examples
#'response_sim <- response_d1extended(rho = c(0.05, 0.5),
#'                                    min_dose = 10, max_dose = 50)
#'response_sim(20)
#'
#'@export
response_d1extended <- function(rho, min_dose, max_dose) {

  response_sim <- function(dose){

    dose <- standard_dose(dose = dose,
                          min_dose = min_dose,
                          max_dose = max_dose)

    beta <- rep(NA, 2)
    beta[1] <- logit(rho[1])
    beta[2] <- logit(rho[2]) - logit(rho[1])

    design_matrix <- cbind(1, dose)
    lp <- design_matrix %*% beta
    p <- as.numeric(plogis(lp))
    out <- rbinom(n = length(dose), size = 1, prob = p)
    return(out)
  }

  out <- response_sim
  return(out)
}

#'Generating a response function based on the EWOC Proportional Hazards model
#'
#'@import stats
#'
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
#'events.
#'@param shape a numerical value indicating the true value of the parameter shape.
#'It is only necessary if 'distribution' = "weibull".
#'
#'@return A function with dose as an input and a Binomial variable based on the
#'parameters as an output.
#'
#'@examples
#'response_sim <- response_d1ph(rho = 0.05, mtd = 40, theta = 0.33,
#'                              min_dose = 30, max_dose = 50,
#'                              tau = 10, distribution = "exponential")
#'response_sim(40)
#'
#'@export
response_d1ph <- function(rho, mtd, theta, min_dose, max_dose,
                          tau, distribution, shape = NULL) {

  gamma <- standard_dose(dose = mtd,
                         min_dose = min_dose,
                         max_dose = max_dose)

  response_sim <- function(dose){

    dose <- standard_dose(dose = dose,
                          min_dose = min_dose,
                          max_dose = max_dose)

    if (distribution == "weibull") {
      if (is.null(shape))
        stop("Weibull distribution requires a shape parameter.")
    } else {
      shape <- 1
    }

    u <- runif(length(dose))
    design <- cbind(1, dose)

    beta <- rep(NA, 2)
    beta[1] <- log(-log(1 - rho)/(tau^shape))
    beta[2] <- log(log(1 - theta)/log(1 - rho))/gamma
    out <- as.numeric((-log(u)*exp(-design%*%beta))^(1/shape))

    return(out)
  }

  out <- response_sim
  return(out)
}

