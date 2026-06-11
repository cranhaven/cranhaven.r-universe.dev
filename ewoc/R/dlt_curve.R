#'Plot the DLT curve based on the EWOC classical model
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
#'@param dose_set a numerical vector of doses that probability of DLT will
#'be calculated.
#'@export
dlt_curve_d1classical <- function(mtd, rho, theta, min_dose, max_dose,
                                dose_set = NULL){

  gamma <- (mtd - min_dose)/(max_dose - min_dose)

  aux_pdlt <- function(x){
    x <- (x - min_dose)/(max_dose - min_dose)

    beta <- rep(NA, 2)
    beta[1] <- logit(rho)
    beta[2] <- (logit(theta) - logit(rho))/gamma
    eta <- beta[1] + beta[2]*x
    out <- plogis(eta, 0, 1)
    return(out)
  }

  dp <- data.frame(x = c(min_dose, max_dose))

  gp <- ggplot(dp, aes_string(x = "x")) +
    stat_function(fun = aux_pdlt) +
    labs(y = "P(DLT|dose)", x = "dose") +
    geom_hline(yintercept = theta, linetype = 2) +
    theme_bw() +
    scale_x_continuous(breaks =
                         c(seq(min_dose, max_dose,
                               by = (max_dose - min_dose)/5), mtd))

  if (!is.null(dose_set)){
    gp <- gp + scale_x_continuous(breaks = c(dose_set, mtd))
    p <- aux_pdlt(dose_set)
    out <- list(plot = gp, pdlt = p)
  } else {
    out <- gp
  }

  return(out)
}



#'Plot the DLT curve based on the EWOC extended model
#'
#'@import stats
#'
#'@param rho a numerical vector indicating the true value of the parameters
#'rho_0 and rho_1.
#'@param theta a numerical value defining the proportion of expected patients
#'to experience a medically unacceptable, dose-limiting toxicity (DLT) if
#'administered the MTD.
#'@param min_dose a numerical value defining the lower bound of the support of
#'the MTD.
#'@param max_dose a numerical value defining the upper bound of the support of
#'the MTD.
#'@param dose_set a numerical vector of doses that probability of DLT will
#'be calculated.
#'@export
dlt_curve_d1extended <- function(rho, theta, min_dose, max_dose,
                                 dose_set = NULL){

  gamma <- (logit(theta) - logit(rho[1]))/(logit(rho[2]) - logit(rho[1]))
  mtd <- round(inv_standard_dose(gamma,
                                 min_dose = min_dose,
                                 max_dose = max_dose), 2)

  aux_pdlt <- function(x){
    x <- (x - min_dose)/(max_dose - min_dose)

    beta <- rep(NA, 2)
    beta[1] <- logit(rho[1])
    beta[2] <- logit(rho[2]) - logit(rho[1])
    eta <- beta[1] + beta[2]*x
    out <- plogis(eta, 0, 1)
    return(out)
  }

  dp <- data.frame(x = c(min_dose, max_dose))

  gp <- ggplot(dp, aes_string(x = "x")) +
    stat_function(fun = aux_pdlt) +
    labs(y = "P(DLT|dose)", x = "dose") +
    geom_hline(yintercept = theta, linetype = 2) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks =
                         c(seq(min_dose, max_dose,
                               by = (max_dose - min_dose)/5), mtd))

  if (!is.null(dose_set)){
    gp <- gp + scale_x_continuous(breaks = c(dose_set, mtd))
    p <- aux_pdlt(dose_set)
    out <- list(plot = gp, pdlt = p)
  } else {
    out <- gp
  }

  return(out)
}


#'Plot the DLT curve based on the EWOC proportional hazards model
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
#'events. It can be defined as 'exponential' or 'weibull'.
#'@param shape a numerical value indicating the true value of the parameter shape.
#'It is only necessary if 'distribution' = "weibull".
#'@param dose_set a numerical vector of doses that probability of DLT will
#'be calculated.
#'@export
dlt_curve_d1ph <- function(mtd, rho, theta, min_dose, max_dose,
                           shape, tau, distribution = "exponential",
                           dose_set = NULL){

  gamma <- (mtd - min_dose)/(max_dose - min_dose)

  aux_pdlt <- function(x){
    x <- (x - min_dose)/(max_dose - min_dose)

    if (distribution != "weibull")
      shape <- 1

    beta <- rep(NA, 2)
    beta[1] <- log(-log(1 - rho[1])/(tau^shape))
    beta[2] <- log(log(1 - theta)/log(1 - rho[1]))/gamma
    eta <- beta[1] + beta[2]*x
    out <- plogis(eta, 0, 1)
    return(out)
  }

  dp <- data.frame(x = c(min_dose, max_dose))

  gp <- ggplot(dp, aes_string(x = "x")) +
    stat_function(fun = aux_pdlt) +
    labs(y = "P(DLT|dose)", x = "dose") +
    geom_hline(yintercept = theta, linetype = 2) +
    theme_bw() +
    scale_x_continuous(breaks =
                         c(seq(min_dose, max_dose,
                               by = (max_dose - min_dose)/5), mtd))

  if (!is.null(dose_set)){
    gp <- gp + scale_x_continuous(breaks = c(dose_set, mtd))
    p <- aux_pdlt(dose_set)
    out <- list(plot = gp, pdlt = p)
  } else {
    out <- gp
  }

  return(out)
}
