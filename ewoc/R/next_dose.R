next_dose.ewoc_d1classical <- function(data){

  rho <- data$mcmc$rho
  gamma <- data$mcmc$gamma
  beta <- data$mcmc$beta

  mtd <- inv_standard_dose(dose = gamma,
                           min_dose = data$limits$min_dose,
                           max_dose = data$limits$max_dose)

  next_dose <- quantile(mtd, probs = data$alpha)

  next_dose <- ifelse(next_dose > data$limits$last_dose,
                      data$limits$last_dose,
                      ifelse(next_dose < data$limits$first_dose,
                             data$limits$first_dose, next_dose))

  if (data$type == "continuous")
    if ((next_dose - data$current_dose) > data$max_increment)
      next_dose <- data$current_dose + data$max_increment

  if (data$type == "discrete"){
    next_dose <- rounding_system(dose = next_dose,
                                 grid = data$dose_set,
                                 rounding = data$rounding)

    if (data$no_skip_dose)
      if (which(data$dose_set == next_dose) -
          which(data$dose_set == data$current_dose) > 1)
        next_dose <- data$dose_set[(which(data$dose_set == data$current_dose) + 1)]
  }

  next_gamma <- standard_dose(dose = next_dose,
                              min_dose = data$limits$min_dose,
                              max_dose = data$limits$max_dose)

  pdlt <- as.numeric(plogis(cbind(1, next_gamma)%*%t(beta)))

    out <- list(mtd = mtd, pdlt = pdlt, next_dose = next_dose,
                rho = rho, gamma = gamma, sample = data$mcmc$sample)
  return(out)
}


next_dose.ewoc_d1extended <- function(data){

  rho <- data$mcmc$rho
  beta <- data$mcmc$beta

  scale <- logit(rho[, 2]) - logit(rho[, 1])
  gamma <- (logit(data$theta) - logit(rho[, 1]))/scale
  mtd <- inv_standard_dose(dose = gamma,
                           min_dose = data$limits$min_dose,
                           max_dose = data$limits$max_dose)

  next_dose <- quantile(mtd, probs = data$alpha)

  next_dose <- ifelse(next_dose > data$limits$last_dose,
                      data$limits$last_dose,
                      ifelse(next_dose < data$limits$first_dose,
                             data$limits$first_dose, next_dose))

  if (data$type == "continuous")
    if ((next_dose - data$current_dose) > data$max_increment)
      next_dose <- data$current_dose + data$max_increment

  if (data$type == "discrete"){
    next_dose <- rounding_system(dose = next_dose,
                                 grid = data$dose_set,
                                 rounding = data$rounding)

    if (data$no_skip_dose)
      if (which(data$dose_set == next_dose) -
          which(data$dose_set == data$current_dose) > 1)
        next_dose <- data$dose_set[(which(data$dose_set == data$current_dose) + 1)]
  }

  next_gamma <- standard_dose(dose = next_dose,
                              min_dose = data$limits$min_dose,
                              max_dose = data$limits$max_dose)

  pdlt <- as.numeric(plogis(cbind(1, next_gamma)%*%t(beta)))

  out <- list(mtd = mtd, pdlt = pdlt, next_dose = next_dose,
              rho = rho, gamma = gamma, sample = data$mcmc$sample)
  return(out)
}

next_dose.ewoc_d1ph <- function(data){

  gamma <- data$mcmc$gamma - 10^(-2)
  shape <- data$mcmc$shape
  rho <- data$mcmc$rho
  beta <- data$mcmc$beta

  mtd <- inv_standard_dose(dose = gamma,
                           min_dose = data$limits$min_dose,
                           max_dose = data$limits$max_dose)

  next_dose <- quantile(mtd, probs = data$alpha)

  next_dose <- ifelse(next_dose > data$limits$last_dose,
                      data$limits$last_dose,
                      ifelse(next_dose < data$limits$first_dose,
                             data$limits$first_dose, next_dose))

  if (data$type == "continuous")
    if ((next_dose - data$current_dose) > data$max_increment)
      next_dose <- data$current_dose + data$max_increment

  if (data$type == "discrete"){
    next_dose <- rounding_system(dose = next_dose,
                                 grid = data$dose_set,
                                 rounding = data$rounding)

    if (data$no_skip_dose)
      if (which(data$dose_set == next_dose) -
          which(data$dose_set == data$current_dose) > 1)
        next_dose <- data$dose_set[(which(data$dose_set == data$current_dose) + 1)]
  }

  next_gamma <- standard_dose(dose = next_dose,
                              min_dose = data$limits$min_dose,
                              max_dose = data$limits$max_dose)

  if (data$distribution != "weibull")
    shape <- 1
  pdlt <- as.numeric(1 - exp(-exp(cbind(1, next_gamma)%*%t(beta))*
                             (data$tau^shape)))

  out <- list(mtd = mtd, pdlt = pdlt, next_dose = next_dose,
              rho = rho, shape = shape, gamma = gamma,
              sample = data$mcmc$sample)
  return(out)
}

