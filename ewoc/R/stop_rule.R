#'Generating a stop rule function for EWOC classical model
#
#'@param step an object from the class 'ewoc_d1classical'.
#'
#'@details The stop rule function is evaluated at each step of the trial.
#'It can defined based on any information contained in the object 'step' that
#'is the output from one of the functions 'ewoc_d1classical'.
#'
#'@return a logical character indicating if the trial should be stopped or not.
#'
#'@examples
#'\dontshow{
#'DLT <- 0
#'dose <- 20
#'step_zero <- ewoc_d1classical(DLT ~ dose, type = 'discrete',
#'                            theta = 0.33, alpha = 0.25,
#'                            min_dose = 0, max_dose = 100,
#'                            dose_set = seq(0, 100, 20),
#'                            rho_prior = matrix(1, ncol = 2, nrow = 1),
#'                            mtd_prior = matrix(1, ncol = 2, nrow = 1),
#'                            rounding = "nearest")
#'stop_rule_d1classical(step_zero)
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 1, sample_size = 2,
#'                        alpha_strategy = "conditional",
#'                        response_sim = response_sim,
#'                        stop_rule_sim = stop_rule_d1classical,
#'                        ncores = 1)
#'}
#'
#'\dontrun{
#'DLT <- 0
#'dose <- 20
#'step_zero <- ewoc_d1classical(DLT ~ dose, type = 'discrete',
#'                            theta = 0.33, alpha = 0.25,
#'                            min_dose = 0, max_dose = 100,
#'                            dose_set = seq(0, 100, 20),
#'                            rho_prior = matrix(1, ncol = 2, nrow = 1),
#'                            mtd_prior = matrix(1, ncol = 2, nrow = 1),
#'                            rounding = "nearest")
#'stop_rule_d1classical(step_zero)
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 2, sample_size = 30,
#'                        alpha_strategy = "conditional",
#'                        response_sim = response_sim,
#'                        stop_rule_sim = stop_rule_d1_classical,
#'                        ncores = 1)
#'}
#'@export
stop_rule_d1classical <- function(step){

  n_sim <- length(step$trial$rho)
  p1 <- rep(NA, n_sim)

  for (i in 1:n_sim){
    pdlt_sim <- pdlt_d1classical(rho = step$rho[i],
                               mtd = step$mtd[i],
                               theta = step$trial$theta,
                               min_dose = step$trial$min_dose,
                               max_dose = step$trial$max_dose)
    p1[i] <- pdlt_sim(step$trial$first_dose)
  }

  out <- ifelse(mean(p1[i] > step$trial$theta) > 0.95, TRUE, FALSE)
  return(out)
}


#'Generating a stop rule function for EWOC extended model
#
#'@param step an object from the class 'ewoc_d1extended'.
#'
#'@details The stop rule function is evaluated at each step of the trial.
#'It can defined based on any information contained in the object 'step' that
#'is the output from one of the functions ewoc_d1extended'.
#'
#'@return a logical character indicating if the trial should be stopped or not.
#'
#'@examples
#'\dontshow{
#'DLT <- 0
#'dose <- 20
#'step_zero <- ewoc_d1extended(DLT ~ dose, type = 'discrete',
#'                            theta = 0.33, alpha = 0.25,
#'                            min_dose = 0, max_dose = 100,
#'                            dose_set = seq(0, 100, 20),
#'                            rho_prior = matrix(1, ncol = 2, nrow = 2),
#'                            rounding = "nearest")
#'stop_rule_d1extended(step_zero)
#'response_sim <- response_d1extended(rho = c(0.05, 0.95),
#'                                    min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 1, sample_size = 2,
#'                        alpha_strategy = "conditional",
#'                        response_sim = response_sim,
#'                        stop_rule_sim = stop_rule_d1extended,
#'                        ncores = 1)
#'}
#'
#'\dontrun{
#'DLT <- 0
#'dose <- 20
#'step_zero <- ewoc_d1extended(DLT ~ dose, type = 'discrete',
#'                            theta = 0.33, alpha = 0.25,
#'                            min_dose = 0, max_dose = 100,
#'                            dose_set = seq(0, 100, 20),
#'                            rho_prior = matrix(1, ncol = 2, nrow = 2),
#'                            rounding = "nearest")
#'stop_rule_d1extended(step_zero)
#'response_sim <- response_d1extended(rho = c(0.05, 0.95),
#'                                    min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 2, sample_size = 30,
#'                        alpha_strategy = "conditional",
#'                        response_sim = response_sim,
#'                        stop_rule_sim = stop_rule_d1extended,
#'                        ncores = 1)
#'}
#'@export
stop_rule_d1extended <- function(step){

  n_sim <- nrow(step$trial$rho)
  p1 <- rep(NA, n_sim)

  for (i in 1:n_sim){
    pdlt_sim <- pdlt_d1extended(rho = step$rho[i, ],
                                min_dose = step$trial$min_dose,
                                max_dose = step$trial$max_dose)
    p1[i] <- pdlt_sim(step$trial$first_dose)
  }

  out <- ifelse(mean(p1[i] > step$trial$theta) > 0.95, TRUE, FALSE)
  return(out)
}


#'Generating a stop rule function for EWOC proportional hazards model
#
#'@param step an object from the class 'ewoc_d1ph'.
#'
#'@details The stop rule function is evaluated at each step of the trial.
#'It can defined based on any information contained in the object 'step' that
#'is the output from one of the functions 'ewoc_d1ph'.
#'
#'@return a logical character indicating if the trial should be stopped or not.
#'
#'@examples
#'\dontshow{
#'time <- 9
#'status <- 0
#'dose <- 20
#'step_zero <- ewoc_d1ph(cbind(time, status) ~ dose, type = 'discrete',
#'                       theta = 0.33, alpha = 0.25, tau = 10,
#'                       min_dose = 20, max_dose = 100,
#'                       dose_set = seq(20, 100, 20),
#'                       rho_prior = matrix(1, ncol = 2, nrow = 1),
#'                       mtd_prior = matrix(1, ncol = 2, nrow = 1),
#'                       distribution = 'exponential',
#'                       rounding = 'nearest')
#'stop_rule_d1ph(step_zero)
#'response_sim <- response_d1ph(rho = 0.05, mtd = 20, theta = 0.33,
#'                              min_dose = 10, max_dose = 50,
#'                              tau = 10, distribution = "exponential")
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                       n_sim = 1, sample_size = 2,
#'                       alpha_strategy = "conditional",
#'                       response_sim = response_sim,
#'                       stop_rule_sim = stop_rule_d1ph,
#'                       ncores = 1)
#'}
#'
#'\dontrun{
#'time <- 9
#'status <- 0
#'dose <- 20
#'step_zero <- ewoc_d1ph(cbind(time, status) ~ dose, type = 'discrete',
#'                       theta = 0.33, alpha = 0.25, tau = 10,
#'                       min_dose = 20, max_dose = 100,
#'                       dose_set = seq(20, 100, 20),
#'                       rho_prior = matrix(1, ncol = 2, nrow = 1),
#'                       mtd_prior = matrix(1, ncol = 2, nrow = 1),
#'                       distribution = 'exponential',
#'                       rounding = 'nearest')
#'stop_rule_d1ph(step_zero)
#'response_sim <- response_d1ph(rho = 0.05, mtd = 20, theta = 0.33,
#'                              min_dose = 10, max_dose = 50,
#'                              tau = 10, distribution = "exponential")
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                       n_sim = 2, sample_size = 30,
#'                       alpha_strategy = "conditional",
#'                       response_sim = response_sim,
#'                       stop_rule_sim = stop_rule_d1ph,
#'                       ncores = 1)
#'}
#'@export
stop_rule_d1ph <- function(step){

  n_sim <- nrow(step$trial$rho)
  p1 <- rep(NA, n_sim)

  for (i in 1:n_sim){
    pdlt_sim <- pdlt_d1ph(rho = step$rho[i],
                          mtd = step$mtd[i],
                          theta = step$trial$theta,
                          min_dose = step$trial$min_dose,
                          max_dose = step$trial$max_dose,
                          tau = step$trial$tau,
                          distribution = step$trial$distribution)
    p1[i] <- pdlt_sim(step$trial$first_dose)
  }

  out <- ifelse(mean(p1[i] > step$trial$theta) > 0.95, TRUE, FALSE)
  return(out)
}
