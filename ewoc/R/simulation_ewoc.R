#'EWOC simulation
#'
#'Generic function for simulating EWOC trials.
#'
#'@param step_zero an object from the classes either 'ewoc_d1classical' or 'ewoc_d1extended' or
#''ewoc_d1ph' created using the first cohort data.
#'@param n_sim a number indicating the number of phase I clinical trials
#'to be simulated.
#'@param sample_size a number indicating the number of patients enrolled for
#'each clinical trial.
#'@param response_sim a function which is self-contained and will be used
#'as a generator function of the response variables in the simulation.
#'Its only input is 'dose' and output is the indicator of DLT for classical and
#'extended EWOC and the time until DLT for proportional hazards EWOC.
#'@param n_cohort a number indicating the number of patients enrolled at each cohort.
#'It is only used for 'ewoc_d1classical' and 'ewoc_d1extended'.
#'@param fixed_first_cohort a logical value indicating if the first cohort
#'should be randomly generated or be fixed as the input in 'step_zero'.
#'@param alpha_strategy a character indicating the strategy to apply for the
#'feasibility value. Default is "constant". Options are "increasing" and
#'"conditional".
#'@param alpha_rate a numerical value indicating the rate of the
#'feasibility strategy. Only necessary if alpha_strategy is either
#''increasing' or 'conditional'.
#'@param stop_rule_sim a function having as an input an object containing all
#'the information related to the trial as the returned object trial from either
#'\code{ewoc_d1classical}, \code{ewoc_d1extended}, \code{ewoc_d1ph} and as
#'output a logical value indicating the trial should be stopped.
#'@param ncores a numeric value indicating the number of cores to be used in the
#'simulation performed in parallel. Use parallel::detectCores() to check the number of
#'cores available.
#'@param seed is an integer value, containing the random number generator (RNG) state for random number generation.
#'@param ... For an object \code{step_zero} with class 'ewoc_d1ph',
#'the argument \code{rate_sim} which controls the rate of accrue of patients following a Poisson process. The default is 1.
#'
#'@return \code{alpha_sim} a matrix \code{n_sim} x \code{sample_size} containing
#'the values of feasibility used for each step in the trial and each trial in
#'the simulation.
#'@return \code{dlt_sim} a matrix \code{n_sim} x \code{sample_size} containing
#'ones and zeros indicating the occurrence of DLT (1) and the absence of DLT (0)
#'for each step in the trial and each trial in the simulation.
#'@return \code{dose_sim} a matrix \code{n_sim} x \code{sample_size} containing
#'the doses assigned for each step in the trial and each trial in the simulation.
#'@return \code{mtd_sim} a numeric vector \code{n_sim} x 1 containing
#'the recommended MTD for each trial in the simulation.
#'@return \code{rho_sim} a numeric vector \code{n_sim} x k containing
#'the estimated rho parameter(s) for each trial in the simulation, where k = 1
#'for ewoc_d1classical, ewoc_d1ph, and k = 2 for ewoc_d1extended.
#'
#'@examples
#'\dontshow{
#'### classical EWOC
#'DLT <- 0
#'dose <- 20
#'step_zero <- ewoc_d1classical(DLT ~ dose, type = 'discrete',
#'                           theta = 0.33, alpha = 0.25,
#'                           min_dose = 20, max_dose = 100,
#'                           dose_set = seq(20, 100, 20),
#'                           rho_prior = matrix(1, ncol = 2, nrow = 1),
#'                           mtd_prior = matrix(1, ncol = 2, nrow = 1),
#'                           rounding = "nearest")
#'response_sim <- response_d1classical(rho = 0.05, mtd = 60, theta = 0.33,
#'                                  min_dose = 20, max_dose = 100)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                      n_sim = 1, sample_size = 2, n_cohort = 1,
#'                      alpha_strategy = "conditional",
#'                      response_sim = response_sim,
#'                      fixed_first_cohort =  TRUE,
#'                      ncores = 1)
#'
#'### Extended EWOC
#'DLT <- 0
#'dose <- 20
#'step_zero <- ewoc_d1extended(DLT ~ dose, type = 'discrete',
#'                          theta = 0.33, alpha = 0.25,
#'                          min_dose = 20, max_dose = 100,
#'                          dose_set = seq(20, 100, 20),
#'                          rho_prior = matrix(1, ncol = 2, nrow = 2),
#'                          rounding = "nearest")
#'response_sim <- response_d1extended(rho = c(0.05, 0.5),
#'                                  min_dose = 20, max_dose = 100)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                      n_sim = 1, sample_size = 2, n_cohort = 1,
#'                      alpha_strategy = "conditional",
#'                      response_sim = response_sim,
#'                      ncores = 1)
#'
#'### PH EWOC
#'time <- 0
#'status <- 0
#'dose <- 20
#'
#'step_zero <- ewoc_d1ph(cbind(time, status) ~ dose, type = 'discrete',
#'                      theta = 0.33, alpha = 0.25, tau = 10,
#'                      min_dose = 20, max_dose = 100,
#'                      dose_set = seq(20, 100, 20),
#'                      rho_prior = matrix(1, ncol = 2, nrow = 1),
#'                      mtd_prior = matrix(1, ncol = 2, nrow = 1),
#'                      distribution = 'exponential',
#'                      rounding = 'nearest')
#'response_sim <- response_d1ph(rho = 0.05, mtd = 40, theta = 0.33,
#'                             min_dose = 20, max_dose = 100,
#'                             tau = 10, distribution = "exponential")
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                      n_sim = 1, sample_size = 2, n_cohort = 1,
#'                      alpha_strategy = "conditional",
#'                      response_sim = response_sim,
#'                      fixed_first_cohort = TRUE,
#'                      ncores = 1)
#'}
#'
#'\dontrun{
#'### Classical EWOC
#'DLT <- 0
#'dose <- 20
#'step_zero <- ewoc_d1classical(DLT ~ dose, type = 'discrete',
#'                           theta = 0.33, alpha = 0.25,
#'                           min_dose = 20, max_dose = 100,
#'                           dose_set = seq(20, 100, 20),
#'                           rho_prior = matrix(1, ncol = 2, nrow = 1),
#'                           mtd_prior = matrix(1, ncol = 2, nrow = 1),
#'                           rounding = "nearest")
#'response_sim <- response_d1classical(rho = 0.05, mtd = 60, theta = 0.33,
#'                                  min_dose = 20, max_dose = 100)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                       n_sim = 2, sample_size = 30, n_cohort = 1,
#'                       alpha_strategy = "conditional",
#'                       response_sim = response_sim,
#'                       ncores = 1)
#'
#'### Extended EWOC
#'DLT <- 0
#'dose <- 20
#'step_zero <- ewoc_d1extended(DLT ~ dose, type = 'discrete',
#'                           theta = 0.33, alpha = 0.25,
#'                           min_dose = 20, max_dose = 100,
#'                           dose_set = seq(20, 100, 20),
#'                           rho_prior = matrix(1, ncol = 2, nrow = 2),
#'                           rounding = "nearest")
#'response_sim <- response_d1extended(rho = c(0.05, 0.5),
#'                                   min_dose = 20, max_dose = 100)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                       n_sim = 2, sample_size = 30, n_cohort = 1,
#'                       alpha_strategy = "conditional",
#'                       response_sim = response_sim,
#'                       ncores = 1)
#'
#'### PH EWOC
#'time <- 0
#'status <- 0
#'dose <- 20
#'
#'step_zero <- ewoc_d1ph(cbind(time, status) ~ dose, type = 'discrete',
#'                      theta = 0.33, alpha = 0.25, tau = 10,
#'                      min_dose = 20, max_dose = 100,
#'                      dose_set = seq(20, 100, 20),
#'                      rho_prior = matrix(1, ncol = 2, nrow = 1),
#'                      mtd_prior = matrix(1, ncol = 2, nrow = 1),
#'                      distribution = 'exponential',
#'                      rounding = 'nearest')
#'response_sim <- response_d1ph(rho = 0.05, mtd = 60, theta = 0.33,
#'                             min_dose = 20, max_dose = 100,
#'                             tau = 10, distribution = "exponential")
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                       n_sim = 2, sample_size = 30, n_cohort = 1,
#'                       alpha_strategy = "conditional",
#'                       response_sim = response_sim,
#'                       ncores = 1)
#'}
#'
#'@importFrom foreach foreach %dopar%
#'@importFrom doRNG %dorng%
#'@importFrom doParallel registerDoParallel stopImplicitCluster
#'
#'@export
ewoc_simulation <- function(step_zero, n_sim, sample_size, response_sim,
                            fixed_first_cohort = TRUE, n_cohort = 1,
                            alpha_strategy = "conditional",
                            alpha_rate = 0.05,
                            stop_rule_sim = NULL,
                            ncores = 1, seed = 1234,
                            ...){

  if (n_cohort != 1 & class(step_zero)[1] == "ewoc_d1ph")
    stop("Proportional Hazards EWOC does not support cohort with more than one patient.")

  UseMethod("ewoc_simulation", object = step_zero)
}


#'@export
ewoc_simulation.ewoc_d1classical <- function(step_zero, n_sim, sample_size, response_sim,
                                           fixed_first_cohort = TRUE, n_cohort = 1,
                                           alpha_strategy = "conditional",
                                           alpha_rate = 0.05,
                                           stop_rule_sim = NULL,
                                           ncores = 1, seed = 1234, ...){

  if (is.null(response_sim))
    stop("'response_sim' function should be defined.")
  if (((sample_size - nrow(step_zero$trial$design_matrix)) %% n_cohort) != 0)
    stop("Sample size minus cohort size for first cohort is not a multiple of `n_cohort`.")

  dose_sim <- matrix(NA, ncol = sample_size, nrow = n_sim)
  dlt_sim <- matrix(NA, ncol = sample_size, nrow = n_sim)
  mtd_sim <- matrix(NA, ncol = 1, nrow = n_sim)
  rho_sim <- matrix(NA, ncol = 1, nrow = n_sim)
  alpha_sim <- matrix(NA, ncol = sample_size, nrow = n_sim)

  registerDoParallel(ncores)
  set.seed(seed)
  result <-
    foreach(i = 1:n_sim,
            .combine='comb',
            .multicombine=TRUE,
            .init=list(list(), list(), list(), list(), list())) %dorng% {

              dose <- as.numeric(step_zero$trial$design_matrix[, 2])

              if (fixed_first_cohort) {
                dlt <- as.numeric(step_zero$trial$response)
              } else {
                dlt <- response_sim(dose = dose)
              }

              alpha <- rep(step_zero$trial$alpha, n_cohort)

              j <- (length(dose)+1)

              while (j <= sample_size) {

                formula <- dlt[1:(j-1)] ~ dose[1:(j-1)]
                resolution <- ifelse(!is.na(dlt), 1, 0)

                alpha[j:(j + n_cohort - 1)] <- feasibility(alpha = alpha[1:(j-1)],
                                                           strategy = alpha_strategy,
                                                           dlt = dlt[1:(j-1)],
                                                           resolution = resolution[1:(j-1)],
                                                           rate = alpha_rate)

                update <- ewoc_d1classical(formula,
                                         type = step_zero$trial$type,
                                         theta = step_zero$trial$theta,
                                         alpha = alpha[j],
                                         min_dose = step_zero$trial$min_dose,
                                         max_dose = step_zero$trial$max_dose,
                                         first_dose = step_zero$trial$first_dose,
                                         last_dose = step_zero$trial$last_dose,
                                         dose_set = step_zero$trial$dose_set,
                                         max_increment = step_zero$trial$max_increment,
                                         no_skip_dose = step_zero$trial$no_skip_dose,
                                         rho_prior = step_zero$trial$rho_prior,
                                         mtd_prior = step_zero$trial$mtd_prior,
                                         rounding = step_zero$trial$rounding)

                if (!is.null(stop_rule_sim))
                  if (stop_rule_sim(update)){
                    dose[j:sample_size] <- NA
                    dlt[j:sample_size] <- NA
                    mtd_estimate <- NA
                    rho_estimate <- NA
                    break
                  }

                dose[j:(j + n_cohort - 1)] <- update$next_dose
                dlt[j:(j + n_cohort - 1)] <- response_sim(dose = dose[j:(j + n_cohort - 1)])

                j <- j + n_cohort
              }

              update <- ewoc_d1classical(formula,
                                       type = step_zero$trial$type,
                                       theta = step_zero$trial$theta,
                                       alpha = alpha[length(alpha)],
                                       min_dose = step_zero$trial$min_dose,
                                       max_dose = step_zero$trial$max_dose,
                                       first_dose = step_zero$trial$first_dose,
                                       last_dose = step_zero$trial$last_dose,
                                       dose_set = step_zero$trial$dose_set,
                                       max_increment = step_zero$trial$max_increment,
                                       no_skip_dose = step_zero$trial$no_skip_dose,
                                       rho_prior = step_zero$trial$rho_prior,
                                       mtd_prior = step_zero$trial$mtd_prior,
                                       rounding = step_zero$trial$rounding)

              mtd_estimate <- update$next_dose
              rho_estimate <- median(update$rho)

              list(dose, dlt, mtd_estimate, rho_estimate, alpha)
            }
  stopImplicitCluster()

  dose_sim <- matrix(as.numeric(result[[1]]), nrow = n_sim, ncol = sample_size)
  dlt_sim <-  matrix(as.numeric(result[[2]]), nrow = n_sim, ncol = sample_size)
  mtd_sim <- as.numeric(result[[3]])
  rho_sim <- as.numeric(result[[4]])
  alpha_sim <- matrix(as.numeric(result[[5]]), nrow = n_sim, ncol = sample_size)

  out <- list(trial = step_zero$trial,
              dose_sim = dose_sim, dlt_sim = dlt_sim,
              mtd_sim = mtd_sim, rho_sim = rho_sim, alpha_sim = alpha_sim)

  class(out) <- c("ewoc_simulation_d1classical", "nocov")
  return(out)
}

#'@importFrom foreach foreach %dopar%
#'@importFrom doRNG %dorng%
#'@importFrom doParallel registerDoParallel stopImplicitCluster
#'@export
ewoc_simulation.ewoc_d1extended <- function(step_zero, n_sim, sample_size, response_sim,
                                            fixed_first_cohort = TRUE, n_cohort = 1,
                                            alpha_strategy = "conditional",
                                            alpha_rate = 0.05,
                                            stop_rule_sim = NULL,
                                            ncores = 1, seed = 1234, ...){

  if (is.null(response_sim))
    stop("'response_sim' function should be defined.")
  if (nrow(step_zero$trial$design_matrix) != n_cohort)
    stop("The number of patients in the first cohort is not the same number of patients in `n_cohort`.")
  if ((sample_size %% n_cohort) != 0)
    stop("`sample_size` is not a multiple of `n_cohort`.")

  dose_sim <- matrix(NA, ncol = sample_size, nrow = n_sim)
  dlt_sim <- matrix(NA, ncol = sample_size, nrow = n_sim)
  mtd_sim <- matrix(NA, ncol = 1, nrow = n_sim)
  rho_sim <- matrix(NA, ncol = 1, nrow = n_sim)
  alpha_sim <- matrix(NA, ncol = sample_size, nrow = n_sim)

  registerDoParallel(ncores)
  set.seed(seed)
  result <-
    foreach(i = 1:n_sim,
            .combine='comb',
            .multicombine=TRUE,
            .init=list(list(), list(), list(), list(), list())) %dorng% {
              dose <- as.numeric(step_zero$trial$design_matrix[, 2])

              if (fixed_first_cohort) {
                dlt <- as.numeric(step_zero$trial$response)
              } else {
                dlt <- response_sim(dose = dose)
              }

              alpha <- rep(step_zero$trial$alpha, n_cohort)

              j <- (length(dose)+1)

              while (j <= sample_size) {

                formula <- dlt[1:(j-1)] ~ dose[1:(j-1)]
                resolution <- ifelse(!is.na(dlt), 1, 0)

                  alpha[j:(j + n_cohort - 1)] <- feasibility(alpha = alpha[(j-1)],
                                              strategy = alpha_strategy,
                                              dlt = dlt[1:(j-1)],
                                              resolution = resolution[1:(j-1)],
                                              rate = alpha_rate)

                  update <- ewoc_d1extended(formula,
                                            type = step_zero$trial$type,
                                            theta = step_zero$trial$theta,
                                            alpha = alpha[j],
                                            min_dose = step_zero$trial$min_dose,
                                            max_dose = step_zero$trial$max_dose,
                                            first_dose = step_zero$trial$first_dose,
                                            last_dose = step_zero$trial$last_dose,
                                            dose_set = step_zero$trial$dose_set,
                                            max_increment = step_zero$trial$max_increment,
                                            no_skip_dose = step_zero$trial$no_skip_dose,
                                            rho_prior = step_zero$trial$rho_prior,
                                            rounding = step_zero$trial$rounding)

                  if (!is.null(stop_rule_sim))
                    if (stop_rule_sim(update)){
                      dose[j:sample_size] <- NA
                      dlt[j:sample_size] <- NA
                      mtd_estimate <- NA
                      rho_estimate <- NA
                      break
                    }

                  dose[j:(j + n_cohort - 1)] <- update$next_dose
                  dlt[j:(j + n_cohort - 1)] <- response_sim(dose = dose[j:(j + n_cohort - 1)])
                  mtd_estimate <- update$next_dose
                  rho_estimate <- median(update$rho)

                  j <- j + n_cohort
              }

              update <- ewoc_d1extended(formula,
                                        type = step_zero$trial$type,
                                        theta = step_zero$trial$theta,
                                        alpha = alpha[length(alpha)],
                                        min_dose = step_zero$trial$min_dose,
                                        max_dose = step_zero$trial$max_dose,
                                        first_dose = step_zero$trial$first_dose,
                                        last_dose = step_zero$trial$last_dose,
                                        dose_set = step_zero$trial$dose_set,
                                        max_increment = step_zero$trial$max_increment,
                                        no_skip_dose = step_zero$trial$no_skip_dose,
                                        rho_prior = step_zero$trial$rho_prior,
                                        rounding = step_zero$trial$rounding)

              mtd_estimate <- update$next_dose
              rho_estimate <- apply(update$rho, 2, median)

              list(dose, dlt, mtd_estimate, rho_estimate, alpha)
            }
  stopImplicitCluster()

  dose_sim <- matrix(as.numeric(result[[1]]), nrow = n_sim, ncol = sample_size)
  dlt_sim <-  matrix(as.numeric(result[[2]]), nrow = n_sim, ncol = sample_size)
  mtd_sim <- as.numeric(result[[3]])
  rho_sim <- matrix(as.numeric(result[[4]]), nrow = n_sim, ncol = 2)
  alpha_sim <- matrix(as.numeric(result[[5]]), nrow = n_sim, ncol = sample_size)

  out <- list(trial = step_zero$trial,
              dose_sim = dose_sim, dlt_sim = dlt_sim,
              mtd_sim = mtd_sim, rho_sim = rho_sim, alpha_sim = alpha_sim)
  class(out) <- c("ewoc_simulation_d1extended", "nocov")
  return(out)
}

#'@importFrom foreach foreach %dopar%
#'@importFrom doRNG %dorng%
#'@importFrom doParallel registerDoParallel stopImplicitCluster
#'@export
ewoc_simulation.ewoc_d1ph <- function(step_zero, n_sim, sample_size, response_sim,
                                      fixed_first_cohort = TRUE, n_cohort = 1,
                                      alpha_strategy = "conditional",
                                      alpha_rate = 0.05,
                                      stop_rule_sim = NULL,
                                      ncores = 1, seed = 1234, ...){

  ndots <- list(...)
  rate <- ifelse(!is.null(ndots$rate_sim), ndots$rate_sim, 1)

  if (is.null(response_sim))
    stop("'response_sim' function should be defined.")
  if (length(alpha_strategy) != 1)
    stop("'alpha_strategy' should be defined.")

  dose_sim <- matrix(NA, ncol = sample_size, nrow = n_sim)
  dlt_sim <- matrix(NA, ncol = sample_size, nrow = n_sim)
  time_sim <- matrix(NA, ncol = sample_size, nrow = n_sim)
  mtd_sim <- matrix(NA, ncol = 1, nrow = n_sim)
  rho_sim <- matrix(NA, ncol = 1, nrow = n_sim)
  alpha_sim <- matrix(NA, ncol = sample_size, nrow = n_sim)

  registerDoParallel(ncores)
  set.seed(seed)
  result <-
    foreach(i = 1:n_sim,
            .combine='comb',
            .multicombine=TRUE,
            .init=list(list(), list(), list(), list(), list(), list(), list())) %dorng% {

              dlt <- as.numeric(step_zero$trial$response[, 2])
              dose <- as.numeric(step_zero$trial$design_matrix[, 2])
              alpha <- as.numeric(step_zero$trial$alpha)

              event_time <- ifelse(dlt == 1, as.numeric(step_zero$trial$response[, 1]),
                                   (response_sim(dose = dose) +
                                      max(as.numeric(step_zero$trial$response[, 1]))))
              event_time <- c(event_time, rep(NA, (sample_size - length(event_time))))
              current_time <- max(step_zero$trial$response[, 1])
              initial_time <- rep(0, sample_size)
              j <- 1

              while ((current_time - initial_time[sample_size]) <= step_zero$trial$tau) {

                current_time <- current_time + rexp(1, rate)

                j <- j + 1

                if (j <= sample_size)
                  initial_time[j:sample_size] <- current_time

                time_cens <- ifelse(event_time > (current_time - initial_time),
                                    ifelse((current_time - initial_time) >
                                             step_zero$trial$tau, step_zero$trial$tau,
                                           (current_time - initial_time)),
                                    ifelse(event_time > step_zero$trial$tau,
                                           step_zero$trial$tau, event_time))

                dlt <- ifelse(event_time > (current_time - initial_time), 0,
                              ifelse(event_time > step_zero$trial$tau, 0, 1))

                resolution <- ifelse(dlt == 1, 1,
                                     ifelse((current_time - initial_time) >
                                              step_zero$trial$tau, 1, 0))

                if (j <= sample_size){
                  alpha[j] <- feasibility(alpha = alpha[(j-1)],
                                                 strategy = alpha_strategy,
                                                 dlt = dlt[1:(j-1)],
                                                 resolution = resolution[1:(j-1)],
                                                 rate = alpha_rate)

                  formula <- cbind(time_cens[1:(j-1)], dlt[1:(j-1)]) ~ dose[1:(j-1)]
                  update <- ewoc_d1ph(formula,
                                      type = step_zero$trial$type,
                                      theta = step_zero$trial$theta,
                                      alpha = alpha[j],
                                      tau = step_zero$trial$tau,
                                      min_dose = step_zero$trial$min_dose,
                                      max_dose = step_zero$trial$max_dose,
                                      first_dose = step_zero$trial$first_dose,
                                      last_dose = step_zero$trial$last_dose,
                                      dose_set = step_zero$trial$dose_set,
                                      max_increment = step_zero$trial$max_increment,
                                      no_skip_dose = step_zero$trial$no_skip_dose,
                                      rho_prior = step_zero$trial$rho_prior,
                                      mtd_prior = step_zero$trial$mtd_prior,
                                      shape_prior = step_zero$trial$shape_prior,
                                      distribution = step_zero$trial$distribution,
                                      rounding = step_zero$trial$rounding)

                  if (!is.null(stop_rule_sim))
                    if (stop_rule_sim(update)){
                      dose[j:sample_size] <- NA
                      dlt[j:sample_size] <- NA
                      event_time[j:sample_size] <- NA
                      mtd_estimate <- NA
                      rho_estimate <- NA
                      break
                    }

                  dose[j] <- update$next_dose
                  event_time[j] <- response_sim(dose = dose[j])
                }

                update <- ewoc_d1ph(formula,
                                    type = step_zero$trial$type,
                                    theta = step_zero$trial$theta,
                                    alpha = alpha[length(alpha)],
                                    tau = step_zero$trial$tau,
                                    min_dose = step_zero$trial$min_dose,
                                    max_dose = step_zero$trial$max_dose,
                                    first_dose = step_zero$trial$first_dose,
                                    last_dose = step_zero$trial$last_dose,
                                    dose_set = step_zero$trial$dose_set,
                                    max_increment = step_zero$trial$max_increment,
                                    no_skip_dose = step_zero$trial$no_skip_dose,
                                    rho_prior = step_zero$trial$rho_prior,
                                    mtd_prior = step_zero$trial$mtd_prior,
                                    shape_prior = step_zero$trial$shape_prior,
                                    distribution = step_zero$trial$distribution,
                                    rounding = step_zero$trial$rounding)


                mtd_estimate <- update$next_dose
                rho_estimate <- median(update$rho)
              }

              list(event_time, dose, dlt, mtd_estimate, rho_estimate, alpha, current_time)
            }
  stopImplicitCluster()

  time_sim <- matrix(as.numeric(result[[1]]), nrow = n_sim, ncol = sample_size)
  dose_sim <- matrix(as.numeric(result[[2]]), nrow = n_sim, ncol = sample_size)
  dlt_sim <-  matrix(as.numeric(result[[3]]), nrow = n_sim, ncol = sample_size)
  mtd_sim <- as.numeric(result[[4]])
  rho_sim <- as.numeric(result[[5]])
  alpha_sim <- matrix(as.numeric(result[[6]]), nrow = n_sim, ncol = sample_size)
  total_time <- as.numeric(result[[7]])


  out <- list(trial = step_zero$trial,
              time_sim = time_sim, dose_sim = dose_sim, dlt_sim = dlt_sim,
              mtd_sim = mtd_sim, rho_sim = rho_sim, alpha_sim = alpha_sim,
              total_time = total_time)
  class(out) <- c("ewoc_simulation_d1ph", "nocov")
  return(out)
}


