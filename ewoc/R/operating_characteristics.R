#'Operating characteristics for EWOC simulations
#'
#'Generic operating characteristics for one or more scenarios in EWOC simulations.
#'
#'@param sim_list a list of 'ewoc_simulation' objects for different scenarios
#'created using the \code{\link[ewoc]{ewoc_simulation}} function.
#'@param pdlt_list a list of functions to calculate the probability of toxicity with a numeric vector
#'of doses as input and a numeric vector of probabilities as output.
#'@param mtd_list a list of numerical values indicating the true MTD for each scenario.
#'@param toxicity_margin a numerical value of the acceptable margin of distance from the
#'\code{target_rate}.
#'@param mtd_margin  a numerical value of the acceptable margin of distance from the
#'\code{mtd_list}.
#'
#'@return \code{dlt_rate} See \code{\link[ewoc]{dlt_rate}}.
#'@return \code{dose_toxicity} See \code{\link[ewoc]{optimal_toxicity}}.
#'@return \code{mtd_toxicity} See \code{\link[ewoc]{optimal_toxicity}}.
#'@return \code{statistics} See \code{\link[ewoc]{mtd_bias}} and \code{\link[ewoc]{mtd_mse}}.
#'@return \code{dose_efficiency} See \code{\link[ewoc]{optimal_mtd}}.
#'@return \code{mtd_efficiency} See \code{\link[ewoc]{optimal_mtd}}.
#'@return \code{stop} See \code{\link[ewoc]{stop_rule}}.
#'
#'@references Diniz, M. A., Tighiouart, M., & Rogatko, A. (2019). Comparison between continuous and discrete doses for model based designs in cancer dose finding. PloS one, 14(1).
#'
#'@examples
#'\dontshow{
#'### Only one simulation
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
#'pdlt <- pdlt_d1classical(rho = 0.05, mtd = 60, theta = 0.33,
#'                       min_dose = 20, max_dose = 100)
#'
#'opc(sim_list = list(sim), pdlt_list = list(pdlt),
#'    mtd_list = list(60), toxicity_margin = 0.05, mtd_margin = 6)
#'
#'### Two or more simulations
#'
#'sim_list <- list()
#'mtd_list <- list()
#'pdlt_list <- list()
#'
#'DLT <- 0
#'dose <- 20
#'step_zero <- ewoc_d1classical(DLT ~ dose, type = 'discrete',
#'                           theta = 0.33, alpha = 0.25,
#'                           min_dose = 20, max_dose = 100,
#'                           dose_set = seq(20, 100, 20),
#'                           rho_prior = matrix(1, ncol = 2, nrow = 1),
#'                           mtd_prior = matrix(1, ncol = 2, nrow = 1),
#'                           rounding = "nearest")
#'mtd_list[[1]] <- 60
#'response_sim <- response_d1classical(rho = 0.05, mtd = mtd_list[[1]],
#'                                  theta = 0.33,
#'                                  min_dose = 20, max_dose = 100)
#'sim_list[[1]] <- ewoc_simulation(step_zero = step_zero,
#'                      n_sim = 1, sample_size = 2, n_cohort = 1,
#'                      alpha_strategy = "conditional",
#'                      response_sim = response_sim,
#'                      fixed_first_cohort =  TRUE,
#'                      ncores = 1)
#'pdlt_list[[1]] <- pdlt_d1classical(rho = 0.05, mtd = mtd_list[[1]],
#'                                theta = 0.33,
#'                                min_dose = 20, max_dose = 100)
#'
#'mtd_list[[2]] <- 40
#'response_sim <- response_d1classical(rho = 0.05, mtd = mtd_list[[2]],
#'                                  theta = 0.33,
#'                                  min_dose = 20, max_dose = 100)
#'sim_list[[2]] <- ewoc_simulation(step_zero = step_zero,
#'                      n_sim = 1, sample_size = 2, n_cohort = 1,
#'                      alpha_strategy = "conditional",
#'                      response_sim = response_sim,
#'                      fixed_first_cohort =  TRUE,
#'                      ncores = 1)
#'
#'pdlt_list[[2]] <- pdlt_d1classical(rho = 0.05, mtd = mtd_list[[2]],
#'                                theta = 0.33,
#'                                min_dose = 20, max_dose = 100)
#'
#'opc(sim_list = sim_list, pdlt_list = pdlt_list,
#'    mtd_list = mtd_list, toxicity_margin = 0.05, mtd_margin = 6)
#'}
#'
#'
#'\dontrun{
#'### Only one simulation
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
#'                      n_sim = 1, sample_size = 30, n_cohort = 1,
#'                      alpha_strategy = "conditional",
#'                      response_sim = response_sim,
#'                      fixed_first_cohort =  TRUE,
#'                      ncores = 1)
#'
#'pdlt <- pdlt_d1classical(rho = 0.05, mtd = 60, theta = 0.33,
#'                       min_dose = 20, max_dose = 100)
#'
#'opc(sim_list = list(sim), pdlt_list = list(pdlt),
#'    mtd_list = list(60), toxicity_margin = 0.05, mtd_margin = 6)
#'
#'### Two or more simulations
#'
#'sim_list <- list()
#'mtd_list <- list()
#'pdlt_list <- list()
#'
#'DLT <- 0
#'dose <- 20
#'step_zero <- ewoc_d1classical(DLT ~ dose, type = 'discrete',
#'                           theta = 0.33, alpha = 0.25,
#'                           min_dose = 20, max_dose = 100,
#'                           dose_set = seq(20, 100, 20),
#'                           rho_prior = matrix(1, ncol = 2, nrow = 1),
#'                           mtd_prior = matrix(1, ncol = 2, nrow = 1),
#'                           rounding = "nearest")
#'mtd_list[[1]] <- 60
#'response_sim <- response_d1classical(rho = 0.05, mtd = mtd_list[[1]],
#'                                  theta = 0.33,
#'                                  min_dose = 20, max_dose = 100)
#'sim_list[[1]] <- ewoc_simulation(step_zero = step_zero,
#'                      n_sim = 1, sample_size = 30, n_cohort = 1,
#'                      alpha_strategy = "conditional",
#'                      response_sim = response_sim,
#'                      fixed_first_cohort =  TRUE,
#'                      ncores = 1)
#'pdlt_list[[1]] <- pdlt_d1classical(rho = 0.05, mtd = mtd_list[[1]],
#'                                theta = 0.33,
#'                                min_dose = 20, max_dose = 100)
#'
#'mtd_list[[2]] <- 40
#'response_sim <- response_d1classical(rho = 0.05, mtd = mtd_list[[2]],
#'                                  theta = 0.33,
#'                                  min_dose = 20, max_dose = 100)
#'sim_list[[2]] <- ewoc_simulation(step_zero = step_zero,
#'                      n_sim = 1, sample_size = 30, n_cohort = 1,
#'                      alpha_strategy = "conditional",
#'                      response_sim = response_sim,
#'                      fixed_first_cohort =  TRUE,
#'                      ncores = 1)
#'
#'pdlt_list[[2]] <- pdlt_d1classical(rho = 0.05, mtd = mtd_list[[2]],
#'                                theta = 0.33,
#'                                min_dose = 20, max_dose = 100)
#'
#'opc(sim_list = sim_list, pdlt_list = pdlt_list,
#'    mtd_list = mtd_list, toxicity_margin = 0.05, mtd_margin = 6)
#'}
#'
#'@export
opc <- function(sim_list, pdlt_list, mtd_list,
                toxicity_margin = NULL, mtd_margin = NULL){

  aux_opc <- function(sim, pdlt, mtd, toxicity_margin, mtd_margin){
    UseMethod("opc", sim)
  }

  if (length(sim_list) > 1){
     temp <- mapply(aux_opc, sim_list, pdlt_list, mtd_list,
                    MoreArgs =
                    list(toxicity_margin = toxicity_margin,
                        mtd_margin = mtd_margin),
                    SIMPLIFY = FALSE)
     dlt_rate <-
       Reduce(rbind, lapply(temp, function(x) x$dlt_rate))
     dose_toxicity <-
       Reduce(rbind, lapply(temp, function(x) x$dose_toxicity))
     mtd_toxicity <-
       Reduce(rbind, lapply(temp, function(x) x$mtd_toxicity))
     dose_efficiency <-
       Reduce(rbind, lapply(temp, function(x) x$dose_efficiency))
     mtd_efficiency <-
       Reduce(rbind, lapply(temp, function(x) x$mtd_efficiency))
     stop <-
       Reduce(rbind, lapply(temp, function(x) x$stop))

     out <- list(dlt_rate = dlt_rate,
                 dose_toxicity = dose_toxicity,
                 mtd_toxicity = mtd_toxicity,
                 dose_efficiency = dose_efficiency,
                 mtd_efficiency = mtd_efficiency,
                 stop = stop)

  } else {
    out <- aux_opc(sim_list[[1]], pdlt_list[[1]], mtd_list[[1]],
                   toxicity_margin, mtd_margin)
  }

  return(out)
}

#'@export
opc.nocov <- function(sim, pdlt, mtd,
                      toxicity_margin, mtd_margin){

  ### DLT rate
  aux_dlt <- function(sim, pdlt = NULL, toxicity_margin = NULL){
    if (!is.null(toxicity_margin) & !is.null(pdlt)){
      out <- as.data.frame(dlt_rate(sim$dlt_sim,
                      target_rate = sim$trial$theta,
                      margin = toxicity_margin))
    } else {
      s1 <- dlt_rate(sim$dlt_sim)$average
      out <- data.frame(average = s1)
    }

    return(out)
  }

  dlt_rate <- aux_dlt(sim, pdlt, toxicity_margin)

  ### Dose Toxicity
  aux_dose_toxicity <- function(sim, pdlt = NULL, toxicity_margin = NULL){
    if (!is.null(toxicity_margin) & !is.null(pdlt)){
      out <- as.data.frame(
        optimal_toxicity(sim$dose_sim,
                         target_rate = sim$trial$theta,
                         margin = toxicity_margin,
                         pdlt = pdlt))
    } else {
      out <- NULL
    }

    return(out)
  }

  dose_toxicity <- aux_dose_toxicity(sim, pdlt, toxicity_margin)

  ### MTD Toxicity
  aux_mtd_toxicity <- function(sim, pdlt = NULL, toxicity_margin = NULL){
    if (!is.null(toxicity_margin) & !is.null(pdlt)){
      out <- as.data.frame(
        optimal_toxicity(sim$mtd_sim,
                         target_rate = sim$trial$theta,
                         margin = toxicity_margin,
                         pdlt = pdlt))
    } else {
      out <- NULL
    }

    return(out)
  }

  mtd_toxicity <- aux_mtd_toxicity(sim, pdlt, toxicity_margin)

  ### Statistical Measures
  aux_statistical <- function(sim, true_mtd){
    s1 <- mtd_bias(sim$mtd_sim, true_mtd)
    s2 <- mtd_mse(sim$mtd_sim, true_mtd)
    out <- data.frame(bias = s1, mse = s2)
  }

  statistics <- aux_statistical(sim, mtd)

  ### Dose Efficiency
  aux_dose_efficiency <- function(sim, true_mtd, mtd_margin){
    if (!is.null(mtd_margin)){
      out <- as.data.frame(optimal_mtd(sim$dose_sim,
                                       true_mtd = true_mtd,
                                       margin = mtd_margin))
      colnames(out) <- c("dose.interval",
                         "dose.underdose",
                         "dose.overdose")
    } else {
      out <- NULL
    }
    return(out)
  }

  dose_efficiency <- aux_dose_efficiency(sim, mtd, mtd_margin)

  ### MTD Eficiency

  aux_mtd_efficiency <- function(sim, true_mtd, mtd_margin){
    if (!is.null(mtd_margin)){
      out <- as.data.frame(optimal_mtd(sim$mtd_sim,
                                       true_mtd = true_mtd,
                                       margin = mtd_margin))
      colnames(out) <- c("mtd.interval",
                         "mtd.underdose",
                         "mtd.overdose")
    } else {
      out <- NULL
    }
    return(out)
  }

  mtd_efficiency <- aux_mtd_efficiency(sim, mtd, mtd_margin)

  ### Stopping Rule
  aux_stop <- function(sim){
    out <- as.data.frame(stop_rule(sim$dlt_sim, ncol(sim$dlt_sim), digits = 2))
  }

  stop<- aux_stop(sim)

  out <- list(dlt_rate = dlt_rate,
              dose_toxicity = dose_toxicity,
              mtd_toxicity = mtd_toxicity,
              dose_efficiency = dose_efficiency,
              mtd_efficiency = mtd_efficiency,
              stop = stop)
}


overdose_loss <- function (mtd_estimate, true_mtd, alpha) {

  out <- ifelse(mtd_estimate < true_mtd, alpha*(true_mtd - mtd_estimate),
                (1 - alpha)*(mtd_estimate - true_mtd))
  return(out)
}

#'Evaluation of the DLT rate
#'
#'Calculate the DLT rate for each trial, the average DLT rate, the percent
#'of trials which have \eqn{DLT rate > target_rate + margin}, the percent
#'of trials which have \eqn{DLT rate < target_rate - margin} and the percent
#'of trials which have \eqn{target_rate - margin < DLT rate < target_rate + margin}.
#'
#'@param dlt_matrix a matrix of the number of DLT for each step of the trial (column)
#'and for each trial (row).
#'@param trial a logical value indicating if the DLT rate for each trial should be returned.
#'@param target_rate a numerical value of the target rate of DLT.
#'@param margin a numerical value of the acceptable distance from the \code{target_rate}.
#'@param digits a numerical value indicating the number of digits.
#'
#'@return \code{trial} a numerical vector of the DLT rate for each trial.
#'@return \code{average} a numerical value of the average of DLT rate considering a batch of trials.
#'@return \code{upper} the percent of trials which the
#'\code{DLT rate > target_rate + margin} if \code{margin != NULL} and
#'\code{target_rate != NULL}.
#'@return \code{lower} the percent of trials which the
#'\code{DLT rate < target_rate - margin} if \code{margin != NULL} and
#'\code{target_rate != NULL}.
#'@return \code{interval} the percent of trials which the
#'\code{target_rate - margin < DLT rate < target_rate + margin} if \code{margin != NULL} and
#'\code{target_rate != NULL}.
#'
#'@examples
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
#'stop_rule_sim(step_zero)
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 1, sample_size = 2,
#'                        alpha_strategy = "increasing",
#'                        response_sim = response_sim,
#'                        stop_rule_sim = stop_rule_sim,
#'                        ncores = 2)
#'dlt_rate(sim$dlt_sim)
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
#'stop_rule_sim(step_zero)
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 2, sample_size = 30,
#'                        alpha_strategy = "increasing",
#'                        response_sim = response_sim,
#'                        stop_rule_sim = stop_rule_sim,
#'                        ncores = 2)
#'dlt_rate(sim$dlt_sim)
#'}
#'
#'@export
dlt_rate <- function(dlt_matrix, trial = FALSE,
                     target_rate = NULL, margin = NULL, digits = 2) {

  if (!is.matrix(dlt_matrix))
    dlt_matrix <- matrix(dlt_matrix, nrow = 1)

  aux_upper <- function(dlt, target_rate, margin) {
    out <- ifelse(mean(dlt, na.rm = TRUE) > target_rate + margin, 1, 0)
    return(out)
  }

  aux_lower <- function(dlt, target_rate, margin) {
    out <- ifelse(mean(dlt, na.rm = TRUE) < target_rate - margin, 1, 0)
    return(out)
  }

  dlt_trial <- round(rowMeans(dlt_matrix, na.rm = TRUE), digits)
  dlt_average <- round(mean(dlt_trial, na.rm = TRUE), digits)

  if (!is.null(margin) & !is.null(target_rate)){
    dlt_upper <- apply(dlt_matrix, 1, aux_upper,
                    target_rate = target_rate, margin = margin)
    dlt_upper <- round(mean(dlt_upper, na.rm = TRUE)*100, digits)

    dlt_lower <- apply(dlt_matrix, 1, aux_lower,
                         target_rate = target_rate, margin = margin)
    dlt_lower <- round(mean(dlt_lower, na.rm = TRUE)*100, digits)

    dlt_interval <- 100 - dlt_upper - dlt_lower

    out <- list(average = dlt_average,
                upper = dlt_upper, lower = dlt_lower, interval = dlt_interval)

    if (trial)
      out <- list(trial = dlt_trial, average = dlt_average,
                  upper = dlt_upper, lower = dlt_lower, interval = dlt_interval)


  } else {
    out <- list(trial = dlt_trial, average = dlt_average)

    if (trial)
      out <- list(average = dlt_average)
  }
  return(out)
}

#'Evaluation of the stopping rule
#'
#'Calculate the average, minimum, maximum number of patients to stop a trial and
#'the percent of stopped trials. Stopped trials contain NA after the last
#'assigned dose.
#'
#'@param dlt_matrix Matrix of the number of DLT for each step of the trial (column)
#'and for each trial (row).
#'@param sample_size a numerical value indicating the expected sample size.
#'@param digits a numerical value indicating the number of digits.
#'
#'@return A list consisting of
#'\itemize{
#' \item{\code{average}: }{Average number of patients to stop a trial.}
#' \item{\code{min}: }{Minimum number of patients to stop a trial.}
#' \item{\code{max}: }{Maximum number of patients to stop a trial.}
#' \item{\code{nstop}: }{Percent of stopped trials}.
#'}
#'
#'@examples
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
#'stop_rule_sim(step_zero)
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 1, sample_size = 2,
#'                        alpha_strategy = "increasing",
#'                        response_sim = response_sim,
#'                        stop_rule_sim = stop_rule_sim,
#'                        ncores = 2)
#'stop_rule(sim$dlt_sim)
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
#'stop_rule_sim(step_zero)
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 2, sample_size = 30,
#'                        alpha_strategy = "increasing",
#'                        response_sim = response_sim,
#'                        stop_rule_sim = stop_rule_sim,
#'                        ncores = 2)
#'stop_rule(sim$dlt_sim)
#'}
#'
#'@export
stop_rule <- function(dlt_matrix, sample_size, digits = 2) {

  if (!is.matrix(dlt_matrix))
    dlt_matrix <- matrix(dlt_matrix, nrow = 1)

  index <- which(rowSums(!is.na(dlt_matrix)) < sample_size, arr.ind = TRUE)
  temp <- dlt_matrix[index, ]

  if (!is.matrix(temp))
    temp <- matrix(temp, nrow = 1)

  if(length(index) > 0) {
    result <- apply(temp, 1, function(x) sum(!is.na(x)))
  } else {
    result <- 0
  }

  out <- list(average = mean(result),
              min = min(result),
              max = max(result),
              nstop = round(100*length(index)/
                              nrow(dlt_matrix), digits))
  return(out)
}


#'Percent of doses in relation the optimal MTD interval
#'
#'Calculate the percent of doses which are inside the optimal MTD interval \code{[true_MTD -
#'margin ; true_MTD + margin]}.
#'
#'@param dose_matrix a numerical matrix or vector of assigned doses for each step of the trial (column)
#'and for each trial (row).
#'@param true_mtd a numerical value of the true Maximum Tolerable Dose.
#'@param margin a numerical value of the acceptable margin of distance from the
#'\code{true_mtd}.
#'@param digits a numerical value indicating the number of digits.
#'
#'@return \code{interval} the average percent of doses which are inside the optimal MTD interval.
#'@return \code{underdose} the average percent of doses which are smaller than the lower limit of the optimal MTD interval.
#'@return \code{overdose} the average percent of doses which are greater than the upper limit of the optimal MTD interval.
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
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 1, sample_size = 2,
#'                        alpha_strategy = "increasing",
#'                        response_sim = response_sim,
#'                        ncores = 2)
#'optimal_mtd(sim$mtd_sim, true_mtd = 20, margin = 0.1*20)
#'optimal_mtd(sim$dose_sim, true_mtd = 20, margin = 0.1*20)
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
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 2, sample_size = 30,
#'                        alpha_strategy = "increasing",
#'                        response_sim = response_sim,
#'                        ncores = 2)
#'optimal_mtd(sim$mtd_sim, true_mtd = 20, margin = 0.1*20)
#'optimal_mtd(sim$dose_sim, true_mtd = 20, margin = 0.1*20)
#'}
#'
#'@export
optimal_mtd <- function(dose_matrix, true_mtd, margin, digits = 2) {

  if (!is.matrix(dose_matrix))
    dose_matrix <- matrix(dose_matrix, nrow = 1)

  aux_interval <- function(dose, true_mtd,  margin) {
    observed_number <- sum(dose > true_mtd - margin & dose < true_mtd + margin)
    out <- round(100*observed_number/length(dose), digits)
    return(out)
  }

  aux_underdose <- function(dose, true_mtd,  margin) {
    observed_number <- sum(dose < true_mtd - margin)
    out <- round(100*observed_number/length(dose), digits)
    return(out)
  }

  aux_overdose <- function(dose, true_mtd,  margin) {
    observed_number <- sum(dose > true_mtd + margin)
    out <- round(100*observed_number/length(dose), digits)
    return(out)
  }

  percent <- apply(dose_matrix, 1, aux_overdose,
                   true_mtd = true_mtd,  margin = margin)
  overdose <- round(mean(percent, na.rm = TRUE), digits)

  percent <- apply(dose_matrix, 1, aux_underdose,
                   true_mtd = true_mtd,  margin = margin)
  underdose <- round(mean(percent, na.rm = TRUE), digits)

  percent <- apply(dose_matrix, 1, aux_interval,
                   true_mtd = true_mtd,  margin = margin)
  interval <- round(mean(percent, na.rm = TRUE), digits)

  out <- list(interval = interval, underdose = underdose, overdose = overdose)

  return(out)
}

#'Percent of doses in relation the optimal toxicity interval
#'
#'Calculate the percent of doses which are inside the optimal toxicity interval \code{[target rate -
#'margin ; target rate + margin]}.
#'
#'@param dose_matrix a numerical matrix of assigned doses for each step of the trial (column)
#'and for each trial (row).
#'@param target_rate a numerical value of the target DLT rate.
#'@param margin a numerical value of the acceptable margin of distance from the
#'\code{target_rate}.
#'@param pdlt a function to calculate the probability of toxicity with a numeric vector of doses as input and a numeric vector of probabilities as output.
#'@param digits a numerical value indicating the number of digits.
#'
#'@return \code{interval} the average percent of doses which are inside the optimal toxicity interval.
#'@return \code{underdose} the average percent of doses which are smaller than the lower limit of the optimal toxicity interval.
#'@return \code{overdose} the average percent of doses which are greater than the upper limit of the optimal toxicity interval.
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
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'pdlt_sim <- pdlt_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                           min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 1, sample_size = 2,
#'                        alpha_strategy = "increasing",
#'                        response_sim = response_sim,
#'                        ncores = 2)
#'optimal_toxicity(sim$mtd_sim, target_rate = 0.33, margin = 0.05, pdlt = pdlt_sim)
#'optimal_toxicity(sim$dose_sim, target_rate = 0.33, margin = 0.05, pdlt = pdlt_sim)
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
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'pdlt_sim <- pdlt_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                           min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 2, sample_size = 30,
#'                        alpha_strategy = "increasing",
#'                        response_sim = response_sim,
#'                        ncores = 2)
#'optimal_toxicity(sim$mtd_sim, target_rate = 0.33, margin = 0.05, pdlt = pdlt_sim)
#'optimal_toxicity(sim$dose_sim, target_rate = 0.33, margin = 0.05, pdlt = pdlt_sim)
#'}
#'
#'
#'@export
optimal_toxicity <- function(dose_matrix, target_rate, margin, pdlt, digits = 2) {

  if (!is.matrix(dose_matrix))
    dose_matrix <- matrix(dose_matrix, nrow = 1)

  aux_interval <- function(dose, theta,  margin) {
    prob <- pdlt(dose)
    observed_number <- sum(prob > theta - margin & prob < theta + margin)
    out <- round(100*observed_number/length(dose), digits)
    return(out)
  }

  aux_underdose <- function(dose, theta,  margin) {
    prob <- pdlt(dose)
    observed_number <- sum(prob < theta - margin)
    out <- round(100*observed_number/length(dose), digits)
    return(out)
  }

  aux_overdose <- function(dose, theta,  margin) {
    prob <- pdlt(dose)
    observed_number <- sum(prob > theta + margin)
    out <- round(100*observed_number/length(dose), digits)
    return(out)
  }

  percent <- apply(dose_matrix, 1, aux_interval,
                   theta = target_rate,  margin = margin)
  interval <- round(mean(percent, na.rm = TRUE), digits)

  percent <- apply(dose_matrix, 1, aux_underdose,
                   theta = target_rate,  margin = margin)
  underdose <- round(mean(percent, na.rm = TRUE), digits)

  percent <- apply(dose_matrix, 1, aux_overdose,
                   theta = target_rate,  margin = margin)
  overdose <- round(mean(percent, na.rm = TRUE), digits)

  out <- list(interval = interval, underdose = underdose, overdose = overdose)

  return(out)
}

#'Bias of the MTD estimates
#'
#'Calculate the bias.
#'
#'@param mtd_estimate a numerical vector of the MTD estimates.
#'@param true_mtd a numerical value of the true Maximum Tolerable Dose.
#'
#'@return Bias of the MTD estimates.
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
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 1, sample_size = 2,
#'                        alpha_strategy = "increasing",
#'                        response_sim = response_sim,
#'                        ncores = 2)
#'mtd_bias(sim$mtd_sim, true_mtd = 20)
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
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 2, sample_size = 30,
#'                        alpha_strategy = "increasing",
#'                        response_sim = response_sim,
#'                        ncores = 2)
#'mtd_bias(sim$mtd_sim, true_mtd = 20)
#'}
#'
#'@export
mtd_bias <- function(mtd_estimate, true_mtd) {
  out <- mean(mtd_estimate - true_mtd, na.rm = TRUE)
  return(out)
}

#'Mean Square Error of the MTD estimates
#'
#'Calculate the Mean Square Error (MSE).
#'
#'@param mtd_estimate a numerical vector of the MTD estimates.
#'@param true_mtd a numerical value of the true Maximum Tolerable Dose.
#'
#'@return MSE of the MTD estimates.
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
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 1, sample_size = 2,
#'                        alpha_strategy = "increasing",
#'                        response_sim = response_sim,
#'                        ncores = 2)
#'mtd_mse(sim$mtd_sim, true_mtd = 20)
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
#'response_sim <- response_d1classical(rho = 0.05, mtd = 20, theta = 0.33,
#'                                   min_dose = 10, max_dose = 50)
#'sim <- ewoc_simulation(step_zero = step_zero,
#'                        n_sim = 2, sample_size = 30,
#'                        alpha_strategy = "increasing",
#'                        response_sim = response_sim,
#'                        ncores = 2)
#'mtd_mse(sim$mtd_sim, true_mtd = 20)
#'}
#'
#'@export
mtd_mse <- function(mtd_estimate, true_mtd) {
  out <- mean((mtd_estimate - true_mtd)^2, na.rm = TRUE)
  return(out)
}

#'Accuracy Index
#'
#'Calculate the Accuracy Index.
#'
#'@param mtd_estimate a numerical vector of the MTD estimates.
#'@param dose_set a numerical vector of allowable doses in the trial.
#'@param true_prob a numerical vector of the true probabilities associated
#'with 'dose_set'.
#'@param theta a numerical value defining the proportion of expected patients
#'to experience a medically unacceptable, dose-limiting toxicity (DLT) if
#'administered the MTD.
#'@param loss a loss function between the true probabilities of toxicity
#''true_prob' and the target DLT rate 'theta'.
#'@param alpha a numerical value indicating the weight of overdose for the
#'overdose loss function.
#'
#'@references Cheung, Y. K. (2011). Dose finding by the continual reassessment method. CRC Press.
#'
#'@return Accuracy Index for given loss function of the MTD estimates.
#'
accuracy_index <- function (mtd_estimate, dose_set, true_prob, theta,
                            loss = c("squared", "absolute", "classification",
                                     "overdose"), alpha = NULL) {

  mtd_estimate <- factor(mtd_estimate, levels = dose_set)
  estimate_prob <- prop.table(table(mtd_estimate))

  if (loss == "squared")
    dist <- (true_prob -  theta)^2
  if (loss == "absolute")
    dist <- abs(true_prob -  theta)
  if (loss == "classification")
    dist <- as.numeric(true_prob !=  theta)
  if (loss == "overdose") {
    if (!is.null(alpha)) {
      dist <- overdose_loss(true_prob, theta, alpha)
    } else {
      stop("loss = 'overdose' requires a value for alpha.")
    }
  }
  out <- 1 - length(dose_set)*sum(dist*estimate_prob)/sum(dist)

  return(out)
}

#'Average Toxicity Number
#'
#'Calculate the Average Toxicity Number.
#'
#'@param dose a numerical matrix of assigned doses for each step of the trial (column)
#'and for each trial (row).
#'@param dose_set a numerical vector of allowable doses in the trial.
#'@param true_prob a numerical vector of the true probabilities associated
#'with 'dose_set'.
#'@param theta a numerical value defining the proportion of expected patients
#'to experience a medically unacceptable, dose-limiting toxicity (DLT) if
#'administered the MTD.
#'
#'@references Cheung, Y. K. (2011). Dose finding by the continual reassessment method. CRC Press.
#'
#'@return Average Toxicity Number.
#'
average_toxicity <- function (dose, dose_set, true_prob, theta) {

  aux_toxicity <- function (x, dose_set, true_prob) {
    x <- factor(x, levels = dose_set)
    freq <- as.numeric(table(x))
    observed_average <- sum(true_prob*freq)
  }

  observed_trial <- apply(dose, 1, FUN = aux_toxicity,
                          dose_set = dose_set, true_prob = true_prob)

  expected_average <- ncol(dose)*theta

  out <- list(observed_average = mean(observed_trial),
              expected_average = expected_average)

  return(out)
}






