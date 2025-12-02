# plumber.R

#* @apiTitle ReliaGrowR API
#* @apiDescription An API for the ReliaGrowR package
#* @apiContact list(name = "API Issues", url = "https://github.com/paulgovan/ReliaGrowR/issues")
#* @apiLicense list(name = "CC-BY-4.0", url = "https://creativecommons.org/licenses/by/4.0/")
#* @apiVersion 0.1

# Source scripts
source("plumb-duane.R")
source("plumb-rga.R")
source("plumb-gof.R")

#* Run a Duane Analysis
#* @param times Cumulative failure times (comma separated)
#* @param failures The number of failures at each corresponding time in times (comma separated)
#* @param conf.int:boolean Whether to compute confidence intervals (default: FALSE)
#* @param conf.level:numeric Confidence level for intervals (default: 0.95)
#* @post /duane
function(times, failures, conf.int = FALSE, conf.level = 0.95) {

  # Input validation
  times <- as.numeric(unlist(strsplit(times, ",")))
  failures <- as.numeric(unlist(strsplit(failures, ",")))
  conf.int <- as.logical(conf.int)
  conf.level <- as.numeric(conf.level)

  result <- duane(times = times, failures = failures,
                  conf.int = conf.int, conf.level = conf.level)

  return(result)
}

#* Plot Method for Duane Analysis
#* @param times Cumulative failure times (comma separated)
#* @param failures The number of failures at each corresponding time in times (comma separated)
#* @param conf.int:boolean Whether to compute confidence intervals (default: FALSE)
#* @param conf.level:numeric Confidence level for intervals (default: 0.95)
#* @param log Logical; whether to use logarithmic scales (default: TRUE)
#* @post /plot.duane
function(times, failures, conf.int = FALSE, conf.level = 0.95, log = TRUE) {

  # Input validation
  times <- as.numeric(unlist(strsplit(times, ",")))
  failures <- as.numeric(unlist(strsplit(failures, ",")))
  conf.int <- as.logical(conf.int)
  conf.level <- as.numeric(conf.level)
  log <- as.logical(log)

  result <- duane(times = times, failures = failures,
                  conf.int = conf.int, conf.level = conf.level)
  plot.duane(result)
}

#* Run a Crow-AMSAA or Piecewise NHPP Analysis
#* @param times Cumulative failure times (comma separated)
#* @param failures The number of failures at each corresponding time in times (comma separated)
#* @param model_type Type of model: "Crow-AMSAA" or "Piecewise NHPP" (default: "Crow-AMSAA")
#* @param breaks Cumulative time breakpoints for Piecewise NHPP (comma separated, optional)
#* @param conf_level Confidence level for intervals (default: 0.95)
#* @post /rga
function(times, failures, model_type = "Crow-AMSAA", breaks = NULL, conf_level = 0.95) {

  # Input processing
  times <- as.numeric(unlist(strsplit(times, ",")))
  failures <- as.numeric(unlist(strsplit(failures, ",")))
  conf_level <- as.numeric(conf_level)
  if (!is.null(breaks)) {
    breaks <- as.numeric(unlist(strsplit(breaks, ",")))
  }

  result <- rga(times = times, failures = failures,
                model_type = model_type, breaks = breaks,
                conf_level = conf_level)
}

#* Plot Method for RGA Objects
#* @param times Cumulative failure times (comma separated)
#* @param failures The number of failures at each corresponding time in times (comma separated)
#* @param model_type Type of model: "Crow-AMSAA" or "Piecewise NHPP" (default: "Crow-AMSAA")
#* @param breaks Cumulative time breakpoints for Piecewise NHPP (comma separated, optional)
#* @param conf_level Confidence level for intervals (default: 0.95)
#* @param conf_bounds:boolean Whether to include confidence bounds (default: TRUE)
#* @param legend:boolean Whether to show the legend (default: TRUE)
#* @param log:boolean Whether to use a log-log scale (default: FALSE)
#* @param legend_pos Position of the legend (default: "bottomright")
#* @post /plot.rga
#*
function(times, failures, model_type = "Crow-AMSAA", breaks = NULL, conf_level = 0.95,
         conf_bounds = TRUE, legend = TRUE, log = FALSE, legend_pos = "bottomright") {

  # Input processing
  times <- as.numeric(unlist(strsplit(times, ",")))
  failures <- as.numeric(unlist(strsplit(failures, ",")))
  model_type <- as.character(model_type)
  conf_level <- as.numeric(conf_level)
  if (!is.null(breaks)) {
    breaks <- as.numeric(unlist(strsplit(breaks, ",")))
  }
  conf_level <- as.numeric(conf_level)
  conf_bounds <- as.logical(conf_bounds)
  legend <- as.logical(legend)
  log <- as.logical(log)
  legend_pos <- as.character(legend_pos)

  result <- rga(times = times, failures = failures,
                model_type = model_type, breaks = breaks,
                conf_level = conf_level)
  plot.rga(result, conf_bounds = conf_bounds, legend = legend, log = log, legend_pos = legend_pos)
}

#* Calculate Required Sample Size or Test Time for Reliability Demonstration Test
#* @param target Target reliability to demonstrate (e.g., 0.9)
#* @param mission_time Mission time for the reliability target (e.g., 1000 hours)
#* @param conf_level Confidence level for the demonstration (e.g., 0.95)
#* @param beta Shape parameter of the Weibull distribution (default: 1 for Exponential)
#* @param n Sample size (number of units to test, optional)
#* @param T Test time (duration of the test, optional)
#* @post /rdt
rdt <- function(target, mission_time, conf_level,
                beta = 1, n = NULL, T = NULL) {

  # Input validation
  target <- as.numeric(target)
  mission_time <- as.numeric(mission_time)
  conf_level <- as.numeric(conf_level)
  beta <- as.numeric(beta)
  if (!is.null(n)) n <- as.numeric(n)
  if (!is.null(T)) T <- as.numeric(T)

  if (target <= 0 || target >= 1) {
    stop("Target reliability must be between 0 and 1 (exclusive).")
  }
  if (mission_time <= 0) {
    stop("Mission time must be greater than 0.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("Confidence level must be between 0 and 1 (exclusive).")
  }
  if (beta <= 0) {
    stop("Shape parameter beta must be greater than 0.")
  }
  if (!is.null(n) & !is.null(T)) {
    stop("Please provide only one of n (sample size) or T (test time), not both.")
  }
  if (is.null(n) & is.null(T)) {
    stop("Please provide one of n (sample size) or T (test time).")
  }

  # Scale parameter under H0 (Weibull with specified beta)
  eta0 <- mission_time / (-log(target))^(1/beta)

  if (!is.null(n) & is.null(T)) {
    # Solve for required test time
    T_req <- eta0 * ((-log(1 - conf_level)) / n)^(1/beta)
    result <- list(Distribution = ifelse(beta==1,"Exponential","Weibull"),
                   Beta = beta,
                   Target_Reliability = target,
                   Mission_Time = mission_time,
                   Required_Test_Time = T_req,
                   Input_Sample_Size = n)

  } else if (is.null(n) & !is.null(T)) {
    # Solve for required sample size
    n_req <- ceiling((-log(1 - conf_level)) / ((T/eta0)^beta))
    result <- list(Distribution = ifelse(beta==1,"Exponential","Weibull"),
                   Beta = beta,
                   Target_Reliability = target,
                   Mission_Time = mission_time,
                   Required_Sample_Size = n_req,
                   Input_Test_Time = T)

  } else {
    stop("Please provide exactly one of n (sample size) or T (test time).")
  }

  return(result)

}

