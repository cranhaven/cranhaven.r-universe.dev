#---------------------------------------
# R-package Birp
#---------------------------------------

#---------------------------------------
# Internal functions, not exported
#---------------------------------------

#' Function to convert an argument to a string and add it to a list if necessary
#' @param options A list where x should be added to
#' @param name A string specifying the name of the argument
#' @param x An R object to be added to the list
#' @return An updated list options
#' @keywords internal
.addToList.birp <- function(options, name, x){
  if (is.character(x)){
    options[[name]] <- x
  } else if (is.integer(x) | is.numeric(x)){
    options[[name]] <- paste0(x, collapse = ",")
  } else if (is.null(x)){
    # ignore, argument is empty
  } else if (is.na(x)){
    # ignore, argument is empty
  } else if (is.logical(x)){
    if (x){
      # is TRUE -> set flag for parameters().exists()
      options[[name]] <- ""
    } # else is FALSE -> ignore flag
  } else {
    stop(paste0("Unknown type ", class(x), " for name ", name, "!"))
  }
  return(options)
}

#' Function to print posterior summaries
#' @param x A list containing different posterior summaries
#' @param param_name A string indicating the parameter name (gamma or Delta)
#' @return No return value, called for side effects.
#' @keywords internal
.printPostSummary.birp <- function(x, param_name){
  cat(" - ", param_name, ": [", paste0(x$names, collapse=", "), "]\n", sep = "")
  cat("   - Posterior mean of ", param_name, ": [", paste0(x$posterior_mean, collapse=", "), "]\n", sep = "")
  cat("   - Posterior median of ", param_name, ": [", paste0(x$posterior_median, collapse=", "), "]\n", sep = "")
  cat("   - Posterior 5% quantile of ", param_name, ": [", paste0(x$posterior_q05, collapse=", "), "]\n", sep = "")
  cat("   - Posterior 95% quantile of ", param_name, ": [", paste0(x$posterior_q95, collapse=", "), "]\n", sep = "")
  cat("   - Posterior probability of positive P(", param_name, " >= 0): [", paste0(x$prob_positive, collapse=", "), "]\n", sep = "")
}

#' Function to add a hatched polygon to a plot
#' @param shading Shading color. If \code{NA}, shading is omitted
#' @param left An integer indicating the left-most value on the x-axis 
#' @param right An integer indicating the right-most value on the x-axis 
#' @return No return value, called for side effects.
#' @keywords internal
.plotShadingPolygon.birp <- function(shading, left, right){
  if (!is.na(shading)){
    x <- c(left, right, right, left, left)
    y <- par("usr")[c(3,3,4,4,3)]
    polygon(x, y, col = shading, border = NA, density = 20, angle = -45)
  }
}

#' Function to open an empty plot for plotting the posterior probabilities
#' @param xlim The x-limits (x1, x2) of the plot
#' @param ylim The y-limits (y1, y2) of the plot
#' @param xlab Name of x axis
#' @param ylab Name of y axis
#' @param shadingIncrease Shading color for the range gamma > 0. If \code{NA}, shading is omitted
#' @param shadingDecrease Shading color for the range gamma < 0. If \code{NA}, shading is omitted
#' @param lineAtZero If \code{TRUE}, adds a dashed line indicating 0.
#' @return No return value, called for side effects.
#' @keywords internal
.openPosteriorPlot.birp <- function(xlim, ylim,
                                    xlab, ylab,
                                    shadingIncrease, shadingDecrease,
                                    lineAtZero,
                                    ...){
  # Open plot
  plot(0, type = 'n', xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)
  
  # Add shading polygons
  equalityCoordinate <- 0
  .plotShadingPolygon.birp(shadingIncrease, equalityCoordinate, par("usr")[2])
  .plotShadingPolygon.birp(shadingDecrease, par("usr")[1], equalityCoordinate)
  
  # Add line at gamma = 0
  if (lineAtZero){
    lines(rep(equalityCoordinate, 2), par("usr")[3:4], col = 'black', lty = 2)
  }
}

#' Function to generate a nice axis label with greek gamma and subscript
#' @param index The index in the subscript
#' @return A string
#' @keywords internal
.getLabelGamma.birp <- function(x, index){
  return(substitute(paste(gamma[index], ' (', name, ')'), list(index=index, name=x$post_gamma$names[index])))
}

#' Function to add text box to plot denoting P(gamma > 0 | n) or P(gamma < 0 | n) for single gammas (same for Delta)
#' @param post The posterior probabilities (gamma or Delta)
#' @param param_name The parameter name to show
#' @return No return value, called for side effects.
#' @keywords internal
.addTextSingleGammaDelta.birp <- function(post, param_name = "gamma") {
  diffFromBorder <- 0.01 * diff(par("usr")[1:2])
  sym <- as.symbol(param_name)
  pp <- post$prob_positive[!post$is_fix]
  if (pp > 0.5) {
    ttext <- bquote(paste("P(", .(sym), " >= 0 | n) = ", .(round(pp, 3))))
    text(par("usr")[2] - diffFromBorder, par("usr")[4], adj = c(1, 1.5), labels = ttext)
  } else {
    ttext <- bquote(paste("P(", .(sym), " < 0 | n) = ", .(round(1 - pp, 3))))
    text(par("usr")[1] + diffFromBorder, par("usr")[4], adj = c(0, 1.5), labels = ttext)
  }
}

#' Function to add legend to plot denoting gammas
#' @param num The number of gamma/Deltas
#' @param legend Add a legend to the plot
#' @param dens A list containing the densities for each gamma
#' @param xlim The x-limits (x1, x2) of the plot
#' @param col Line color, one per epoch
#' @param lwd Line width, one per epoch
#' @param lty Line type, one per epoch
#' @param ... additional parameters passed to the function.
#' @return No return value, called for side effects.
#' @keywords internal
.addLegendMultiGamma.birp <- function(num, legend, dens, xlim, col, lwd, lty, ...) {
  max.y <- max(dens[[1]]$y)
  max.x <- dens[[1]]$x[dens[[1]]$y == max(dens[[1]]$y)]
  if (num > 1) {
    for (e in 2:num) {
      if (max(dens[[e]]$y) > max.y) {
        max.y <- max(dens[[e]]$y)
        max.x <- dens[[e]]$x[dens[[e]]$y == max(dens[[e]]$y)]
      }
    }
  }
  legend.pos <- if (max.x < xlim[1] + diff(xlim) / 2) "topright" else "topleft"
  legend(legend.pos, legend, col = col, lwd = lwd, lty = lty, ...)
}

#' Function to get the start of each epoch, including for a hypothetical epoch after the last epoch
#' @param times An integer or numeric vector with time points
#' @param times_of_change An integer or numeric vector with times of change
#' @return An integer or numeric vector with starting points for each epoch
#' @keywords internal
.getEpochStarts.birp <- function(times, times_of_change){
  return(c(min(times), times_of_change, max(times[length(times)], times_of_change + 1)))
}

#' Function to calculate rho 
#' @param times An integer or numeric vector with time points
#' @param times_of_change An integer or numeric vector with times of change
#' @return A numeric matrix containing rho for each time point and epoch
#' @keywords internal
.calculateRho.birp <- function(times, times_of_change){
  epoch_start_T <- .getEpochStarts.birp(times, times_of_change)
  num_epochs <- length(times_of_change) + 1
  rho <- matrix(0, nrow = length(times), ncol = num_epochs)
  for (e in 1:num_epochs){
    rho[times >= epoch_start_T[e+1], e] <- epoch_start_T[e+1] - epoch_start_T[e]
    within_epoch <- times < epoch_start_T[e+1] & times > epoch_start_T[e]
    rho[within_epoch ,e] <- times[within_epoch] - epoch_start_T[e]
  }
  return(rho)
}

#' Compute step-change indicator matrix for Delta
#'
#' For a set of evaluation times and a set of step-change times, computes a matrix
#' where entry \code{[i, m]} is 1 if observation time \code{i} is greater than or equal
#' to step-change time \code{m}, and 0 otherwise. Used to accumulate the step-change
#' contributions \eqn{\sum_m \mathbb{1}_{t_k \geq T_m} \Delta(g, m)}.
#'
#' @param eval_times Numeric vector; times at which to evaluate the indicator.
#' @param times_of_change Numeric vector; the step-change times \eqn{T_1, \ldots, T_{M-1}}.
#' @return A matrix with \code{length(eval_times)} rows and \code{length(times_of_change)} columns.
#' @keywords internal
.calculatePsi.birp <- function(eval_times, times_of_change){
  if (length(times_of_change) == 0){
    return(matrix(0, nrow = length(eval_times), ncol = 0))
  }
  sapply(times_of_change, function(T_m) as.numeric(eval_times >= T_m))
}

#' Function to check if a file exists and generate error message if it was not found
#' @param path A file path
#' @param files A vector of character strings corresponding to file names found in the path
#' @param patterns A vector of patterns to search for within 'files'
#' @param allowMultiMatch Logical. If \code{TRUE}, multiple matches are allowed
#' @param sep The field separator character of the input file
#' @param mustExist Logical. If \code{TRUE}, an exception is raised if the file does not exist
#' @return A string denoting the filename(s)
#' @keywords internal
.checkFile.birp <- function(path, files, patterns, allowMultiMatch = FALSE, sep = "\t", mustExist = TRUE){
  filename <- files
  for (p in 1:length(patterns)){
    filename <- filename[grepl(patterns[p], filename)]
  }
  
  if (mustExist & length(filename) == 0){
    stop(paste0("No file containing the pattern '", paste0(patterns, collapse = ","), "' found in directory '", path, "'!"))
  }
  if (!allowMultiMatch & length(filename) > 1){
    stop(paste0("Multiple files containing the pattern '", paste0(patterns, collapse = ","), "' found in directory '", path, "'!"))
  }
  return(filename)
}

#' Function to open a file and generate error message if it was not found
#' @param path A file path.
#' @param files A vector of character strings corresponding to file names found in the path.
#' @param patterns A vector of patterns to search for within 'files'.
#' @param sep The field separator character of the input file
#' @param mustExist Logical. If \code{TRUE}, an exception is raised if the file does not exist
#' @return A file connection.
#' @keywords internal
.openFile.birp <- function(path, files, patterns, sep = "\t", header = TRUE, mustExist = TRUE){
  filename <- .checkFile.birp(path, files, patterns, sep = sep, mustExist = mustExist)
  if (length(filename) == 0){ return(data.frame()) }
  fz <- file.size(file.path(path, filename))
  if (fz == 0 | fz == 3){ return(data.frame()) } # empty file
  f <- read.table(file.path(path, filename), header = header, check.names = FALSE, sep = sep)
  return(f)
}

#' Function to parse posterior results of gamma and Delta
#' @param param_name A string defining the parameter name (gamma or Delta)
#' @param meanVar A data frame containing the posterior mean and variance of all parameters
#' @param trace A data frame containing the MCMC trace of all parameters
#' @param posterior_summary A data frame containing the posterior probabilities
#' @return A list with relevant posterior statistics
#' @keywords internal
.parsePosteriorGammaDelta.birp <- function(param_name, meanVar, trace, posterior_summary){
  res <- list(
    exists = FALSE,
    is_fix = TRUE,
    posterior_mean = NULL,
    trace = NULL,
    posterior_median = NULL,
    posterior_q05 = NULL,
    posterior_q95 = NULL,
    prob_positive = NULL,
    posterior_summary = NULL,
    names = NULL,
    num = 0
  )
  if (any(grepl(param_name, meanVar$name))){
    res$exists <- TRUE
    res$trace <- as.matrix(trace[,grepl(param_name, names(trace))])
    res$is_fix <- apply(res$trace, 2, function(x) all(x == 0))
    res$posterior_mean <- meanVar$posterior_mean[grepl(param_name, meanVar$name)]
    res$posterior_median <- apply(res$trace, 2, median)
    res$posterior_q05 <- apply(res$trace, 2, quantile, probs=0.05)
    res$posterior_q95 <- apply(res$trace, 2, quantile, probs=0.95)
    res$prob_positive <- diag(as.matrix(posterior_summary[,2:ncol(posterior_summary)]))
    res$posterior_summary <- posterior_summary
    res$names <- names(posterior_summary)[2:ncol(posterior_summary)]
    res$num <- length(names(posterior_summary)) - 1
  }
  return(res)
}

#' Function to create an object of type birp 
#' @param data An object of type \link{birp_data}, corresponding to filtered data used for inference
#' @param meanVar A data frame containing the posterior mean and variance of all parameters
#' @param trace A data frame containing the MCMC trace of all parameters
#' @param gamma A data frame containing the posterior probabilities regarding gamma
#' @param Delta A data frame containing the posterior probabilities regarding Delta
#' @param timepoints An integer vector containing the timepoints at which counts were obtained
#' @param timesOfChange A numeric or integer vector specifying the times of change
#' @param rate_design A matrix specifying the BACI configuration for the rates of change (gamma, see details).
#' @param step_design A matrix specifying the BACI configuration for the step changes (Delta, see details).
#' @param CI_groups A character vector specifying the names of the control-intervention (CI) group
#' @param state A data frame containing the posterior mean values of all parameters inferred by birp
#' @return An object of type birp
#' @details
#' The `rate_design` and `step_design` matrices define a Before-After Control-Impact experimental design for the rates of change (gamma) and the step changes (Delta), respectively, with the following format:
#' - Each **row** represents a group (e.g., Control or Intervention). The **first column** specifies the group name (e.g. 'Control' or 'Intervention').
#' - Each **column after the first** represents a different epoch. The numbers in these columns indicate which change parameter (\eqn{\gamma} or \eqn{\Delta}) to assign for each group and epoch.
#' For example, BACI = matrix(c("A", "B", 1, 1, 1, 2), nrow = 2) corresponds to a canonical BACI design where the first row represents the control group (A) and the second row represents the intervention group (B). 
#' Please see the vignette for more examples. 
#' 
#' @keywords internal
.createObjBirp.birp <- function(data, meanVar, trace, gamma, Delta, timepoints, timesOfChange, rate_design, step_design, CI_groups, state){
  
  # Calculate statistics on posteriors of gamma and Delta
  post_gamma <- .parsePosteriorGammaDelta.birp("gamma", meanVar, trace, gamma)
  post_Delta <- .parsePosteriorGammaDelta.birp("Delta", meanVar, trace, Delta)
  
  # Calculate statistics on logSigma (if stochastic)
  post_sigma <- list(log_sigma_posterior_mean = NULL,
                     sigma_posterior_mean = NULL)
  if (any(grepl("logSigma", meanVar$name))){
    post_sigma$log_sigma_posterior_mean <- meanVar$posterior_mean[grepl("logSigma", meanVar$name)]
    post_sigma$sigma_posterior_mean <- mean(exp(trace$logSigma)) # as mean(exp(x)) != exp(mean(x))
  }
  
  # Define results
  x <- list(data = data,
            meanVar = meanVar,
            trace = trace,
            post_gamma = post_gamma,
            post_Delta = post_Delta,
            num_epochs = length(timesOfChange) + 1,
            times_of_change = as.numeric(timesOfChange),
            rate_design = rate_design,
            step_design = step_design,
            CI_groups = CI_groups$CI_groups,
            state = state,
            post_sigma = post_sigma,
            timepoints = timepoints
            )
  class(x) <- "birp"
  
  return(x)
}



#---------------------------------------
# Constructor
#---------------------------------------

#' Create a \code{birp} Object
#'
#' This function runs the Markov Chain Monte Carlo (MCMC) algorithm on a \code{birp_data} object to estimate model parameters and returns a fitted \code{birp} object.
#' @param data A \link{birp_data} object containing the input data.
#' @param change A string indicating the type of change to infer. Options are 'rate' (infer exponential rates of change), 'step' (infer step changes) or 'both' (infer both rate and step change). By default, 'rate' is used.
#' @param timesOfChange Numeric or integer vector specifying the times of change (change points) for the model.
#' @param negativeBinomial Logical; if \code{TRUE}, fits a negative binomial model instead of the default Poisson model.
#' @param stochastic Logical; if \code{TRUE}, fits a stochastic trend model instead of the default deterministic trend model.
#' @param rate_design Optional matrix specifying the BACI (Before-After-Control-Impact) design for the rates of change (gamma, see Details). Only applies if \code{change="rate"} or \code{change="both"}.
#' @param step_design Optional matrix specifying the BACI (Before-After-Control-Impact) design for the step changes (Delta, see Details). Only applies if \code{change="step"} or \code{change="both"}.
#' @param assumeTrueDetectionProbability Logical; if \code{TRUE}, provided detection probabilities are treated as true probabilities (logit-transformed without standardization).
#' @param iterations Integer; total number of MCMC iterations to run.
#' @param numBurnin Integer; number of burn-in cycles to run.
#' @param burnin Integer; number of MCMC iterations per burn-in cycle.
#' @param thinning Integer; thinning interval for saving MCMC samples. Only every \code{thinning}th iteration is retained.
#' @param verbose Logical; if \code{FALSE}, suppresses console output.
#' @return An object of type \code{birp} containing MCMC results and model estimates.
#' 
#' @details
#' The `rate_design` and `step_design` matrices define a Before-After Control-Impact experimental design for the rates of change (gamma) and the step changes (Delta), respectively, with the following format:
#' - Each **row** represents a group (e.g., Control or Intervention). The **first column** specifies the group name (e.g. 'Control' or 'Intervention').
#' - Each **column after the first** represents a different epoch. The numbers in these columns indicate which change parameter (\eqn{\gamma} or \eqn{\Delta}) to assign for each group and epoch.
#' For example, BACI = matrix(c("A", "B", 1, 1, 1, 2), nrow = 2) corresponds to a canonical BACI design where the first row represents the control group (A) and the second row represents the intervention group (B). 
#' Please see the vignette for more examples.
#'  
#' @examples 
#' data <- simulate_birp()
#' est <- birp(data)
#' @export
birp <- function(data,
                 change = "rate",
                 timesOfChange = c(),
                 negativeBinomial = FALSE,
                 stochastic = FALSE,
                 rate_design = NULL,
                 step_design = NULL,
                 assumeTrueDetectionProbability = FALSE,
                 iterations = 100000,
                 numBurnin = 10,
                 burnin = 1000,
                 thinning = 10, 
                 verbose = TRUE
                 ){
  # Check for valid arguments
  stopifnot(class(data) == "birp_data")
  
  # Create named list of function arguments 
  args <- c(as.list(environment()))
  
  # Get temporary directory where output will be written
  out <- file.path(tempdir(), "birp")
  # Create directory and make sure files are deleted at the end
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)

  # Parse options and convert to string
  options <- list(task = "infer", out = out)
  for (i in 1:length(args)){
    if (names(args)[i] == "data") next # skip data: no command-line argument
    if (names(args)[i] == "rate_design") next # skip rate_design: no command-line argument
    if (names(args)[i] == "step_design") next # skip step_design: no command-line argument
    options <- .addToList.birp(options, names(args)[i], args[[i]])
  }
  
  # Add input data names
  rcpp_data <- data$data
  options[["data"]] <- paste(data$method_names, collapse = ",")
  
  # Add rate_design and step_design (if provided)
  if (!is.null(rate_design)){
    options[["rate_design"]] <- "rate_design"
    rcpp_data$rate_design <- rate_design
  }
  if (!is.null(step_design)){
    options[["step_design"]] <- "step_design"
    rcpp_data$step_design <- step_design
  }
  
  # Run MCMC
  res <- .birp_interface(options, rcpp_data)
  
  # Properly format Rcpp data frames
  res <- sapply(res, function(x) {if(is.list(x)){ return(list2DF(x))}})

  # Read output files
  meanVar <- res[[paste0(out, "_meanVar.txt")]]
  trace <- res[[paste0(out, "_trace.txt")]]
  gamma <- res[[paste0(out, "_gammaSummaries.txt")]]
  Delta <- res[[paste0(out, "_DeltaSummaries.txt")]]
  timepoints <- res[[paste0(out, "_timepoints.txt")]]
  CI_groups <- res[[paste0(out, "_CI_groups.txt")]]
  state <- res[[paste0(out, "_state.txt")]]
  
  # Read filtered data and convert to birp data object
  filtered_data <- .getDataAllMethods.birp_data(out, "filtered", res)
  
  # Get times of change: might have changed from original input as birp removes pre- or postdating TOCs
  timesOfChange <- res[[paste0(out, "_timesOfChange.txt")]]
  
  # Get BACI configuration for rates and step changes
  rate_design <- res[[paste0(out, "_BACI_gamma_configuration.txt")]]
  step_design <- res[[paste0(out, "_BACI_Delta_configuration.txt")]]
  
  # Create and return birp object
  x <- .createObjBirp.birp(filtered_data, meanVar, trace, gamma, Delta, timepoints, timesOfChange, rate_design, step_design, CI_groups, state)
  return(x)
}

#' Create a birp Object from Command-Line Output Files
#'
#' This function creates a birp object by reading the output files generated by the command-line version of the birp tool.
#' 
#' @param path Character string specifying the directory path containing all birp output files.
#' @return An object of type \code{birp} containing MCMC results and model estimates read from files.
#' @examples 
#' est <- birp_from_command_line(file.path(system.file("extdata", package = "birp")))
#' @export
birp_from_command_line <- function(path){
  # Check for valid arguments
  stopifnot(is.character(path))
  
  # Get all files in that directory
  files <- list.files(path)
  if (length(files) == 0){ stop(paste0("Directory ", path, " is empty!")) }
  
  # Read the names of all (filtered) input files
  namesCounts <- .checkFile.birp(path = path, files = files, patterns = "_filtered_counts.txt", 
                                 allowMultiMatch = TRUE)
  data <- birp_data_from_file(file.path(path, namesCounts), sep = "\t")
  
  # Read MCMC output files
  meanVar <- .openFile.birp(path, files, "_meanVar.txt")
  trace <- .openFile.birp(path, files, "_trace.txt")
  gamma <- .openFile.birp(path, files, "_gammaSummaries.txt")
  Delta <- .openFile.birp(path, files, "_DeltaSummaries.txt")
  timepoints <- .openFile.birp(path, files, "_timepoints.txt")
  CI_groups <- .openFile.birp(path, files, "_CI_groups.txt", header = TRUE)
  state <- .openFile.birp(path, files, "_state.txt", header = TRUE)
  
  # Get times of change
  timesOfChange <- .openFile.birp(path, files, "_timesOfChange.txt", header = FALSE, mustExist = FALSE)
  
  # Get BACI configuration files for rates and step changes
  rate_design <- .openFile.birp(path, files, "_BACI_gamma_configuration.txt", header = FALSE, mustExist = FALSE)
  step_design <- .openFile.birp(path, files, "_BACI_Delta_configuration.txt", header = FALSE, mustExist = FALSE)
  
  # Create and return birp object
  x <- .createObjBirp.birp(data, meanVar, trace, gamma, Delta, timepoints, timesOfChange, rate_design, step_design, CI_groups, state)
  return(x)
}


#' Assess whether a Poisson model can replace the Negative Binomial model
#' 
#' This function tests if the Poisson model is appropriate by simulating replicate datasets under the Poisson assumption and comparing the overdispersion parameter estimates with those obtained from the Negative Binomial (NB) model fit to the original data.
#' @param x A \code{birp} object estimated under a negative binomial model.
#' @param stochastic Logical; if \code{TRUE}, use a stochastic trend model, otherwise deterministic (default).
#' @param numRep Integer; number of replicate datasets to simulate (default 100).
#' @param cutoff Numeric; significance threshold for the fraction of replicates where NB overdispersion exceeds Poisson estimate (default 0.05).
#' @param plot Logical; if \code{TRUE}, plot the distributions of overdispersion parameters from simulated Poisson replicates (default \code{TRUE}).
#' @param verbose Logical; if \code{FALSE}, suppress console output (default \code{TRUE}).
#' 
#' @return A list containing:
#' \item{keep_NB}{Logical scalar, \code{TRUE} if NB model should be kept (data shows overdispersion). If \code{FALSE}, birp should be re-run using the Poisson model to gain power.}
#' \item{keep_NB_per_method}{Logical vector indicating whether NB should be kept for each method.}
#' \item{frac}{Numeric vector with fractions of replicates where Poisson simulated overdispersion exceeded observed NB overdispersion.}
#' \item{b_Pois}{Matrix of overdispersion parameter estimates from Poisson-simulated replicates.}
#' \item{b_x}{Numeric vector of overdispersion parameter estimates from the original NB fit.}

#' @examples 
#' data <- simulate_birp()
#' est <- birp(data, negativeBinomial = TRUE)
#' res_assess <- assess_NB(est, numRep = 5)
#' @export
assess_NB <- function(x, stochastic = FALSE, numRep = 100, cutoff = 0.05, plot = TRUE, verbose = TRUE){
  # get estimated b from negative binomial model
  b_names <- x$meanVar$name[grepl("^b_", x$meanVar$name)]
  b_x <- x$meanVar$posterior_mean[grepl("^b_", x$meanVar$name)]
  
  # check if x was estimated with NB
  if (length(b_names) == 0){
    stop("Birp object does not contain any information on estimated overdispersion parameter b. Please make sure it was inferred under the negative binomial model (negativeBinomial = TRUE).")
  } 
  
  b_Pois <- matrix(0, nrow = numRep, ncol = length(x$data$method_names))
  for (i in 1:numRep){
    # simulate under Poisson assumption
    sim <- simulate_birp_from_results(x, negativeBinomial = FALSE, stochastic = stochastic, verbose = verbose)
    
    # infer NB
    est <- birp(sim, timesOfChange = x$times_of_change, 
                negativeBinomial = TRUE, stochastic = stochastic,
                rate_design = x$rate_design, 
                step_design = x$step_design, verbose = verbose)
    
    # get estimate of b (per method)
    b_Pois[i,] <- est$meanVar$posterior_mean[grepl("^b_", est$meanVar$name)]
  }
  
  # visualize
  if (plot){
    for (i in 1:ncol(b_Pois)){
      dens <- density(b_Pois[,i])
      plot(dens, xlim = range(c(dens$x, b_x)), xlab = b_names[i], ylab = "Density", main = b_names[i])
      abline(v = b_x[i], col = "red", lty = 2)
    }
  }

  # calculate the fraction of replicates where b_Pois > b_x
  frac <- numeric(length(x$data$method_names))
  for (i in 1:ncol(b_Pois)){
    frac[i] <- sum(b_Pois[,i] > b_x[i]) / numRep
  }

  # per method: check if fraction of replicates where b_Pois > b_x is smaller than cutoff
  # null hypothesis = Poisson
  # if the b from the Negative Binomial model is at the right tail -> most b from Poisson are smaller -> keep NB assumption
  # if the b from the Negative Binomial model is within the distribution of the b from Poisson -> switch to Poisson
  keep_NB <- frac < cutoff
  if (any(keep_NB)){
    message("Rejected null hypothesis (Poisson) with for methods", paste0(x$data$method_names[keep_NB], collapse = ", "), "with p-values", paste0(frac[keep_NB], collapse = ", "), ". It is recommended to keep the NB model to account for overdispersion.")
  } else {
    message("Could not reject null hypothesis (Poisson) with for all methods", paste0(x$data$method_names, collapse = ", "), "with p-values", paste0(frac, collapse = ", "), ". It is recommended to rerun birp under the Poisson model (negativeBinomial = FALSE) to gain power.")
  }
  
  return(list(keep_NB = any(keep_NB), keep_NB_per_method = keep_NB, frac = frac, b_Pois = b_Pois, b_x = b_x))
}

#---------------------------------------
# Methods for printing
#---------------------------------------

#' Print a birp object
#'
#' Prints a summary of the estimated parameters from a \code{birp} model.
#'
#' @param x A \code{birp} object.
#' @param ... Additional arguments passed to internal methods (currently unused).
#' @return Invisibly returns the input \code{x}, called for side effects.
#' @export
#' @seealso \code{\link{birp}}
#' @examples
#' data <- simulate_birp()
#' est <- birp(data)
#' print(est)
#' 

print.birp <- function(x, ...){
  cat("Birp estimates:\n")
  if (x$num_epochs > 1){ # only for print multi-epoch
    cat(" - times of change: [", paste0(x$times_of_change, collapse = ", "), "]\n", sep = "")
  }
  if (x$post_gamma$exists){ .printPostSummary.birp(x$post_gamma, "gamma") }
  if (x$post_Delta$exists){ .printPostSummary.birp(x$post_Delta, "Delta") }
  
  invisible(x)
}

#' Summary method for birp objects
#'
#' Provides a printed summary of model estimates for a \code{birp} object.
#'
#' @param object A \code{birp} object.
#' @param ... Additional arguments passed to \code{print.birp}.
#' @return Invisibly returns the input \code{object}, called for side effects.
#' @export
#' @seealso \code{\link{birp}}
#' @examples
#' data <- simulate_birp()
#' est <- birp(data)
#' summary(est)
#' 
summary.birp <- function(object, ...){
  print.birp(object, ...)
}

#' Posterior probability of a population trend
#'
#' Computes the posterior probability that a population trend is increasing
#' or decreasing.
#'
#' @param x A `birp` object.
#' @param positive Logical. If `TRUE` (default), returns the posterior
#'   probability of an increasing (positive) trend,
#'   \eqn{P(\gamma_m > 0 \mid y)}. If `FALSE`, returns the posterior
#'   probability of a decreasing (negative) trend,
#'   \eqn{P(\gamma_m < 0 \mid y)}.
#' @param gamma Integer. Index of the gamma parameter for which to return the
#'   posterior probability. If `NULL` (default), posterior probabilities for
#'   all gamma parameters are returned.
#'
#' @return If `gamma = NULL`, a numeric vector containing posterior
#'   probabilities for all rate parameters. Otherwise, a single numeric value.
#'
#' @seealso [birp()]
#'
#' @examples
#' data <- simulate_birp()
#' est <- birp(data)
#'
#' # Posterior probabilities of increasing trends
#' prob_trend(est)
#'
#' # Posterior probability for a specific gamma
#' prob_trend(est, gamma = 1)
#'
#' # Posterior probabilities of decreasing trends
#' prob_trend(est, positive = FALSE)
#'
#' @export
prob_trend <- function(x, positive = TRUE, gamma = NULL) {
  # Get full matrix with posterior probabilities
  s <- x$post_gamma$posterior_summary
  
  if (is.null(s)){
    stop("No gamma were inferred. Use 'prob_step' to get posterior probabilities of a step change.")
  }
  
  # Remove rownames
  s <- as.matrix(s[, -1, drop = FALSE]) 
  
  # Get diagonal: posterior probabilities for gamma > 0
  pp <- diag(s)
  
  # Retrieve full vector
  if (is.null(gamma)) {
    return(if (positive) pp else 1 - pp)
  }
  
  # Retrieve a single gamma
  if (length(gamma) != 1 || !is.numeric(gamma) || 
      gamma %% 1 != 0 || gamma < 1 || gamma > x$post_gamma$num) {
    stop("`gamma` must be an integer between 1 and ", x$post_gamma$num, ".")
  }
  
  if (positive) {
    pp[gamma]
  } else {
    1 - pp[gamma]
  }
}

#' Posterior probability of a step change
#'
#' Computes the posterior probability that a step change is increasing (positive) or decreasing (negative)
#'
#' @param x A `birp` object.
#' @param positive Logical. If `TRUE` (default), returns the posterior
#'   probability of an increasing (positive) step change,
#'   \eqn{P(\Delta_m > 0 \mid y)}. If `FALSE`, returns the posterior
#'   probability of a decreasing (negative) step change,
#'   \eqn{P(\Delta_m < 0 \mid y)}.
#' @param Delta Integer. Index of the Delta parameter for which to return the
#'   posterior probability. If `NULL` (default), posterior probabilities for
#'   all Delta parameters are returned.
#'
#' @return If `Delta = NULL`, a numeric vector containing posterior
#'   probabilities for all step change parameters. Otherwise, a single numeric value.
#'
#' @seealso [birp()]
#'
#' @examples
#' data <- simulate_birp()
#' est <- birp(data, change = "step")
#'
#' # Posterior probabilities of positive step changes
#' prob_step(est)
#'
#' # Posterior probability for a specific Delta
#' prob_step(est, Delta = 1)
#'
#' # Posterior probabilities of negative step changes
#' prob_step(est, positive = FALSE)
#'
#' @export
prob_step <- function(x, positive = TRUE, Delta = NULL) {
  # Get full matrix with posterior probabilities
  s <- x$post_Delta$posterior_summary
  
  if (is.null(s)){
    stop("No Delta were inferred. Use 'prob_trend' to get posterior probabilities of a trend change.")
  }
  
  s <- as.matrix(s[, -1, drop = FALSE]) # remove rownames
  
  # Get diagonal: posterior probabilities for Delta > 0
  pp <- diag(s)
  
  # Retrieve full vector
  if (is.null(Delta)) {
    if (positive) 
      return(pp)
    return(1 - pp)
  }
  
  # Retrieve a single Delta
  if (length(Delta) != 1 || !is.numeric(Delta) || 
      Delta %% 1 != 0 || Delta < 1 || Delta > x$post_Delta$num) {
    stop("`Delta` must be an integer between 1 and ", x$post_Delta$num, ".")
  }
  
  if (positive) {
    return(pp[Delta])
  }
  return(1 - pp[Delta])
}

#' Pairwise posterior comparisons of population trends
#'
#' Computes pairwise posterior probabilities that one population trend
#' exceeds another.
#'
#' Element \code{[i, j]} of the returned matrix equals
#' \eqn{P(\gamma_i > \gamma_j \mid y)},
#' the posterior probability that the trend associated with row \code{i}
#' is greater than the trend associated with column \code{j}.
#'
#' Values close to 1 indicate strong evidence that
#' \eqn{\gamma_i > \gamma_j}, values close to 0 indicate strong evidence
#' that \eqn{\gamma_i < \gamma_j}, and values near 0.5 indicate little
#' evidence for either comparison
#'
#' @param x A `birp` object.
#'
#' @return A square matrix of pairwise posterior probabilities. Element
#'   \code{[i, j]} gives \eqn{P(\gamma_i > \gamma_j \mid y)}.
#'
#' @seealso [birp()], [prob_trend()]
#'
#' @examples
#' data <- simulate_birp(timepoints = 1:5)
#' est <- birp(data, timesOfChange = c(2,4))
#'
#' prob_trend_diff(est)
#'
#' @export
prob_trend_diff <- function(x) {
  # Get full matrix with posterior probabilities
  s <- x$post_gamma$posterior_summary
  
  if (is.null(s)) {
    stop("No gamma parameters were inferred. 
         Use 'prob_step()' to obtain posterior probabilities of step changes."
    )
  }
  
  # properly assign row- and column names
  rn <- s[, 1]
  s <- as.matrix(s[, -1, drop = FALSE])
  rownames(s) <- rn
  colnames(s) <- rn
  
  # Set diagonal to NA
  diag(s) <- NA
  
  return(s)
}


#---------------------------------------
# Methods for plotting
#---------------------------------------

#' Plot posterior distributions of rate and/or step change parameters
#'
#' Plots the posterior densities of the rate (gamma) and/or step change (Delta) parameters estimated by a \code{birp} object.
#'
#' @param x A \code{birp} object.
#' @param change Character; which parameters to plot. One of \code{"rate"}, \code{"step"}, or
#'   \code{"both"}. Default is \code{"both"} if both exist, otherwise whichever exists.
#' @param shadingIncrease Character or color specification; Shading color for the range where the
#'   parameter is greater than 0. If \code{NA}, shading is omitted. Default is \code{NA}.
#' @param shadingDecrease Character or color specification; Shading color for the range where the
#'   parameter is less than 0. If \code{NA}, shading is omitted. Default is \code{"#f2c7c7"}.
#' @param col Character vector or color values; Line color(s) for the density plots. Recycled
#'   per parameter type. Default is \code{"black"}.
#' @param lwd Numeric vector; Line width(s) for the density plots. Recycled per parameter type.
#'   Default is \code{1}.
#' @param lty Numeric or character vector; Line type(s) for the density plots. If a single value
#'   is provided, it is recycled. Default cycles through \code{1:n} within each parameter type.
#' @param xlim Numeric vector of length 2; Optional x-axis limits applied to all panels.
#'   If \code{NA}, limits are determined automatically. Default is \code{NA}.
#' @param ylim Numeric vector of length 2; Optional y-axis limits applied to all panels.
#'   If \code{NA}, limits are determined automatically. Default is \code{NA}.
#' @param add Logical; If \code{TRUE}, adds the densities to an existing plot (only valid when
#'   \code{change} is \code{"gamma"} or \code{"Delta"}). Default is \code{FALSE}.
#' @param xlab Character (or expression) vector of length 1 or 2; Label(s) for the x-axis.
#'   When \code{change = "both"}, provide two labels (one per panel) or a single value recycled
#'   for both. Defaults to \code{expression(gamma)} / \code{expression(Delta)} as appropriate.
#' @param ylab Character; Label for the y-axis. Default is \code{"Posterior density"}.
#' @param legend Character vector of legend labels, or \code{NA} to suppress the legend.
#'   Defaults to the names stored in the respective \code{post_*} object.
#' @param lineAtZero Logical; If \code{TRUE}, adds a vertical line at x = 0. Default is \code{TRUE}.
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{lines}} and
#'   \code{\link[graphics]{plot}}.
#'
#' @return No return value, called for side effects.
#'
#' @export
#' @seealso \code{\link{birp}}
#' @examples
#' data <- simulate_birp(timepoints = 1:5)
#' est <- birp(data, change = "both")
#' plot(est)
#' plot(est, change = "rate")
#' plot(est, change = "step")
plot.birp <- function(x,
                       change = if (x$post_gamma$exists && x$post_Delta$exists) "both"
                       else if (x$post_gamma$exists) "rate"
                       else "step",
                       shadingIncrease = NA,
                       shadingDecrease = "#f2c7c7",
                       col = "black",
                       lwd = 1,
                       lty = NULL,
                       xlim = NA,
                       ylim = NA,
                       add = FALSE,
                       xlab = NULL,
                       ylab = "Posterior density",
                       legend = NULL,
                       lineAtZero = TRUE,
                       ...) {
  
  change <- match.arg(change, c("rate", "step", "both"))
  
  # --- Validate requested parameters exist ---
  if (change %in% c("rate", "both") && !x$post_gamma$exists) {
    stop("'change' includes \"rate\" but no rates (gamma) were inferred in this birp object.")
  }
  if (change %in% c("step", "both") && !x$post_Delta$exists) {
    stop("'change' includes \"step\" but no step changes (Delta) were inferred in this birp object.")
  }
  if (change == "both" && add) {
    stop("'add = TRUE' is not supported when 'change = \"both\"' (two panels are drawn).")
  }
  
  # --- Build a list of parameter blocks to iterate over ---
  blocks <- list()
  if (change %in% c("rate", "both")) {
    blocks[["rate"]] <- list(
      post   = x$post_gamma,
      xlab   = expression(gamma),
      legend = x$post_gamma$names[!x$post_gamma$is_fix]
    )
  }
  if (change %in% c("step", "both")) {
    blocks[["step"]] <- list(
      post   = x$post_Delta,
      xlab   = expression(Delta),
      legend = x$post_Delta$names[!x$post_Delta$is_fix]
    )
  }
  n_blocks <- length(blocks)
  
  # --- Resolve xlab (allow user to pass 1 or 2 values) ---
  if (is.null(xlab)) {
    xlab_list <- lapply(blocks, `[[`, "xlab")   # defaults per block
  } else {
    xlab_vec  <- if (!is.list(xlab)) list(xlab) else xlab   # wrap scalars
    xlab_list <- rep_len(xlab_vec, n_blocks)
  }
  
  # --- Resolve legend (allow user to pass a list or a single vector) ---
  if (is.null(legend)) {
    legend_list <- lapply(blocks, `[[`, "legend")
  } else {
    legend_list <- if (!is.list(legend)) rep(list(legend), n_blocks) else legend
  }
  
  # --- Split into panels when plotting both ---
  if (change == "both") {
    old_par <- par(mfrow = c(1, 2))
    on.exit(par(old_par), add = TRUE)
  }
  
  # --- Draw each block ---
  for (i in seq_along(blocks)) {
    blk      <- blocks[[i]]
    post     <- blk$post
    n_params <- post$num
    
    # Recycle aesthetics independently per block
    col_i <- rep_len(col, n_params)
    lwd_i <- rep_len(lwd, n_params)
    lty_i <- if (is.null(lty)) seq_len(n_params) else rep_len(lty, n_params)
    
    # Compute densities
    dens <- vector("list", n_params)
    for (e in seq_len(n_params)) {
      dens[[e]] <- stats::density(post$trace[, e])
    }
    
    # Axis limits
    xlim_i <- if (any(is.na(xlim))) range(sapply(dens, function(d) range(d$x))) else xlim
    ylim_i <- if (any(is.na(ylim))) range(sapply(dens, function(d) range(d$y))) else ylim
    
    # Open plot (unless adding to an existing one)
    if (!add) {
      .openPosteriorPlot.birp(
        xlim_i, ylim_i,
        xlab_list[[i]], ylab,
        shadingIncrease, shadingDecrease,
        lineAtZero, ...
      )
    }
    
    # Draw density lines
    for (e in seq_len(n_params)) {
      # If fix: don't draw line
      if (post$is_fix[e]){ next }
      lines(dens[[e]], col = col_i[e], lwd = lwd_i[e], lty = lty_i[e], ...)
    }
    
    # Legend / annotation
    leg_i <- legend_list[[i]]
    if (!any(is.na(leg_i))) {
      if (n_params == 1 | sum(!post$is_fix) == 1) {
        param_name <- ifelse(names(blocks)[i] == "rate", "gamma", "Delta")
        .addTextSingleGammaDelta.birp(post, param_name = param_name)
      } else {
        .addLegendMultiGamma.birp(post$num, leg_i, dens, xlim_i, col_i, lwd_i, lty_i, ...)
      }
    }
  }
}

#' Plot joint posterior of two gamma parameters
#'
#' Plots a 2D density contour for the joint posterior of two gamma parameters from a \code{birp} object.
#'
#' @param x A \code{birp} object.
#' @param gamma1 Integer; Index of the first gamma parameter to plot on the x-axis. Default is the first inferred gamma.
#' @param gamma2 Integer; Index of the second gamma parameter to plot on the y-axis. Default is the second inferred gamma.
#' @param xlab Character; Label for the x-axis. Default is dynamically set based on \code{gamma1}.
#' @param ylab Character; Label for the y-axis. Default is dynamically set based on \code{gamma2}.
#' @param xlim Numeric vector of length 2; Optional x-axis limits. Default is the range of gamma1 and gamma2 values.
#' @param ylim Numeric vector of length 2; Optional y-axis limits. Default is the same as \code{xlim}.
#' @param col Character or color specification; Color for contour lines. Default is \code{"deeppink"}.
#' @param diag.col Character or \code{NA}; Color of the diagonal line (\code{y=x}). Use \code{NA} to omit. Default is \code{"black"}.
#' @param diag.lwd Numeric; Line width of the diagonal line. Default is 1.
#' @param diag.lty Numeric or character; Line type of the diagonal line. Default is 1 (solid).
#' @param zero.col Character or \code{NA}; Color of the zero reference lines (at \code{x=0} and \code{y=0}). Use \code{NA} to omit. Default is \code{"black"}.
#' @param zero.lwd Numeric; Line width of the zero reference lines. Default is 1.
#' @param zero.lty Numeric or character; Line type of the zero reference lines. Default is 2 (dashed).
#' @param print.p Logical; If \code{TRUE}, adds an annotation showing the posterior probability \code{P(gamma1 < gamma2 | data)} or \code{P(gamma1 > gamma2 | data)}. Default is \code{TRUE}.
#' @param add Logical; If \code{TRUE}, adds the contour plot to an existing plot. Default is \code{FALSE}.
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{contour}}.
#' @return No return value; called for side effects (plotting).
#'
#' @export
#' @seealso \code{\link{birp}}
#' @examples 
#' data <- simulate_birp(timesOfChange = 2)
#' est <- birp(data, timesOfChange = 2)
#' plot_epoch_pair(est)

plot_epoch_pair <- function(x, 
                            gamma1 = which(!x$post_gamma$is_fix)[1],
                            gamma2 = which(!x$post_gamma$is_fix)[2],
                            xlab = .getLabelGamma.birp(x, gamma1),
                            ylab = .getLabelGamma.birp(x, gamma2),
                            xlim = range(x$post_gamma$trace[,c(gamma1, gamma2)]),
                            ylim = xlim,
                            col = "deeppink",
                            diag.col = "black",
                            diag.lwd = 1,
                            diag.lty = 1,
                            zero.col = "black",
                            zero.lwd = 1,
                            zero.lty = 2,
                            print.p = TRUE,
                            add = FALSE,
                            ...){
  # check if x has at least 2 epochs
  if (x$post_gamma$num < 2) {
    stop("Need at least 2 gamma!")
  }
  if (sum(!x$post_gamma$is_fix) < 2){
    stop("Need at least 2 gamma that were inferred!")
  }
  
  # Check parameters
  if (is.na(gamma1) | gamma1 < 1 | gamma1 > x$post_gamma$num){ 
    stop("Gamma ", gamma1, " does not exist!")
  }
  if (is.na(gamma2) | gamma2 < 1 | gamma2 > x$post_gamma$num){ 
    stop("Gamma ", gamma2, " does not exist!")
  }
  
  # Obtain density estimates
  dens <- MASS::kde2d(x$post_gamma$trace[,gamma1], x$post_gamma$trace[,gamma2])
  
  #make 2D density plot
  contour(dens$x, dens$y, dens$z, 
          xlim = xlim, ylim = ylim, 
          col = col, 
          xlab = xlab, ylab = ylab, 
          add = add,
          ...)
  
  # Add diagonal
  if (!add & !is.na(diag.lwd) & diag.lwd > 0){
    abline(0, 1, col = diag.col, lwd = diag.lwd, lty = diag.lty)
  }
  
  # Add lines at zero
  if (!add & !is.na(zero.lwd) & zero.lwd > 0){
    if (xlim[1] <= 0 & xlim[2] >= 0){
      lines(c(0, 0), par("usr")[3:4], col = zero.col, lwd = zero.lwd, lty = zero.lty, ...)
    }
    if (ylim[1] <= 0 & ylim[2] >= 0){
      lines(par("usr")[1:2], c(0, 0), col = zero.col, lwd = zero.lwd, lty = zero.lty, ...)
    }
  }
  
  # Print P(gamma1 < gamma2)
  q <- prob_trend_diff(x)[gamma1, gamma2]

  if (!add & print.p){
    text(par("usr")[1] + 0.005 * diff(par("usr")[1:2]), 
         par("usr")[4] - 0.03 * diff(par("usr")[3:4]), 
         pos = 4, 
         labels = substitute(
             paste('P(', gamma[name1], ' > ', gamma[name2], ' | n) = ', q),
             list(name1 = gamma1, name2 = gamma2, q = round(q, digits=4))))
  }
}


#' Plot Posterior Trend Estimates
#'
#' Visualizes posterior trends from a \code{birp} object by plotting the median and quantile intervals of the estimated relative densities over time. Optionally, vertical lines can be added to mark epoch boundaries and survey timepoints.
#'
#' @param x A \code{birp} object containing MCMC trace and model outputs.
#' @param CI_group Integer; Index of the control-intervention group to plot. Default is 1.
#' @param n_points Integer; Number of points to evaluate the trend over time. Default is 1000.
#' @param quantiles Numeric vector; Quantiles to plot as shaded intervals. Must be in (0, 1). Default is c(0.99, 0.9, 0.5, 0.25).
#' @param quantile.col Character vector or color values; Fill colors for quantile polygons. Default is shades of gray.
#' @param quantile.border Character or NA; Border color for quantile polygons. Use NA to omit borders. Default is NA.
#' @param median.col Character; Color of the median trend line. Default is "deeppink".
#' @param median.lwd Numeric; Line width for the median trend. Default is 1.
#' @param median.lty Numeric or character; Line type for the median trend line. Default is 1 (solid).
#' @param epoch.col Character or color specification; Color for lines representing epoch boundaries. Default is \code{"black"}.
#' @param epoch.lwd Numeric; Line width for epoch boundary lines. Default is 1.
#' @param epoch.lty Numeric or character; Line type for epoch boundary lines. Default is 1 (solid).
#' @param times.col Character or color specification; Color for vertical lines representing measurement times. Default is \code{"black"}.
#' @param times.lwd Numeric; Line width for measurement time lines. Default is 1.
#' @param times.lty Numeric or character scalar; Line type for measurement time lines. Default is 2 (dashed).
#' @param log Logical; If \code{TRUE}, plot relative densities on a logarithmic scale; otherwise plot on the original scale. Default is \code{FALSE}.
#' @param xlab Character; Label for the x-axis. Default is \code{"Time"}.
#' @param ylab Character; Label for the y-axis. Default dynamically set to either \code{"log Relative Density"} or \code{"Relative Density"}.
#' @param main Character; Main title of the plot. Defaults to the name of the selected CI group.
#' @param ... Additional graphical parameters passed to the base \code{plot} function.
#' @return No return value, called for side effects.
#'
#' @export
#' @seealso \code{\link{birp}}
#' @importFrom grDevices gray
#' @importFrom stats quantile
#' @examples 
#' data <- simulate_birp()
#' est <- birp(data)
#' plot_trend(est)
plot_trend <- function(x, 
                       CI_group = 1,
                       n_points = 1000, 
                       quantiles = c(0.99, 0.9, 0.5, 0.25), 
                       quantile.col = "gray"(seq(1, 0, length.out = length(quantiles)+2)[2:(length(quantiles)+1)]), 
                       quantile.border = NA,
                       median.col = "deeppink",
                       median.lwd = 2,
                       median.lty = 1,
                       epoch.col = "black",
                       epoch.lwd = 1,
                       epoch.lty = 1,
                       times.col = "black",
                       times.lwd = 1,
                       times.lty = 2,
                       log = FALSE,
                       xlab = "Time",
                       ylab = paste(c("log", "Relative Density")[c(log, TRUE)], collapse=" "),
                       main = x$CI_groups[CI_group],
                       ...){
  if (!x$post_gamma$exists && !x$post_Delta$exists){
    stop("Neither gamma nor Delta were inferred - nothing to plot.")
  }
  
  if (CI_group > length(x$CI_groups)){
    stop(paste0("Invalid CI_group index ", CI_group, "!"))
  }
  
  # Check parameters
  if(max(quantiles) > 1.0){ stop("Provided quantiles must be <= 1.0!") }
  if(min(quantiles) <= 0.0){ stop("Provided quantiles must be > 0.0!") }
  
  xlim <- range(x$timepoints)
  times_of_change <- x$times_of_change
  
  has_gamma <- x$post_gamma$exists & sum(!x$post_gamma$is_fix) > 0
  has_Delta <- x$post_Delta$exists & sum(!x$post_Delta$is_fix) > 0
  
  # Get gammas of CI group, if any were inferred
  gamma.cols <- NULL
  if (has_gamma){
    relevant_gamma_names <- as.character(x$rate_design[CI_group,2:ncol(x$rate_design)])
    # Get indices of gamma
    gamma.cols <- as.numeric(sapply(relevant_gamma_names, function(name) which(x$post_gamma$names == name)))
  }
  
  # Get Deltas of CI group, if any were inferred
  Delta.cols <- NULL
  if (has_Delta){
    relevant_Delta_names <- as.character(x$step_design[CI_group,2:ncol(x$step_design)])
    Delta.cols <- as.numeric(sapply(relevant_Delta_names, function(name) which(x$post_Delta$names == name)))
  }
  
  # Get times of change that should be marked in plot
  # (highlight if either gamma or Delta differs across that boundary)
  highlight_times_of_change <- c()
  if (x$num_epochs > 1){
    for (i in 2:x$num_epochs){
      gamma_changes <- has_gamma && !is.null(gamma.cols) && (gamma.cols[i] != gamma.cols[i-1])
      Delta_present <- has_Delta && !is.null(Delta.cols) && (Delta.cols[i] != Delta.cols[i-1])
      if (gamma_changes || Delta_present){
        highlight_times_of_change <- c(highlight_times_of_change, x$times_of_change[i-1])
      }
    }
  }
  # Add last Delta (at the final timepoint, xlim[2])
  if (has_Delta && !is.null(Delta.cols) && length(Delta.cols) > 0){
    highlight_times_of_change <- c(highlight_times_of_change, xlim[2])
  }
  
  # Prepare calculations of means
  epoch_ranges <- c(xlim[1], 
                    times_of_change[times_of_change > xlim[1] & times_of_change < xlim[2]], 
                    xlim[2])
  epoch_length <- epoch_ranges[2:length(epoch_ranges)] - epoch_ranges[1:(length(epoch_ranges)-1)]
  rho <- .calculateRho.birp(epoch_ranges, times_of_change)
  num_epochs <- length(epoch_length)
  
  # Step times for Delta: end of each epoch, i.e. all internal times_of_change
  # plus the final timepoint (end of the last epoch). Length == num_epochs.
  Delta_step_times <- epoch_ranges[2:length(epoch_ranges)]
  
  # Prepare points at which to calculate rates
  xvals <- seq(xlim[1], xlim[2], length.out = n_points)
  rho_x <- .calculateRho.birp(xvals, times_of_change)
  mcmc_length <- nrow(x$trace)
  rates <- matrix(0, ncol = length(xvals), nrow = mcmc_length - 1)
  
  # Step-indicator matrices: psi[i,m] = 1{ t_i >= Delta_step_times[m] }
  psi_x <- NULL
  if (has_Delta && !is.null(Delta.cols) && length(Delta.cols) > 0){
    psi_x <- .calculatePsi.birp(xvals, Delta_step_times)
  }
  
  gamma_is_fix_zero <- x$post_gamma$is_fix[gamma.cols]
  
  for (i in 2:mcmc_length){
    # Calculate mean to normalize / align
    # Prevent underflow by normalizing with mean
    # NOTE: deliberately gamma-only here - this computes the average value of the
    # gamma-driven exponential growth over each epoch, used as a normalization
    # constant. Mixing in Delta jumps breaks the sign-cancellation that keeps
    # (change[m+1]-change[m])/gamma[m] positive regardless of gamma's sign.
    change <- 0
    if (has_gamma){
      change <- rho %*% x$post_gamma$trace[i,gamma.cols]
    
      meanLog <- mean(change)
      change <- exp(change - meanLog)
      
      gamma_i <- x$post_gamma$trace[i,gamma.cols]
      numer <- change[2:(num_epochs+1),1] - change[1:num_epochs]
      
      epoch_avg <- numeric(num_epochs)
      epoch_avg[!gamma_is_fix_zero] <- numer[!gamma_is_fix_zero] / gamma_i[!gamma_is_fix_zero] / epoch_length[!gamma_is_fix_zero]
      epoch_avg[gamma_is_fix_zero]  <- change[1:num_epochs][gamma_is_fix_zero]
      
      average <- sum(epoch_avg)
    }
    
    # Calc normalized rates: gamma contribution, normalized, plus the (separate,
    # additive) Delta step contribution
    if (has_gamma){
      rates[i-1,] <- rho_x %*% gamma_i - log(average) - meanLog
    }
    if (has_Delta){
      rates[i-1,] <- rates[i-1,] + psi_x %*% x$post_Delta$trace[i,Delta.cols]
    }
  }
  
  if(!log){
    rates <- exp(rates)
  }
  
  # Calculate quantiles and median of distribution
  probs <- sort(c((1-quantiles)/2, 0.5, 1-(1-quantiles)/2))
  quant <- apply(rates, 2, quantile, probs = probs)
  
  # Open plot
  plot(0, type = 'n', xlim = xlim, ylim = range(quant, na.rm = TRUE), xlab = xlab, ylab = ylab, main = main)
  
  # Add epochs
  if(!is.na(epoch.lwd) & epoch.lwd>0){
    for(t in highlight_times_of_change[highlight_times_of_change > xlim[1] & highlight_times_of_change < xlim[2]]){
      lines(rep(t, 2), par("usr")[3:4], col = epoch.col, lwd = epoch.lwd, lty = epoch.lty)
    }
  }
  
  # Add times with measurements
  if(!is.na(times.lwd) & times.lwd>0){
    for(t in x$timepoints[x$timepoints >= xlim[1] & x$timepoints <= xlim[2]]){
      lines(rep(t, 2), par("usr")[3:4], col = times.col, lwd = times.lwd, lty = times.lty)
    }
  }
  
  # Plot quantiles
  n_probs <- length(probs)
  x_poly <- c(xvals, rev(xvals), xvals[1])
  for(q in 1:length(quantiles)){
    y_poly <- c(quant[q,], rev(quant[n_probs-q+1,]), quant[q,1])
    polygon(x_poly, y_poly, col = quantile.col[q], border = quantile.border)
  }
  
  # Plot median
  if (!is.na(median.lwd) & median.lwd > 0){
    lines(xvals, quant[length(quantiles) + 1,], lwd = median.lwd, lty = median.lty, col = median.col)
  }
}

#' Plot MCMC Traces and Posterior Densities
#'
#' Visualizes the MCMC trace plots and posterior densities of the gamma and Delta parameters from a \code{birp} object.
#'
#' @param x A \code{birp} object containing posterior samples.
#' @param col Character vector; Colors for trace and density plots. Default is c("black", "blue").
#'
#' @return No return value; the function is called for its side effects (plotting).
#'
#' @export
#' @seealso \code{\link{birp}}
#' @examples 
#' data <- simulate_birp()
#' est <- birp(data)
#' plot_mcmc(est)

plot_mcmc <- function(x, col=c("black", "blue")){
  # Layout
  on.exit(layout(matrix(1)))
  layout(matrix(1:(2*x$post_gamma$num + 2*x$post_Delta$num), 
                ncol = 2, byrow=TRUE), widths = c(2,1))
  
  # Plot MCMC and posterior for each epoch
  mcmc_len <- nrow(x$trace)
  
  # Loop over gamma and Delta
  posteriors <- list(x$post_gamma, x$post_Delta)
  param_names <- c("gamma", "Delta")
  
  for (p in 1:length(posteriors)){
    post <- posteriors[[p]]
    param_name <- param_names[p]
    
    if (post$exists){
      for(i in 1:post$num){
        # Plot trace
        xax <- 1:nrow(post$trace)
        plot(xax, post$trace[,i], xlab = "Iteration (thinned)", ylab = bquote(.(as.name(param_name))[.(i)]))
        
        # Plot density
        plot(stats::density(post$trace[,i]),  main="", xlab=bquote(.(as.name(param_name))[.(i)]), ylab="Posterior density")
      }
    }
  }
}



