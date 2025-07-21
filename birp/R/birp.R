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
  return(substitute(paste(gamma[index], ' (', name, ')'), list(index=index, name=x$gamma_names[index])))
}

#' Function to add text box to plot denoting P(gamma > 0 | n) or P(gamma < 0 | n) for single gammas
#' @param x A birp object
#' @return No return value, called for side effects.
#' @keywords internal
.addTextSingleGamma.birp <- function(x){
  diffFromBorder <- 0.01 * diff(par("usr")[1:2])
  if(x$prob_gamma_positive > 0.5){
    ttext <- bquote(paste("P(", gamma, " > 0 | n) = ", .(round(x$prob_gamma_positive, 3))))
    text(par("usr")[2] - diffFromBorder, par("usr")[4], adj = c(1, 1.5), labels = ttext)
  } else {
    ttext <- bquote(paste("P(", gamma, " < 0 | n) = ", .(round(1 - x$prob_gamma_positive, 3))))
    text(par("usr")[1] + diffFromBorder, par("usr")[4], adj = c(0, 1.5), labels = ttext)
  }
}

#' Function to add legend to plot denoting gammas
#' @param x A birp object
#' @param legend Add a legend to the plot
#' @param dens A list containing the densities for each gamma
#' @param xlim The x-limits (x1, x2) of the plot
#' @param col Line color, one per epoch
#' @param lwd Line width, one per epoch
#' @param lty Line type, one per epoch
#' @param ... additional parameters passed to the function.
#' @return No return value, called for side effects.
#' @keywords internal
.addLegendMultiGamma.birp <- function(x, legend, dens, xlim, col, lwd, lty, ...){
  # Add legend
  # Check if highest density is left or right of plot
  max.y <- max(dens[[1]]$y)
  max.x <- dens[[1]]$x[dens[[1]]$y == max(dens[[1]]$y)]
  if (x$num_gamma > 1){
    for (e in 2:x$num_gamma){
      if (max(dens[[e]]$y) > max.y){
        max.y <- max(dens[[e]]$y)
        max.x <- dens[[e]]$x[dens[[e]]$y == max(dens[[e]]$y)]
      }
    }
  }
  
  if (max.x < xlim[1] + diff(xlim)/2){
    legend.pos <- 'topright'
  } else {
    legend.pos <- 'topleft'
  }
  
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
  if (file.size(file.path(path, filename)) == 3){ return(data.frame()) } # empty file
  f <- read.table(file.path(path, filename), header = header, check.names = FALSE, sep = sep)
  return(f)
}

#' Function to create an object of type birp 
#' @param data An object of type \link{birp_data}, corresponding to filtered data used for inference
#' @param meanVar A data frame containing the posterior mean and variance of all parameters
#' @param trace A data frame containing the MCMC trace of all parameters
#' @param gamma A data frame containing the posterior probabilities regarding gamma
#' @param timepoints An integer vector containing the timepoints at which counts were obtained
#' @param timesOfChange A numeric or integer vector specifying the times of change
#' @param BACI A matrix specifying the BACI configuration. Each row of the matrix corresponds to a control/intervention group, and each column to an epoch. The very first column specifies the name of the control-intervention group and must match the groups specified in data. The values of the matrix specify which gamma to use for each group and epoch. E.g. BACI = matrix(c("A", "B", 1, 1, 1, 2), nrow = 2) corresponds to a canonical BACI design where the first row represents the control group (A) and the second row represents the intervention group (B)
#' @param CI_groups A character vector specifying the names of the control-intervention (CI) group
#' @param state A data frame containing the posterior mean values of all parameters inferred by birp
#' @return An object of type birp
#' @keywords internal
.createObjBirp.birp <- function(data, meanVar, trace, gamma, timepoints, timesOfChange, BACI, CI_groups, state){
  # Calculate statistics on gamma
  gamma_posterior_mean <- meanVar$posterior_mean[grepl("gamma", meanVar$name)]
  trace_gamma <- as.matrix(trace[,grepl("gamma", names(trace))])
  gamma_posterior_median <- apply(trace_gamma, 2, median)
  gamma_posterior_q05 <- apply(trace_gamma, 2, quantile, probs=0.05)
  gamma_posterior_q95 <- apply(trace_gamma, 2, quantile, probs=0.95)
  matrix_gamma <- as.matrix(gamma[,2:ncol(gamma)])
  prob_gamma_positive <- diag(matrix_gamma)
  
  # Calculate statistics on logSigma (if stochastic)
  log_sigma_posterior_mean <- NULL
  sigma_posterior_mean <- NULL
  if (any(grepl("logSigma", meanVar$name))){
    log_sigma_posterior_mean <- meanVar$posterior_mean[grepl("logSigma", meanVar$name)]
    sigma_posterior_mean <- mean(exp(trace$logSigma)) # as mean(exp(x)) != exp(mean(x))
  }
  
  # Define results
  x <- list(data = data,
            meanVar = meanVar,
            trace = trace,
            trace_gamma = trace_gamma,
            gamma = gamma,
            num_epochs = length(timesOfChange) + 1,
            gamma_names = names(gamma)[2:ncol(gamma)],
            num_gamma = length(names(gamma)) - 1,
            times_of_change = as.numeric(timesOfChange),
            BACI = BACI,
            CI_groups = CI_groups$CI_groups,
            state = state,
            gamma_posterior_mean = gamma_posterior_mean,
            gamma_posterior_median = gamma_posterior_median,
            gamma_posterior_q05 = gamma_posterior_q05,
            gamma_posterior_q95 = gamma_posterior_q95,
            prob_gamma_positive = prob_gamma_positive,
            log_sigma_posterior_mean = log_sigma_posterior_mean,
            sigma_posterior_mean = sigma_posterior_mean,
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
#' @param timesOfChange Numeric or integer vector specifying the times of change (change points) for the model.
#' @param negativeBinomial Logical; if \code{TRUE}, fits a negative binomial model instead of the default Poisson model.
#' @param stochastic Logical; if \code{TRUE}, fits a stochastic trend model instead of the default deterministic trend model.
#' @param BACI Optional matrix specifying the BACI (Before-After-Control-Impact) design. Each row corresponds to a control/intervention group and each column to an epoch. The first column contains the control-intervention group names (matching those in \code{data}), and subsequent columns specify which gamma (rate of change) parameter to use for each group and epoch. For example, \code{BACI = matrix(c("A", "B", 1, 1, 1, 2), nrow = 2)} corresponds to a canonical BACI design with control group "A" and intervention group "B".
#' @param assumeTrueDetectionProbability Logical; if \code{TRUE}, provided detection probabilities are treated as true probabilities (logit-transformed without standardization).
#' @param iterations Integer; total number of MCMC iterations to run.
#' @param numBurnin Integer; number of burn-in cycles to run.
#' @param burnin Integer; number of MCMC iterations per burn-in cycle.
#' @param thinning Integer; thinning interval for saving MCMC samples. Only every \code{thinning}th iteration is retained.
#' @param verbose Logical; if \code{FALSE}, suppresses console output.

#' @return An object of type \code{birp} containing MCMC results and model estimates.
#' @examples 
#' data <- simulate_birp()
#' est <- birp(data)
#' @export
birp <- function(data,
                 timesOfChange = c(),
                 negativeBinomial = FALSE,
                 stochastic = FALSE,
                 BACI = NULL,
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

  # Parse options and convert to string
  options <- list(task = "infer", out = out)
  for (i in 1:length(args)){
    if (names(args)[i] == "data") next # skip data: no command-line argument
    if (names(args)[i] == "BACI") next # skip BACI: no command-line argument
    options <- .addToList.birp(options, names(args)[i], args[[i]])
  }
  
  # Add input data names
  rcpp_data <- data$data
  options[["data"]] <- paste(data$method_names, collapse = ",")
  
  # Add BACI (if provided)
  if (!is.null(BACI)){
    options[["BACI"]] <- "BACI"
    rcpp_data$BACI <- BACI
  }
  
  # Run MCMC
  res <- .birp_interface(options, rcpp_data)
  
  # Properly format Rcpp data frames
  res <- sapply(res, function(x) {if(is.list(x)){ return(list2DF(x))}})

  # Read output files
  meanVar <- res[[paste0(out, "_meanVar.txt")]]
  trace <- res[[paste0(out, "_trace.txt")]]
  gamma <- res[[paste0(out, "_gammaSummaries.txt")]]
  timepoints <- res[[paste0(out, "_timepoints.txt")]]
  CI_groups <- res[[paste0(out, "_CI_groups.txt")]]
  state <- res[[paste0(out, "_state.txt")]]
  
  # Read filtered data and convert to birp data object
  filtered_data <- .getDataAllMethods.birp_data(out, "filtered", res)
  
  # Get times of change: might have changed from original input as birp removes pre- or postdating TOCs
  timesOfChange <- res[[paste0(out, "_timesOfChange.txt")]]
  
  # Get BACI configuration
  BACI <- res[[paste0(out, "_BACI_configuration.txt")]]
  
  # Create and return birp object
  x <- .createObjBirp.birp(filtered_data, meanVar, trace, gamma, timepoints, timesOfChange, BACI, CI_groups, state)
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
  timepoints <- .openFile.birp(path, files, "_timepoints.txt")
  CI_groups <- .openFile.birp(path, files, "_CI_groups.txt", header = TRUE)
  state <- .openFile.birp(path, files, "_state.txt", header = TRUE)
  
  # Get times of change
  timesOfChange <- .openFile.birp(path, files, "_timesOfChange.txt", header = FALSE, mustExist = FALSE)
  
  # Get BACI file
  BACI <- .openFile.birp(path, files, "_BACI_configuration.txt", header = FALSE)
  
  # Create and return birp object
  x <- .createObjBirp.birp(data, meanVar, trace, gamma, timepoints, timesOfChange, BACI, CI_groups, state)
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
    
    # infer NB (with BACI only if necessary to avoid warning)
    if (length(x$data$CI_groups) > 1 & length(x$times_of_change) > 0){
      est <- birp(sim, timesOfChange = x$times_of_change, 
                  negativeBinomial = TRUE, stochastic = stochastic,
                  BACI = x$BACI, verbose = verbose)
    } else {
      est <- birp(sim, timesOfChange = x$times_of_change, 
                  negativeBinomial = TRUE, stochastic = stochastic, verbose = verbose)
    }
    
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
  cat(" - Gamma: [", paste0(x$gamma_names, collapse=", "), "]\n", sep = "")
  cat(" - Posterior mean of gamma: [", paste0(x$gamma_posterior_mean, collapse=", "), "]\n", sep = "")
  cat(" - Posterior median of gamma: [", paste0(x$gamma_posterior_median, collapse=", "), "]\n", sep = "")
  cat(" - Posterior 5% quantile of gamma: [", paste0(x$gamma_posterior_q05, collapse=", "), "]\n", sep = "")
  cat(" - Posterior 95% quantile of gamma: [", paste0(x$gamma_posterior_q95, collapse=", "), "]\n", sep = "")
  cat(" - Posterior probability of increasing trend P(gamma > 0): [", paste0(x$prob_gamma_positive, collapse=", "), "]\n", sep = "")
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

#---------------------------------------
# Methods for plotting
#---------------------------------------

#' Plot posterior distributions of gamma parameters
#'
#' Plots the posterior densities of the gamma parameters estimated by a \code{birp} object.
#'
#' @param x A \code{birp} object.
#' @param shadingIncrease Character or color specification; Shading color for the range where the gamma parameter is greater than 0 (\code{gamma > 0}). If \code{NA}, shading is omitted. Default is \code{NA}.
#' @param shadingDecrease Character or color specification; Shading color for the range where the gamma parameter is less than 0 (\code{gamma < 0}). If \code{NA}, shading is omitted. Default is \code{"#f2c7c7"}.
#' @param col Character vector or color values; Line color(s) for the density plots. If a single value is provided, it is recycled for all gamma parameters. Default is \code{"black"}.
#' @param lwd Numeric vector; Line width(s) for the density plots. If a single value is provided, it is recycled. Default is 1.
#' @param lty Numeric or character vector; Line type(s) for the density plots. If a single value is provided, it is recycled. Default is \code{1:x$num_gamma}.
#' @param xlim Numeric vector of length 2; Optional x-axis limits. If \code{NA}, limits are determined automatically from the density data. Default is \code{NA}.
#' @param ylim Numeric vector of length 2; Optional y-axis limits. If \code{NA}, limits are determined automatically from the density data. Default is \code{NA}.
#' @param add Logical; If \code{TRUE}, adds the densities to an existing plot. Otherwise, creates a new plot. Default is \code{FALSE}.
#' @param xlab Character; Label for the x-axis. Default is \code{expression(gamma)}.
#' @param ylab Character; Label for the y-axis. Default is \code{"Posterior density"}.
#' @param legend Character vector of legend labels, or \code{NA} to suppress the legend. Default is \code{x$gamma_names}.
#' @param lineAtZero Logical; If \code{TRUE}, adds a vertical line at x = 0 to indicate no effect. Default is \code{TRUE}.
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{lines}} (when plotting the densities) and \code{\link[graphics]{plot}} (when creating a new plot).
#'
#' @return No return value, called for side effects.
#'
#' @export
#' @seealso \code{\link{birp}}
#' @examples 
#' data <- simulate_birp()
#' est <- birp(data)
#' plot(est)

plot.birp <- function(x,
                      shadingIncrease = NA,
                      shadingDecrease = "#f2c7c7",
                      col = "black",
                      lwd = 1,
                      lty = 1:x$num_gamma,
                      xlim = NA,
                      ylim = NA,
                      add = FALSE,
                      xlab = expression(gamma),
                      ylab = "Posterior density",
                      legend = x$gamma_names,
                      lineAtZero = TRUE,
                      ...){
  # Recycle col, lwd and lty
  col <- rep_len(col, x$num_gamma)
  lwd <- rep_len(lwd, x$num_gamma)
  lty <- rep_len(lty, x$num_gamma)
  
  # Calculate all densities
  dens <- list(x$num_gamma)
  for (e in 1:x$num_gamma){
    dens[[e]] <- stats::density(x$trace_gamma[,e])
  }
  
  # Get limits
  if (any(is.na(xlim))){
    xlim <- range(sapply(dens, function(d) range(d$x)))
  }
  if (any(is.na(ylim))){
    ylim <- range(sapply(dens, function(d) range(d$y)))
  }
  
  # Open plot
  if (!add){
    .openPosteriorPlot.birp(xlim, ylim, xlab, ylab, shadingIncrease, shadingDecrease, lineAtZero, ...)
  }
  
  # Plot densities
  for (e in 1:x$num_gamma){
    lines(dens[[e]], col = col[e], lwd = lwd[e], lty = lty[e], ...)
  }
  
 
  # Add legend?
  if (!any(is.na(legend))){
    if (x$num_gamma == 1){
      .addTextSingleGamma.birp(x)
    } else {
      .addLegendMultiGamma.birp(x, legend, dens, xlim, col, lwd, lty, ...)
    }
  }
}


#' Plot joint posterior of two gamma parameters
#'
#' Plots a 2D density contour for the joint posterior of two gamma parameters from a \code{birp} object.
#'
#' @param x A \code{birp} object.
#' @param gamma1 Integer; Index of the first gamma parameter to plot on the x-axis. Default is 1.
#' @param gamma2 Integer; Index of the second gamma parameter to plot on the y-axis. Default is 2.
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
                            gamma1 = 1,
                            gamma2 = 2,
                            xlab = .getLabelGamma.birp(x, gamma1),
                            ylab = .getLabelGamma.birp(x, gamma2),
                            xlim = range(x$trace_gamma[,c(gamma1, gamma2)]),
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
  if (x$num_gamma < 2) {
    stop("Need at least 2 gamma!")
  }
  
  # Check parameters
  if (gamma1 < 1 | gamma1 > x$num_gamma){ stop("Gamma ", gamma1, " does not exist!") }
  if (gamma2 < 1 | gamma2 > x$num_gamma){ stop("Gamma ", gamma2, " does not exist!") }
  
  # Obtain density estimates
  dens <- MASS::kde2d(x$trace_gamma[,gamma1], x$trace_gamma[,gamma2])
  
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
  q <- sum(x$trace_gamma[,gamma1] < x$trace_gamma[,gamma2]) / nrow(x$trace_gamma)
  
  if (!add & print.p){
    if (q < 0.5){
      text(par("usr")[1] + 0.005 * diff(par("usr")[1:2]), 
           par("usr")[4] - 0.03 * diff(par("usr")[3:4]), 
           pos = 4, 
           labels = substitute(
               paste('P(', gamma[name1], ' < ', gamma[name2], ' | n) = ', q),
               list(name1 = gamma1, name2 = gamma2, q = round(q, digits=4))))
    } else {
      text(par("usr")[2] - 0.005 * diff(par("usr")[1:2]), 
           par("usr")[3] + 0.03 * diff(par("usr")[3:4]), 
           pos = 2, 
           labels = substitute(
             paste('P(', gamma[name1], ' > ', gamma[name2], ' | n) = ', q),
             list(name1 = gamma1, name2 = gamma2, q = round(1 - q, digits=4))))
    }
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
                            median.lwd = 1,
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
  if (CI_group > length(x$CI_groups)){
    stop(paste0("Invalid CI_group index ", CI_group, "!"))
  }
  
  # Check parameters
  if(max(quantiles) > 1.0){ stop("Provided quantiles must be <= 1.0!") }
  if(min(quantiles) <= 0.0){ stop("Provided quantiles must be > 0.0!") }

  # Get gammas of CI group
  relevant_gamma_names <- as.character(x$BACI[CI_group,])
  # Get indices of gamma
  gamma.cols <- as.numeric(sapply(relevant_gamma_names, function(name) which(x$gamma_names == name)))
  
  xlim <- range(x$timepoints)
  times_of_change <- x$times_of_change
  
  # Get times of change that should be marked in plot
  highlight_times_of_change <- c()
  if (x$num_epochs > 1){
    for (i in 2:x$num_epochs){
      if (gamma.cols[i] != gamma.cols[i-1]){
        highlight_times_of_change <- c(highlight_times_of_change, x$times_of_change[i-1])
      }
    }
  }
  
  # Prepare calculations of means
  epoch_ranges <- c(xlim[1], times_of_change[times_of_change > xlim[1] & times_of_change < xlim[2]], xlim[2])
  epoch_length <- epoch_ranges[2:length(epoch_ranges)] - epoch_ranges[1:(length(epoch_ranges)-1)]
  rho <- .calculateRho.birp(epoch_ranges, times_of_change)
  num_epochs <- length(epoch_length)

  # Prepare points at which to calculate rates
  xvals <- seq(xlim[1], xlim[2], length.out = n_points)
  rho_x <- .calculateRho.birp(xvals, times_of_change)
  mcmc_length <- nrow(x$trace)
  rates <- matrix(0, ncol = length(xvals), nrow = mcmc_length - 1)
  
  for (i in 2:mcmc_length){
    # Calculate mean to normalize / align
    # Prevent underflow by normalizing with mean
    change <- rho %*% x$trace_gamma[i,gamma.cols]
    meanLog <- mean(change)
    change <- exp(change - meanLog)
    average <- sum((change[2:(num_epochs+1),1] - change[1:num_epochs]) / x$trace_gamma[i,gamma.cols] / epoch_length)
    
    # Calc normalized rates
    rates[i-1,] <- rho_x %*% x$trace_gamma[i,gamma.cols] - log(average) - meanLog
  }
  
  if(!log){
    rates <- exp(rates)
  }
  
  # Calculate quantiles and median of distribution
  probs <- sort(c((1-quantiles)/2, 0.5, 1-(1-quantiles)/2))
  quant <- apply(rates, 2, quantile, probs = probs)
  
  # Open plot
  plot(0, type = 'n', xlim = xlim, ylim = range(quant, na.rm = TRUE), xlab = xlab, ylab = ylab, main = main, ...)
  
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
#' Visualizes the MCMC trace plots and posterior densities of the gamma parameters from a \code{birp} object.
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
  layout(matrix(1:(2*x$num_gamma), ncol = 2, byrow=TRUE), widths = c(2,1))
  
  # Plot MCMC and posterior for each epoch
  mcmc_len <- nrow(x$trace)
  
  for(i in 1:x$num_gamma){
    # Plot trace
    xax <- 1:nrow(x$trace_gamma)
    plot(xax, x$trace_gamma[,i], xlab = "Iteration (thinned)", ylab = bquote(gamma[.(i)]))

    # Plot density
    plot(stats::density(x$trace_gamma[,i]),  main="", xlab=bquote(gamma[.(i)]), ylab="Posterior density")
  }
}



