#---------------------------------------
# R-package Birp
#---------------------------------------

#---------------------------------------
# Internal functions, not exported
#---------------------------------------

#' This function check if a birp_data object is valid
#' @param data A list of dataframes to validate
#' @return No return value, called for side effects.
#' @keywords internal
.validate.birp_data <- function(data){
  if (!is.list(data) | !all(sapply(data, is.data.frame))){
    stop("Argument 'data' of constructor 'birp_data' should be a single data frame or a list of data frames!")
  }
  
  # Check data frames (most checks happen in C++, only check minimum here)
  for (i in 1:length(data)){
    if (ncol(data[[i]]) < 4) stop("Data frame should have at least four columns!")
    
    if (!("timepoint" %in% names(data[[i]]))) stop("Data frame should contain a column with name 'timepoint'")
    if (!("location" %in% names(data[[i]]))) stop("Data frame should contain a column with name 'location'")
    if (!any(grepl("counts", names(data[[i]])))) stop("Data frame should contain a column starting with 'counts'")
    if (!any(grepl("covEffort", names(data[[i]])) | grepl("effort", names(data[[i]])))) stop("Data frame should contain a column starting with 'covEffort' or 'effort'")
  }
}

#' This function reads data for all methods into a list
#' @param out A string corresponding to the output prefix
#' @param res A list of dataframes from Rcpp
#' @return An instance of birp_data
#' @keywords internal
.getDataAllMethods.birp_data <- function(out, what, res){
  names <- res[[paste0(out, "_", what, "_allFilesGenerated.txt")]]$V0
  all_df <- list()
  for (i in 1:length(names)) { 
    bn_out <- basename(out)
    bn_name <- basename(names[i])
    nice_name <- strsplit(bn_name, split = paste0(bn_out, "_"))[[1]][2]
    nice_name <- strsplit(nice_name, split = paste0("_", what, "_counts.txt"))[[1]][1]
    all_df[[nice_name]] <- res[[names[i]]]
  }
  return(birp_data_from_data_frame(all_df)) 
}

#---------------------------------------
# Constructors
#---------------------------------------

#'  Create a 'birp_data' Object from a Data Frame or List of Data Frames
#'
#' Constructs a 'birp_data' object from a single data frame or a list of data frames, with one per method.
#' @param data A data frame or a list of data frames, each representing one method. Each data frame must include the following columns: 'timepoint', 'location', 'counts', 'effort', and 'CI_group'. Each row represents a survey conducted at a specific timepoint and location, and for a specific control-intervention (CI) group.
#' @return An object of type \link{birp_data}
#' 
#' @export
#' @examples
#' df <- data.frame(
#'   timepoint = 1:10,
#'   location = rep(1, 10),
#'   counts = runif(10, 0, 100),
#'   effort = rexp(10),
#'   CI_group = "intervention"
#' )
#' data <- birp_data_from_data_frame(df)

birp_data_from_data_frame <- function(data){
  if (is.data.frame(data)){ data <- list(data) }
  
  .validate.birp_data(data)
  
  # Get method names
  method_names <- names(data)
  if (is.null(method_names)){
    method_names <- paste0("Method_", 1:length(data))
    names(data) <- method_names
  }
  
  # Get times and locations
  times <- unique(sort(unlist(sapply(data, function(x) x$timepoint))))
  locations <- unique(sort(unlist(sapply(data, function(x) x$location))))
  
  # Get control-intervention groups (optional)
  CI_groups <- unique(sort(unlist(sapply(data, function(x) x$CI_group))))
  if (is.null(CI_groups)){ 
    CI_groups <- "group_1"
    for (i in 1:length(data)){ data[[i]]$CI_group <- CI_groups }
  }
  
  # Get CI group per location
  tmp <- c()
  for (i in 1:length(data)){ tmp <- rbind(tmp, unique(cbind(data[[i]]$location, data[[i]]$CI_group))) }
  CI_group_per_location <- numeric(length(locations))
  for (i in 1:nrow(tmp)){ # check if each location is assigned to a single group only
    if (sum(tmp[i,1] == tmp[,1] & tmp[i,2] != tmp[,2]) > 0){ 
      stop(paste0("Location ", tmp[i,1], " is assigned to multiple control-intervention groups! Please assign a location to a single group only."))
    }
    CI_group_per_location[locations == tmp[i,1]] <- tmp[i,2]
  }
  
  # Get number of data sets (location-method-combination)
  num_data_sets <- sum(sapply(data, function(x) length(unique(x$location))))
  
  x <- list(data = data,
            method_names = method_names,
            times = times,
            locations = locations,
            CI_groups = CI_groups,
            CI_group_per_location = CI_group_per_location,
            num_data_sets = num_data_sets
            )
  class(x) <- "birp_data"
  
  return(x)
}

#' Create a birp_data Object from Count and Effort Matrices
#'
#' Constructs a 'birp_data' object from matrices of observed counts and corresponding efforts for a single method.
#' @param counts A matrix of observed counts (J locations × K timepoints). Each row corresponds to a location and each column to a timepoint.
#' @param efforts A matrix of observation effort with the same dimensions as `counts`.
#' @param times A vector of length K specifying the timepoints.
#' @param CI_groups A vector of length J specifying the control-intervention (CI) group for each location. Defaults to a single group (`group_1`) if not provided.
#' @param location_names Optional names for the locations. Defaults to `"Location_1"`, `"Location_2"`, etc.
#' @return An object of type birp_data
#' @examples 
#' data <- birp_data(c(10,20,30), c(100,200,300), c(1,2,5))
#' @export
birp_data <- function(counts, efforts, times, CI_groups = NULL, location_names = NULL){
  # Check variables
  if (any(counts < 0, na.rm = TRUE) | any(efforts < 0, na.rm = TRUE))  stop("Counts and efforts can not be negative!")
  if (any(is.na(times))) stop("NA in times are not allowed!")
  if (any(efforts == 0 & counts > 0)) stop("Counts can not be > 0 if effort = 0!")
  
  # Check dimensionality
  if (is.vector(counts)){
    counts <- matrix(counts, nrow = 1)
  }
  if (!is.matrix(counts)){
    counts <- as.matrix(counts)
  }
  if (is.vector(efforts)){
    efforts <- matrix(efforts, nrow = 1)
  }
  if (!is.matrix(efforts)){
    efforts <- as.matrix(efforts)
  }
  
  # Assign default values go CI_groups and location_names, if necessary
  if (is.null(CI_groups)){ CI_groups <- rep("group_1", nrow(counts)) }
  if (is.null(location_names)){ location_names <- paste0("Location_", 1:nrow(counts)) }
  
  if (sum(dim(counts) == dim(efforts)) != 2) stop("Counts and efforts must have the same dimensionality!")
  if (length(CI_groups) != nrow(counts)) stop("Length of CI_groups must match the number of rows of counts!")
  if (length(location_names) != nrow(counts)) stop("Length of location_names must match the number of rows of counts!")
  if (length(times) != ncol(counts)) stop("Length of times must match the number of columns of counts!")
  
  # Create data frame
  dat <- data.frame()
  for (j in 1:nrow(counts)){
    for (k in 1:ncol(counts)){
      if (is.na(counts[j,k]) | is.na(efforts[j,k])){ next; } # ignore counts or efforts with NA
      dat <- rbind(dat, c(location_names[j], times[k], CI_groups[j], counts[j,k], efforts[j,k]))
    }
  }
  names(dat) <- c("location", "timepoint", "CI_group", "counts", "effort")
  
  # call constructor of birp_data
  b <- birp_data_from_data_frame(dat)
  
  return(b)
}

#' Create a birp_data Object from File(s)
#'
#' Constructs a 'birp_data' object from one or more input files, each representing data for a different method.
#' @param filenames A character vector of file paths. Each file must contain a data frame with the columns 'timepoint', 'location', 'counts', 'effort' and 'CI_group'.
#' @param method_names Optional vector of method names corresponding to the input files. If not provided, names are inferred from the file names.
#' @param sep The field separator used in the files (default is comma).
#' @return An object of type \link{birp_data}
#' @examples
#' dir <- system.file("extdata", package = "birp")
#' filenames <- file.path(dir, "birp_Method_1_simulated_counts.txt")
#' data <- birp_data_from_file(filenames = filenames, sep = "\t")
#' @export
birp_data_from_file <- function(filenames, method_names = NA, sep = ","){
  if (!is.na(method_names) & length(method_names) != length(filenames)){
     stop("Number of method names must match the number of filenames!")
  }
  
  # Check if filenames is actually a "masterfile" containing other filenames
  if (length(filenames) == 1){
    f <- read.table(filenames[1], sep = sep, header = FALSE)
    if (ncol(f) == 1){
      filenames <- f[,1]
      tryCatch({
        f <- read.table(filenames[1], sep = sep, header = TRUE)
      }, warning = function(e){
        stop("Failed to correctly parse input file! Please make sure the correct separator ('sep') is used.")
      }, error = function(e) {
        stop("Failed to correctly parse input file! Please make sure the correct separator ('sep') is used.")
      })
    }
  }
  
  # Read files
  dat <- list()
  for (i in 1:length(filenames)){
    f <- read.table(filenames[i], sep = sep, header = TRUE)
    # Get method name from filename
    if (!is.na(method_names)){
      method_name <- method_names[i]
    } else {
      tmp <- basename(filenames[i])
      tmp <- strsplit(tmp, "\\.")[[1]]
      method_name <- paste(tmp[-length(tmp)], collapse = "_")
      if (method_name %in% names(dat)){
        stop("Method names obtained from filenames are not unique! Please provide as extra argument 'method_names'.")
      } 
    }
    # Store in list
    dat[[method_name]] <- f
  }
  
  # call constructor of birp_data
  b <- birp_data_from_data_frame(dat)
  
  return(b)
}

#' Simulate Data for BIRP Models
#' Generates simulated count data using the BIRP model framework with user-defined parameters.
#' @param timepoints Integer vector specifying time points.
#' @param timesOfChange Integer vector indicating time points at which change in growth rate (gamma) occurs.
#' @param gamma Numeric vector denoting the values of gamma to simulate. If NULL, all gamma will be set to zero
#' @param negativeBinomial Logical; if \code{TRUE}, use negative binomial instead of Poisson.
#' @param stochastic Logical; if \code{TRUE}, simulate abundance as a stochastic process instead of deterministic.
#' @param numLocations Integer; number of spatial locations.
#' @param numMethods Integer; number of sampling methods.
#' @param numCIGroups Integer; number of control–intervention groups.
#' @param numCovariatesEffort Integer; number of effort covariates.
#' @param numCovariatesDetection Integer; number of detection covariates.
#' @param BACI Optional matrix specifying BACI design (see Details).
#' @param n_bar Expected average total observations per time point (across all locations).
#' @param N_0 Optional numeric; initial abundance. If NULL, n_bar will be used instead
#' @param a A numeric value or vector; detection parameter(s) for the negative binomial distribution. Can be a single value (shared across methods) or a vector of values (one per method).
#' @param logSigma Optional numeric; log standard deviation of abundance process in the stochastic model. If NULL, logSigma will be set to -1
#' @param logPhi Optional numeric; log standard deviation of detection process in the stochastic model. If NULL, logPhi will be simulated according to the model assumptions
#' @param covariatesEffort Specifies how effort is calculated for covariates. Accepts: (1) a single number used for all covariates and locations; (2) a numeric vector with one value per covariate (applied to all locations); (3) a distribution string to simulate effort from, e.g., "gamma(a, b)" or "uniform(a, b)"; or (4) a vector of such distribution strings, one per covariate.
#' @param covariatesDetection Specifies how detection probabilities are calculated for covariates. Accepts: (1) a single number for all covariates and locations; (2) a numeric vector with one value per covariate (applied to all locations); (3) a distribution string, e.g., "normal(a, b)" or "uniform(a, b)"; or (4) a vector of such distribution strings, one per covariate.
#' @param proportionZeroEffort Proportion of time–location–method combinations with zero effort (0 to 1).
#' @param verbose Logical; if \code{TRUE}, print progress messages.
#' @return An object of type \link{birp_data} containing the simulated dataset.
#' @details
#' The `BACI` matrix defines a Before-After Control-Impact experimental design. It must be a binary matrix with two columns and one row per observation.  
#' - The first column indicates the time period (`0 = before`, `1 = after`).  
#' - The second column indicates the treatment type (`0 = control`, `1 = impact`).  
#' This allows modeling interactions between time and treatment to isolate impact effects.
#' @examples 
#' data <- simulate_birp()
#' @export
simulate_birp <- function(timepoints = c(1,2,3),
                          timesOfChange = c(),
                          gamma = NULL,
                          negativeBinomial = FALSE,
                          stochastic = FALSE,
                          numLocations = 2,
                          numMethods = 1,
                          numCIGroups = 1,
                          numCovariatesEffort = 1,
                          numCovariatesDetection = 0,
                          BACI = NULL,
                          n_bar = 1000,
                          N_0 = NULL,
                          a = NULL,
                          logSigma = NULL,
                          logPhi = NULL,
                          covariatesEffort = "gamma(1, 2)",
                          covariatesDetection = "normal(0, 1)",
                          proportionZeroEffort = 0,
                          verbose = TRUE
                          ) {
  # Create named list of function arguments 
  args <- c(as.list(environment()))
  
  # Get temporary directory where output will be written
  out <- file.path(tempdir(), "birp")
  
  # Parse options and convert to string
  options <- list(task = "simulate", out = out)
  for (i in 1:length(args)){
    if (names(args)[i] == "BACI") next # skip BACI: no command-line argument
    options <- .addToList.birp(options, names(args)[i], args[[i]])
  }
  
  # Add input BACI names
  rcpp_data <- list(x = c())
  if (!is.null(BACI)){
    options[["BACI"]] <- "BACI"
    rcpp_data <- list(BACI = BACI)
  }

  # Run simulation
  res <- .birp_interface(options, rcpp_data)
  
  # Properly format Rcpp data frames
  res <- sapply(res, function(x) {if(is.list(x)){ return(list2DF(x))}})
  
  # Assemble all counts files that were generated; return birp_data instance
  x <- .getDataAllMethods.birp_data(out, "simulated", res)
  return(x)
}

#' This function simulates a birp_data object using all parameter estimates, dimensionality (methods, locations, timepoints) and the total number of counts nu_ij of a birp object
#' @param x An object of type \code{birp}.
#' @param negativeBinomial Logical; if \code{TRUE}, simulate counts using a negative binomial distribution instead of Poisson.
#' @param stochastic Logical; if \code{TRUE}, use a stochastic model with log-normal fluctuations.
#' @param mu A numeric vector specifying values of \eqn{\mu} for the negative binomial model, with one value per method-location combination. If \code{NULL}, \eqn{\mu_i} for method \eqn{i} is set to \eqn{1 / \text{number of locations}}.
#' @param b A numeric vector specifying values of \eqn{b} for the negative binomial model (one per method). If \code{NULL}, all \eqn{b_i} are set to 1.
#' @param logSigma A single numeric value specifying \code{logSigma} for the stochastic model. If \code{NULL}, \code{logSigma} is set to -1.
#' @param logPhi A numeric vector specifying values of \code{logPhi} for the stochastic model. If \code{NULL}, values are simulated according to the model assumptions.
#' @param verbose Logical; if \code{FALSE}, suppresses console output.
#' @return An object of type \link{birp_data}
#' @examples 
#' data  <- simulate_birp()
#' x <- birp(data)
#' data2 <- simulate_birp_from_results(x)
#' @export
simulate_birp_from_results <- function(x,
                              negativeBinomial = FALSE,
                              stochastic = FALSE,
                              mu = NULL,
                              b = NULL,
                              logSigma = NULL,
                              logPhi = NULL,
                              verbose = TRUE
                              ) {
  # Check for valid arguments
  stopifnot(class(x) == "birp")
  
  # Create named list of function arguments 
  args <- c(as.list(environment()))
  
  # Get temporary directory where output will be written
  out <- file.path(tempdir(), "birp")
  
  # Parse options and convert to string
  options <- list(task = "simulate", out = out)
  for (i in 1:length(args)){
    if (names(args)[i] == "x") next # skip x: no command-line argument
    options <- .addToList.birp(options, names(args)[i], args[[i]])
  }
  
  # Add input data names
  rcpp_data <- x$data$data
  options[["data"]] <- paste(x$data$method_names, collapse = ",")
  
  # Add BACI (if needed)
  if (length(x$data$CI_groups) > 1 & length(x$times_of_change) > 0){
    options[["BACI"]] <- "BACI"
    rcpp_data$BACI <- x$BACI
  }
  
  # Add times of change
  options <- .addToList.birp(options, "timesOfChange", x$times_of_change)
  
  # Add values for all parameters that were estimated (posterior mean)
  options[["initVals"]] <- "state"
  # Make sure state does not contain unnecessary parameters
  state <- x$state
  if (!negativeBinomial){
    state <- state[state[,1] != "b",]
    state <- state[state[,1] != "N",]
    state <- state[state[,1] != "mu",]
  }
  if (!stochastic){
    state <- state[state[,1] != "logSigma",]
    state <- state[state[,1] != "logPhi",]
  }
  rcpp_data$state <- state
  
  # Run simulation
  res <- .birp_interface(options, rcpp_data)
  
  # Properly format Rcpp data frames
  res <- sapply(res, function(x) {if(is.list(x)){ return(list2DF(x))}})
  
  # Assemble all counts files that were generated; return birp_data instance
  data <- .getDataAllMethods.birp_data(out, "simulated", res)
  return(data)
}

#---------------------------------------
# Printing birp_data
#---------------------------------------

#' Print a birp_data object
#' Prints a summary of a \code{birp_data} object, including the number of methods, locations, control-intervention (CI) groups, and timepoints, as well as the names or identifiers for each.
#' 
#' @param x A \code{birp_data} object to be printed.
#' @param ... Additional arguments passed to function.
#' @return No return value; this function is called for its side effects (printing to console).
#' @examples 
#' data <- simulate_birp()
#' print(data)
#' @export
print.birp_data <- function(x, ...){
  cat("birp_data object for", length(x$method_names), "method(s),", length(x$locations), "location(s),", length(x$CI_groups), "control-intervention (CI) group(s) and", length(x$times), "time points:\n");
  cat(" - methods: [", paste(x$method_names, collapse=", "), "]\n", sep="");
  cat(" - locations: [", paste(x$locations, collapse=", "), "]\n", sep="");
  cat(" - time points: [", paste(x$times, collapse=", "), "]\n", sep="");
  cat(" - control-intervention (CI) groups: [", paste(x$CI_groups, collapse=", "), "]\n", sep="");
  cat(" - total number of data points: ", sum(sapply(x$data, nrow)),"\n", sep="");
  
  invisible(x)
}

#' Summarize a birp_data object
#'
#' Provides a printed summary of the contents of a \code{birp_data} object.
#' @param object A \code{birp_data} object to be summarized.
#' @param ... Additional arguments.
#' @return No return value; this function is called for its side effects (printing to console).
#'
#' @examples 
#' data <- simulate_birp()
#' summary(data)
#' @export
summary.birp_data <- function(object, ...){
  print.birp_data(object);
}

#---------------------------------------
# Plotting birp_data
#---------------------------------------

#' Plot a birp_data Object
#'
#' This function plots observed counts per unit of effort over time, for each method-location combination in a \code{birp_data} object.
#'
#' @param x A \code{birp_data} object to be plotted.
#' @param col A vector of colors, recycled to match the number of locations.
#' @param lwd A vector of line widths, recycled to match the number of method-location combinations.
#' @param lty A vector of line types, recycled to match the number of methods.
#' @param pch A vector of plotting characters, recycled to match the number of control-intervention (CI) groups.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param legend.x The x-position for the legend. Use \code{NA} to omit the legend.
#' @param legend.y The y-position for the legend.
#' @param legend.bty Box type for the legend; either \code{"o"} (default) or \code{"n"}.
#' @param xlim Numeric vector specifying the x-axis limits.
#' @param ylim Numeric vector specifying the y-axis limits. If \code{NA}, limits are computed automatically.
#' @param ... Additional graphical parameters passed to \code{plot()} or \code{lines()}.
#' @return No return value. Called for side effects.
#' @examples 
#' data <- simulate_birp()
#' plot(data)
#' @export
plot.birp_data <- function(x, 
                           col = 1:length(x$locations),
                           lwd = 1,
                           lty = 1:length(x$method_names),
                           pch = 1:length(x$CI_groups),
                           xlab = "time",
                           ylab = "counts per unit of effort",
                           legend.x = "topright",
                           legend.y = NULL,
                           legend.bty = "o",
                           xlim = range(as.numeric(x$times)),
                           ylim = NA,
                           ...){
  col <- rep_len(col, length(x$locations))
  lwd <- rep_len(lwd, x$num_data_sets)
  lty <- rep_len(lty, length(x$method_names))
  pch <- rep_len(pch, length(x$CI_groups))
  
  # Estimate rates: counts / efforts per method-location combination
  rates <- list()
  counter <- 1
  # Loop over all methods
  for (i in 1:length(x$data)){
    method <- x$data[[i]]
    loc <- unique(sort(method$location))
    ix_counts <- which(grepl("counts", names(method)))
    ix_effort <- which(grepl("covEffort", names(method)) | grepl("effort", names(method)))
    if (length(ix_counts) != 1) stop("Only support plotting birp_data for single-species (single column)!")
    if (length(ix_effort) != 1) stop("Only support plotting birp_data when effort is fixed (single column)!")
    # Loop over all locations for current method
    for (j in 1:length(loc)){
      # Get all data for this method-location combination
      method_loc <- method[method$location == loc[j],]
      # Only keep non-zero efforts
      keep <- method_loc[,ix_effort] > 0
      counts <- as.numeric(method_loc[keep, ix_counts])
      efforts <- as.numeric(method_loc[keep, ix_effort])
      times <- method_loc$timepoint[keep]
      # Store rate, timepoints and name
      rates[[counter]] <- list(rates = counts / efforts, times = times)
      counter <- counter + 1
    }
  }
  
  if (any(is.na(ylim))){ ylim <- range(unlist(sapply(rates, function(x) x$rates))) }
  
  # Open plot
  plot(0, type='n', xlim = xlim, xlab = xlab, ylab = ylab, ylim = ylim)
  counter <- 1
  for (i in 1:length(x$data)){
    method <- x$data[[i]]
    loc <- unique(sort(method$location))
    for (j in 1:length(loc)){
      ix_loc <- which(x$locations == loc[j])
      ix_group <- which(x$CI_group_per_location[ix_loc] == x$CI_groups)
      lines(rates[[counter]]$times, rates[[counter]]$rates, 
            col = col[ix_loc], lwd = lwd[counter], lty = lty[i], pch = pch[ix_group], type = 'b')
      counter <- counter + 1
    }
  }  
  
  # Add legend
  if(!is.na(legend.x)){
    legend(x = legend.x, y = legend.y, 
           bty = legend.bty, 
           legend = c(x$method_names, x$locations, x$CI_groups), 
           lwd = lwd, 
           lty = c(lty, rep(1, length(x$locations) + length(x$CI_groups))),
           col = c(rep("black", length(x$method_names)), col, rep("black", length(x$CI_groups))),
           pch = c(rep(1, length(x$locations) + length(x$method_names)), pch))
  }
}
