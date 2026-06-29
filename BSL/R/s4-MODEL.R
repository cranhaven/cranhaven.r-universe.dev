setClassUnion("functionOrNULL", c("function", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))

#' S4 class ``MODEL''
#' @description The S4 class contains the simulation and summary statistics
#'   function and other necessary arguments for a model to run in the main
#'   \code{bsl} function.
#' @rawRd \Rdversion{1.1}
#' @slot fnSim A function that simulates data for a given parameter value. The
#'   first argument should be the parameters. Other necessary arguments
#'   (optional) can be specified with \code{simArgs}.
#' @slot fnSimVec A vectorised function that simulates a number of datasets
#'   simultaneously for a given parameter value. If this is not \code{NULL},
#'   vectorised simulation function will be used instead of \code{fnSim}. The
#'   first two arguments should be the number of simulations to run and
#'   parameters, respectively. Other necessary arguments (optional) can be
#'   specified with \code{simArgs}. The output must be a list of each simulation
#'   result.
#' @slot fnSum A function for computing summary statistics of data. The first
#'   argument should be the observed or simulated dataset. Other necessary
#'   arguments (optional) can be specified with \code{sumArgs}. The users should
#'   code this function carefully so the output have fixed length and never
#'   contain any \code{Inf} value.
#' @slot fnLogPrior A function that computes the log of prior density for a
#'   parameter. The default is \code{NULL}, which uses an improper flat prior
#'   over the real line for each parameter. The function must have a single
#'   input: a vector of parameter values.
#' @slot simArgs A list of additional arguments to pass into the simulation
#'   function. Only use when the input \code{fnSim} or \code{fnSimVec} requires
#'   additional arguments. The default is \code{NULL}.
#' @slot sumArgs A list of additional arguments to pass into the summary
#'   statistics function. Only use when the input \code{fnSum} requires
#'   additional arguments. The default is \code{NULL}.
#' @slot theta0 Initial guess of the parameter value, which is used as the
#'   starting value for MCMC.
#' @slot thetaNames Expression, parameter names.
#' @slot ns The number of summary statistics of a single observation. Note this
#'   will be generated automatically, thus is not required for initialisation.
#' @slot test Logical, whether a short simulation test will be ran upon
#'   initialisation.
#' @slot verbose Logical, whether to print verbose messages when initialising a
#'   ``MODEL'' object.
#' @export
MODEL <- setClass("MODEL",
                  slots = c(fnSim = "functionOrNULL",
                            fnSimVec = "functionOrNULL",
                            fnSum = "functionOrNULL",
                            fnLogPrior = "functionOrNULL",
                            simArgs = "listOrNULL",
                            sumArgs = "listOrNULL",
                            theta0 = "numeric",
                            thetaNames = "expression",
                            ns = "integer",
                            test = "logical",
                            verbose = "logical")
)

#' Constructor for class ``MODEL''
#' @description \code{newModel} is the constructor function for a \code{MODEL}
#'   object.
#' @param fnSim         A function that simulates data for a given parameter
#'   value. The first argument should be the parameters. Other necessary
#'   arguments (optional) can be specified with \code{simArgs}.
#' @param fnSimVec      A vectorised function that simulates a number of
#'   datasets simultaneously for a given parameter value. The first two
#'   arguments should be the number of simulations to run and parameters,
#'   respectively. Other necessary arguments (optional) can be specified with
#'   \code{simArgs}. The output must be a list of each simulation result or a
#'   matrix with each row corresponding to a simulation.
#' @param simArgs        A list of additional arguments to pass into the simulation
#'   function. Only use when the input \code{fnSim} requires additional
#'   arguments.
#' @param fnSum         A function for computing summary statistics of data. The
#'   first argument should be the observed or simulated dataset. Other necessary
#'   arguments (optional) can be specified with \code{sumArgs}.
#' @param sumArgs        A list of additional arguments to pass into the summary
#'   statistics function. Only use when the input \code{fnSum} requires
#'   additional arguments.
#' @param fnLogPrior    A function that computes the log of prior density for a
#'   parameter. If this is missing, the prior by default is an improper flat
#'   prior over the real line for each parameter. The function must have a
#'   single input: a vector of parameter values.
#' @param theta0        Initial guess of the parameter value.
#' @param thetaNames    A string vector of parameter names, which must have the
#'   same length as the parameter vector.
#' @param test          Logical, whether a short simulation test will be ran
#'   upon initialisation.
#' @param verbose       Logical, whether to print verbose messages when
#'   initialising a ``MODEL'' object.
#' @examples
#' # set up the model for the ma2 example
#' data(ma2)
#' m <- newModel(fnSim = ma2_sim, fnSum = ma2_sum, simArgs = ma2$sim_args,
#'                   theta0 = ma2$start, fnLogPrior = ma2_prior, verbose = FALSE)
#' validObject(m)
#'
#' # benchmark the serial and vectorised simulation function (require the rbenchmark package)
#' m1 <- newModel(fnSim = ma2_sim, fnSum = ma2_sum, simArgs = ma2$sim_args,
#'             theta0 = ma2$start, fnLogPrior = ma2_prior)
#' m2 <- newModel(fnSimVec = ma2_sim_vec, fnSum = ma2_sum, simArgs = ma2$sim_args,
#'             theta0 = ma2$start, fnLogPrior = ma2_prior)
#' require("rbenchmark")
#' \dontshow{
#' benchmark(serial  = simulation(m1, n = 50, theta = c(0.6, 0.2)),
#'           vectorised  = simulation(m2, n = 50, theta = c(0.6, 0.2)))
#' }
#' \dontrun{
#' benchmark(serial  = simulation(m1, n = 1000, theta = c(0.6, 0.2)),
#'           vectorised  = simulation(m2, n = 1000, theta = c(0.6, 0.2)))
#' }
#'
#' @rdname MODEL-class
#' @export
newModel <- function(fnSim, fnSimVec, fnSum, fnLogPrior, simArgs, sumArgs, theta0, thetaNames, test = TRUE, verbose = TRUE) {
  new(Class = "MODEL", fnSim = fnSim, fnSimVec = fnSimVec, fnSum = fnSum, fnLogPrior = fnLogPrior,
      simArgs = simArgs, sumArgs = sumArgs, theta0 = theta0, thetaNames = thetaNames, test = test, verbose = verbose)
}

setMethod("initialize",
          signature = "MODEL",
          definition = function(.Object, fnSim, fnSimVec, simArgs, fnSum, sumArgs, fnLogPrior, theta0, thetaNames,
                                test = TRUE, verbose = TRUE) {
            if (verbose) cat("*** initialize \"MODEL\" ***\n")
            has.fnSim <- !missing(fnSim) || !missing(fnSimVec)
            if (verbose) cat(paste("has simulation function:", has.fnSim, "\n"))
            has.fnSum <- !missing(fnSum)
            if (verbose) cat(paste("has summary statistics function:", has.fnSum, "\n"))
            has.theta0 <- !missing(theta0)
            if (verbose) cat(paste("has initial guess / point estimate of the parameter:", has.theta0, "\n"))
            if (has.fnSim && has.fnSum && has.theta0) {
              if (!missing(fnSim)) .Object@fnSim <- fnSim
              if (!missing(fnSimVec)) .Object@fnSimVec <- fnSimVec
              if (!missing(simArgs)) .Object@simArgs <- simArgs
              .Object@fnSum <- fnSum
              if (!missing(sumArgs)) .Object@sumArgs <- sumArgs
              if (!missing(fnLogPrior)) .Object@fnLogPrior <- fnLogPrior
              if (is.null(.Object@fnLogPrior)) {
                .Object@fnLogPrior <- function(theta) 0
                if (verbose) cat("No prior has been defined in the model, use the default improper flat prior\n")
              }
              .Object@theta0 <- theta0
              .Object@test <- test
              .Object@verbose <- verbose
              validObject(.Object)
              .Object@ns <- getns(.Object)

              if (missing(thetaNames)) { # missing
                if (!is.null(names(theta0))) { # use the name of theta0
                  .Object@thetaNames <- as.expression(names(theta0))
                } else { # use default theta names
                  thetaNames <- vector("expression", length(theta0))
                  for (i in 1 : length(theta0)) {
                    thetaNames[i] <- as.expression(substitute(theta[j], list(j = i)))
                  }
                  .Object@thetaNames <- thetaNames
                }
              } else { # !missing
                if (is.null(thetaNames)) { # use default theta names
                  thetaNames <- vector("expression", length(theta0))
                  for (i in 1 : length(theta0)) {
                    thetaNames[i] <- as.expression(substitute(theta[j], list(j = i)))
                  }
                  .Object@thetaNames <- thetaNames
                } else { # has thetaNames
                  if (length(thetaNames) != length(theta0)) {
                    if (verbose) cat(paste("The length of thetaNames does not match the length of theta0,", length(theta0), "\n"))
                  } else {
                    .Object@thetaNames <- as.expression(thetaNames)
                  }
                }
              }
            } else {
              if (verbose) cat("an empty (invalid) MODEL object has been created due to one or more missing slots\n")
            }
            if (verbose) cat("*** end initialize ***\n")
            return (.Object)
          }
)

setValidity("MODEL",
            method = function(object) {
              if (is.null(object@fnSim) && is.null(object@fnSimVec)) {
                return("No available simulation function is provided")
              }

              if (object@test) {
                if (object@verbose) cat("running a short simulation test ... ")
                # test the simulation function
                if (!is.null(object@fnSimVec)) {
                  x <- try(do.call(object@fnSimVec, c(list(10, object@theta0), object@simArgs)))
                  if (inherits(x, "try-error")) {
                    return("Fail to run simulations with the given vectorised simulation function")
                  }
                  if (!(is.matrix(x) && nrow(x) == 10) && !(is.list(x) && length(x) == 10)) {
                    return("Output from the vectorised simulation function must be either a matrix
                            (each row corresponds to a simulation) or a list")
                  }
                } else {
                  x <- list()
                  for (i in 1 : 10) {
                    temp <- try(do.call(object@fnSim, c(list(object@theta0), object@simArgs)))
                    if (inherits(temp, "try-error")) {
                      return("Fail to run simulations with the given simulation function")
                    }
                    x[[i]] <- temp
                  }
                }

                # test the summary statistics function
                if (is.matrix(x)) {
                  ssx <- try(do.call(object@fnSum, c(list(x[1, ]), object@sumArgs)))
                } else {
                  ssx <- try(do.call(object@fnSum, c(list(x[[1]]), object@sumArgs)))
                }
                if (inherits(ssx, "try-error")) {
                  return("Fail to get summary statistics with the given summary statistics function")
                }
                if (!is.numeric(ssx)) {
                  return("The output of the summary statistics function must be numeric")
                }
                if (object@verbose) cat("success\n")
              }

              # prior
              if (object@fnLogPrior(object@theta0) == -Inf) {
                return("The given parameter value theta0 has no prior support\n")
              }

              # pass all checks
              TRUE
            }
)

#' Run simulations with a give "MODEL" object
#' @description see \code{\link{MODEL}}
#' @param model A ``MODEL'' object.
#' @param ... Other arguments.
setGeneric("simulation", function(model, ...) standardGeneric("simulation"))
#' @description \code{simulation} runs a number of simulations and computes the
#'   correponding summary statistics with the provided model.
#' @param model         A ``MODEL'' class object.
#' @param n The number of simulations to run.
#' @param theta The parameter value.
#' @param summStat Logical indicator whether the correpsonding summary statistics
#'   should be returned or not. The default is \code{TRUE}.
#' @param seed A seed number to pass to the \code{set.seed} function. The
#'   default is \code{NULL}, when no seed number is specified. Please note
#'   \code{parallel} also affects the result even with the same seed.
#' @inheritParams BSL-class
#' @inheritParams bsl
#' @return A list of simulation results using the given parameter. \code{x}
#'   contains the raw simulated datasets. \code{ssx} contains the summary
#'   statistics.
#' @rdname MODEL-class
#' @export
setMethod("simulation",
          signature(model = "MODEL"),
          definition = function(model, n = 1, theta = model@theta0, summStat = TRUE, parallel = FALSE, parallelArgs = NULL, seed = NULL) {
            if (!is.null(seed)) {
			  if (parallel) doRNG::registerDoRNG()
		      set.seed(seed)
			}
            flagVec <- !is.null(model@fnSimVec)
            if (flagVec && parallel) {
              parallel <- FALSE
              warning("Parallel computation is disabled for vecotised simulations")
            }
            if (!parallel & !is.null(parallelArgs)) {
              warning("\"parallelArgs\" is omitted in serial computing")
            }
            if (model@fnLogPrior(theta) == -Inf) {
              warning("The given parameter has no prior support")
            }
            if (n == 1 && parallel) {
              parallel <- FALSE
              warning("Parallel computation is disabled for n = 1")
            }

            ssx <- NULL
            if (parallel) { # parallel
              parallelArgs$.export <- c(parallelArgs$.export, "model")
			  suppressWarnings(
                x <- do.call(foreach, c(list(j = 1:n), parallelArgs)) %dopar% {
                  do.call(model@fnSim, c(list(theta), model@simArgs))
                }
			  )
              if (summStat) {
			    suppressWarnings(
                  ssx <- do.call(foreach, c(list(j = 1:n, .combine = rbind), parallelArgs)) %dopar% {
                    do.call(model@fnSum, c(x[j], model@sumArgs))
                  }
				)
              }
              if (is.atomic(x[[1]]) && is.vector(x[[1]])) { # reduce to matrix
                if (length(unique(sapply(x, FUN = length))) == 1) {
                  x <- matrix(unlist(x), ncol = length(x[[1]]), byrow = TRUE)
                }
              }

            } else { # not parallel
              if (flagVec) { # vectorised
                if (n == 1) {
                  x <- do.call(model@fnSimVec, c(list(1, theta), model@simArgs))
                  if (is.matrix(x)) {
                    x <- as.vector(x)
                    if (summStat) ssx <- do.call(model@fnSum, c(list(x), model@sumArgs))
                  } else {
                    if (summStat) ssx <- do.call(model@fnSum, c(x, model@sumArgs))
                  }
                } else {
                  x <- do.call(model@fnSimVec, c(list(n, theta), model@simArgs))
                  if (summStat) {
                    ns <- ifelse(length(model@ns) == 0, getns(model), model@ns)
                    if (is.matrix(x)) {
                      temp <- apply(x, FUN = function(y) do.call(model@fnSum, c(list(y), model@sumArgs)), MARGIN = 1)
                      ssx <- matrix(temp, nrow = n, ncol = ns, byrow = TRUE)
                    } else {
                      temp <- sapply(x, FUN = function(y) do.call(model@fnSum, c(list(y), model@sumArgs)))
                      ssx <- matrix(temp, nrow = n, ncol = ns, byrow = TRUE)
                    }
                    # if (!is.vector(ssx)) {
                    #     ssx <- t(ssx)
                    # }
                  }
                }

              } else { # serial
                if (n == 1) {
                  x <- do.call(model@fnSim, c(list(theta), model@simArgs))
                  if (summStat) ssx <- do.call(model@fnSum, c(list(x), model@sumArgs))
                } else {
                  x <- vector("list", n)
                  ns <- ifelse(length(model@ns) == 0, getns(model), model@ns)
                  if (summStat) ssx <- array(0, c(n, ns))
                  for (j in 1 : n) {
                    x[[j]] <- do.call(model@fnSim, c(list(theta), model@simArgs))
                    if (summStat) {
                      ssx[j, ] <- do.call(model@fnSum, c(x[j], model@sumArgs))
                    }
                  }
                  if (is.atomic(x[[1]]) && is.vector(x[[1]])) { # reduce to matrix
                    if (length(unique(sapply(x, FUN = length))) == 1) {
                      x <- matrix(unlist(x), ncol = length(x[[1]]), byrow = TRUE)
                    }
                  }
                }
              }
            }
            return (list(x = x, ssx = ssx))
          }
)

#' Compute the summary statistics with the given data
#' @description see \code{\link{MODEL}}
#' @param x The data to pass to the summary statistics function.
#' @param model A ``MODEL'' object.
setGeneric("summStat", function(x, model) standardGeneric("summStat"))
#' @description \code{summStat} computes the summary statistics with the given data and model object.
#' The summary statistics function and relevant arguments are obtained from the model.
#' @param x The data to pass to the summary statistics function.
#' @inheritParams BSL-class
#' @inheritParams bsl
#' @return A vector of the summary statistics.
#' @rdname MODEL-class
#' @export
setMethod("summStat",
          signature(model = "MODEL"),
          definition = function(x, model) {
            stopifnot(!is.null(x))
            do.call(model@fnSum, c(list(x), model@sumArgs))
          }
)

setGeneric("fn", function(.Object) standardGeneric("fn"))
setMethod("fn",
          signature = c(.Object = "MODEL"),
          definition = function(.Object) {
            if (!is.null(.Object@fnSimVec)) { # use vectorised simulation function
              fn <- function(n, theta) {
                x <- do.call(.Object@fnSimVec, c(list(n, theta), .Object@simArgs))
                if (is.matrix(x)) {
                  ssx <- apply(x, FUN = function(y) do.call(.Object@fnSum, c(list(y), .Object@sumArgs)), MARGIN = 1)
                } else {
                  ssx <- sapply(x, FUN = function(y) do.call(.Object@fnSum, c(list(y), .Object@sumArgs)))
                }
                if (is.vector(ssx)) {
                  return(as.matrix(ssx))
                }
                return(t(ssx))
              }
              fnPar <- NULL
            } else { # non-vectorised simulation function
              fnPar <- function(n, theta, parallelArgs = list()) {
                j <- NULL
                parallelArgs$.export <- c(parallelArgs$.export, ".Object")
                do.call(foreach, c(list(j = 1:n, .combine = rbind), parallelArgs)) %dopar% {
                  x <- do.call(.Object@fnSim, c(list(theta), .Object@simArgs))
                  do.call(.Object@fnSum, c(list(x), .Object@sumArgs))
                }
              }

              fn <- function(n, theta) {
                ns <- ifelse(length(.Object@ns) == 0, getns(.Object), .Object@ns)
                ssx <- array(0, c(n, ns))
                for (j in 1:n) {
                  x <- do.call(.Object@fnSim, c(list(theta), .Object@simArgs))
                  ssx[j, ] <- do.call(.Object@fnSum, c(list(x), .Object@sumArgs))
                }
                ssx
              }
            }
            return(list(fn = fn, fnPar = fnPar))
          }
)

setGeneric("getns", valueClass = "integer", function(model) standardGeneric("getns"))
setMethod("getns",
          signature = c(model = "MODEL"),
          definition = function(model) {
            if (!is.null(model@fnSimVec)) {
              x <- do.call(model@fnSimVec, c(list(2, model@theta0), model@simArgs))
            } else {
              x <- list()
              x[[1]] <- do.call(model@fnSim, c(list(model@theta0), model@simArgs))
            }
            if (is.matrix(x)) {
              y <- x[1, ]
            } else {
              y <- x[[1]]
            }
            ns <- length(do.call(model@fnSum, c(list(y), model@sumArgs)))
            return(as.integer(ns))
          }
)
