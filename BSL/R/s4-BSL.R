#' @include s4-MODEL.R
NULL

setOldClass("difftime")
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("logicalOrMatrixOrNULL", c("logical", "matrix", "NULL"))
# setClassUnion("listOrNULL", c("list", "NULL")) # defined in s4-MODEL.R
# setClassUnion("functionOrNULL", c("function", "NULL")) # defined in s4-MODEL.R

#' S4 class ``BSL''.
#' @description The S4 class ``BSL'' is produced by running function
#'   \code{\link{bsl}} and contains the result of a BSL run. Basic S4 methods
#'   \code{show}, \code{summary} and \code{plot} are provided. \code{theta} and
#'   \code{loglike} returns the MCMC samples of parameter values and estimated
#'   log-likelihoods.
#' @rawRd \Rdversion{1.1}
#' @slot theta Object of class ``matrix''. MCMC samples from the joint
#'   approximate posterior distribution of the parameters.
#' @slot loglike Object of class ``numeric''. Accepted MCMC samples of the
#'   estimated log-likelihood values.
#' @slot call Object of class ``call''. The original code that was used to call
#'   the method.
#' @slot model Object of class ``MODEL''.
#' @slot acceptanceRate Object of class ``numeric''. The acceptance rate of the
#'   MCMC algorithm.
#' @slot earlyRejectionRate Object of class ``numeric''. The early rejection
#'   rate of the algorithm (early rejection may occur when using bounded prior
#'   distributions).
#' @slot errorRate Object of class ``numeric''. The error rate. If any infinite
#'   summary statistic or infinite log-likelihood estimate occurs during the
#'   process, it is marked as an error and the proposed parameter will be
#'   rejected.
#' @slot y Object of class ``ANY''. The observed data.
#' @slot n Object of class ``numeric''. The number of simulations from the model
#'   per MCMC iteration to estimate the synthetic likelihood.
#' @slot M Object of class ``numeric''. The number of MCMC iterations.
#' @slot covRandWalk Object of class ``matrix''. The covariance matrix used in
#'   multivariate normal random walk proposals.
#' @slot method Object of class ``character''. The character argument indicating
#'   the used method.
#' @slot shrinkage Object of class ``characterOrNULL''. The character argument
#'   indicating the shrinkage method.
#' @slot penalty Object of class ``numericOrNULL''. The penalty value.
#' @slot GRC Object of class ``logical''. Whether the Gaussian rank correlation
#'   matrix is used.
#' @slot logitTransform Object of class ``logical''. The logical argument
#'   indicating whether a logit transformation is used in the algorithm.
#' @slot logitTransformBound Object of class ``matrixOrNULL''. The matrix of
#'   logitTransformBound.
#' @slot standardise Object of class ``logical''. The logical argument that
#'   determines whether to standardise the summary statistics.
#' @slot parallel Object of class ``logical''. The logical value indicating
#'   whether parallel computing is used in the process.
#' @slot parallelArgs Object of class ``listOrNULL''. The list of additional
#'   arguments to pass into the \code{foreach} function.
#' @slot time Object of class ``difftime''. The running time.
#' @slot gamma Object of class ``numeric''. MCMC samples of gamma parameter
#'   values of the mean adjustment or variance inflation for method
#'   ``BSLmisspec''.
#' @slot misspecType Object of class ``characterOrNULL''. The character argument
#'   indicating whether mean adjustment ("mean") or variance inflation
#'   ("variance") to be used in "BSLmisspec" method.
#' @slot tau Object of class ``numeric''. Parameter of the prior distribution
#'   for "BSLmisspec" method. For mean adjustment, \code{tau} is the scale of
#'   the Laplace distribution. For variance inflation, \code{tau} is the mean of
#'   the exponential distribution.
#' @slot whitening Object of class ``logicalOrMatrixOrNULL''. A logical argument
#'   determines whether Whitening transformation is used in ``BSL'' method with
#'   Warton's shrinkage, or just the Whitening matrix used.
#'
#' @examples
#' \dontshow{
#' # a toy example
#' toy_simVec <- function(n, theta) matrix(rnorm(n, theta), nrow = n) # the simulation function
#' toy_sum <- function(x) x # the summary statistic function
#' model <- newModel(fnSimVec = toy_simVec, fnSum = toy_sum, theta0 = 0) # create the model object
#' result_toy <- bsl(y = 1, n = 50, M = 100, model = model, covRandWalk = matrix(1), verbose = 0)
#' summary(result_toy)
#' plot(result_toy)
#' }
#' \dontrun{
#' # a toy example
#' toy_simVec <- function(n, theta) matrix(rnorm(n, theta), nrow = n) # the simulation function
#' toy_sum <- function(x) x # the summary statistic function
#' model <- newModel(fnSimVec = toy_simVec, fnSum = toy_sum, theta0 = 0) # create the model object
#' result_toy <- bsl(y = 1, n = 100, M = 1e4, model = model, covRandWalk = matrix(1))
#' summary(result_toy)
#' plot(result_toy)
#' }
#'
#' @aliases BSLclass
#' @export
setClass("BSL",
         slots = c(theta = "matrix",
                   loglike = "numeric",
                   model = "MODEL",
                   acceptanceRate = "numeric",
                   earlyRejectionRate = "numeric",
                   errorRate = "numeric",
                   y = "ANY",
                   n = "numeric",
                   M = "numeric",
                   covRandWalk = "matrix",
                   method = "character",
                   shrinkage = "characterOrNULL",
                   penalty = "numericOrNULL",
                   standardise = "logical",
                   GRC = "logical",
                   gamma = "matrix",
                   misspecType = "characterOrNULL",
                   tau = "numeric",
                   whitening = "logicalOrMatrixOrNULL",
                   logitTransform = "logical",
                   logitTransformBound = "matrixOrNULL",
                   parallel = "logical",
                   parallelArgs = "listOrNULL",
                   time = "difftime",
                   call = "call")
)

setValidity("BSL",
            method = function(object) {
                if (any(length(object@theta) == 0, length(object@M) == 0)) { # slots that must include in bsl class
                    warnings('empty slot "theta" or "M" in the "bsl" object')
                } else {
                    errors <- character()
                    p <- ncol(object@theta)
                    M <- nrow(object@theta)
                    if (M != object@M) {
                        msg <- paste('The number of rows of theta', M, 'does not match the number of iterations M', object@M)
                        error <- c(errors, msg)
                    }
                    
                    temp <- length(object@loglike)
                    if (temp != 0 && temp != M) {
                        msg <- paste('The number of iterations M', M, 'does not match the length of loglike', temp)
                        error <- c(errors, msg)
                    }
                    
                    temp <- class(object@model)
                    if (temp != 'MODEL') {
                        msg <- paste('model must be a MODEL class object')
                        error <- c(errors, msg)
                    } else {
                        invisible(capture.output(validObject(object@model)))
                    }
                    
                    if (nrow(object@covRandWalk) != p || ncol(object@covRandWalk) != p) {
                        msg <- paste('covRandWalk must be a', p, 'by', p, 'square matrix')
                        error <- c(errors, msg)
                    }
                    
                    if (!is.null(object@logitTransformBound)) {
                        if (nrow(object@logitTransformBound) != p || ncol(object@logitTransformBound) != 2L) {
                            msg <- paste('logitTransformBound must be a', p, 'by', 2, 'matrix')
                            error <- c(errors, msg)
                        }
                    }
                    
                    if (length(errors) == 0) {
                        return (TRUE)
                    } else {
                        return (errors)
                    }
                }
            }
)

#' @param object   A ``BSL'' class object.
#' @rdname BSL-class
#' @export
setMethod("show",
          signature = c(object = "BSL"),
          definition = function(object) {
              digits = max(3L, getOption("digits") - 3L)
              cat("\nCall:\n", paste(deparse(object@call), sep = "\n", collapse = "\n"),
                  "\n\n", sep = "")
              if (nrow(object@theta)) {
                  cat("Summary of theta:\n")
                  summ <- summary(object@theta)
                  attr(summ, 'dimnames') = list(NULL, object@model@thetaNames)
                  print.default(format(summ, digits = digits), print.gap = 2L,
                                quote = FALSE)
              }
              else cat("No theta\n")
              if (length(object@loglike)) {
                  cat("Summary of loglikelihood:\n")
                  summ <- summary(object@loglike)
                  print.default(format(summ, digits = digits), print.gap = 2L,
                                quote = FALSE)
              }
              else cat("No loglikelihood\n")
              if (length(object@acceptanceRate)) {
                  cat("Acceptance Rate:\n")
                  print.default(format(object@acceptanceRate, digits = digits), print.gap = 2L,
                                quote = FALSE)
              }
              else cat("No acceptance rate\n")
              if (length(object@earlyRejectionRate)) {
                  cat("Early Rejection Rate:\n")
                  print.default(format(object@earlyRejectionRate, digits = digits), print.gap = 2L,
                                quote = FALSE)
              }
              else cat("No early rejection rate\n")
              cat("\n")
          }
)

#' @param object   A ``BSL'' class object.
#' @param burnin the number of MCMC burn-in steps to be taken.
#' @param thetaNames Parameter names to be shown in the summary table. If not
#'   given, parameter names of the ``BSL'' object will be used by default.
#' @rdname BSL-class
#' @export
setMethod("summary",
          signature = c(object = "BSL"),
          definition = function(object, burnin = 0, thetaNames = NULL) {
              theta <- getTheta(object, burnin = burnin)
              #theta <- as.matrix(object@theta[(burnin + 1) : nrow(object@theta), ])
              n <- object@n
              M <- nrow(theta)
              p <- ncol(theta)
              if (is.null(thetaNames)) {
                  if (!is.null(object@model@thetaNames)) {
                      thetaNames <- object@model@thetaNames
                  } else {
                      thetaNames <- vector('expression', p)
                      for (i in 1:p) {
                          thetaNames[i] <- as.expression(substitute(theta[j],list(j=i)))
                      }
                  }
              }
              if (length(object@model@thetaNames) != p) {
                  warning('length of "thetaNames" does not match number of parameters\n')
              }
              accRate <- round(mean(diff(theta[, 1]) !=0), 2)
              ess <- round(effectiveSize(theta), 0)
              # ess <- round(effectiveSize(theta) / n / M * 1000000, 0)
              summ <- c(n, accRate*100, ess)
              names(summ) <- c('n', 'acc. rate (%)', paste('ESS', thetaNames))
              return(summ)
          }
)

#' @param x           A ``BSL'' class object to plot.
#' @param which       An integer argument indicating which plot function to be
#'   used. The default, \code{1L}, uses the plain \code{plot} to visualise the
#'   result. \code{2L} uses ggplot2 to draw the plot.
#' @param thin        A numeric argument indicating the gap between samples to
#'   be taken when thinning the MCMC draws. The default is \code{1}, which means
#'   no thinning is used.
#' @param thetaTrue   A set of true parameter values to be included on the plots
#'   as a reference line. The default is \code{NULL}.
#' @param options.plot  A list of additional arguments to pass into the
#'   \code{plot} function. Only use when \code{which} is \code{1L}.
#' @param top         A character argument of the combined plot title if
#'   \code{which} is \code{2L}.
#' @param options.density  A list of additional arguments to pass into the
#'   \code{geom_density} function. Only use when \code{which} is \code{2L}.
#' @param options.theme  A list of additional arguments to pass into the
#'   \code{theme} function. Only use when \code{which} is \code{2L}.
#' @rdname BSL-class
#' @export
setMethod("plot",
          signature = c(x = "BSL"),
          definition = function(x, which = 1L, thin = 1, burnin = 0, thetaTrue = NULL, options.plot = NULL,
                                top = 'Approximate Univariate Posteriors', options.density = list(), options.theme = list()) {
              if (which == 1L) {
                  if (length(options.density) != 0 || length(options.theme) != 0) {
                      warning('"options.density" and "options.theme" are ignored when which = 1')
                  }
                  marginalPostDefault(x, thin, burnin, thetaTrue, options.plot)
              } else if (which == 2L) {
                  if (!is.null(options.plot)) {
                      warning('"options.plot" is ignored when which = 2')
                  }
                  marginalPostGgplot(x, thin, burnin, thetaTrue, top, options.density, options.theme)
              } else {
                  stop('Indicate a supported plot number, 1 for R default density plot or 2 for ggplot density plot')
              }
          }
)

# Plot the univariate marginal posterior plot of a bsl class object using the R
# default plot function.
marginalPostDefault <- function(x, thin = 1, burnin = 0, thetaTrue = NULL, options.plot = NULL) {
    theta <- getTheta(x, burnin = burnin, thin = thin)
    n <- nrow(theta)
    p <- ncol(theta)
    a <- floor(sqrt(p))
    b <- ceiling(p / a)
    if (!is.null(thetaTrue) & length(thetaTrue) != p) {
        stop('Length of thetaTrue does not match the number of parameters.')
    }
    thetaNames <- x@model@thetaNames
    oldpar <- par(no.readonly = TRUE)    # get current user par settings
    on.exit(par(oldpar))            # reset current user par settings at the end of the function
    par(mfrow = c(a, b))
    for(k in 1:p) {
        d <- density(theta[, k])
        if ('main' %in% names(options.plot)) {
            do.call(plot, c(list(d, xlab = thetaNames[k]), options.plot))
        } else {
            do.call(plot, c(list(d, xlab = thetaNames[k], main = NA), options.plot))
        }
        if (!is.null(thetaTrue)) {
            abline(v = thetaTrue[k], col = 'forestgreen', lty = 3)
        }
    }
}

# Plot the univariate marginal posterior plot of a bsl class object using the
# ggplot2 package.
marginalPostGgplot <- function(x, thin = 1, burnin = 0, thetaTrue = NULL, top = 'Approximate Univariate Posteriors', options.density = list(), options.theme = list()) {
    theta <- getTheta(x, burnin = burnin, thin = thin)
    n <- nrow(theta)
    p <- ncol(theta)
    a <- floor(sqrt(p))
    b <- ceiling(p / a)
    if (!is.null(thetaTrue) & length(thetaTrue) != p) {
        stop('Length of thetaTrue does not match the number of parameters.')
    }
    samples <- data.frame(theta)
    thetaNames <- x@model@thetaNames
    plist <- list()
    for (i in 1 : p) {
        plist[[i]] <- ggplot(samples, aes_string(x = colnames(samples)[i])) +
            do.call(geom_density, options.density) +
            geom_hline(yintercept = 0, colour = "grey", size = 0.75) + {
                if (!is.null(thetaTrue)) {
                    geom_vline(xintercept = thetaTrue[i], color = 'forestgreen', linetype = 'dashed', size = 0.5)
                }
            } +
            labs(x = thetaNames[i], y = 'density') +
            do.call(theme, options.theme)
    }
    do.call('grid.arrange', c(plist, nrow = a, ncol = b, top = top))
}

#' Obtain the samples from a "BSL" object
#' @description see \code{\link{BSLclass}}
#' @param object   A ``BSL'' class object.
#' @param ... Other arguments.
#' @return The matrix of samples, after removing burn-in and thinning.
setGeneric("getTheta", function(object, ...) standardGeneric("getTheta"))
#' @rdname BSL-class
#' @export
setMethod("getTheta",
          signature = c(object = "BSL"),
          definition = function(object, burnin = 0, thin = 1) {
              as.matrix(object@theta[seq((burnin + 1), nrow(object@theta), by = thin), ])
          }
)

#' Obtain the log-likelihoods from a "BSL" object
#' @description see \code{\link{BSLclass}}
#' @param object   A ``BSL'' class object.
#' @param ... Other arguments.
#' @return The vector of log likelihood evaluations, after removing burn-in and thinning.
setGeneric("getLoglike", function(object, ...) standardGeneric("getLoglike"))
#' @rdname BSL-class
#' @export
setMethod("getLoglike",
          signature = c(object = "BSL"),
          definition = function(object, burnin = 0, thin = 1) {
              object@loglike[seq((burnin + 1), length(object@loglike), by = thin)]
          }
)

#' Obtain the gamma samples (the latent parameters for BSLmisspec method) from a
#' "BSL" object
#' @description see \code{\link{BSLclass}}
#' @param object   A ``BSL'' class object.
#' @param ... Other arguments.
#' @return The matrix of gamma samples (the latent parameters for BSLmisspec
#' method), after removing burn-in and thinning.
setGeneric("getGamma", function(object, ...) standardGeneric("getGamma"))
#' @rdname BSL-class
#' @export
setMethod("getGamma",
          signature = c(object = "BSL"),
          definition = function(object, burnin = 0, thin = 1) {
              as.matrix(object@gamma[seq((burnin + 1), nrow(object@gamma), by = thin), ])
          }
)
