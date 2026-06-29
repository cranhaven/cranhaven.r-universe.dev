#' @include s4-BSL.R
NULL

#' Performing BSL, uBSL, semiBSL and BSLmisspec
#'
#' @description This is the main function for performing MCMC BSL (with a
#'   standard or non-standard likelihood estimator) to sample from the
#'   approximate posterior distribution. A couple of extentions to the standard
#'   approach are available by changing the following arguments, \code{method},
#'   \code{shrinkage}, \code{whitening}, \code{misspecType}. Parallel computing
#'   is supported with the R package \code{foreach}.
#'
#' @param y				The observed data. Note this should be the raw dataset NOT the
#'   set of summary statistics.
#' @param n				The number of simulations from the model per MCMC iteration for
#'   estimating the synthetic likelihood.
#' @param M				The number of MCMC iterations.
#' @param model         A ``MODEL'' object generated with function
#'   \code{newModel}. See \code{\link{newModel}}.
#' @param covRandWalk	The covariance matrix of a multivariate normal random walk
#'   proposal distribution used in the MCMC.
#' @param method        A string argument indicating the method to be used. The
#'   default, ``BSL'', runs standard BSL. ``uBSL'' uses the unbiased estimator
#'   of a normal density of \insertCite{Ghurye1969;textual}{BSL}. ``semiBSL''
#'   runs the semi-parametric BSL algorithm and is more robust to non-normal
#'   summary statistics. ``BSLmisspec'' estimates the Gaussian synthetic
#'   likelihood whilst acknowledging that there may be incompatibility between
#'   the model and the observed summary statistic \insertCite{Frazier2019}{BSL}.
#' @param shrinkage     A string argument indicating which shrinkage method to
#'   be used. The default is \code{NULL}, which means no shrinkage is used.
#'   Shrinkage estimation is only available for methods ``BSL'' and ``semiBSL''.
#'   Current options are ``glasso'' for the graphical lasso method of
#'   \insertCite{Friedman2008;textual}{BSL} and ``Warton'' for the ridge
#'   regularisation method of \insertCite{Warton2008;textual}{BSL}.
#' @param penalty		The penalty value to be used for the specified shrinkage
#'   method. Must be between zero and one if the shrinkage method is ``Warton''.
#'
#' @param logitTransformBound A \eqn{p} by \eqn{2} numeric matrix indicating the
#'   upper and lower bounds of parameters if a logit transformation is used on
#'   the parameter space, where \eqn{p} is the number of parameters. The default
#'   is \code{NULL}, which means no logit transformation is used. It is also
#'   possible to define other transformations within the simulation and prior
#'   function from \code{model}. The first column contains the lower bound of
#'   each parameter and the second column contains the upper bound. Infinite
#'   lower or upper bounds are also supported, eg.
#'   \code{matrix(c(1,Inf,0,10,-Inf,0.5),3,2,byrow=TRUE)}.
#' @param standardise	A logical argument that determines whether to standardise
#'   the summary statistics before applying the graphical lasso. This is only
#'   valid if method is ``BSL'', shrinkage is ``glasso'' and penalty is not
#'   \code{NULL}. The diagonal elements will not be penalised if the shrinkage
#'   method is ``glasso''. The default is \code{FALSE}.
#' @param GRC           A logical argument indicating whether the Gaussian rank
#'   correlation matrix \insertCite{Boudt2012}{BSL} should be used to estimate
#'   the covariance matrix in ``BSL'' method. The default is \code{FALSE}, which
#'   uses the sample covariance by default.
#' @param whitening     This argument determines whether Whitening transformation
#'   should be used in ``BSL'' method with Warton's shrinkage. Whitening
#'   transformation helps decorrelate the summary statistics, thus encouraging
#'   sparsity of the synthetic likelihood covariance matrix. This might allow
#'   heavier shrinkage to be applied without losing much accuracy, hence
#'   allowing the number of simulations to be reduced. By default, \code{NULL}
#'   represents no Whitening transformation. Otherwise this is enabled if a
#'   Whitening matrix is provided. See \code{\link{estimateWhiteningMatrix}} for
#'   the function to estimate the Whitening matrix.
#' @param misspecType   A string argument indicating which type of model
#'   misspecification to be used. The two options are "mean" and "variance".
#'   Only used when method is ``BSLmisspec''. The default, \code{NULL}, means no
#'   model misspecification is considered.
#' @param tau           A numeric argument, parameter of the prior distribution
#'   for "BSLmisspec" method. For mean adjustment, \code{tau} is the scale of
#'   the Laplace distribution. For variance inflation, \code{tau} is the mean of
#'   the exponential distribution. Only used when method is ``BSLmisspec''.
#' @param parallel		A logical value indicating whether parallel computing should
#'   be used for simulation and summary statistic evaluation. The default is
#'   \code{FALSE}. When model simulation is fast, it may be preferable to
#'   perform serial or vectorised computations to avoid significant
#'   communication overhead between workers. Parallel computation can only be
#'   used if not using a vectorised simulation function, see \code{\link{MODEL}}
#'   for options of vectorised simulation function.
#' @param parallelArgs	A list of additional arguments to pass into the
#'   \code{foreach} function. Only used when parallel computing is enabled,
#'   default is \code{NULL}.
#' @param plotOnTheFly  A logical or numeric argument defining whether or by how
#'   many iterations a posterior figure will be plotted during running. If
#'   \code{TRUE}, a plot of approximate univariate posteriors based on the
#'   current accepted samples will be shown every one thousand iterations.
#'   The default is \code{FALSE}.
#' @param verbose               An integer indicating the verbose style. 0L
#'   means no verbose messages will be printed. 1L uses a custom progress bar to
#'   track the progress. 2L prints the iteration numbers (\code{1:M}) to track
#'   the progress. The default is 1L.
#'
#' @param theta0		Deprecated, will be removed in the future, use \code{model}
#'   instead. Initial guess of the parameter value, which is used as the
#'   starting value for MCMC.
#' @param fnSim         Deprecated, will be removed in the future, use
#'   \code{model} instead. A function that simulates data for a given parameter
#'   value. The first argument should be the parameters. Other necessary
#'   arguments (optional) can be specified with \code{simArgs}.
#' @param fnSum         Deprecated, will be removed in the future, use
#'   \code{model} instead. A function for computing summary statistics of data.
#'   The first argument should be the observed or simulated dataset. Other
#'   necessary arguments (optional) can be specified with \code{sumArgs}.
#' @param fnPrior       Deprecated, will be removed in the future, use
#'   \code{model} instead. A function that computes the log prior density for a
#'   parameter. The default is \code{NULL}, which uses an improper flat prior
#'   over the real line for each parameter. The function must have a single
#'   input: a vector of parameter values.
#' @param simArgs	    Deprecated, will be removed in the future, use
#'   \code{model} instead. A list of additional arguments to pass into the
#'   simulation function. Only use when the input \code{fnSim} requires
#'   additional arguments. The default is \code{NULL}.
#' @param sumArgs	    Deprecated, will be removed in the future, use
#'   \code{model} instead. A list of additional arguments to pass into the
#'   summary statistics function. Only use when the input \code{fnSum} requires
#'   additional arguments. The default is \code{NULL}.
#' @param thetaNames	Deprecated, will be removed in the future, use \code{model}
#'   instead. A string vector of parameter names, which must have the same
#'   length as the parameter vector. The default is \code{NULL}.
#'
#' @return 				An object of class \code{bsl} is returned, see \code{\link{BSL}}
#'   for more information of the S4 class.
#'
#' @references
#'
#' \insertAllCited{}
#'
#' \insertRef{Price2018}{BSL}
#'
#' \insertRef{An2019}{BSL}
#'
#' \insertRef{An2018}{BSL}
#'
#' @examples
#' \dontshow{
#' toy_sim <- function(n, theta) matrix(rnorm(n, theta), nrow = n)
#' toy_sum <- function(x) x
#'
#' model <- newModel(fnSimVec = toy_sim, fnSum = toy_sum, theta0 = 0)
#'
#' result_toy <- bsl(y = 1, n = 10, M = 100, model = model, covRandWalk = matrix(1),
#'     method = "BSL", verbose = FALSE)
#' summary(result_toy)
#' plot(result_toy)
#' }
#' \dontrun{
#' # This is just a minimal test run, please see package built-in examples for more
#' # comprehensive usages of the function
#' toy_sim <- function(n, theta) matrix(rnorm(n, theta), nrow = n)
#' toy_sum <- function(x) x
#' model <- newModel(fnSimVec = toy_sim, fnSum = toy_sum, theta0 = 0)
#'
#' result_toy <- bsl(y = 1, n = 100, M = 1e4, model = model, covRandWalk = matrix(1),
#'     method = "BSL", plotOnTheFly = TRUE)
#' summary(result_toy)
#' plot(result_toy)
#' }
#'
#' @author    Ziwen An, Leah F. South and Christopher Drovandi
#' @seealso   \code{\link{ma2}}, \code{\link{cell}}, \code{\link{mgnk}} and
#'   \code{\link{toad}} for examples. \code{\link{selectPenalty}} for a function
#'   to tune the BSLasso tuning parameter and \code{\link{plot}} for functions
#'   related to visualisation.
#' @export
bsl <- function(y, n, M, model, covRandWalk, theta0, fnSim, fnSum, method = c("BSL", "uBSL",
                                                                              "semiBSL", "BSLmisspec"), shrinkage = NULL, penalty = NULL, fnPrior = NULL, simArgs = NULL,
                sumArgs = NULL, logitTransformBound = NULL, standardise = FALSE, GRC = FALSE, whitening = NULL,
                misspecType = NULL, tau = 1, parallel = FALSE, parallelArgs = NULL,
                thetaNames = NULL, plotOnTheFly = FALSE, verbose = 1L) {
  
  method <- match.arg(method)
  if (is.null(misspecType)) {
    flagType <- FALSE
  } else {
    flagType <- TRUE
    misspecType <- match.arg(misspecType, c("mean", "variance"))
  }
  if (method != "BSLmisspec" && flagType) {
    warning("\"misspecType\" will be ignored because method is not \"BSLmisspec\"")
  }
  if (method == "BSLmisspec" && !flagType) {
    stop("\"misspecType\" must be provided to enable \"BSLmisspec\" method")
  }
  if (!parallel & !is.null(parallelArgs)) {
    warning("\"parallelArgs\" is omitted in serial computing")
  }
  if (!is.null(shrinkage)) {
    flagShrinkage <- TRUE
    shrinkage <- match.arg(shrinkage, c("glasso", "Warton"))
  } else {
    flagShrinkage <- FALSE
  }
  if (!flagShrinkage && !is.null(penalty)) {
    warning("\"penalty\" will be ignored because no shrinkage method is specified")
  }
  if (flagShrinkage && is.null(penalty)) {
    stop("a penalty value must be specified to enable shrinkage estimation")
  }
  if (!flagShrinkage && standardise) {
    warning("\"standardise\" will be ignored because shrinkage method is not \"glasso\"")
  }
  
  # deprecated arguments
  if (!missing(theta0)) {
    warning("theta0 will be deprecated in the future, use model instead, see \"?model\"")
  }
  if (!missing(fnSim)) {
    warning("fnSim will be deprecated in the future, use model instead, see \"?model\"")
  }
  if (!missing(fnSum)) {
    warning("fnSum will be deprecated in the future, use model instead, see \"?model\"")
  }
  if (!is.null(simArgs)) {
    warning("simArgs will be deprecated in the future, use model instead, see \"?model\"")
  }
  if (!is.null(sumArgs)) {
    warning("sumArgs will be deprecated in the future, use model instead, see \"?model\"")
  }
  if (!is.null(fnPrior)) {
    warning("fnPrior will be deprecated in the future, use model instead, see \"?model\"")
  }
  if (!is.null(thetaNames)) {
    warning("thetaNames will be deprecated in the future, use model instead, see \"?model\"")
  }
  
  if (missing(model)) {
    if (is.null(fnPrior)) {
      fnLogPrior <- NULL
    } else {
      fnLogPrior <- function(...) log(fnPrior(...))
    }
    model <- model(fnSim = fnSim, fnSum = fnSum, simArgs = simArgs, sumArgs = sumArgs,
                   fnLogPrior = fnLogPrior, theta0 = theta0, thetaNames = thetaNames)
  } else {
    stopifnot(inherits(model, "MODEL"))
  }
  
  ns <- model@ns
  if (method == "semiBSL" && ns < 2) {
    stop("The dimension of summary statistic must be at least 2 to use method \"semiBSL\"")
  }
  if (is.null(whitening)) {
    flagWhitening <- FALSE
    ssyTilde <- NULL
  } else if (is.atomic(whitening) & is.matrix(whitening)) {
    if (all(dim(whitening) == c(ns, ns))) {
      flagWhitening <- TRUE
    } else {
      stop(paste("The Whitening matrix must be of dimension", ns, "by", ns))
    }
  } else if (is.atomic(whitening) & length(whitening) == 1) {
    flagWhitening <- as.logical(whitening)
    if (flagWhitening) {
      if (verbose) cat("estimating the Whitening matrix ... ")
      whitening <- estimateWhiteningMatrix(n = 1e3, model = model)
      if (verbose) cat("finish\n")
    } else {
      whitening <- ssyTilde <- NULL
    }
  } else {
    stop("invalid argument \"whitening\"")
  }
  
  if (!flagShrinkage && flagWhitening) {
    warning("\"whitening\" will be ignored because shrinkage method is not \"Warton\"")
  }
  if (flagShrinkage) {
    if (shrinkage != "glasso" && standardise) {
      warning("standardisation is only supported if shrinkage is \"glasso\"")
    }
    if (shrinkage != "Warton" && flagWhitening) {
      warning("Whitening is only supported if shrinkage is \"Warton\"")
    }
  }
  
  if (verbose) {
    if (flagType) typeText <- switch(misspecType,
                                     "mean"     = "mean-adjusted",
                                     "variance" = "variance-inflated")
    methodText <- switch(method,
                         "BSL"        = "standard BSL",
                         "uBSL"       = "unbiased BSL",
                         "semiBSL"    = "semi-BSL",
                         "BSLmisspec" = paste("BSL with", typeText, "model misspecification")
    )
    shrinkageText <- paste("shrinkage:", ifelse(flagShrinkage, shrinkage, "disabled"))
    whiteningText <- paste("whitening:", ifelse(flagWhitening, "enabled", "disabled"))
    cat("*** running ", methodText, ", ", shrinkageText, ", ", whiteningText, " *** \n", sep = "")
    # cat(shrinkageText, ", ", whiteningText, "\n", sep = "")
  }
  
  p <- length(model@theta0)
  fnLogPrior <- model@fnLogPrior
  logitTransform <- !is.null(logitTransformBound)
  if (logitTransform) {
    if (any(dim(logitTransformBound) != c(p, 2))) {
      stop("\"logitTransformBound\" must be a p by 2 matrix, where p is the length of parameter")
    }
  }
  
  cl <- match.call()
  startTime <- Sys.time()
  
  # initialise parameters
  ssy <- do.call(model@fnSum, c(list(y), model@sumArgs))
  stopifnot(length(ssy) == ns)
  thetaCurr <- model@theta0
  loglikeCurr <- Inf
  if (logitTransform) {
    thetaTildeCurr <- paraLogitTransform(thetaCurr, logitTransformBound)
  }
  theta <- array(0, c(M, p), dimnames = list(NULL, thetaNames))
  loglike <- numeric(M) # ignore if method is not "BSLmisspec"
  gamma <- array(0, c(M, ns))
  countAcc <- countEar <- countErr <- 0
  
  if (flagWhitening) {
    ssyTilde <- c(tcrossprod(ssy, whitening))
  }
  if (method == "BSLmisspec") {
    gammaCurr <- switch(misspecType,
                        mean = numeric(ns),
                        variance = rep(tau, ns))
  }
  
  # map the simulation function
  if (parallel) {
    myFnSimSum <- function(n, theta) fn(model)$fnPar(n, theta, parallelArgs)
  } else {
    myFnSimSum <- fn(model)$fn
  }
  
  # plot-on-the-fly
  if (plotOnTheFly) {
    if (plotOnTheFly == 1) {
      plotOnTheFly  <- 1000
    }
    oldpar <- par(no.readonly = TRUE)    # get current user par settings
    on.exit(par(oldpar))            # reset current user par settings at the end of the function
    a <- floor(sqrt(p))
    b <- ceiling(p/a)
    par(mfrow = c(a, b))
  }
  
  # if (verbose) cat("initialising parameters ... ")
  while (is.infinite(loglikeCurr)) {
    # simulate with thetaProp and calculate the summary statistics
    ssx <- myFnSimSum(n, thetaCurr)
    if (any(is.infinite(ssx))) {
      stop("Inf detected in the summary statistics vector, this will cause an error in likelihood evaluation")
    }
    
    # compute the loglikelihood
    loglikeCurr <- switch(method,
                          BSL = gaussianSynLike(ssy, ssx, shrinkage, penalty, standardise, GRC, whitening, ssyTilde, log = TRUE, verbose = verbose),
                          uBSL = gaussianSynLikeGhuryeOlkin(ssy, ssx, log = TRUE, verbose = verbose),
                          semiBSL = semiparaKernelEstimate(ssy, ssx, shrinkage = shrinkage, penalty = penalty),
                          BSLmisspec = synLikeMisspec(ssy, ssx, type = misspecType, gamma = gammaCurr, log = TRUE, verbose = verbose)
    )
    
    if (method == "BSLmisspec") {
      ssxCurr <- ssx
      stdCurr <- attr(loglikeCurr, "std")
    }
  }
  # if (verbose) cat("finish\n")
  
  if (verbose == 1L) timeStart <- Sys.time()
  for (i in 1:M) {
    
    flush.console()
    if (verbose == 2L)  {
      cat("i =", i, "\n")
    }
    
    if (method == "BSLmisspec") {
      gammaCurr <- switch(misspecType,
                          mean = sliceGammaMean(ssy, ssxCurr, loglikeCurr, gammaCurr, tau, std = stdCurr),
                          variance = sliceGammaVariance(ssy, ssxCurr, loglikeCurr, gammaCurr, tau, std = stdCurr)
      )
      loglikeCurr <- attr(gammaCurr, "loglike")
    }
    
    # multivariate normal random walk to the proposed value of theta
    if (!logitTransform) {
      thetaProp <- c(mvtnorm::rmvnorm(1, mean = thetaCurr, sigma = covRandWalk))
      logp2 <- 0
    } else {
      thetaTildeCurr <- paraLogitTransform(thetaCurr, logitTransformBound)
      # thetaTildeProp <- mvrnorm(1, thetaTildeCurr, covRandWalk)
      thetaTildeProp <- mvtnorm::rmvnorm(1, mean = thetaTildeCurr, sigma = covRandWalk)
      thetaProp <- paraLogitBackTransform(thetaTildeProp, logitTransformBound)
      logp2 <- jacobianLogitTransform(thetaTildeProp, logitTransformBound, TRUE) -
        jacobianLogitTransform(thetaTildeCurr, logitTransformBound, TRUE)
    }
    
    # early rejection if the proposed theta falls outside of prior coverage
    # / feasible region
    if (!is.null(fnLogPrior)) {
      logp1 <- fnLogPrior(thetaProp) - fnLogPrior(thetaCurr)
      if (logp1 == -Inf) {
        if (verbose == 2L) {
          cat("*** early rejection ***\n")
        } else if (verbose == 1L){
          timeElapsed <- difftime(Sys.time(), timeStart, units = "secs")
          timeLeft <- timeElapsed / i * (M - i)
          elapsed <- myTimeStr(timeElapsed)
          left <- myTimeStr(timeLeft)
          acc <- paste0(formatC(100 * countAcc / i, format = "f", digits = 2), "%")
          myMiniProgressBar(i / M, txt1 = paste(sprintf("%-2.1f%% finished,", 100 * i / M), "i =", i, "*** early rejection ***"),
                            txt2 = paste0("acceptance rate = ", acc, ", elapsed = ", elapsed, ", remaining = ", left),
                            style = 3, label = c("=", ".", ""))
          flush.console()
        }
        theta[i, ] <- thetaCurr
        loglike[i] <- loglikeCurr
        if (method == "BSLmisspec") {
          gamma[i, ] <- gammaCurr
        }
        countEar <- countEar + 1
        next
      }
    } else {
      logp1 <- 0
    }
    log_prob <- logp1 + logp2
    
    # simulate with thetaProp and calculate the summary statistics
    ssx <- myFnSimSum(n, thetaProp)
    
    # reject if inifite value is detected in ssx
    if (any(is.infinite(ssx))) {
      if (verbose == 2L) {
        cat("*** reject (infinite ssx) ***\n")
      } else if (verbose == 1L){
        timeElapsed <- difftime(Sys.time(), timeStart, units = "secs")
        timeLeft <- timeElapsed / i * (M - i)
        elapsed <- myTimeStr(timeElapsed)
        left <- myTimeStr(timeLeft)
        acc <- paste0(formatC(100 * countAcc / i, format = "f", digits = 2), "%")
        myMiniProgressBar(i / M, txt1 = paste(sprintf("%-2.1f%% finished,", 100 * i / M), "i =", i, "*** reject (infinite ssx) ***"),
                          txt2 = paste0("acceptance rate = ", acc, ", elapsed = ", elapsed, ", remaining = ", left),
                          style = 3, label = c("=", ".", ""))
        flush.console()
      }
      theta[i, ] <- thetaCurr
      loglike[i] <- loglikeCurr
      if (method == "BSLmisspec") {
        gamma[i, ] <- gammaCurr
      }
      countErr <- countErr + 1
      next
    }
    
    # compute the loglikelihood
    loglikeProp <- switch(method,
                          BSL = gaussianSynLike(ssy, ssx, shrinkage, penalty, standardise, GRC, whitening, ssyTilde, log = TRUE, verbose = verbose),
                          uBSL = gaussianSynLikeGhuryeOlkin(ssy, ssx, log = TRUE, verbose = verbose),
                          semiBSL = semiparaKernelEstimate(ssy, ssx, shrinkage = shrinkage, penalty = penalty),
                          BSLmisspec = synLikeMisspec(ssy, ssx, type = misspecType, gamma = gammaCurr, log = TRUE, verbose = verbose)
    )
    
    if (loglikeProp == Inf) {
      if (verbose == 2L) {
        cat("*** reject (positive infinite loglike) ***\n")
      } else if (verbose == 1L){
        timeElapsed <- difftime(Sys.time(), timeStart, units = "secs")
        timeLeft <- timeElapsed / i * (M - i)
        elapsed <- myTimeStr(timeElapsed)
        left <- myTimeStr(timeLeft)
        acc <- paste0(formatC(100 * countAcc / i, format = "f", digits = 2), "%")
        myMiniProgressBar(i / M, txt1 = paste(sprintf("%-2.1f%% finished,", 100 * i / M), "i =", i, "*** reject (positive infinite loglike) ***"),
                          txt2 = paste0("acceptance rate = ", acc, ", elapsed = ", elapsed, ", remaining = ", left),
                          style = 3, label = c("=", ".", ""))
        flush.console()
      }
      theta[i, ] <- thetaCurr
      loglike[i] <- loglikeCurr
      if (method == "BSLmisspec") {
        gamma[i, ] <- gammaCurr
      }
      countErr <- countErr + 1
      next
    }
    
    log_rloglike <- loglikeProp - loglikeCurr
    if (runif(1) < exp(log_prob + log_rloglike) ) {
      if (verbose == 2L) {
        cat("*** accept ***\n")
      } else if (verbose == 1L){
        timeElapsed <- difftime(Sys.time(), timeStart, units = "secs")
        timeLeft <- timeElapsed / i * (M - i)
        elapsed <- myTimeStr(timeElapsed)
        left <- myTimeStr(timeLeft)
        acc <- paste0(formatC(100 * countAcc / i, format = "f", digits = 2), "%")
        myMiniProgressBar(i / M, txt1 = paste(sprintf("%-2.1f%% finished,", 100 * i / M), "i =", i, "*** accept ***"),
                          txt2 = paste0("acceptance rate = ", acc, ", elapsed = ", elapsed, ", remaining = ", left),
                          style = 3, label = c("=", ".", ""))
        flush.console()
      }
      thetaCurr <- thetaProp
      loglikeCurr <- loglikeProp
      if (method == "BSLmisspec") {
        ssxCurr <- ssx
        stdCurr <- attr(loglikeProp, "std")
      }
      countAcc <- countAcc + 1
    } else {
      if (verbose == 1L){
        timeElapsed <- difftime(Sys.time(), timeStart, units = "secs")
        timeLeft <- timeElapsed / i * (M - i)
        elapsed <- myTimeStr(timeElapsed)
        left <- myTimeStr(timeLeft)
        acc <- paste0(formatC(100 * countAcc / i, format = "f", digits = 2), "%")
        myMiniProgressBar(i / M, txt1 = paste(sprintf("%-2.1f%% finished,", 100 * i / M), "i =", i),
                          txt2 = paste0("acceptance rate = ", acc, ", elapsed = ", elapsed, ", remaining = ", left),
                          style = 3, label = c("=", ".", ""))
        flush.console()
      }
    }
    
    theta[i, ] <- thetaCurr
    loglike[i] <- loglikeCurr
    if (method == "BSLmisspec") {
      gamma[i, ] <- gammaCurr
    }
    
    if (plotOnTheFly) {
      if (i %% plotOnTheFly == 0) {
        for (k in 1:p) {
          plot(density(theta[1:i, k]), main = NA, xlab = thetaNames[k],
               col = 1, lty = 1)
        }
      }
    }
  }
  if (verbose == 1L) cat('\n')
  
  accRate <- countAcc/M
  earRate <- countEar/M
  errRate <- countErr/M
  time <- difftime(Sys.time(), startTime)
    
  result <- new("BSL", theta = theta, loglike = loglike, call = cl, model = model,
                acceptanceRate = accRate, earlyRejectionRate = earRate, errorRate = errRate,
                y = y, n = n, M = M, covRandWalk = covRandWalk, method = method,
                shrinkage = shrinkage, penalty = penalty, standardise = standardise, GRC = GRC,
                logitTransform = logitTransform, logitTransformBound = logitTransformBound,
                parallel = parallel, parallelArgs = parallelArgs, time = time,
                gamma = gamma, misspecType = misspecType, tau = tau, whitening = whitening)
  return(result)
}
