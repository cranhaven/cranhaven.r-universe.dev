#' Selecting the Penalty Parameter
#'
#' @description This is the main function for selecting the shrinkage (graphical
#'   lasso or Warton's estimator) penalty parameter for method BSL or semiBSL
#'   based on a point estimate of the parameters. Parallel computing is
#'   supported with the R package \code{foreach}. The penalty selection method
#'   is outlined in \insertCite{An2019;textual}{BSL}.
#'
#' @param ssy                   A summary statistic vector for the observed
#'   data.
#' @param n                     A vector of possible values of \code{n}, the
#'   number of simulations from the model per MCMC iteration for estimating the
#'   synthetic likelihood.
#' @param lambda            A list, with each entry containing the vector of
#'   penalty values to test for the corresponding choice of \code{n}.
#' @param theta                 A point estimate of the parameter value which
#'   all of the simulations will be based on. By default, if \code{theta} is
#'   \code{NULL}, it will be replaced by \code{theta0} from the given
#'   \code{model}.
#' @param M                     The number of repeats to use in estimating the
#'   standard deviation of the estimated log synthetic likelihood.
#' @param sigma                 The standard deviation of the log synthetic
#'   likelihood estimator to aim for, usually a value between 1 and 2. This
#'   parameter helps to control the mixing of a Markov chain.
#' @param method                A string argument indicating the method to be
#'   used. If the method is ``BSL'', the shrinkage is applied to the Gaussian
#'   covariance matrix. Otherwise if the method is ``semiBSL'', the shrinkage is
#'   applied to the correlation matrix of the Gaussian copula.
#' @param shrinkage     A string argument indicating which shrinkage method to
#'   be used. Current options are ``glasso'' for the graphical lasso method of
#'   \insertCite{Friedman2008;textual}{BSL} and ``Warton'' for the ridge
#'   regularisation method of \insertCite{Warton2008;textual}{BSL}.
#' @param parallelSim           A logical value indicating whether parallel
#'   computing should be used for simulation and summary statistic evaluation.
#'   Default is \code{FALSE}.
#' @param parallelSimArgs       A list of additional arguments to pass into the
#'   \code{foreach} function. Only used when \code{parallelSim} is \code{TRUE},
#'   default is \code{NULL}.
#' @param parallelMain          A logical value indicating whether parallel
#'   computing should be used to computing the graphical lasso function. Notice
#'   that this should only be turned on when there are a lot of candidate values
#'   in \code{lambda}. Default is \code{FALSE}.
#' @param ... Other arguments to pass to \code{\link{gaussianSynLike}} (``BSL''
#'   method) or \code{\link{semiparaKernelEstimate}} (``semiBSL'' method).
#' @inheritParams bsl
#'
#' @return 				An S4 object \code{PENALTY} of the penalty selection results. The
#'   \code{show} and \code{plot} methods are provided with the S4 class.
#'
#' @examples
#' \dontrun{
#' data(ma2)
#' model <- newModel(fnSimVec = ma2_sim_vec, fnSum = ma2_sum, simArgs = ma2$sim_args,
#'                   theta0 = ma2$start, fnLogPrior = ma2_prior)
#' theta <- c(0.6,0.2)
#'
#' # Performing tuning for BSLasso (BSL with glasso shrinkage estimation)
#' ssy <- ma2_sum(ma2$data)
#' lambda_all <- list(exp(seq(-3,0.5,length.out=20)), exp(seq(-4,-0.5,length.out=20)),
#'                    exp(seq(-5.5,-1.5,length.out=20)), exp(seq(-7,-2,length.out=20)))
#' set.seed(100)
#' sp_ma2 <- selectPenalty(ssy = ssy, n = c(50, 150, 300, 500), lambda_all, theta = theta,
#'     M = 100, sigma = 1.5, model = model, method = 'BSL', shrinkage = 'glasso')
#' sp_ma2
#' plot(sp_ma2)
#' }
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @author    Ziwen An, Leah F. South and Christopher Drovandi
#' @seealso   \code{PENALTY} for the usage of the S4 class. \code{\link{ma2}},
#'   \code{\link{cell}} and \code{\link{mgnk}} for examples. \code{\link{bsl}}
#'   for the main function to run BSL.
#' @export
selectPenalty <- function(ssy, n, lambda, M, sigma = 1.5, model, theta = NULL,
                          method = c("BSL", "semiBSL"), shrinkage = c("glasso", "Warton"),
                          parallelSim = FALSE, parallelSimArgs = NULL, parallelMain = FALSE, verbose = 1L, ...) {
  method <- match.arg(method)
  shrinkage <- match.arg(shrinkage)
  if (!verbose %in% c(0, 1, 2)) {
    stop("verbose must be 0 or 1 or 2")
  }
  if (!parallelSim & !is.null(parallelSimArgs)) {
    warning("\"parallelSimArgs\" is omitted in serial computing")
  }
  stopifnot(inherits(model, "MODEL"))

  if (is.null(theta)) {
    theta <- model@theta0
  }
  n <- as.vector(n)
  N <- length(n)
  if (is.atomic(lambda) && is.vector(lambda)) {
      lambda <- rep(list(lambda), N)
  }
  if (length(lambda) != N) {
    stop("lambda must be a list with the same length as n")
  }
  ns <- length(ssy)
  call <- match.call()

  if (verbose) {
    cat("*** selecting penalty with", method, "likelihood and", shrinkage, "shrinkage estimator ***\n")
  }
  nMax <- max(n)
  loglike <- vector("list", N)
  for (i in 1 : N) loglike[[i]] <- array(NA, c(M, length(lambda[[i]])))

  # map the simulation function
  if (parallelSim) {
    myFnSimSum <- function(n, theta) fn(model)$fnPar(n, theta, parallelSimArgs)
  } else {
    myFnSimSum <- fn(model)$fn
  }

  if (verbose == 1L) timeStart <- Sys.time()
  for (m in 1 : M) {

    flush.console()
    if (verbose == 2L)  {
      cat("m =", m, "\n")
    } else if (verbose == 1L){
      timeElapsed <- difftime(Sys.time(), timeStart, units = "secs")
      timeLeft <- timeElapsed / m * (M - m)
      elapsed <- myTimeStr(timeElapsed)
      left <- myTimeStr(timeLeft)
      myMiniProgressBar(m / M, txt1 = paste("m =", m),
                        txt2 = paste0("elapsed = ", elapsed, ", remaining = ", left),
                        style = 2, label = c("=", ".", "|"))
      flush.console()
    }

    # simulate with theta_prop and calculate summaries
    ssx <- myFnSimSum(nMax, theta)

    for (i in 1 : N) {
      nCurr <- n[i]
      nLambda <- length(lambda[[i]])
      ssxCurr <- ssx[sample(nMax, nCurr), ]

      if (!parallelMain) {
        for (k in 1 : nLambda) {
          lambdaCurr = lambda[[i]][k]
          loglike[[i]][m, k] <- switch(method,
                                       "BSL" = gaussianSynLike(ssy, ssxCurr, shrinkage = shrinkage, penalty = lambdaCurr, log = TRUE, ...),
                                       "semiBSL" = semiparaKernelEstimate(ssy, ssxCurr, shrinkage = shrinkage, penalty = lambdaCurr, log = TRUE, ...))
        }
      } else {
        loglike[[i]][m, ] <- foreach(k = 1 : nLambda, .combine = c, .packages = "glasso",
                                     .export = c("gaussianSynLike", "semiparaKernelEstimate")) %dopar% {
                                       lambdaCurr = lambda[[i]][k]
                                       switch(method,
                                              "BSL" = gaussianSynLike(ssy, ssxCurr, shrinkage = shrinkage, penalty = lambdaCurr, log = TRUE, ...),
                                              "semiBSL" = semiparaKernelEstimate(ssy, ssxCurr, shrinkage = shrinkage, penalty = lambdaCurr, log = TRUE, ...))
                                     }
      }
    }
  }
  if (verbose == 1L) cat("\n")

  ret <- PENALTY(loglike = loglike, n = n, lambda = lambda, sigma = sigma, model = model, call = call)

  return(ret)
}
