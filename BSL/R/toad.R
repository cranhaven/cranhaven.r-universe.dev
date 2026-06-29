#' Toad example
#'
#' @description This example estimates the parameter for the toad example. The
#'   model simulates the movement of an amphibian called Fowler's toad. The
#'   model is proposed by \insertCite{Marchand2017;textual}{BSL}. This example
#'   includes both simulated and real data. The real data is obtained from 
#'   the supplementary material of \insertCite{Marchand2017;textual}{BSL}.
#'   The journal article \insertCite{An2022;textual}{BSL} provides a full
#'   description of how to use this package for the toad example.
#'
#' @param theta A vector of proposed model parameters,
#'   \eqn{\alpha}, \eqn{\gamma} and \eqn{p_0}.
#' @param ntoads The number of toads to simulate in the observation.
#' @param ndays The number of days observed.
#' @param model Which model to be used: 1 for the random return model, 2 for the
#'   nearest return model, and 3 for the distance-based return probability
#'   model. The default is 1.
#' @param d0 Characteristic distance for model 3. Only used if \code{model} is
#'   3.
#' @param na Logical. This is the index matrix for missing observations. By
#'   default, \code{matrix(FALSE, ndays, ntoads)} indicates there is no
#'   missingness in the observation matrix.
#' @param X The data matrix.
#' @param p The numeric vector of probabilities to compute the quantiles.
#' @param lag The lag of days to compute the summary statistics, default as 1,
#'   2, 4 and 8.
#'
#' @details The example includes the three different returning models of
#'   \insertCite{Marchand2017;textual}{BSL}. Please see
#'   \insertCite{Marchand2017;textual}{BSL} for a full description of the toad
#'   model, and also \insertCite{An2018;textual}{BSL} for Bayesian inference
#'   with the semi-BSL method.
#'
#' @usage data(toad)
#'
#' @section datasets (simulated and real):
#'
#'   A simulated dataset and a real dataset are provided in this example. Both
#'   datasets contain observations from 66 toads for 63 days. The simulated
#'   dataset is simulated with parameter
#'   \eqn{\theta = (1.7, 35,
#'   0.6)}. This is the data used in \insertCite{An2018;textual}{BSL}. The real
#'   dataset is obtained from the supplementary data of
#'   \insertCite{Marchand2017;textual}{BSL}.
#'
#'   \itemize{
#'
#'   \item \code{data_simulated}:  A 63
#'   \eqn{\times} 66 matrix of the observed
#'   toad locations (simulated data).
#'
#'   \item \code{data_real}:       A 63
#'   \eqn{\times} 66 matrix of the observed
#'   toad locations (real data).
#'
#'   \item \code{cov}: The covariance matrix of a multivariate normal random
#'   walk proposal distribution used in the MCMC, in the form of a 3
#'   \eqn{\times} 3 matrix.
#'
#'   \item \code{theta0}: A vector of suitable initial values of the parameters
#'   for MCMC.
#'
#'   \item \code{sim_args_simulated} and \code{sim_args_real}: A list of the
#'   arguments to pass into the simulation function.
#'
#'   \itemize{
#'
#'   \item \code{ndays}: The number of days observed.
#'
#'   \item \code{ntoads}: The total number of toads being observed.
#'
#'   \item \code{model}: Indicator of which model to be used.
#'
#'   \item \code{na}: Indicator matrix for missingness.
#'
#'   }
#'
#'   }
#'
#' @examples
#' \dontrun{
#' require(doParallel) # You can use a different package to set up the parallel backend
#' 
#' data(toad)
#' 
#' ## run standard BSL for the simulated dataset
#' model1 <- newModel(fnSim = toad_sim, fnSum = toad_sum, theta0 = toad$theta0,
#'                    fnLogPrior = toad_prior, simArgs = toad$sim_args_simulated, 
#'                    thetaNames = expression(alpha,gamma,p[0]))
#' paraBound <- matrix(c(1,2,0,100,0,0.9), 3, 2, byrow = TRUE)
#' 
#' # Performing BSL (reduce the number of iterations M if desired)
#' # Opening up the parallel pools using doParallel
#' cl <- makeCluster(min(detectCores() - 1,2))
#' registerDoParallel(cl)
#' resultToadSimulated <- bsl(toad$data_simulated, n = 1000, M = 10000, model = model1,
#'                            covRandWalk = toad$cov, logitTransformBound = paraBound,
#'                            parallel = TRUE, verbose = 1L, plotOnTheFly = 100)
#' stopCluster(cl)
#' registerDoSEQ()
#' show(resultToadSimulated)
#' summary(resultToadSimulated)
#' plot(resultToadSimulated, thetaTrue = toad$theta0, thin = 20)
#' 
#' ## run standard BSL for the real dataset
#' model2 <- newModel(fnSim = toad_sim, fnSum = toad_sum, theta0 = toad$theta0,
#'                    fnLogPrior = toad_prior, simArgs = toad$sim_args_real,
#'                    thetaNames = expression(alpha,gamma,p[0]))
#' paraBound <- matrix(c(1,2,0,100,0,0.9), 3, 2, byrow = TRUE)
#' 
#' # Performing BSL (reduce the number of iterations M if desired)
#' # Opening up the parallel pools using doParallel
#' cl <- makeCluster(min(detectCores() - 1,2))
#' registerDoParallel(cl)
#' resultToadReal <- bsl(toad$data_real, n = 1000, M = 10000, model = model2,
#'                       covRandWalk = toad$cov, logitTransformBound = paraBound,
#'                       parallel = TRUE, verbose = 1L, plotOnTheFly = 100)
#' stopCluster(cl)
#' registerDoSEQ()
#' show(resultToadReal)
#' summary(resultToadReal)
#' plot(resultToadReal, thetaTrue = toad$theta0, thin = 20)
#' }
#'
#' @references
#'
#' \insertAllCited()
#'
#' @author                                 Ziwen An, Leah F. South and
#'   Christopher Drovandi
#' @name toad
NULL



#' @describeIn toad Simulates data from the model, using C++ in the backend.
#' @export
toad_sim <- function (theta, ntoads, ndays, model = 1, d0 = 100, na = matrix(FALSE, ndays, ntoads)) {
    stopifnot(model %in% 1:3)
    X <- sim_toad(theta, ntoads, ndays, model, d0)
    X[na] <- NA
    return (X)
}

#' @describeIn toad Computes the summary statistics for this example. The summary 
#' statistics are the log differences between adjacent quantiles and also the median.
#' @export
toad_sum <- function(X, lag = c(1,2,4,8), p = seq(0,1,0.1)) {
    nlag <- length(lag)
    ssx <- c()
    for (k in 1 : nlag) {
        disp <- obsMat2deltax(X, lag[k])
		indret <- disp < 10
		noret <- disp[!indret]
        logdiff <- log(diff(quantile(noret, probs = p, names = FALSE)))
		ssx <- c(ssx, mean(indret), median(noret), logdiff)
    }
    return (ssx)
}

#' @describeIn toad Evaluates the log prior at the chosen parameters.
#' @export
toad_prior <- function(theta) {
    log(theta[1] > 1 & theta[1] < 2 & theta[2] > 0 & theta[2] < 80 & theta[3] > 0 & theta[3] < 1)
}
