##############################################################################
# Parameter estimation using particle Metropolis-Hastings in a SV
# with a proposal adapted from a pilot run.
#
# Johan Dahlin <uni (at) johandahlin.com.nospam>
# Documentation at https://github.com/compops/pmh-tutorial
# Published under GNU General Public License
##############################################################################

#' Parameter estimation in a simple stochastic volatility model
#'
#' @description
#' Minimal working example of parameter estimation in a stochastic volatility
#' model using the particle Metropolis-Hastings algorithm with a bootstrap
#' particle filter providing an unbiased estimator of the likelihood. The
#' code estimates the parameter posterior for three parameters using
#' real-world data.
#' @details
#' The Particle Metropolis-Hastings (PMH) algorithm makes use of a Gaussian
#' random walk as the proposal for the parameters. The data are scaled
#' log-returns from the OMXS30 index during the period from January 2, 2012
#' to January 2, 2014.
#'
#' This version of the code makes use of a proposal that is tuned using a run of
#' \code{\link{example3_sv}} and therefore have better mixing
#' properties.
#' @param noBurnInIterations The number of burn-in iterations in the PMH
#' algorithm. Must be smaller than \code{noIterations}.
#' @param noIterations The number of iterations in the PMH algorithm. 100
#' iterations takes about a minute on a laptop to execute.
#' @param noParticles The number of particles to use when estimating the likelihood.
#' @param initialTheta The initial guess of the parameters theta.
#' @param syntheticData If TRUE, data is not downloaded from the Internet. This is only used when running tests of the package.
#' @return
#' The function returns the estimated marginal parameter posteriors for each
#' parameter, the trace of the Markov chain and the resulting autocorrelation
#' function. The data is also presented with an estimate of the
#' log-volatility.
#'
#' The function returns a list with the elements:
#' \itemize{
#' \item{thhat: The estimate of the mean of the parameter posterior.}
#' \item{xhat: The estimate of the mean of the log-volatility posterior.}
#' \item{thhatSD: The estimate of the standard deviation of the parameter
#' posterior.}
#' \item{xhatSD: The estimate of the standard deviation of the log-volatility
#' posterior.}
#' \item{iact: The estimate of the integrated autocorrelation time for each
#' parameter.}
#' \item{estCov: The estimate of the covariance of the parameter posterior.}
#' }
#' @references 
#' Dahlin, J. & Schon, T. B. "Getting Started with Particle 
#' Metropolis-Hastings for Inference in Nonlinear Dynamical Models." 
#' Journal of Statistical Software, Code Snippets,
#' 88(2): 1--41, 2019.
#' @author 
#' Johan Dahlin \email{uni@@johandahlin.com}
#' @note
#' See Section 6.3.1 in the reference for more details.
#' @example ./examples/example4
#' @keywords
#' misc
#' @export
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb
#' @importFrom graphics abline
#' @importFrom graphics hist
#' @importFrom graphics layout
#' @importFrom graphics lines
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom graphics polygon
#' @importFrom stats acf
#' @importFrom stats density
#' @importFrom stats sd
#' @importFrom stats var

example4_sv <- function(noBurnInIterations=2500, noIterations=7500, noParticles=500,
                         initialTheta=c(0, 0.9, 0.2), syntheticData=FALSE) {

  # Set the random seed to replicate results in tutorial
  set.seed(10)

  ##############################################################################
  # Load data
  ##############################################################################
  if (syntheticData) {
    y <- rnorm(10)
  } else {
    d <-
      Quandl::Quandl(
        "NASDAQOMX/OMXS30",
        start_date = "2012-01-02",
        end_date = "2014-01-02",
        type = "zoo"
      )
    y <- as.numeric(100 * diff(log(d$"Index Value")))
  }

  ##############################################################################
  # PMH
  ##############################################################################

  stepSize <- matrix(
    c(
      0.137255431,-0.0016258103,
      0.0015047492,-0.0016258103,
      0.0004802053,-0.0009973058,
      0.0015047492,-0.0009973058,
      0.0031307062
    ),
    ncol = 3,
    nrow = 3
  )
  stepSize <- 2.562^2 / 3 * stepSize
  res <- particleMetropolisHastingsSVmodel(y, initialTheta, noParticles, noIterations, stepSize)

  ##############################################################################
  # Plot the results
  ##############################################################################
  noIterationsToPlot <- min(c(1500, noIterations - noBurnInIterations))
  iact <- makePlotsParticleMetropolisHastingsSVModel(y, res, noBurnInIterations,
                                                     noIterations, noIterationsToPlot)

  ##############################################################################
  # Compute and save the results
  ##############################################################################

  # Extract the states after burn-in
  resTh <- res$theta[noBurnInIterations:noIterations, ]
  resXh <- res$xHatFiltered[noBurnInIterations:noIterations, ]

  # Estimate the posterior mean and the corresponding standard deviation
  thhat   <- colMeans(resTh)
  thhatSD <- apply(resTh, 2, sd)

  # Estimate the log-volatility and the corresponding standad deviation
  xhat    <- colMeans(resXh)
  xhatSD  <- apply(resXh, 2, sd)

  # Estimate the covariance of the posterior to tune the proposal
  estCov <- var(resTh)

  list(thhat=thhat, xhat=xhat, thhatSD=thhatSD, xhatSD=xhatSD, iact=iact, estCov=estCov)
}
