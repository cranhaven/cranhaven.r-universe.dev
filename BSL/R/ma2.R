#' An MA(2) model
#'
#' @description In this example we wish to estimate the parameters of a simple
#'   MA(2) time series model. We provide the data and tuning parameters required
#'   to reproduce the results in \insertCite{An2019;textual}{BSL}.
#'   The journal article \insertCite{An2022;textual}{BSL} provides a full
#'   description of how to use this package for the toad example.
#'
#' @param theta     A vector of proposed model parameters,
#'   \eqn{\theta_1} and
#'   \eqn{\theta_2}.
#' @param n         The number of simulations to run with the vectorised
#'   simulation function.
#' @param x			Observed or simulated data in the format of a vector of length
#'   \eqn{TT}.
#' @param TT         The number of observations.
#' @param epsilon   The skewness parameter in the sinh-arcsinh transformation.
#' @param delta   The kurtosis parameter in the sinh-arcsinh transformation.
#'
#' @details This example is based on estimating the parameters of a basic MA(2)
#'   time series model of the form
#'
#'   \deqn{y_t = z_t + \theta_1 z_{t-1} + \theta_2 z_{t-2},}
#'
#'   where \eqn{t=1,\ldots,TT} and \eqn{z_t \sim N(0,1)}
#'   for \eqn{t=-1,0,\ldots,TT}. A uniform
#'   prior is used for this example, subject to the restrictions that
#'   \eqn{-2<\theta_1<2},
#'   \eqn{\theta_1+\theta_2>-1}
#'    and
#'   \eqn{\theta_1-\theta_2<1}
#'    so that invertibility of the time series is satisfied. The summary
#'   statistics are simply the full data.
#'
#' @section A simulated dataset:
#'
#'   An example ``observed'' dataset and the tuning parameters relevant to that
#'   example can be obtained using \code{data(ma2)}. This ``observed'' data is a
#'   simulated dataset with
#'   \eqn{\theta_1 = 0.6},
#'   \eqn{\theta_2=0.2} and
#'   \eqn{TT=50}. Further information about this model and the specific choices
#'   of tuning parameters used in BSL and BSLasso can be found in An et al.
#'   (2019).
#'
#'   \itemize{
#'
#'   \item \code{data}: A time series dataset, in the form of a vector of length
#'   \eqn{TT}
#'
#'   \item \code{sim_args}: A list containing \eqn{TT=50}
#'
#'
#'   \item \code{start}: A vector of suitable initial values of the parameters
#'   for MCMC
#'
#'   \item \code{cov}: The covariance matrix of a multivariate normal random
#'   walk proposal distribution used in the MCMC, in the form of a 2
#'   \eqn{\times} 2 matrix }
#'
#' @examples
#' \dontrun{
#' # Load the data for this example and set up the model object
#' data(ma2)
#' model <- newModel(fnSimVec = ma2_sim_vec, fnSum = ma2_sum, simArgs = ma2$sim_args,
#'                   theta0 = ma2$start, fnLogPrior = ma2_prior)
#' thetaExact <- c(0.6, 0.2)
#'
#' # reduce the number of iterations M if desired for all methods below
#' # Method 1: standard BSL
#' resultMa2BSL <- bsl(y = ma2$data, n = 500, M = 300000, model = model, covRandWalk = ma2$cov,
#'                     method = "BSL", verbose = 1L)
#' show(resultMa2BSL)
#' summary(resultMa2BSL)
#' plot(resultMa2BSL, thetaTrue = thetaExact, thin = 20)
#'
#' # Method 2: unbiased BSL
#' resultMa2uBSL <- bsl(y = ma2$data, n = 500, M = 300000, model = model, covRandWalk=ma2$cov,
#'                      method = "uBSL", verbose = 1L)
#' show(resultMa2uBSL)
#' summary(resultMa2uBSL)
#' plot(resultMa2uBSL, thetaTrue = thetaExact, thin = 20)
#'
#' # Method 3: BSLasso (BSL with glasso shrinkage estimation)
#' # tune the penalty parameter fisrt
#' ssy <- ma2_sum(ma2$data)
#' lambdaAll <- list(exp(seq(-5.5,-1.5,length.out=20)))
#' set.seed(100)
#' penaltyGlasso <- selectPenalty(ssy = ssy, n = 300, lambdaAll, theta = thetaExact,
#'                         M = 100, sigma = 1.5, model = model, method = "BSL", shrinkage = "glasso")
#' penaltyGlasso
#' plot(penaltyGlasso)
#'
#' resultMa2BSLasso <- bsl(y = ma2$data, n = 300, M = 250000, model = model, covRandWalk=ma2$cov,
#'                         method = "BSL", shrinkage = "glasso", penalty = 0.027, verbose = 1L)
#' show(resultMa2BSLasso)
#' summary(resultMa2BSLasso)
#' plot(resultMa2BSLasso, thetaTrue = thetaExact, thin = 20)
#'
#' # Method 4: BSL with Warton's shrinkage and Whitening
#' # estimate the Whtieing matrix and tune the penalty parameter first
#' W <- estimateWhiteningMatrix(20000, model, method = "PCA", thetaPoint = ma2$start)
#' gammaAll <- list(seq(0.3, 0.8, 0.02))
#' set.seed(100)
#' penaltyWarton <- selectPenalty(ssy = ssy, n = 300, gammaAll, theta = thetaExact,
#'                         M = 100, sigma = 1.2, model = model, method = "BSL", shrinkage = "Warton",
#'                         whitening = W)
#' penaltyWarton
#' plot(penaltyWarton, logscale = FALSE)
#'
#' resultMa2Whitening <- bsl(y = ma2$data, n = 300, M = 250000, model = model, covRandWalk=ma2$cov,
#'                         method = "BSL", shrinkage = "Warton", whitening = W,
#'                         penalty = 0.52, verbose = 1L)
#' show(resultMa2Whitening)
#' summary(resultMa2Whitening)
#' plot(resultMa2Whitening, thetaTrue = thetaExact, thin = 20)
#'
#' # Method 5: semiBSL, the summary statistics function is different from previous methods
#' model2 <- newModel(fnSimVec = ma2_sim_vec, fnSum = ma2_sum, simArgs = ma2$sim_args,
#'                   sumArgs = list(epsilon = 2), theta0 = ma2$start, fnLogPrior = ma2_prior)
#' sim <- simulation(model, n = 1e4, theta = ma2$start, seed = 1) # run a short simulation
#' plot(density(sim$ssx[, 1])) # the first marginal summary statistic is right-skewed
#' resultMa2SemiBSL <- bsl(y = ma2$data, n = 500, M = 200000, model = model2, covRandWalk=ma2$cov,
#'                         method = "semiBSL", verbose = 1L)
#' show(resultMa2SemiBSL)
#' summary(resultMa2SemiBSL)
#' plot(resultMa2SemiBSL, thetaTrue = thetaExact, thin = 20)
#'
#' # Method 6: BSL with consideration of model misspecification (mean adjustment)
#' resultMa2Mean <- bsl(y = ma2$data, n = 500, M = 200000, model = model, covRandWalk=ma2$cov,
#'                         method = "BSLmisspec", misspecType = "mean", verbose = 1L)
#' show(resultMa2Mean)
#' summary(resultMa2Mean)
#' plot(resultMa2Mean, thetaTrue = thetaExact, thin = 20)
#'
#' # Method 7: BSL with consideration of model misspecification (variance inflation)
#' resultMa2Variance <- bsl(y = ma2$data, n = 500, M = 200000, model = model, covRandWalk=ma2$cov,
#'                      method = "BSLmisspec", misspecType = "variance", verbose = 1L)
#' show(resultMa2Variance)
#' summary(resultMa2Variance)
#' plot(resultMa2Variance, thetaTrue = thetaExact, thin = 20)
#'
#' # Plotting the results together for comparison
#' # plot using the R default plot function
#' oldpar <- par()
#' par(mar = c(5, 4, 1, 2), oma = c(0, 1, 2, 0))
#' combinePlotsBSL(list(resultMa2BSL, resultMa2uBSL, resultMa2BSLasso, resultMa2SemiBSL), which = 1,
#'                 thetaTrue = thetaExact, thin = 20, label = c("bsl", "uBSL", "bslasso", "semiBSL"),
#'                 col = c("black", "red", "blue", "green"), lty = 1:4, lwd = 1)
#' mtext("Approximate Univariate Posteriors", outer = TRUE, cex = 1.5)
#'
#' # plot using the ggplot2 package
#' combinePlotsBSL(list(resultMa2BSL, resultMa2uBSL, resultMa2BSLasso, resultMa2SemiBSL), which = 2,
#'     thetaTrue = thetaExact, thin = 20, label = c("bsl", "ubsl", "bslasso", "semiBSL"),
#'     options.color = list(values=c("black", "red", "blue", "green")),
#'     options.linetype = list(values = 1:4), options.size = list(values = rep(1, 4)),
#'     options.theme = list(plot.margin = grid::unit(rep(0.03,4), "npc"),
#'         axis.title = ggplot2::element_text(size=12), axis.text = ggplot2::element_text(size = 8),
#'         legend.text = ggplot2::element_text(size = 12)))
#' par(mar = oldpar$mar, oma = oldpar$oma)
#' }
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @author Ziwen An, Leah F. South and Christopher Drovandi
#'
#' @name ma2
#' @usage data(ma2)
NULL

#' @describeIn ma2 Simulates an MA(2) time series.
#' @export
ma2_sim <- function(theta, TT) {
    rand <- rnorm(TT + 2)
    y <- rand[3 : (TT+2)] + theta[1] * rand[2 : (TT+1)] + theta[2] * rand[1 : TT]
    return(y)
}

#' @describeIn ma2 Simulates n MA(2) time series with a vectorised simulation
#'   function.
#' @export
ma2_sim_vec <- function(n, theta, TT) {
    rand <- matrix(rnorm(n * (TT + 2)), n, TT + 2)
    y <- rand[, 3 : (TT + 2)] + theta[1] * rand[, 2 : (TT + 1)]
    + theta[2] * rand[, 1 : TT]
    return(y)
}

#' @describeIn ma2 Returns the summary statistics for a given data set. The
#'   skewness and kurtosis of the summary statistics can be controlled via the
#'   \eqn{\epsilon} and
#'   \eqn{\delta} parameters. This is the
#'   sinh-arcsinnh transformation of \insertCite{Jones2009;textual}{BSL}. By default,
#'   the summary statistics function simply returns the raw data. Otherwise, the
#'   transformation is introduced to motivate the ``semiBSL'' method.
#' @export
ma2_sum <- function(x, epsilon = 0, delta = 1) {
    ssx = sinh((asinh(x) + epsilon) / delta)
    return(ssx)
}

#' @describeIn ma2 Evaluates the (unnormalised) log prior, which is uniform
#'   subject to several restrictions related to invertibility of the time
#'   series.
#' @export
ma2_prior <- function(theta) {
    log(theta[2] < 1 & sum(theta) > -1 & diff(theta) > -1)
}
