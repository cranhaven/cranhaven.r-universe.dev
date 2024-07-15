#--- test mclust ---------------------------------------------------------------

context("Gaussian mixture models: projection plots")

source("optimCheck-testfunctions.R")
source("mclust-testfunctions.R")

# loglikelihood on flattened parameter scale
loglik <- function(theta, d, G, ydata) {
  parameters <- theta2par(theta, d, G)
  # simplex-valued probabilities
  if(any(parameters$pro < 0)) return(-Inf) # always sums to 1
  ## # positive-definite variance matrices
  if(any(diag(parameters$variance$cholSigma) <= 0)) return(-Inf)
  sum(dens("EEE", data = ydata,
           parameters = parameters, logarithm = TRUE))
}

ntest <- 5
test_that("mclust::emEEE converges to local mode.", {
  skip_if_not(requireNamespace("mclust", quietly = TRUE),
              "mclust package required to run this test.")
  require(mclust)
  replicate(n = ntest, expr = {
    G <- sample(2:4, 1) # number of components
    d <- sample(2:4, 1) # number of dimensions
    n <- sample(400:600,1) # number of observations
    # simulate data
    # true parameter values
    parameters <- list(pro = rDirichlet(1, rep(1, G)),
                       mean = rMnorm(d, G) + matrix(10 * 1:G, d, G,
                                                    byrow = TRUE),
                       variance = list(modelName = "EEE", d = d, G = G,
                                       cholSigma = chol(crossprod(rMnorm(d)))))
    y <- simEEE(parameters, n = n) # data
    # calculate MLE
    fit <- emEEE(data = y[,-1], parameters = parameters) # fit model
    par.mle <- fit$parameters # convert parameters to MLE
    cholSigma <- tryCatch(chol(par.mle$variance$Sigma),
                          error = function(e) NA)
    if(!anyNA(cholSigma)) {
      par.mle$variance$cholSigma <- cholSigma
      theta.mle <- par2theta(par.mle)
      # projection plots
      ocheck <- optim_proj(fun = function(theta) {
        loglik(theta, d, G, y[,-1])
      }, xsol = theta.mle, xrng = .1, npts = 50, plot = FALSE)
      # largest of min(abs,rel) difference between xsol and xopt
      expect_lt(max.xdiff(ocheck), .01)
    }
  })
})

#-------------------------------------------------------------------------------

context("Gaussian mixture models: \"refit\" with optim")

ntest <- 5
test_that("mclust::emEEE converges to local mode.", {
  skip_if_not(requireNamespace("mclust", quietly = TRUE),
              "mclust package required to run this test.")
  require(mclust)
  replicate(n = ntest, expr = {
    G <- sample(2:4, 1) # number of components
    d <- sample(2:4, 1) # number of dimensions
    n <- sample(400:600,1) # number of observations
    # simulate data
    # true parameter values
    parameters <- list(pro = rDirichlet(1, rep(1, G)),
                       mean = rMnorm(d, G) + matrix(10 * 1:G, d, G,
                                                    byrow = TRUE),
                       variance = list(modelName = "EEE", d = d, G = G,
                                       cholSigma = chol(crossprod(rMnorm(d)))))
    y <- simEEE(parameters, n = n) # data
    # calculate MLE
    fit <- emEEE(data = y[,-1], parameters = parameters) # fit model
    par.mle <- fit$parameters # convert parameters to MLE
    cholSigma <- tryCatch(chol(par.mle$variance$Sigma),
                          error = function(e) NA)
    if(!anyNA(cholSigma)) {
      par.mle$variance$cholSigma <- cholSigma
      theta.mle <- par2theta(par.mle)
      # projection plots
      ocheck <- optim_refit(fun = function(theta) {
        loglik(theta, d, G, y[,-1])
      }, xsol = theta.mle)
      # largest of min(abs,rel) difference between xsol and xopt
      expect_lt(max.xdiff(ocheck), .01)
    }
  })
})
