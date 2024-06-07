#' constructionModelesLassoMLE
#'
#' Construct a collection of models with the Lasso-MLE procedure.
#'
#' @param phiInit an initialization for phi, get by initSmallEM.R
#' @param rhoInit an initialization for rho, get by initSmallEM.R
#' @param piInit an initialization for pi, get by initSmallEM.R
#' @param gamInit an initialization for gam, get by initSmallEM.R
#' @param mini integer, minimum number of iterations in the EM algorithm, by default = 10
#' @param maxi integer, maximum number of iterations in the EM algorithm, by default = 100
#' @param gamma integer for the power in the penaly, by default = 1
#' @param X matrix of covariates (of size n*p)
#' @param Y matrix of responses (of size n*m)
#' @param eps real, threshold to say the EM algorithm converges, by default = 1e-4
#' @param S output of selectVariables.R
#' @param ncores Number of cores, by default = 3
#' @param fast TRUE to use compiled C code, FALSE for R code only
#' @param verbose TRUE to show some execution traces
#'
#' @return a list with several models, defined by phi (the regression parameter reparametrized),
#' rho (the covariance parameter reparametrized), pi (the proportion parameter is the mixture model), llh
#' (the value of the loglikelihood function for this estimator on the training dataset). The list is given
#' for several levels of sparsity, given by several regularization parameters computed automatically.
#'
#' @export
constructionModelesLassoMLE <- function(phiInit, rhoInit, piInit, gamInit, mini,
  maxi, gamma, X, Y, eps, S, ncores, fast, verbose)
{
  if (ncores > 1)
  {
    cl <- parallel::makeCluster(ncores, outfile = "")
    parallel::clusterExport(cl, envir = environment(), varlist = c("phiInit",
      "rhoInit", "gamInit", "mini", "maxi", "gamma", "X", "Y", "eps", "S",
      "ncores", "fast", "verbose"))
  }

  # Individual model computation
  computeAtLambda <- function(lambda)
  {
    if (ncores > 1)
      require("valse")  #nodes start with an empty environment

    if (verbose)
      print(paste("Computations for lambda=", lambda))

    n <- nrow(X)
    p <- ncol(X)
    m <- ncol(Y)
    k <- length(piInit)
    sel.lambda <- S[[lambda]]$selected
    # col.sel = which(colSums(sel.lambda)!=0) #if boolean matrix
    col.sel <- which(sapply(sel.lambda, length) > 0)  #if list of selected vars
    if (length(col.sel) == 0)
      return(NULL)

    # lambda == 0 because we compute the EMV: no penalization here
    res <- EMGLLF(array(phiInit[col.sel, , ], dim=c(length(col.sel),m,k)),
      rhoInit, piInit, gamInit, mini, maxi, gamma, 0,
      as.matrix(X[, col.sel]), Y, eps, fast)

    # Eval dimension from the result + selected
    phiLambda2 <- res$phi
    rhoLambda <- res$rho
    piLambda <- res$pi
    phiLambda <- array(0, dim = c(p, m, k))
    for (j in seq_along(col.sel))
      phiLambda[col.sel[j], sel.lambda[[j]], ] <- phiLambda2[j, sel.lambda[[j]], ]
    dimension <- length(unlist(sel.lambda))

    ## Affectations
    Gam <- matrix(0, ncol = length(piLambda), nrow = n)
    for (i in 1:n)
    {
      for (r in 1:length(piLambda))
      {
        sqNorm2 <- sum((Y[i, ] %*% rhoLambda[, , r] - X[i, ] %*% phiLambda[, , r])^2)
        Gam[i, r] <- piLambda[r] * exp(-0.5 * sqNorm2) * det(rhoLambda[, , r])
      }
    }
    Gam2 <- Gam/rowSums(Gam)
    affec <- apply(Gam2, 1, which.max)
    proba <- Gam2
    LLH <- c(sum(log(apply(Gam,1,sum))), (dimension + m + 1) * k - 1)
    # ## Computation of the loglikelihood
    # # Precompute det(rhoLambda[,,r]) for r in 1...k
    # detRho <- sapply(1:k, function(r) gdet(rhoLambda[, , r]))
    # sumLogLLH <- 0
    # for (i in 1:n)
    # {
    #   # Update gam[,]; use log to avoid numerical problems
    #   logGam <- sapply(1:k, function(r) {
    #     log(piLambda[r]) + log(detRho[r]) - 0.5 *
    #       sum((Y[i, ] %*% rhoLambda[, , r] - X[i, ] %*% phiLambda[, , r])^2)
    #   })
    #
    #   #logGam <- logGam - max(logGam) #adjust without changing proportions -> change the LLH
    #   gam <- exp(logGam)
    #   norm_fact <- sum(gam)
    #   sumLogLLH <- sumLogLLH + log(norm_fact) - m/2* log(2 * base::pi)
    # }
    #llhLambda <- c(-sumLogLLH/n, (dimension + m + 1) * k - 1)
    list(phi = phiLambda, rho = rhoLambda, pi = piLambda, llh = LLH, affec = affec, proba = proba)
  }

  # For each lambda, computation of the parameters
  out <-
    if (ncores > 1) {
      parallel::parLapply(cl, 1:length(S), computeAtLambda)
    } else {
      lapply(1:length(S), computeAtLambda)
    }

  if (ncores > 1)
    parallel::stopCluster(cl)

  out
}
