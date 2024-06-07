#' selectVariables
#'
#' For a given lambda, construct the sets of relevant variables for each cluster.
#'
#' @param phiInit an initial estimator for phi (size: p*m*k)
#' @param rhoInit an initial estimator for rho (size: m*m*k)
#' @param piInit an initial estimator for pi (size : k)
#' @param gamInit an initial estimator for gamma
#' @param mini  minimum number of iterations in EM algorithm
#' @param maxi  maximum number of iterations in EM algorithm
#' @param gamma  power in the penalty
#' @param glambda grid of regularization parameters
#' @param X    matrix of regressors
#' @param Y    matrix of responses
#' @param thresh real, threshold to say a variable is relevant, by default = 1e-8
#' @param eps   threshold to say that EM algorithm has converged
#' @param ncores Number or cores for parallel execution (1 to disable)
#' @param fast boolean to enable or not the C function call
#'
#' @return a list, varying lambda in a grid, with selected (the indices of variables that are selected),
#' Rho (the covariance parameter, reparametrized), Pi (the proportion parameter)
#'
#' @export
selectVariables <- function(phiInit, rhoInit, piInit, gamInit, mini, maxi, gamma,
  glambda, X, Y, thresh = 1e-08, eps, ncores = 3, fast)
{
  if (ncores > 1) {
    cl <- parallel::makeCluster(ncores, outfile = "")
    parallel::clusterExport(cl = cl, varlist = c("phiInit", "rhoInit", "gamInit",
      "mini", "maxi", "glambda", "X", "Y", "thresh", "eps"), envir = environment())
  }

  # Computation for a fixed lambda
  computeCoefs <- function(lambda)
  {
    params <- EMGLLF(phiInit, rhoInit, piInit, gamInit, mini, maxi, gamma, lambda,
      X, Y, eps, fast)

    p <- ncol(X)
    m <- ncol(Y)

    # selectedVariables: list where element j contains vector of selected variables
    # in [1,m]
    selectedVariables <- lapply(1:p, function(j) {
      # from boolean matrix mxk of selected variables obtain the corresponding boolean
      # m-vector, and finally return the corresponding indices
      if (m>1) {
        seq_len(m)[apply(abs(params$phi[j, , ]) > thresh, 1, any)]
      } else {
        if (any(params$phi[j, , ] > thresh))
          1
        else
          numeric(0)
      }
    })

    list(selected = selectedVariables, Rho = params$rho, Pi = params$pi)
  }

  # For each lambda in the grid, we compute the coefficients
  out <-
    if (ncores > 1) {
      parLapply(cl, glambda, computeCoefs)
    } else {
      lapply(glambda, computeCoefs)
    }
  if (ncores > 1)
    parallel::stopCluster(cl)

  # Suppress models which are computed twice
  # sha1_array <- lapply(out, digest::sha1) out[ duplicated(sha1_array) ]
  selec <- lapply(out, function(model) model$selected)
  ind_dup <- duplicated(selec)
  ind_uniq <- which(!ind_dup)
  out2 <- list()
  for (l in 1:length(ind_uniq))
    out2[[l]] <- out[[ind_uniq[l]]]
  out2
}
