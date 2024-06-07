#' runValse
#'
#' Main function
#'
#' @param X matrix of covariates (of size n*p)
#' @param Y matrix of responses (of size n*m)
#' @param procedure among 'LassoMLE' or 'LassoRank'
#' @param selecMod method to select a model among 'DDSE', 'DJump', 'BIC' or 'AIC'
#' @param gamma integer for the power in the penaly, by default = 1
#' @param mini integer, minimum number of iterations in the EM algorithm, by default = 10
#' @param maxi integer, maximum number of iterations in the EM algorithm, by default = 100
#' @param eps real, threshold to say the EM algorithm converges, by default = 1e-4
#' @param kmin integer, minimum number of clusters, by default = 2
#' @param kmax integer, maximum number of clusters, by default = 10
#' @param rank.min integer, minimum rank in the low rank procedure, by default = 1
#' @param rank.max integer, maximum rank in the low rank procedure, by default = 5
#' @param ncores_outer Number of cores for the outer loop on k
#' @param ncores_inner Number of cores for the inner loop on lambda
#' @param thresh real, threshold to say a variable is relevant, by default = 1e-8
#' @param grid_lambda, a vector with regularization parameters if known, by default numeric(0)
#' @param size_coll_mod (Maximum) size of a collection of models, by default 50
#' @param fast TRUE to use compiled C code, FALSE for R code only
#' @param verbose TRUE to show some execution traces
#' @param plot TRUE to plot the selected models after run
#'
#' @return
#' The selected model (except if the collection of models
#' has less than 11 models, the function returns the collection as it can not select one using Capushe)
#'
#' @examples
#' n = 50; m = 10; p = 5
#' beta = array(0, dim=c(p,m,2))
#' beta[,,1] = 1
#' beta[,,2] = 2
#' data = generateXY(n, c(0.4,0.6), rep(0,p), beta, diag(0.5, p), diag(0.5, m))
#' X = data$X
#' Y = data$Y
#' res = runValse(X, Y, kmax = 5, plot=FALSE)
#' X <- matrix(runif(100), nrow=50)
#' Y <- matrix(runif(100), nrow=50)
#' res = runValse(X, Y, plot=FALSE)
#'
#' @export
runValse <- function(X, Y, procedure = "LassoMLE", selecMod = "DDSE", gamma = 1, mini = 10,
  maxi = 50, eps = 1e-04, kmin = 2, kmax = 3, rank.min = 1, rank.max = 5, ncores_outer = 1,
  ncores_inner = 1, thresh = 1e-08, grid_lambda = numeric(0), size_coll_mod = 50,
  fast = TRUE, verbose = FALSE, plot = TRUE)
{
  n <- nrow(X)
  p <- ncol(X)
  m <- ncol(Y)

  if (verbose) print("main loop: over all k and all lambda")

  if (ncores_outer > 1) {
    cl <- parallel::makeCluster(ncores_outer, outfile = "")
    parallel::clusterExport(cl = cl, envir = environment(), varlist = c("X",
      "Y", "procedure", "selecMod", "gamma", "mini", "maxi", "eps", "kmin",
      "kmax", "rank.min", "rank.max", "ncores_outer", "ncores_inner", "thresh",
      "size_coll_mod", "verbose", "p", "m"))
  }

  # Compute models with k components
  computeModels <- function(k) {
    if (ncores_outer > 1)
      require("valse") #nodes start with an empty environment

    if (verbose)
      print(paste("Parameters initialization for k =", k))
    # smallEM initializes parameters by k-means and regression model in each
    # component, doing this 20 times, and keeping the values maximizing the
    # likelihood after 10 iterations of the EM algorithm.
    P <- initSmallEM(k, X, Y, fast)
    if (length(grid_lambda) == 0) {
      grid_lambda <- computeGridLambda(P$phiInit, P$rhoInit, P$piInit, P$gamInit,
                                       X, Y, gamma, mini, maxi, eps, fast)
    }
    if (length(grid_lambda) > size_coll_mod)
      grid_lambda <- grid_lambda[seq(1, length(grid_lambda), length.out = size_coll_mod)]

    if (verbose)
      print("Compute relevant parameters")
    # select variables according to each regularization parameter from the grid:
    # S$selected corresponding to selected variables
    S <- selectVariables(P$phiInit, P$rhoInit, P$piInit, P$gamInit, mini, maxi,
      gamma, grid_lambda, X, Y, thresh, eps, ncores_inner, fast)

    if (procedure == "LassoMLE") {
      if (verbose)
        print("run the procedure Lasso-MLE")
      # compute parameter estimations, with the Maximum Likelihood Estimator,
      # restricted on selected variables.
      models <- constructionModelesLassoMLE(P$phiInit, P$rhoInit, P$piInit,
        P$gamInit, mini, maxi, gamma, X, Y, eps, S, ncores_inner, fast, verbose)
    } else {
      if (verbose)
        print("run the procedure Lasso-Rank")
      # compute parameter estimations, with the Low Rank Estimator, restricted on
      # selected variables.
      models <- constructionModelesLassoRank(S, k, mini, maxi, X, Y, eps, rank.min,
        rank.max, ncores_inner, fast, verbose)
    }
    # warning! Some models are NULL after running selectVariables
    models <- models[sapply(models, function(cell) !is.null(cell))]
    models
  }

  # List (index k) of lists (index lambda) of models
  models_list <-
    if (ncores_outer > 1) {
      parallel::parLapply(cl, kmin:kmax, computeModels)
    } else {
      lapply(kmin:kmax, computeModels)
    }
  if (ncores_outer > 1) parallel::stopCluster(cl)

  if (!requireNamespace("capushe", quietly = TRUE)) {
    warning("'capushe' not available: returning all models")
    return(models_list)
  }

  # Get summary 'tableauRecap' from models
  tableauRecap <- do.call(rbind, lapply(seq_along(models_list), function(i) {
    models <- models_list[[i]]
    # For a collection of models (same k, several lambda):
    LLH <- sapply(models, function(model) model$llh[1])
    k <- length(models[[1]]$pi)
    sumPen <- sapply(models, function(model) k * (dim(model$rho)[1] + sum(model$phi[,,1] != 0) + 1) - 1)
    data.frame(model = paste(i, ".", seq_along(models), sep = ""), pen = sumPen/n, complexity = sumPen, contrast = -LLH)
  }))
  tableauRecap <- tableauRecap[which(tableauRecap[, 4] != Inf), ]
  if (verbose) print(tableauRecap)

  if (nrow(tableauRecap) > 10) {
    modSel <- capushe::capushe(tableauRecap, n)
    indModSel <- if (selecMod == "DDSE") {
      as.numeric(modSel@DDSE@model)
    } else if (selecMod == "Djump") {
      as.numeric(modSel@Djump@model)
    } else if (selecMod == "BIC") {
      modSel@BIC_capushe$model
    } else if (selecMod == "AIC") {
      modSel@AIC_capushe$model
    }
    listMod <- as.integer(unlist(strsplit(as.character(indModSel), "[.]")))
    modelSel <- models_list[[listMod[1]]][[listMod[2]]]
    modelSel$models <- tableauRecap

    if (plot) plot_valse(X, Y, modelSel)
    return(modelSel)
  }
  tableauRecap
}
