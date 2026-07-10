#' Tune Penalized PLSMM
#'
#' This function tunes a penalized partial linear semiparametric mixed-model (PLSMM) by performing a grid search
#' over a set of hyperparameters to find the best model based on a given criterion.
#'
#' @param x A matrix of predictors.
#' @param y A continuous vector of response variable.
#' @param series A variable representing different series or groups in the data modeled as a random intercept.
#' @param t A numeric vector indicating the time points.
#' @param name_group_var A character string specifying the name of the grouping variable.
#' @param bases A matrix of bases functions.
#' @param gamma_vec A vector of values for the regularization parameter for the coefficients of the nonlinear functions.
#' @param lambda_vec A vector of values for the regularization parameter for the coefficients of the fixed effects.
#' @param timexgroup Logical indicating whether to use a time-by-group interaction. 
#'                   If \code{TRUE}, each group in \code{name_group_var} will have its own estimate of the time effect.
#' @param criterion A character string specifying the criterion to be optimized (\code{'BIC'}, \code{'BICC'}, \code{'EBIC'}).
#' @param ... Additional arguments to be passed to the \code{plsmm_lasso} function.
#'
#' @return A PLSMM object representing the best-tuned model based on the specified criterion.
#'
#' @details This function performs a grid search over the hyperparameters specified by \code{lambda_vec}
#' and \code{gamma_vec} to find the best-fitted PLSMM based on the given criterion. It fits PLSMMs using the
#' \code{plsmm_lasso} function for each combination of hyperparameters and retains only the models that
#' have converged. The best model is selected based on the minimum value of the specified criterion.
#'
#' @seealso \code{\link{plsmm_lasso}}
#' @examples
#' set.seed(123)
#'data_sim <- simulate_group_inter(
#'  N = 50, n_mvnorm = 3, grouped = TRUE,
#'  timepoints = 3:5, nonpara_inter = TRUE,
#'  sample_from = seq(0, 52, 13),
#'  cos = FALSE, A_vec = c(1, 1.5)
#')
#' sim = data_sim$sim
#' x = as.matrix(sim[,-1:-3])
#' y = sim$y
#' series = sim$series
#' t = sim$t
#' bases = create_bases(t)
#' lambdas <- c(0.0046, 0.0001)
#' gammas <- 0.00000001
#' tuned_plsmm <- tune_plsmm(x, y, series, t,
#'                       name_group_var = "group", bases$bases,
#'                       gamma_vec = gammas, lambda_vec = lambdas, timexgroup = TRUE,
#'                       criterion = "BIC"
#' )
#'
#' @export
tune_plsmm <- function(x, y, series, t, name_group_var, bases,
                      gamma_vec, lambda_vec, timexgroup, criterion, ...) {
  grid_hyper <- expand.grid(lambda_vec, gamma_vec)
  plsmm_models <- vector("list", nrow(grid_hyper))

  for (i in 1:nrow(grid_hyper)) {
    lambda_i <- grid_hyper[i, ][[1]]
    gamma_i <- grid_hyper[i, ][[2]]

    plsmm_models[[i]] <- plsmm_lasso(
      x = x, y = y, series = series, t = t,
      name_group_var = name_group_var, bases = bases,
      gamma = gamma_i, lambda = lambda_i,
      timexgroup = timexgroup, criterion = criterion, ...
    )
  }

  plsmm_models <- plsmm_models[sapply(plsmm_models, function(x) x$converged)]

  best_plsmm <- which.min(sapply(plsmm_models, function(x) x$crit))

  return(plsmm_models[[best_plsmm]])
}
