#' Get Details of a Basket Trial Simulation
#'
#' @template design
#' @template dotdotdot
#'
#' @return A list containing the rejection probabilites, posterior means, mean
#' squared errors of all baskets and the family-wise error rate. For some
#' methods the mean limits of HDI intervals are also returned.
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' get_details(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   epsilon = 2, tau = 0, iter = 100)
get_details <- function(design, ...) {
  UseMethod("get_details", design)
}

#' Get Details of a BMA Basket Trial Simulation
#'
#' @template design_bma
#' @template n
#' @template p1
#' @template lambda
#' @template pmp0
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A list containing the rejection probabilites, posterior means,
#' and mean squared errors for all baskets as well as the family-wise error
#' rate.
#' @export
#'
#' @examples
#' design <- setup_bma(k = 3, p0 = 0.2)
#' get_details(design = design, n = 20, p1 = 0.5, lambda = 0.95, pmp0 = 1,
#'   iter = 100)
get_details.bma <- function(design, n, p1 = NULL, lambda, pmp0,
                            iter = 1000, data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)
  targ <- design$p0 == p1

  res <- foreach::foreach(i = 1:nrow(data), .combine = 'cfun2',
                          .options.future = list(seed = TRUE)) %dofuture% {
    res_temp <- suppressWarnings(bmabasket::bma(pi0 = design$p0, y = data[i, ],
      n = rep(n, design$k), pmp0 = pmp0))
    list(
      ifelse(as.vector(res_temp$bmaProbs) > lambda, 1, 0),
      as.vector(res_temp$bmaMeans)
    )
  }
  list(
    Rejection_Probabilities = colMeans(res[[1]]),
    FWER = mean(apply(res[[1]], 1, function(x) any(x[targ] == 1))),
    Mean = colMeans(res[[2]]),
    MSE = colMeans(t(t(res[[2]]) - p1)^2)
  )
}

#' Get Details of a Basket Trial Simulation with the MML Design
#'
#' @template design_cpp
#' @template n
#' @template p1
#' @template lambda
#' @template level
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A list containing the rejection probabilites, posterior means,
#' mean squared errors and mean limits of HDI intervals for all baskets as well
#' as the family-wise error rate.
#' @export
#'
#' @examples
#' design <- setup_mml(k = 3, p0 = 0.2)
#' get_details(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   tune_a = 1, tune_b = 1, iter = 100)
get_details.mml <- function(design, n, p1 = NULL, lambda, level = 0.95,
                            iter = 1000, data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)

  targ <- design$p0 == p1
  weights <- get_weights_mml(design, n = n, ...)

  res <- foreach::foreach(i = 1:nrow(data), .combine = 'cfun1') %dofuture% {
    shape_loop <- beta_borrow_pp(design = design, n = n, r = data[i, ],
      weights = weights)
    res_loop <- ifelse(post_beta(shape_loop, design$p0) >= lambda, 1, 0)
    mean_loop <- apply(shape_loop, 2, function(x) x[1] / (x[1] + x[2]))
    hdi_loop <- apply(shape_loop, 2, function(x) HDInterval::hdi(stats::qbeta,
      shape1 = x[1], shape2 = x[2], credMass = level))
    list(res_loop, mean_loop, hdi_loop[1, ], hdi_loop[2, ])
  }
  list(
    Rejection_Probabilities = colMeans(res[[1]]),
    FWER = mean(apply(res[[1]], 1, function(x) any(x[targ] == 1))),
    Mean = colMeans(res[[2]]),
    MSE = colMeans(t(t(res[[2]]) - p1)^2),
    Lower_CL = colMeans(res[[3]]),
    Upper_CL = colMeans(res[[4]]),
    ECD = mean(rowSums(t(apply(res[[1]], 1, function(x) x != targ))))
  )
}

#' Get Details of a Basket Trial Simulation with the Global MML Design
#'
#' @template design_mmlglobal
#' @template n
#' @template p1
#' @template lambda
#' @template level
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A list containing the rejection probabilites, posterior means,
#' mean squared errors and mean limits of HDI intervals for all baskets as well
#' as the family-wise error rate.
#' @export
#'
#' @examples
#' design <- setup_mmlglobal(k = 3, p0 = 0.2)
#' get_details(design = design, n = 20, p1 = 0.5, lambda = 0.95, iter = 100)
get_details.mmlglobal <- function(design, n, p1 = NULL, lambda, level = 0.95,
                                  iter = 1000, data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)
  targ <- design$p0 == p1

  res <- foreach::foreach(i = 1:nrow(data), .combine = 'cfun1',
                          .options.future = list(seed = TRUE)) %dofuture% {
    shape_loop <- weight_mmlglobal(design = design, n = n, r = data[i, ])
    res_loop <- ifelse(post_beta(shape_loop, design$p0) >= lambda, 1, 0)
    mean_loop <- apply(shape_loop, 2, function(x) x[1] / (x[1] + x[2]))
    hdi_loop <- apply(shape_loop, 2, function(x) HDInterval::hdi(stats::qbeta,
      shape1 = x[1], shape2 = x[2], credMass = level))
    list(res_loop, mean_loop, hdi_loop[1, ], hdi_loop[2, ])
  }

  list(
    Rejection_Probabilities = colMeans(res[[1]]),
    FWER = mean(apply(res[[1]], 1, function(x) any(x[targ] == 1))),
    Mean = colMeans(res[[2]]),
    MSE = colMeans(t(t(res[[2]]) - p1)^2),
    Lower_CL = colMeans(res[[3]]),
    Upper_CL = colMeans(res[[4]]),
    ECD = mean(rowSums(t(apply(res[[1]], 1, function(x) x != targ))))
  )
}

#' Get Details of a BHM Basket Trial Simulation
#'
#' @template design_bhm
#' @template n
#' @template p1
#' @template lambda
#' @template level
#' @template tau_bhm
#' @template iter
#' @template n_mcmc
#' @template data_bhm
#' @template dotdotdot
#'
#' @return A list containing the rejection probabilites, posterior means,
#' mean squared errors and mean limits of HDI intervals for all baskets as well
#' as the family-wise error rate.
#' @export
#'
#' @examples
#' design <- setup_bhm(k = 3, p0 = 0.2, p_target = 0.5)
#' \donttest{get_details(design = design, n = 20, p1 = c(0.2, 0.5, 0.5),
#'   lambda = 0.95, tau_scale = 1, iter = 100)}
get_details.bhm <- function(design, n, p1 = NULL, lambda, level = 0.95,
                            tau_scale, iter = 1000, n_mcmc = 10000,
                            data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_bhmbasket(data = data, design = design, n = n, p = p1,
    iter = iter)
  targ <- design$p0 == p1

  analyses <- suppressMessages(bhmbasket::performAnalyses(
    scenario_list = data,
    evidence_levels = c(lambda, 1 - level),
    method_names = "berry",
    target_rates = rep(design$p_target, design$k),
    prior_parameters_list = bhmbasket::setPriorParametersBerry(
      mu_mean = design$mu_mean,
      mu_sd = design$mu_sd,
      tau_scale = tau_scale
    ),
    n_mcmc_iterations = n_mcmc
  ))

  br <- paste0("c(", paste0("x[", 1:design$k, "] > ", design$p0,
    collapse = ", "), ")")
  res <- bhmbasket::getGoDecisions(
    analyses_list = analyses,
    cohort_names = paste("p", 1:design$k, sep = "_"),
    evidence_levels = rep(lambda, design$k),
    boundary_rules = str2lang(br)
  )$scenario_1$decisions_list$berry[, -1]

  est <- bhmbasket::getEstimates(analyses, point_estimator = "mean",
    alpha_level = (1 - level))$berry

  list(
    Rejection_Probabilities = unname(colMeans(res)),
    FWER = mean(apply(res, 1, function(x) any(x[targ] == 1))),
    Mean = unname(est[, 1]),
    MSE = unname(est[, 7]),
    Lower_CL = unname(est[, 3]),
    Upper_CL = unname(est[, 5]),
    ECD = mean(rowSums(t(apply(res, 1, function(x) x != targ))))
  )
}

#' Get Details of a Basket Trial Simulation with the EXNEX Design
#'
#' @template design_exnex
#' @template n
#' @template p1
#' @template lambda
#' @template level
#' @template tau_exnex
#' @template w_exnex
#' @template iter
#' @template n_mcmc
#' @template data_bhm
#' @template dotdotdot
#'
#' @return A list containing the rejection probabilites, posterior means,
#' mean squared errors and mean limits of HDI intervals for all baskets as well
#' as the family-wise error rate.
#' @export
#'
#' @examples
#' \donttest{design <- setup_exnex(k = 3, p0 = 0.2)
#' get_details(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   tau_scale = 1, w = 0.5, iter = 100)}
get_details.exnex <- function(design, n, p1 = NULL, lambda, level = 0.95,
                              tau_scale, w, iter = 1000, n_mcmc = 10000,
                              data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_bhmbasket(data = data, design = design, n = n, p = p1,
    iter = iter)
  targ <- design$p0 == p1

  analyses <- suppressMessages(bhmbasket::performAnalyses(
    scenario_list = data,
    evidence_levels = c(lambda, 1 - level),
    method_names = "exnex",
    prior_parameters_list = bhmbasket::setPriorParametersExNex(
      mu_mean = design$mu_mean,
      mu_sd = design$mu_sd,
      tau_scale = tau_scale,
      mu_j = rep(design$basket_mean, design$k),
      tau_j = rep(design$basket_sd, design$k),
      w_j = w
    ),
    n_mcmc_iterations = n_mcmc
  ))

  br <- paste0("c(", paste0("x[", 1:design$k, "] > ", design$p0,
    collapse = ", "), ")")
  res <- bhmbasket::getGoDecisions(
    analyses_list = analyses,
    cohort_names = paste("p", 1:design$k, sep = "_"),
    evidence_levels = rep(lambda, design$k),
    boundary_rules = str2lang(br)
  )$scenario_1$decisions_list$exnex[, -1]

  est <- bhmbasket::getEstimates(analyses, point_estimator = "mean",
    alpha_level = (1 - level))$exnex

  list(
    Rejection_Probabilities = unname(colMeans(res)),
    FWER = mean(apply(res, 1, function(x) any(x[targ] == 1))),
    Mean = unname(est[, 1]),
    MSE = unname(est[, 7]),
    Lower_CL = unname(est[, 3]),
    Upper_CL = unname(est[, 5]),
    ECD = mean(rowSums(t(apply(res, 1, function(x) x != targ))))
  )
}

#' Get Details of a Basket Trial Simulation with Fujikawa's Design
#'
#' @template design_fujikawa
#' @template n
#' @template p1
#' @template lambda
#' @template level
#' @template tuning_fujikawa
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A list containing the rejection probabilites, posterior means,
#' mean squared errors and mean limits of HDI intervals for all baskets as well
#' as the family-wise error rate and the experiment-wise power.
#' @export
#'
#' @examples
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' get_details(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   epsilon = 2, tau = 0, iter = 100)
get_details.fujikawa <- function(design, n, p1 = NULL, lambda, level = 0.95,
                                 epsilon, tau, logbase = 2, iter = 1000,
                                 data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)

  targ <- design$p0 == p1
  not_targ <- design$p0 != p1
  weights <- get_weights_jsd(design = design, n = n, epsilon = epsilon,
    tau = tau, logbase = logbase)

  res <- foreach::foreach(i = 1:nrow(data), .combine = 'cfun1') %dofuture% {
      shape_loop <- beta_borrow_fujikawa(design = design, n = n, r = data[i, ],
        weights = weights)
      res_loop <- ifelse(post_beta(shape_loop, design$p0) >= lambda, 1, 0)
      mean_loop <- apply(shape_loop, 2, function(x) x[1] / (x[1] + x[2]))
      hdi_loop <- apply(shape_loop, 2, function(x) HDInterval::hdi(stats::qbeta,
        shape1 = x[1], shape2 = x[2], credMass = level))
      list(res_loop, mean_loop, hdi_loop[1, ], hdi_loop[2, ])
  }

  list(
    Rejection_Probabilities = colMeans(res[[1]]),
    FWER = mean(apply(res[[1]], 1, function(x) any(x[targ] == 1))),
    EWP = mean(apply(res[[1]], 1, function(x) any(x[not_targ] == 1))),
    Mean = colMeans(res[[2]]),
    MSE = colMeans(t(t(res[[2]]) - p1)^2),
    Lower_CL = colMeans(res[[3]]),
    Upper_CL = colMeans(res[[4]]),
    ECD = mean(rowSums(t(apply(res[[1]], 1, function(x) x != targ))))
  )
}

#' Get Details of a Basket Trial Simulation with the Power Prior Design
#' Based on Global JSD Weights
#'
#' @template design_jsdglobal
#' @template n
#' @template p1
#' @template lambda
#' @template level
#' @template tuning_jsdglobal
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A list containing the rejection probabilites, posterior means,
#' mean squared errors and mean limits of HDI intervals for all baskets as well
#' as the family-wise error rate.
#' @export
#'
#' @examples
#' design <- setup_jsdglobal(k = 3, p0 = 0.2)
#' get_details(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   eps_pair = 2, eps_all = 2, iter = 100)
get_details.jsdglobal <- function(design, n, p1 = NULL, lambda, level = 0.95,
                                  eps_pair, tau = 0, eps_all, logbase = 2,
                                  iter = 1000, data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)

  weights_pair <- get_weights_jsd(design = design, n = n, epsilon = eps_pair,
    tau = tau, logbase = logbase)
  targ <- design$p0 == p1

  res <- foreach::foreach(i = 1:nrow(data), .combine = 'cfun1') %dofuture% {
      shape_loop <- beta_borrow_jsdglobal(design = design, n = n, r = data[i, ],
        weights_pair = weights_pair, eps_all = eps_all)
      res_loop <- ifelse(post_beta(shape_loop, design$p0) >= lambda, 1, 0)
      mean_loop <- apply(shape_loop, 2, function(x) x[1] / (x[1] + x[2]))
      hdi_loop <- apply(shape_loop, 2, function(x) HDInterval::hdi(stats::qbeta,
        shape1 = x[1], shape2 = x[2], credMass = level))
      list(res_loop, mean_loop, hdi_loop[1, ], hdi_loop[2, ])
  }

  list(
    Rejection_Probabilities = colMeans(res[[1]]),
    FWER = mean(apply(res[[1]], 1, function(x) any(x[targ] == 1))),
    Mean = colMeans(res[[2]]),
    MSE = colMeans(t(t(res[[2]]) - p1)^2),
    Lower_CL = colMeans(res[[3]]),
    Upper_CL = colMeans(res[[4]]),
    ECD = mean(rowSums(t(apply(res[[1]], 1, function(x) x != targ))))
  )
}

#' Get Details of a Basket Trial Simulation with the Calibrated Power Prior
#' Design
#'
#' @template design_cpp
#' @template n
#' @template p1
#' @template lambda
#' @template level
#' @template tuning_cpp
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A list containing the rejection probabilites, posterior means,
#' mean squared errors and mean limits of HDI intervals for all baskets as well
#' as the family-wise error rate.
#' @export
#'
#' @examples
#' design <- setup_cpp(k = 3, p0 = 0.2)
#' get_details(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   tune_a = 1, tune_b = 1, iter = 100)
get_details.cpp <- function(design, n, p1 = NULL, lambda, level = 0.95,
                            tune_a, tune_b, iter = 1000, data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)

  targ <- design$p0 == p1
  weights <- get_weights_cpp(n = n, tune_a = tune_a, tune_b = tune_b)

  res <- foreach::foreach(i = 1:nrow(data), .combine = 'cfun1') %dofuture% {
      shape_loop <- beta_borrow_pp(design = design, n = n, r = data[i, ],
        weights = weights)
      res_loop <- ifelse(post_beta(shape_loop, design$p0) >= lambda, 1, 0)
      mean_loop <- apply(shape_loop, 2, function(x) x[1] / (x[1] + x[2]))
      hdi_loop <- apply(shape_loop, 2, function(x) HDInterval::hdi(stats::qbeta,
        shape1 = x[1], shape2 = x[2], credMass = level))
      list(res_loop, mean_loop, hdi_loop[1, ], hdi_loop[2, ])
    }
  list(
    Rejection_Probabilities = colMeans(res[[1]]),
    FWER = mean(apply(res[[1]], 1, function(x) any(x[targ] == 1))),
    Mean = colMeans(res[[2]]),
    MSE = colMeans(t(t(res[[2]]) - p1)^2),
    Lower_CL = colMeans(res[[3]]),
    Upper_CL = colMeans(res[[4]]),
    ECD = mean(rowSums(t(apply(res[[1]], 1, function(x) x != targ))))
  )
}

#' Get Details of a Basket Trial Simulation with the Global Calibrated
#' Power Prior Design
#'
#' @template design_cppglobal
#' @template n
#' @template p1
#' @template lambda
#' @template level
#' @template tuning_cppglobal
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A list containing the rejection probabilites, posterior means,
#' mean squared errors and mean limits of HDI intervals for all baskets as well
#' as the family-wise error rate.
#' @export
#'
#' @examples
#' design <- setup_cppglobal(k = 3, p0 = 0.2)
#' get_details(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   tune_a = 1, tune_b = 1, epsilon = 2, iter = 100)
get_details.cppglobal <- function(design, n, p1 = NULL, lambda, level = 0.95,
                                  tune_a, tune_b, epsilon, iter = 1000,
                                  data = NULL, ...) {
  p1 <- check_p1(design = design, p1 = p1, data = data)
  check_params(n = n, lambda = lambda, iter = iter)
  data <- check_data_matrix(data = data, design = design, n = n, p = p1,
    iter = iter)

  targ <- design$p0 == p1
  weights_pair <- get_weights_cpp(n = n, tune_a = tune_a, tune_b = tune_b)

  res <- foreach::foreach(i = 1:nrow(data), .combine = 'cfun1') %dofuture% {
    shape_loop <- beta_borrow_cppglobal(design = design, n = n, r = data[i, ],
      weights_pair = weights_pair, epsilon = epsilon)
    res_loop <- ifelse(post_beta(shape_loop, design$p0) >= lambda, 1, 0)
    mean_loop <- apply(shape_loop, 2, function(x) x[1] / (x[1] + x[2]))
    hdi_loop <- apply(shape_loop, 2, function(x) HDInterval::hdi(stats::qbeta,
      shape1 = x[1], shape2 = x[2], credMass = level))
    list(res_loop, mean_loop, hdi_loop[1, ], hdi_loop[2, ])
  }
  list(
    Rejection_Probabilities = colMeans(res[[1]]),
    FWER = mean(apply(res[[1]], 1, function(x) any(x[targ] == 1))),
    Mean = colMeans(res[[2]]),
    MSE = colMeans(t(t(res[[2]]) - p1)^2),
    Lower_CL = colMeans(res[[3]]),
    Upper_CL = colMeans(res[[4]]),
    ECD = mean(rowSums(t(apply(res[[1]], 1, function(x) x != targ))))
  )
}
