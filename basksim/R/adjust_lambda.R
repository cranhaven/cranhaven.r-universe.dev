#' Adjust Lambda
#'
#' @template design
#' @template dotdotdot
#'
#' @details The default method for \code{adjust_lambda} uses a combination
#' of \code{\link{uniroot}} and grid search and calls \code{\link{toer}}
#' in every iteration. For methods implemented in the \code{bhmbasket} package
#' there are separate methods that are computationally more efficient.
#'
#' @return A list containing the greatest estimated value for \code{lambda} with
#' \code{prec_digits} decimal places which controls the family wise error rate
#' at level \code{alpha} (one-sided) and the estimated family wise error rate
#' for the estimated \code{lambda}.
#' @export
#'
#' @examples
#' design <- setup_cpp(k = 3, p0 = 0.2)
#' adjust_lambda(design = design, n = 20, alpha = 0.05,
#'   design_params = list(tune_a = 1, tune_b = 1), iter = 1000)
adjust_lambda <- function(design, ...) {
  UseMethod("adjust_lambda", design)
}

#' Adjust Lambda
#'
#' @template design
#' @template n
#' @template p1_toer
#' @template alpha
#' @template design_params
#' @template iter
#' @template prec_digits
#' @template dotdotdot
#' @template data
#'
#' @details It is recommended to use \code{data} and then use the same simulated
#' data set for all further calculations. If \code{data = NULL} then
#' new data is generated in each step of the algorithm, so \code{lambda} doesn't
#' necessarily protect the family wise error rate for different simulated data
#' due to Monte Carlo simulation error.
#'
#' @return A list containing the greatest estimated value for \code{lambda} with
#' \code{prec_digits} decimal places which controls the family wise error rate
#' at level \code{alpha} (one-sided) and the estimated family wise error rate
#' for the estimated \code{lambda}.
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' adjust_lambda(design = design, n = 20, alpha = 0.05,
#'   design_params = list(epsilon = 2, tau = 0), iter = 1000)
adjust_lambda.default <- function(design, n, p1 = NULL, alpha = 0.05,
                          design_params = list(), iter = 1000, prec_digits = 3,
                          data = NULL, ...) {
  if (is.null(p1)) p1 <- rep(design$p0, design$k)
  upper_lim <- 1 - 10^(-prec_digits)
  root_fun <- function(x) do.call(toer, args = c(design = list(design),
    n = n, p1 = list(p1), lambda = x, design_params, iter = iter,
    data = list(data),
    ...)) - alpha

  # Use uniroot to find lambda close to the smallest lambda that protects
  # the significance level at alpha
  uni_root <- stats::uniroot(root_fun, interval = c(0.5, upper_lim),
    tol = 10^(-prec_digits))

  if (uni_root$f.root > 0 ) {
    # If rejection prob is greater than alpha after uniroot, round lambda up
    root <- ceiling(uni_root$root * 10^(prec_digits)) / 10^(prec_digits)
    val <- root_fun(root)
    if (val > 0) {
      # If rejection prob is still above alpha, increase lambda
      while (val > 0) {
        root <- root + 10^(-prec_digits)
        val <- root_fun(root)
      }
    }
  } else {
    # If rejection prob is less than alpha after uniroot, round lambda down
    root <- floor(uni_root$root * 10^(prec_digits)) / 10^(prec_digits)
    val <- root_fun(root)
    if (val > 0) {
      # If the rejection prob is greater now than alpha with the rounded-down
      # lambda, then round lambda up
      root <- ceiling(uni_root$root * 10^(prec_digits)) / 10^(prec_digits)
      val <- root_fun(root)
    } else {
      # If the rejection prob is still below alpha, decrease lambda
      repeat {
        root_old <- root
        val_old <- val
        root <- root - 10^(-prec_digits)
        val <- root_fun(root)
        if (val > 0) {
          root <- root_old
          val <- val_old
          break
        }
      }
    }
  }
  list(
    lambda = root,
    toer = val + alpha
  )
}

#' Adjust Lambda for the EXNEX Design
#'
#' @template design
#' @template n
#' @template p1
#' @template alpha
#' @template design_params
#' @template iter
#' @template n_mcmc
#' @template prec_digits
#' @template data
#' @template dotdotdot
#'
#' @return A list containing the greatest estimated value for \code{lambda} with
#' \code{prec_digits} decimal places which controls the family wise error rate
#' at level \code{alpha} (one-sided) and the estimated family wise error rate
#' for the estimated \code{lambda}.
#' @export
#'
#' @examples
#' design <- setup_exnex(k = 3, p0 = 0.2)
#' \donttest{adjust_lambda(design = design, n = 15,
#'   design_params = list(tau_scale = 1, w = 0.5), iter = 100, n_mcmc = 5000)}
adjust_lambda.exnex <- function(design, n, p1 = NULL, alpha = 0.05,
                                design_params = list(), iter = 1000,
                                n_mcmc = 10000, prec_digits = 3, data = NULL,
                                ...) {
  if (is.null(p1)) p1 <- rep(design$p0, design$k)
  data <- check_data_bhmbasket(data = data, design = design, n = n, p = p1,
    iter = iter)
  levels <- seq(1 - alpha - 0.1, 0.999, by = 10^(-prec_digits))

  prior_parameters <- do.call(bhmbasket::setPriorParametersExNex,
    args = c(mu_mean = design$mu_mean, mu_sd = design$mu_sd,
      mu_j = list(rep(design$basket_mean, design$k)),
      tau_j = list(rep(design$basket_sd, design$k)), design_params))

  analyses <- suppressMessages(do.call(bhmbasket::performAnalyses,
    list(
      scenario_list = data,
      evidence_levels = levels,
      method_names = "exnex",
      prior_parameters_list = prior_parameters,
      n_mcmc_iterations = n_mcmc
    )))

  br <- paste0("c(", paste0("x[", 1:design$k, "] > ", design$p0,
    collapse = ", "), ")")
  alphabhm <- function(x) {
    res <- bhmbasket::getGoDecisions(
      analyses_list = analyses,
      cohort_names = paste("p", 1:design$k, sep = "_"),
      evidence_levels = rep(x, design$k),
      boundary_rules = str2lang(br)
    )
    mean(res$scenario_1$decisions_list$exnex[, 1])
  }

  fwers <- sapply(levels, alphabhm)
  ind <- which.max(fwers <= alpha)

  list(
    lambda = levels[ind],
    toer = fwers[ind]
  )
}

#' Adjust Lambda for the BHM Design
#'
#' @template design
#' @template n
#' @template p1
#' @template alpha
#' @template design_params
#' @template iter
#' @template n_mcmc
#' @template prec_digits
#' @template data
#' @template dotdotdot
#'
#' @return A list containing the greatest estimated value for \code{lambda} with
#' \code{prec_digits} decimal places which controls the family wise error rate
#' at level \code{alpha} (one-sided) and the estimated family wise error rate
#' for the estimated \code{lambda}.
#' @export
#'
#' @examples
#' design <- setup_bhm(k = 3, p0 = 0.2, p_target = 0.5)
#' \donttest{adjust_lambda(design = design, n = 15,
#'   design_params = list(tau_scale = 1), iter = 100, n_mcmc = 5000)}
adjust_lambda.bhm <- function(design, n, p1 = NULL, alpha = 0.05,
                              design_params = list(), iter = 1000,
                              n_mcmc = 10000, prec_digits = 3, data = NULL,
                              ...) {
  if (is.null(p1)) p1 <- rep(design$p0, design$k)
  data <- check_data_bhmbasket(data = data, design = design, n = n, p = p1,
    iter = iter)
  levels <- seq(1 - alpha - 0.1, 0.999, by = 10^(-prec_digits))

  prior_parameters <- do.call(bhmbasket::setPriorParametersBerry,
    args = c(mu_mean = design$mu_mean, mu_sd = design$mu_sd, design_params))

  analyses <- suppressMessages(do.call(bhmbasket::performAnalyses,
    list(
      scenario_list = data,
      evidence_levels = levels,
      method_names = "berry",
      target_rates = rep(design$p_target, design$k),
      prior_parameters_list = prior_parameters,
      n_mcmc_iterations = n_mcmc
    )))

  br <- paste0("c(", paste0("x[", 1:design$k, "] > ", design$p0,
    collapse = ", "), ")")
  alphabhm <- function(x) {
    res <- bhmbasket::getGoDecisions(
      analyses_list = analyses,
      cohort_names = paste("p", 1:design$k, sep = "_"),
      evidence_levels = rep(x, design$k),
      boundary_rules = str2lang(br)
    )
    mean(res$scenario_1$decisions_list$berry[, 1])
  }

  fwers <- sapply(levels, alphabhm)
  ind <- which.max(fwers <= alpha)

  list(
    lambda = levels[ind],
    toer = fwers[ind]
  )
}
