globalVariables(".pred")

#' Tidying method for a Lognormal Mixture model.
#'
#' These method tidy the estimates from `survival_ln_mixture` fits into a summary.
#'
#' @param x Fitted model object (survival_ln_mixture).
#' @param effects A character vector including one or more of `"fixed"` and `"auxiliary`.
#' @param conf.int If `TRUE` columns for lower (`cred.low`) and upper (`cred.high`) bounds
#' of the posterior uncertainty intervals are included.
#' @param conf.level A number between 0 and 1 indicating the desired probability mass to include in the
#' intervals. Only used if `conf.int = TRUE`.
#' @param digits How many significant digits should be displayed?
#' @param ... Not used.
#'
#' @return
#'
#' A `data.frame` without rownames. When `effects="fixed"` (the default), tidy.survival_ln_mixutre
#' returns one row for each coefficient for each component of the mixture with three columns:
#' \item{term}{The name of the corresponding term in the model.}
#' \item{estimate}{A point estimate of the coefficient (posterior median).}
#' \item{std.error}{A standard error for the point estimate based on
#' \code{\link[stats]{mad}}. See the \emph{Uncertainty estimates} section in
#' \code{\link[rstanarm]{print.stanreg}} for more details.}
#'
#' Setting \code{effects="auxiliary"} will select the precision and proportion of mixture components parameters.
#' 
#' @examples
#'
#' require(survival)
#' lung$sex <- factor(lung$sex)
#' set.seed(1)
#' mod2 <- survival_ln_mixture(Surv(time, status == 2) ~ sex, lung)
#' tidy(mod2)
#' tidy(mod2, conf.int = TRUE)
#' tidy(mod2, effects = c("fixed", "auxiliary"), conf.int = TRUE)
#'
#' @export
tidy.survival_ln_mixture <- function(x, # nolint: object_name_linter.
                                     effects = "fixed",
                                     conf.int = FALSE, # nolint: object_name_linter.
                                     conf.level = 0.9, # nolint: object_name_linter.
                                     digits = NULL,
                                     ...) {
  rlang::arg_match(effects, c("fixed", "auxiliary"))
  rlang::check_dots_empty(...)
  all_vars <- colnames(x$posterior)
  vars <- c()

  if ("fixed" %in% effects) {
    for (i in x$mixture_groups) {
      vars <- c(vars, colnames(posterior::subset_draws(
        x$posterior,
        paste0(x$predictors_name, "_", i)
      )))
    }
  }

  if ("auxiliary" %in% effects) {
    auxiliary_vars <- all_vars[startsWith(all_vars, "phi")]
    auxiliary_vars <- c(
      auxiliary_vars,
      all_vars[startsWith(all_vars, "eta")]
    )
    vars <- c(vars, auxiliary_vars)
  }

  measures <- c("estimate" = stats::median, "std.error" = stats::mad)
  if (conf.int) {
    measures <- c(measures, c("interval" = function(x) credibility_interval(x, conf.level)))
  }

  post <- posterior::subset_draws(x$posterior, variable = vars)
  post <- posterior::merge_chains(post)
  ret <- posterior::summarise_draws(post, measures)
  names(ret)[1] <- "term"
  ret
}

#' Tidying method for a Lognormal Mixture model (fitted via Expectation-Maximization algorithm).
#'
#' These method tidy the estimates from `survival_ln_mixture` fits into a short summary. It doesn't contain uncertainty estimates since it's a likelihood maximization algorithm.
#'
#' @param x Fitted model object (survival_ln_mixture_em).
#' @param effects A character vector including one or more of `"fixed"` and `"auxiliary`.
#' @param digits How many significant digits should be displayed?
#' @param ... Not used.
#'
#' @return
#'
#' A `data.frame` without rownames. When `effects="fixed"` (the default), tidy.survival_ln_mixutre
#' returns one row for each coefficient for each component of the mixture with two columns:
#' \item{term}{The name of the corresponding term in the model.}
#' \item{estimate}{A point estimate of the coefficient (last iteration value).}
#'
#' Setting \code{effects="auxiliary"} will select the precision and proportion of mixture components parameters.
#' 
#' @examples
#'
#' require(survival)
#' lung$sex <- factor(lung$sex)
#' set.seed(1)
#' mod2 <- survival_ln_mixture_em(Surv(time, status == 2) ~ sex, lung)
#' tidy(mod2)
#' tidy(mod2, effects = c("fixed", "auxiliary"))
#'
#' @export
tidy.survival_ln_mixture_em <- function(x, # nolint: object_name_linter.
                                     effects = "fixed",
                                     digits = NULL,
                                     ...) {
  rlang::arg_match(effects, c("fixed", "auxiliary"))
  rlang::check_dots_empty(...)
  all_vars <- colnames(x$em_iterations)
  vars <- c()
  
  if ("fixed" %in% effects) {
    for (i in x$mixture_groups) {
      vars <- c(vars, paste0(x$predictors_name, "_", i))
    }
  }
  
  if ("auxiliary" %in% effects) {
    auxiliary_vars <- all_vars[startsWith(all_vars, "phi")]
    auxiliary_vars <- c(
      auxiliary_vars,
      all_vars[startsWith(all_vars, "eta")]
    )
    vars <- c(vars, auxiliary_vars)
  }
  
  estimate <- dplyr::select(x$em_iterations, dplyr::all_of(vars)) |> 
    dplyr::slice(nrow(x$em_iterations)) |> 
    as.numeric()
  
  return(tibble::tibble(term = vars,
                        estimate = estimate))
}

#' Funcao auxiliar para calcular intervalo de credibilidade usando quantis.
#' @noRd
credibility_interval <- function(x, conf.level) { # nolint: object_name_linter.
  ret <- unname(stats::quantile(x, probs = c(1 - conf.level, conf.level)))
  return(c("cred.low" = ret[1], "cred.high" = ret[2]))
}

#' Augment data with information from a survival_ln_mixture object
#'
#' Include information about hazard and survival distribution for each individual
#' in a dataset.
#'
#' @param x A `survival_ln_mixture` object.
#' @param newdata A `base::data.frame()` or `tibble::tiblle()` containing all
#' the original predictors used to create x.
#' @param eval_time a vector with the times where the hazard and survival distribuition
#' will be evaluated.
#' @param ... Not used.
#'
#' @return A `tibble::tibble()` with the original covariates and ther survvial and hazard
#' distributions.
#'
#' @export
augment.survival_ln_mixture <- function(x, newdata, eval_time, ...) {
  haz <- predict.survival_ln_mixture(x, newdata, type = "hazard", eval_time = eval_time)
  surv <- predict.survival_ln_mixture(x, newdata, type = "survival", eval_time = eval_time)
  haz <- dplyr::rename(haz, .hazard = .pred)
  surv <- dplyr::rename(surv, .survival = .pred)
  return(dplyr::bind_cols(tibble::as_tibble(newdata), .hazard = haz, .survival = surv))
}

#' Augment data with information from a survival_ln_mixture_em object
#'
#' Include information about hazard and survival distribution for each individual
#' in a dataset.
#' @param x A `survival_ln_mixture_em` object.
#' @param newdata A `base::data.frame()` or `tibble::tiblle()` containing all
#' the original predictors used to create x.
#' @param eval_time a vector with the times where the hazard and survival distribuition
#' will be evaluated.
#' @param ... Not used.
#'
#' @return A `tibble::tibble()` with the original covariates and ther survvial and hazard
#' distributions.
#'
#' @export
augment.survival_ln_mixture_em <- function(x, newdata, eval_time, ...) {
  haz <- predict.survival_ln_mixture_em(x, newdata, type = "hazard", eval_time = eval_time)
  surv <- predict.survival_ln_mixture_em(x, newdata, type = "survival", eval_time = eval_time)
  haz <- dplyr::rename(haz, .hazard = .pred)
  surv <- dplyr::rename(surv, .survival = .pred)
  return(dplyr::bind_cols(tibble::as_tibble(newdata), .hazard = haz, .survival = surv))
}
