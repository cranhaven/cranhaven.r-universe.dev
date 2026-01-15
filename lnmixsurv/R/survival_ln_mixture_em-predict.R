#' Predict from a lognormal_em Mixture Model fitted using EM algorithm.
#'
#' @param object A `survival_ln_mixture_em` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"survival"` for the survival probability.
#' - `"hazard"` for the hazard theoretical hazard.
#'
#' @param eval_time For type = "hazard" or type = "survival", the times for the distribution.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @note Categorical predictors must be converted to factors before the fit,
#' otherwise the predictions will fail.
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @export
predict.survival_ln_mixture_em <- function(object, new_data, type,
                                           eval_time, ...) {
  if (as.character(object$blueprint$formula)[3] != "NULL") {
    new_data <- append_strata_column(new_data)
  }

  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_survival_ln_mixture_em_predict_types())

  predict_survival_ln_mixture_em_bridge(
    type, object, forged$predictors,
    eval_time, new_data, ...
  )
}

valid_survival_ln_mixture_em_predict_types <- function() {
  c("survival", "hazard")
}

# ------------------------------------------------------------------------------
# Bridge
predict_survival_ln_mixture_em_bridge <- function(type, model, predictors,
                                                  eval_time, new_data, ...) {
  predictors <- as.matrix(predictors)

  predict_function <- get_survival_ln_mixture_em_predict_function(type)
  predictions <- predict_function(model, predictors, eval_time, new_data, ...)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

get_survival_ln_mixture_em_predict_function <- function(type) {
  switch(type,
    survival = predict_survival_ln_mixture_em_survival,
    hazard = predict_survival_ln_mixture_em_hazard
  )
}

# ------------------------------------------------------------------------------
# Implementation
predict_survival_ln_mixture_em_time <- function(model, predictors, eval_time, new_data) {
  rlang::abort("Not implemented")
}

predict_survival_ln_mixture_em_survival <- function(model, predictors, eval_time, new_data) {
  extract_surv_haz_em(model, predictors, eval_time, "survival", new_data)
}

predict_survival_ln_mixture_em_hazard <- function(model, predictors, eval_time, new_data) {
  extract_surv_haz_em(model, predictors, eval_time, "hazard", new_data)
}

extract_surv_haz_em <- function(model, predictors, eval_time, type = "survival", new_data) {
  rlang::arg_match(type, c("survival", "hazard"))

  strata <- NULL

  if (as.character(model$blueprint$formula)[3] != "NULL") {
    strata <- new_data$strata
  }

  last_row <- model$em_iterations[nrow(model$em_iterations), -ncol(model$em_iterations)]

  beta <- matrix(
    as.numeric(last_row[
      !startsWith(names(last_row), "eta") & !(startsWith(names(last_row), "phi"))
    ]),
    ncol = length(model$mixture_groups)
  )

  phi <- as.numeric(last_row[startsWith(names(last_row), "phi")])
  eta <- as.numeric(last_row[startsWith(names(last_row), "eta")])

  sigma <- 1 / sqrt(phi)

  m <- apply(beta,
    MARGIN = 2,
    FUN = function(x) predictors %*% x
  )

  if (type == "survival") {
    out <- list()
    if (nrow(predictors) > 1) {
      for (r in 1:nrow(predictors)) {
        out_r <- tibble::tibble(
          .eval_time = eval_time,
          .pred_survival = NA
        )

        out_r$.pred_survival <- as.numeric(predict_survival_em_cpp(eval_time, m, sigma, eta, r))

        out[[r]] <- out_r
      }
    } else {
      out_r <- tibble::tibble(
        .eval_time = eval_time,
        .pred_survival = NA
      )

      out_r$.pred_survival <- as.numeric(
        predict_survival_em_cpp(eval_time, t(as.matrix(m)), sigma, eta, 1)
      )

      out[[1]] <- out_r
    }
  } else if (type == "hazard") {
    out <- list()
    if (nrow(predictors) > 1) {
      for (r in 1:nrow(predictors)) {
        out_r <- tibble::tibble(
          .eval_time = eval_time,
          .pred_hazard = NA
        )

        out_r$.pred_hazard <- as.numeric(predict_hazard_em_cpp(eval_time, m, sigma, eta, r))

        out[[r]] <- out_r
      }
    } else {
      out_r <- tibble::tibble(
        .eval_time = eval_time,
        .pred_hazard = NA
      )

      out_r$.pred_hazard <- as.numeric(predict_hazard_em_cpp(eval_time, t(as.matrix(m)), sigma, eta, 1))

      out[[1]] <- out_r
    }
  }

  if (!is.null(strata)) {
    tibble_out <- tibble::tibble(
      .pred = out,
      strata = strata
    )
  } else {
    tibble_out <- tibble::tibble(.pred = out)
  }

  return(tibble_out)
}

append_strata_column <- function(new_data) {
  new_data$strata <- survival::strata(new_data)
  return(new_data)
}
