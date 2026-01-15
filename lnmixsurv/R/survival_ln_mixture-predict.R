#' Predict from a Lognormal Mixture Model
#'
#' @param object A `survival_ln_mixture` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"time"` for the survival time. **not implmeented**
#' - `"survival"` for the survival probability.
#' - `"hazard"` for the hazard.
#'
#' @param eval_time For type = "hazard" or type = "survival", the times for the distribution.
#'
#' @param interval should interval estimates be added? Options are "none" and "credible".
#'
#' @param level the tail area of the intervals. Default value is 0.95.
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
#' @examples
#'
#' # Categorical variables must be converted to factor before the fit.
#' 
#' require(survival)
#' # Wrong way of doing
#' set.seed(1)
#' mod <- survival_ln_mixture(Surv(time, status == 2) ~ factor(sex), lung, intercept = TRUE)
#' 
#' \dontrun{
#' # this piece of code will throw error
#' predict(mod, data.frame(sex = 1), type = "survival", eval_time = 100)
#' }
#' 
#' # Correct way
#' lung$sex <- factor(lung$sex) # converting to factor before
#' set.seed(1)
#' mod2 <- survival_ln_mixture(Surv(time, status == 2) ~ sex, lung, intercept = TRUE)
#' # Note: the categorical predictors must be a character.
#' predict(mod2, data.frame(sex = "1"), type = "survival", eval_time = 100)
#'
#' @export
predict.survival_ln_mixture <- function(object, new_data, type, eval_time, interval = "none", level = 0.95, ...) {
  if (as.character(object$blueprint$formula)[3] != "NULL") {
    new_data <- append_strata_column(new_data)
  }

  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_survival_ln_mixture_predict_types())

  predict_survival_ln_mixture_bridge(type, object, forged$predictors, eval_time, interval, level, new_data, ...)
}

valid_survival_ln_mixture_predict_types <- function() {
  c("time", "survival", "hazard")
}

# ------------------------------------------------------------------------------
# Bridge

predict_survival_ln_mixture_bridge <- function(type, model, predictors, eval_time, interval, level, new_data, ...) {
  predictors <- as.matrix(predictors)

  predict_function <- get_survival_ln_mixture_predict_function(type)
  predictions <- predict_function(model, predictors, eval_time, interval, level, new_data, ...)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

get_survival_ln_mixture_predict_function <- function(type) {
  switch(type,
    time = predict_survival_ln_mixture_time,
    survival = predict_survival_ln_mixture_survival,
    hazard = predict_survival_ln_mixture_hazard
  )
}

# ------------------------------------------------------------------------------
# Implementation

predict_survival_ln_mixture_time <- function(model, predictors, eval_time, interval, level, new_data) {
  rlang::abort("Not implemented")
}

predict_survival_ln_mixture_survival <- function(model, predictors, eval_time, interval, level, new_data) {
  extract_surv_haz(model, predictors, eval_time, interval, level, "survival", new_data)
}

predict_survival_ln_mixture_hazard <- function(model, predictors, eval_time, interval, level, new_data) {
  extract_surv_haz(model, predictors, eval_time, interval, level, "hazard", new_data)
}

extract_surv_haz <- function(model, predictors, eval_time, interval = "none",
                             level = 0.95, type = "survival", new_data) {
  rlang::arg_match(type, c("survival", "hazard"))
  rlang::arg_match(interval, c("none", "credible"))

  strata <- NULL

  if (as.character(model$blueprint$formula)[3] != "NULL") {
    strata <- new_data$strata
  }

  post <- posterior::merge_chains(model$posterior)

  if (type == "survival") {
    out <- list()
    if (nrow(predictors) > 1) {
      for (r in 1:nrow(predictors)) {
        out_r <- tibble::tibble(
          .eval_time = eval_time
        )

        beta <- lapply(model$mixture_groups, function(x) {
          names <- paste0(model$predictors_name, "_", x)
          return(posterior::subset_draws(post, names))
        })

        phi <- posterior::subset_draws(post, "phi", regex = TRUE)
        eta <- posterior::subset_draws(post, "eta", regex = TRUE)
        sigma <- sqrt(1 / phi)

        preds <- as.data.frame(predict_survival_gibbs_cpp(
          eval_time, predictors[r, ],
          beta, sigma, eta,
          interval == "credible", level
        ))


        if (interval == "credible") {
          colnames(preds) <- c(".pred_survival", ".pred_lower", ".pred_upper")
        } else {
          colnames(preds) <- c(".pred_survival")
        }

        out[[r]] <- dplyr::bind_cols(out_r, preds)
      }
    } else {
      out_r <- tibble::tibble(
        .eval_time = eval_time
      )

      beta <- lapply(model$mixture_groups, function(x) {
        names <- paste0(model$predictors_name, "_", x)
        return(posterior::subset_draws(post, names))
      })

      phi <- posterior::subset_draws(post, "phi", regex = TRUE)
      eta <- posterior::subset_draws(post, "eta", regex = TRUE)
      sigma <- sqrt(1 / phi)

      preds <- as.data.frame(predict_survival_gibbs_cpp(
        eval_time, predictors[1, ],
        beta, sigma, eta,
        interval == "credible", level
      ))

      if (interval == "credible") {
        colnames(preds) <- c(".pred_survival", ".pred_lower", ".pred_upper")
      } else {
        colnames(preds) <- c(".pred_survival")
      }

      out[[1]] <- dplyr::bind_cols(out_r, preds)
    }
  } else { # type == 'hazard'
    out <- list()
    if (nrow(predictors) > 1) {
      for (r in 1:nrow(predictors)) {
        out_r <- tibble::tibble(
          .eval_time = eval_time
        )

        beta <- lapply(model$mixture_groups, function(x) {
          names <- paste0(model$predictors_name, "_", x)
          return(posterior::subset_draws(post, names))
        })

        phi <- posterior::subset_draws(post, "phi", regex = TRUE)
        eta <- posterior::subset_draws(post, "eta", regex = TRUE)
        sigma <- sqrt(1 / phi)

        preds <- as.data.frame(predict_hazard_gibbs_cpp(
          eval_time, predictors[r, ],
          beta, sigma, eta,
          interval == "credible", level
        ))

        if (interval == "credible") {
          colnames(preds) <- c(".pred_hazard", ".pred_lower", ".pred_upper")
        } else {
          colnames(preds) <- c(".pred_hazard")
        }

        out[[r]] <- dplyr::bind_cols(out_r, preds)
      }
    } else {
      out_r <- tibble::tibble(
        .eval_time = eval_time
      )

      beta <- lapply(model$mixture_groups, function(x) {
        names <- paste0(model$predictors_name, "_", x)
        return(posterior::subset_draws(post, names))
      })

      phi <- posterior::subset_draws(post, "phi", regex = TRUE)
      eta <- posterior::subset_draws(post, "eta", regex = TRUE)
      sigma <- sqrt(1 / phi)

      preds <- as.data.frame(predict_hazard_gibbs_cpp(
        eval_time, predictors[1, ],
        beta, sigma, eta,
        interval == "credible", level
      ))

      if (interval == "credible") {
        colnames(preds) <- c(".pred_hazard", ".pred_lower", ".pred_upper")
      } else {
        colnames(preds) <- c(".pred_hazard")
      }

      out[[1]] <- dplyr::bind_cols(out_r, preds)
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
