#' Function used to quick visualize the fitted values (survival estimate) on the data used to fit the model (via EM algorithm or Gibbs).
#'
#' `plot_fit_on_data()` estimates survival/hazard for the data the model was fitted on and plots the results.
#'
#' @param model A `survival_ln_mixture` or `survival_ln_mixture_em` object.
#'
#' @param data A `data.frame()` or `tibble()` containing the data used to fit the model. For appropriate behavior, should be the same object used to generate survival_ln_mixture/survival_ln_mixture_em objects.
#'
#' @param type A character string specifying the type of plot. The default is "survival", but can be "hazard".
#'
#' @param interval A character string specifying the type of interval to be plotted. The default is "none", but can be "credible". The EM algorithm does not provide confidence intervals and this parameter is only support for the Bayesian version (`survival_ln_mixture` object).
#'
#' @param level A numeric value between 0 and 1 specifying the level of the confidence interval. The default is 0.95.
#'
#' @returns A list with two objects, one ggplot (`$ggplot`) with the predictions plotted against the empirical data and a tibble with the predictions (`$preds`).
#'
#' @export
plot_fit_on_data <- function(model, data, type = "survival", interval = "none",
                             level = 0.95) {
  # Checks
  if (!inherits(model, "survival_ln_mixture_em") &
    !inherits(model, "survival_ln_mixture")) {
    stop("The object should be a survival_ln_mixture_em or a survival_ln_mixture object.")
  }

  if (inherits(model, "survival_ln_mixture_em") &
    inherits(model, "survival_ln_mixture")) {
    stop("The object should just one of two: survival_ln_mixture_em or survival_ln_mixture. Can't be both at the sime time.")
  }

  if (!type %in% c("survival", "hazard")) {
    stop("The type should be either 'survival' or 'hazard'.")
  }

  # Check level numeric
  if (!is.numeric(level)) {
    stop("The level should be a numeric value.")
  }

  # Check level
  if (level < 0 | level > 1) {
    stop("The level should be between 0 and 1.")
  }

  # -----
  # Predictions
  # -----
  data <- tibble::tibble(data)
  form <- model$blueprint$formula
  form <- stats::as.formula(paste0(paste(form)[[2]], " ~ ", paste(form)[[3]]))
  vars <- strsplit(paste(form)[[3]], " \\+ ")[[1]]
  if (!model$blueprint$intercept) {
    vars <- vars[vars != "0"]
  }

  # starting variables
  time <- estimate <- .eval_time <- .pred_survival <- .pred_hazard <- .pred_upper <- .pred_lower <- credible_ribbon <- facet_chain <- line_layer <- step_layer <- new_data <- hazard_estimate <- NA

  # empirical values
  km <- survival::survfit(form, data) |>
    broom::tidy()

  if (all(vars != "NULL")) {
    new_data <- data |>
      dplyr::select(dplyr::all_of(vars)) |>
      dplyr::distinct() |>
      append_strata_column()
  } else {
    new_data <- NA
  }

  # which is the type of model fitted
  if (inherits(model, "survival_ln_mixture_em")) {
    # warn that the interval and level will be ignored
    if (interval != "none") {
      warning("The EM algorithm does not provide confidence intervals. It will be ignored.")
    }

    if (all(vars != "NULL")) { # fit with covariates
      preds <- NULL
      for (i in 1:nrow(new_data)) {
        preds <- dplyr::bind_rows(
          preds,
          stats::predict(model,
            new_data[i, names(new_data)[names(new_data) != "strata"]],
            type = type,
            eval_time = km$time[km$strata == new_data$strata[i]]
          ) |>
            tidyr::unnest(.pred)
        )
      }

      preds_joined <- dplyr::left_join(km, preds,
        by = c(
          "time" = ".eval_time",
          "strata"
        )
      )
    } else { # intercept only fit
      preds <- stats::predict(model,
        data.frame(val = NA),
        type = type,
        eval_time = km$time
      ) |>
        tidyr::unnest(.pred)

      preds_joined <- dplyr::left_join(km, preds,
        by = c("time" = ".eval_time")
      )
    }

    if (type == "survival") {
      if (all(vars != "NULL")) { # fit with covariates
        gg <- ggplot(preds_joined) + # basic layer comparing empirical and fitted
          geom_step(aes(x = time, y = estimate, color = strata), alpha = 0.5) + # empirical data layer
          geom_line(aes(x = time, y = .pred_survival, color = strata)) + # predicted data layer
          theme_bw() +
          labs(x = "Time", y = "Survival")
      } else { # intercept only fit
        gg <- ggplot(preds_joined) +
          geom_step(aes(x = time, y = estimate), alpha = 0.5) + # empirical data layer
          geom_line(aes(x = time, y = .pred_survival)) + # predicted data layer
          theme_bw() +
          labs(x = "Time", y = "Survival")
      }
    } else { # type = 'hazard'
      km <- join_empirical_hazard(km)

      if (all(vars != "NULL")) { # fit with covariates
        preds_joined <- dplyr::left_join(km, preds,
          by = c(
            "time" = ".eval_time",
            "strata"
          )
        )

        gg <- ggplot(preds_joined) +
          geom_line(aes(x = time, y = hazard_estimate, color = strata), alpha = 0.5) + # empirical data layer
          geom_line(aes(x = time, y = .pred_hazard, color = strata)) + # predicted data layer
          theme_bw() +
          labs(x = "Time", y = "Hazard")
      } else { # intercept only fit
        preds_joined <- dplyr::left_join(km, preds,
          by = c("time" = ".eval_time")
        )

        gg <- ggplot(preds_joined) +
          geom_line(aes(x = time, y = hazard_estimate), alpha = 0.5) + # empirical data layer
          geom_line(aes(x = time, y = .pred_hazard)) + # predicted data layer
          theme_bw() +
          labs(x = "Time", y = "Hazard")
      }
    }
  } else { # type is 'survival_ln_mixture' (Bayesian)
    nchain <- posterior::nchains(model$posterior)
    credible_ribbon <- NULL
    facet_chain <- NULL

    if (interval == "credible") {
      if (all(vars != "NULL")) {
        credible_ribbon <- geom_ribbon(aes(
          x = time, ymin = .pred_lower, ymax = .pred_upper,
          fill = strata
        ), alpha = 0.3)
      } else {
        credible_ribbon <- geom_ribbon(aes(x = time, ymin = .pred_lower, ymax = .pred_upper),
          alpha = 0.3
        )
      }
    }

    if (nchain > 1) {
      facet_chain <- facet_wrap(~chain)
    }

    if (all(vars != "NULL")) {
      if (nchain > 1) {
        km_chain <- NULL

        for (c in 1:nchain) {
          model_chain <- model
          model_chain$posterior <- posterior::subset_draws(model$posterior, chain = c)

          preds <- NULL

          for (i in 1:nrow(new_data)) {
            preds <- dplyr::bind_rows(
              preds,
              stats::predict(model_chain,
                new_data[i, names(new_data)[names(new_data) != "strata"]],
                type = type,
                eval_time = km$time[km$strata == new_data$strata[i]],
                interval = interval,
                level = level
              ) |>
                tidyr::unnest(.pred)
            )
          }

          if (type == "survival") {
            preds_joined <- dplyr::left_join(km, preds,
              by = c(
                "time" = ".eval_time",
                "strata"
              )
            ) |>
              dplyr::mutate(chain = c)
          } else { # type is hazard
            preds_joined <- dplyr::left_join(join_empirical_hazard(km), preds,
              by = c(
                "time" = ".eval_time",
                "strata"
              )
            ) |>
              dplyr::mutate(chain = c)
          }

          km_chain <- dplyr::bind_rows(km_chain, preds_joined)
        }

        preds_joined <- km_chain
      } else { # 1 chain
        preds <- NULL

        for (i in 1:nrow(new_data)) {
          preds <- dplyr::bind_rows(
            preds,
            stats::predict(model,
              new_data[i, names(new_data)[names(new_data) != "strata"]],
              type = type,
              eval_time = km$time[km$strata == new_data$strata[i]],
              interval = interval,
              level = level
            ) |>
              tidyr::unnest(.pred)
          )
        }

        if (type == "survival") {
          preds_joined <- dplyr::left_join(km, preds,
            by = c(
              "time" = ".eval_time",
              "strata"
            )
          )
        } else { # type is hazard
          preds_joined <- dplyr::left_join(join_empirical_hazard(km), preds,
            by = c(
              "time" = ".eval_time",
              "strata"
            )
          )
        }
      }
    } else { # intercept only fit
      if (nchain > 1) {
        km_chain <- NULL

        for (c in 1:nchain) {
          model_chain <- model
          model_chain$posterior <- posterior::subset_draws(model$posterior, chain = c)

          if (type == "survival") {
            preds_joined <- dplyr::left_join(km,
              stats::predict(model_chain,
                data.frame(val = NA),
                type = type,
                eval_time = km$time,
                interval = interval,
                level = level
              ) |>
                tidyr::unnest(.pred),
              by = c("time" = ".eval_time")
            ) |>
              dplyr::mutate(chain = c)
          } else {
            preds_joined <- dplyr::left_join(join_empirical_hazard(km),
              stats::predict(model_chain,
                data.frame(val = NA),
                type = type,
                eval_time = km$time,
                interval = interval,
                level = level
              ) |>
                tidyr::unnest(.pred),
              by = c("time" = ".eval_time")
            ) |>
              dplyr::mutate(chain = c)
          }

          km_chain <- dplyr::bind_rows(km_chain, preds_joined)
        }

        preds_joined <- km_chain
      } else { # 1 chain
        if (type == "survival") {
          preds_joined <- dplyr::left_join(km,
            stats::predict(model,
              data.frame(val = NA),
              type = type,
              eval_time = km$time,
              interval = interval,
              level = level
            ) |>
              tidyr::unnest(.pred),
            by = c("time" = ".eval_time")
          )
        } else { # type is hazard
          preds_joined <- dplyr::left_join(join_empirical_hazard(km),
            stats::predict(model,
              data.frame(val = NA),
              type = type,
              eval_time = km$time,
              interval = interval,
              level = level
            ) |>
              tidyr::unnest(.pred),
            by = c("time" = ".eval_time")
          )
        }
      }
    }

    if (type == "survival") {
      labs_gg <- labs(x = "Time", y = "Survival")

      if (all(vars != "NULL")) {
        step_layer <- geom_step(aes(x = time, y = estimate, color = strata), alpha = 0.5)
        line_layer <- geom_line(aes(x = time, y = .pred_survival, color = strata))
      } else {
        step_layer <- geom_step(aes(x = time, y = estimate), alpha = 0.5)
        line_layer <- geom_line(aes(x = time, y = .pred_survival))
      }
    } else { # type = 'hazard'
      labs_gg <- labs(x = "Time", y = "Hazard")

      if (all(vars != "NULL")) {
        step_layer <- geom_line(aes(x = time, y = hazard_estimate, color = strata), alpha = 0.5)
        line_layer <- geom_line(aes(x = time, y = .pred_hazard, color = strata))
      } else {
        step_layer <- geom_line(aes(x = time, y = hazard_estimate), alpha = 0.5)
        line_layer <- geom_line(aes(x = time, y = .pred_hazard))
      }
    }

    gg <- ggplot(preds_joined) +
      step_layer +
      line_layer +
      credible_ribbon +
      labs_gg +
      theme_bw() +
      facet_chain
  }

  return(list(
    preds = preds_joined,
    ggplot = gg
  ))
}

append_strata_column <- function(new_data) {
  new_data$strata <- survival::strata(new_data)
  return(new_data)
}
