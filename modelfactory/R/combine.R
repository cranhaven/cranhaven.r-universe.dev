#' Combine model metrics for n number of lm, glm, and lmer models
#'
#' `stack_metrics()` calculates basic model metrics like MSE for the models
#' passed in, then stacks them in a dataframe for comparison. This supports
#' lm, glm, and lmer models, and different metrics are calculated for each.
#' This does not perform model selection based on a given criteria, but it
#' makes the tedious task of, say, comparing R-squared across several models
#' very easy.
#'
#' @param ... lm, glm, or lmer models to summarize and combine.
#'
#' @return A [tibble()] that includes a variety of evaluation metrics.
#' @export
#'
#' @examples
#' # lm example -------------------------------------------
#' lm_1 = lm(mpg ~ cyl + disp + hp, data = mtcars)
#' lm_2 = lm(mpg ~ hp + drat + wt, data = mtcars)
#' lm_3 = lm(mpg ~ ., data = mtcars)
#' lm_combined = stack_metrics(lm_1, lm_2, lm_3)
#' lm_combined
#'
#' # glm example ------------------------------------------
#' glm_1 = glm(vs ~ drat + hp, data = mtcars)
#' glm_2 = glm(vs ~ wt + qsec, data = mtcars)
#' glm_3 = glm(vs ~ ., data = mtcars)
#' glm_combined = stack_metrics(glm_1, glm_2, glm_3)
#' glm_combined
#'
#' # lme4 example -----------------------------------------
#' lmer_1 = lme4::lmer(Sepal.Length ~ (1 | Species), data = iris)
#' lmer_2 = lme4::lmer(Sepal.Length ~ (1 | Species) + Petal.Length, data = iris)
#' lmer_combined = stack_metrics(lmer_1, lmer_2)
#' lmer_combined
stack_metrics = function(...) {

  models = list(...)
  model_sum = lapply(models, summary)

  if (length(models) == 0) {
    stop("No models were passed into the function.")
  }

  # make sure every item is an lm summary
  if (model_type_check(model_sum, "summary.lm")) {
    # combine everything into a tibble to output
    tibble::tibble(
      model = as.character(get_metric(model_sum, "call")),
      r.squared = get_metric(model_sum, "r.squared"),
      adj.r.squared = get_metric(model_sum, "adj.r.squared"),
      MSE = calc_metric(model_sum, "MSE"),
      RMSE = calc_metric(model_sum, "RMSE"),
      MAE = calc_metric(model_sum, "MAE")
    )
  } else if (model_type_check(model_sum, "summary.glm")) {
    # data frame but for glm objects
    tibble::tibble(
      model = as.character(get_metric(model_sum, "call")),
      deviance = get_metric(model_sum, "deviance"),
      AIC = get_metric(model_sum, "aic"),
      BIC = unlist(lapply(models, function(x) stats::BIC(x)))
    )
  } else if (model_type_check(model_sum, "summary.merMod")) {
    # data frame but for lmer objects
    tibble::tibble(
      model = as.character(get_metric(model_sum, "call")),
      deviance = -2 * get_metric(model_sum, "logLik"),
      AIC = unlist(lapply(models, function(x) stats::AIC(x))),
      BIC = unlist(lapply(models, function(x) stats::BIC(x)))
    )
  } else {
    stop("Model type not yet supported! Make sure all of your models are of
         the same type.")
  }
}

#' Stack coefficents, confidence intervals, and standard errors for n models.
#'
#' `stack_coeff()` takes several lm or glm models, pulls out their coefficients,
#' standard errors, and confidence intervals, and stacks everything into a
#' [tibble()] for easy comparison across models.
#'
#' @param ... lm or glm models to summarize and combine.
#' @param ci width of confidence, default = 0.95.
#'
#' @return A [tibble()] with coefficients, confidence intervals, and standard
#' errors.
#' @export
#'
#' @examples
#' # multiple lm example ----------------------------------
#' lm_1 = lm(mpg ~ cyl + disp + hp, data = mtcars)
#' lm_2 = lm(mpg ~ hp + drat + wt, data = mtcars)
#' lm_3 = lm(mpg ~ ., data = mtcars)
#' lm_combined = stack_coeff(lm_1, lm_2, lm_3)
#' lm_combined
#'
#' # sometimes you might just want 1 model's summary ------
#' single_lm = stack_coeff(lm_1)
#' single_lm
#'
#' # glm example ------------------------------------------
#' glm_1 = glm(vs ~ drat + hp, data = mtcars)
#' glm_2 = glm(vs ~ wt + qsec, data = mtcars)
#' glm_3 = glm(vs ~ ., data = mtcars)
#' glm_combined = stack_coeff(glm_1, glm_2, glm_3)
#' glm_combined
stack_coeff = function(..., ci = 0.95) {

  models = list(...)
  output = data.frame()

  if (length(models) == 0) {
    stop("No models were passed into the function.")
  } else if (!model_type_check(models, "lm", summary = FALSE) &
             !model_type_check(models, "glm", summary = FALSE)) {
    stop("Model type not yet supported! Try lm or glm models.")
  }

  # iteratively build dataframes since we're not summarizing them individually
  for (model in models) {
    temp_data = data.frame(model_name = as.character(model$call)[2],
                           summary(model)$coef[, c('Estimate',
                                                   'Std. Error',
                                                   'Pr(>|t|)')],
                           stats::confint(model, level = ci)) |>
                tibble::rownames_to_column(var = 'coefficient')
    output = dplyr::bind_rows(output, temp_data)
  }

  # rename things and output a tibble()
  new_names = c("coefficient", "model_name", "estimate", "std_error",
            "p_value", "lower_ci", "upper_ci")
  names(output) = new_names
  output = tibble::tibble(output)

  return(output)
}



