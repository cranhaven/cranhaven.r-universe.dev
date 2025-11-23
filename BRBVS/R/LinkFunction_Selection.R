#' Select Best Link Function for Bivariate Copula Survival Models
#'
#' Selects the best link function for bivariate copula survival models based on AIC or BIC measures.
#' It evaluates different margins (link functions) for the survival models and selects the one with the lowest AIC or BIC.
#'
#' @param data A data frame containing the dataset.
#' @param cens1 Censoring indicator for the first time to event.
#' @param cens2 Censoring indicator for the second time to event.
#' @param lowerBt1 Character. Name of the lower bound for the first time to event.
#' @param lowerBt2 Character. Name of the lower bound for the second time to event.
#' @param upperBt1 Character. Name of the upper bound for the first time to event.
#' @param upperBt2 Character. Name of the upper bound for the second time to event.
#' @param measure Character. Measure to be minimized during the selection process. Either 'AIC' or 'BIC'. Default is 'AIC'.
#' @param eta1 Formula for the first survival model equation. Default is NULL.
#' @param eta2 Formula for the second survival model equation. Default is NULL.
#' @param input_equation Logical. If TRUE, uses the provided `eta1` and `eta2` formulas. If FALSE, generates formulas using all predictors in `data`. Default is FALSE.
#'
#' @return A list containing:
#'   - `best_margin_S1`: The best margin (link function) for the first survival model.
#'   - `measure`: The AIC or BIC value for the best margin of the first survival model.
#'   - `model_S1`: The fitted model for the first survival model with the best margin.
#'   - `best_margin_S2`: The best margin (link function) for the second survival model.
#'   - `measure_S2`: The AIC or BIC value for the best margin of the second survival model.
#'   - `model_S2`: The fitted model for the second survival model with the best margin.
#'
#' @export
#'
#' @examples
#' \donttest{
#' ###############################################
#' # Example based on AREDS dataset
#' # This analysis serves solely as a
#' # demonstration of the function's capabilities.
#' ###############################################
#' data(AREDS)
#'
#' results <- Select_link_BivCop(data = AREDS, cens1 = AREDS$cens1,
#'                               lowerBt1 = 't11', lowerBt2 = 't21',
#'                               upperBt1 = 't12', upperBt2 = 't22',
#'                               cens2 = AREDS$cens2, measure = 'AIC')
#' print(results)
#' }
#'
Select_link_BivCop <- function(data, cens1, cens2,lowerBt1 = 't11', lowerBt2 = 't21',
                               upperBt1 = 't12', upperBt2 = 't22' , measure = "AIC", eta1 = NULL, eta2 = NULL, input_equation = FALSE) {
  # List of margins to evaluate


  if(!("cens1" %in% names(data))  ) stop("You must provide both censoring indicators.")
  if(!("cens2" %in% names(data))  ) stop("You must provide both censoring indicators.")
  if(!("t11" %in% names(data))  ) stop("You must provide all the time to events. See package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  if(!("t12" %in% names(data))  ) stop("You must provide all the time to events. See package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  if(!("t22" %in% names(data))  ) stop("You must provide all the time to events. See package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  if(!("t21" %in% names(data))  ) stop("You must provide all the time to events. See package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  if (!measure %in% c("AIC", "BIC")) {
    stop("Invalid value for measure. Choose either 'AIC' or 'BIC'.")
  }

  if (input_equation == TRUE) {
    eta1 = eta1
    eta2 = eta2
  } else {
    censvar <- grep("^cens", names(data), value = TRUE)
    predictors <- setdiff(names(data), c("t11", "t12", "t21", "t22", censvar))

    # Creating initial formulas
    eta1 <- stats::as.formula(paste(lowerBt1, "~ s(",lowerBt1 , ", bs = \"mpi\") + ", paste(predictors, collapse = " + "), sep = ''))
    eta2 <- stats::as.formula(paste(lowerBt2, "~ s(", lowerBt2, ", bs = \"mpi\") + ", paste(predictors, collapse = " + "), sep = ''))
  }

  margins <- c("PH", "PO", "probit")

  # Initial best measure set to a large number
  best_measure_value_1m <- Inf
  best_measure_value_2m <- Inf
  best_margin_1m <- NULL
  best_margin_2m <- NULL
  best_models_1m <- list()
  best_models_2m <- list()

  # Loop through each margin and fit models for eq1 and eq2
  for (margin in margins) {
    model_eq1 <- try(GJRM::gamlss(list(eta1), data = data, surv = TRUE, margin = margin,
                                  cens = cens1, type.cen = "mixed", upperB = upperBt1), silent = TRUE)

    model_eq2 <- try(GJRM::gamlss(list(eta2), data = data, surv = TRUE, margin = margin,
                                  cens = cens2, type.cen = "mixed", upperB = upperBt2), silent = TRUE)


    # Check if the models were fitted successfully
    if (inherits(model_eq1, "try-error") || inherits(model_eq2, "try-error")) {
      next
    }
    # Combine the models' results for AIC or BIC
    if (measure == "AIC") {
      measure_value_1m <- stats::AIC(model_eq1)
      measure_value_2m <- stats::BIC(model_eq2)
    } else if (measure == "BIC") {
      measure_value_1m <- stats::BIC(model_eq1)
      measure_value_2m <- stats::BIC(model_eq2)
    } else {
      stop("Please specify a measure to select the best margin. Options are 'AIC' or 'BIC'.")
    }

    # Check if this is the best (lowest) measure value found so far
    if (measure_value_1m < best_measure_value_1m) {
      best_measure_value_1m <- measure_value_1m
      best_margin_1m <- margin
      best_models_1m <- list(model_eq1 = model_eq1)
    }

    if (measure_value_2m < best_measure_value_2m) {
      best_measure_value_2m <- measure_value_2m
      best_margin_2m <- margin
      best_models_2m <- list(model_eq2 = model_eq2)
    }
  }

  # Print the results in a tabular format to the console
  cat("\nSummary of Best Margins for Survival Analysis:\n")
  cat("-------------------------------------------------\n")
  cat(sprintf("Survival 1:\nBest Link Function: %s\n%s Value: %f\n", best_margin_1m, measure, best_measure_value_1m))
  cat("-------------------------------------------------\n")
  cat(sprintf("Survival 2:\nBest Link Function: %s\n%s Value: %f\n", best_margin_2m, measure, best_measure_value_2m))
  cat("-------------------------------------------------\n")

  # Return the best margin and the corresponding models
  return(list(
    best_margin_S1 = best_margin_1m,
    measure = best_measure_value_1m,
    model_S1 = best_models_1m,
    best_margin_S2 = best_margin_2m,
    measure_S2 = best_measure_value_2m,
    model_S2 = best_models_2m
  ))
}
