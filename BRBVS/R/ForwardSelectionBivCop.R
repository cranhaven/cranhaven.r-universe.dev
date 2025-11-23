#' Forward Selection for Bivariate Copula Survival Models
#'
#' This function performs forward selection based on AIC or BIC measures for bivariate copula survival models.
#' It iteratively adds variables to the model to minimize the specified measure, either AIC or BIC.
#'
#' @param data A data frame containing the dataset.
#' @param lowerBt1 Character. Name of the lower bound for the first time to event.
#' @param lowerBt2 Character. Name of the lower bound for the second time to event.
#' @param upperBt1 Character. Name of the upper bound for the first time to event.
#' @param upperBt2 Character. Name of the upper bound for the second time to event.
#' @param copula Character. Type of copula to be used in the model. Default is 'N' (Normal copula).
#' @param margins Character vector. Margins to be used in the copula model. Default is c('PH', 'PH').
#' @param measure Character. Measure to be minimized during the selection process. Either 'AIC' or 'BIC'. Default is 'AIC'.
#' @param cens1 Censoring indicator for the first time to event.
#' @param cens2 Censoring indicator for the second time to event.
#'
#' @return A list containing:
#'   - `Results`: A data frame with the steps, models, and the corresponding AIC/BIC values.
#'   - `Equations`: A list with the final model equations for the selected variables.
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
#' subsetAREDS <- AREDS[, c('t11', 't12', 't21', 't22', 'SevScale1E',
#'                          'SevScale2E', 'cens1', 'cens2', 'cens')]
#' results <- forward_selection_BivCop(data = subsetAREDS, lowerBt1 = 't11', lowerBt2 = 't21',
#'                                     upperBt1 = 't12', upperBt2 = 't22',
#'                                     copula = 'N', margins = c('PH', 'PH'),
#'                                     measure = 'AIC', cens1 = AREDS$cens1,
#'                                     cens2 = AREDS$cens2)
#' print(results)
#' }
#'
forward_selection_BivCop <- function(data, lowerBt1 = 't11', lowerBt2 = 't21',
                                     upperBt1 = 't12', upperBt2 = 't22',
                                     copula = 'N', margins = c('PH', 'PH'),
                                     measure = 'AIC', cens1, cens2) {


  if(!("cens1" %in% names(data))  ) stop("You must provide both censoring indicators.")
  if(!("cens2" %in% names(data))  ) stop("You must provide both censoring indicators.")
  if(!("t11" %in% names(data))  ) stop("You must provide all the time to events. See package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  if(!("t12" %in% names(data))  ) stop("You must provide all the time to events. See package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  if(!("t22" %in% names(data))  ) stop("You must provide all the time to events. See package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  if(!("t21" %in% names(data))  ) stop("You must provide all the time to events. See package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  if (!measure %in% c("AIC", "BIC")) {
    stop("Invalid value for measure. Choose either 'AIC' or 'BIC'.")
  }
  # Check for copula
  valid_copulas = c("N", "C0", "C90", "C180", "C270", "GAL0", "GAL90", "GAL180", "GAL270",
                    "J0", "J90", "J180", "J270", "G0", "G90", "G180", "G270", "F", "AMH", "FGM", "T", "PL", "HO")
  if (!(copula %in% valid_copulas)) {
    stop("Error: Invalid value for copula see package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  }

  # Check for margins
  valid_margins = c("PH", "PO", "probit")
  if (!(margins[1] %in% valid_margins)) {
    stop("Error: Invalid value for the first margin see package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  }
  if (!(margins[2] %in% valid_margins)) {
    stop("Error: Invalid value for the second margin see package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  }

  # defining formulae for intercept model
  eta1 <- stats::as.formula(paste(lowerBt1, '~ s(', lowerBt1, ', bs = "mpi")', sep = ''))
  eta2 <- stats::as.formula(paste(lowerBt2, '~ s(', lowerBt2, ', bs = "mpi")', sep = ''))
  eta3 <- NULL

  f.l <- list(eta1, eta2)

  # fitting intercept model, M0
  M0 <- try(GJRM::gjrm(f.l, data = data, surv = TRUE,
                       copula = copula, margins = margins,
                       cens1 = cens1, cens2 = cens2, model = "B",
                       upperBt1 = upperBt1, upperBt2 = upperBt2), silent = TRUE)

  if (measure == 'AIC') {
    MeasureMO <- stats::AIC(M0)
  } else {
    MeasureMO <- stats::BIC(M0)
  }

  included <- c()

  # extraction of censoring variable
  censvar <- grep("^cens", names(data), value = TRUE)
  # Candidates are all the columns minus the t1, t2, and censoring.
  candidates <- setdiff(names(data), c(lowerBt1, lowerBt2, upperBt1, upperBt2, censvar))

  best_candidate_names <- c('(intercept)')
  Measure_best <- c(MeasureMO)

  while (length(candidates) > 0) {
    met_candidates <- numeric()
    for (i in 1:length(candidates)) {
      if (is.null(eta3)) {
        f.l <- list(stats::as.formula(paste(deparse(eta1), '+', candidates[i], sep = '')),
                    stats::as.formula(paste(deparse(eta2), '+', candidates[i], sep = '')),
                    stats::as.formula(paste('~', candidates[i])))
      } else {
        f.l <- list(stats::as.formula(paste(deparse(eta1), '+', candidates[i], sep = '')),
                    stats::as.formula(paste(deparse(eta2), '+', candidates[i], sep = '')),
                    stats::as.formula(paste(deparse(eta3), '+', candidates[i], sep = '')))
      }

      if (measure == 'AIC') {
        met_candidates[i] <- stats::AIC(GJRM::gjrm(f.l, data = data, surv = TRUE,
                                                   copula = copula, margins = margins,
                                                   cens1 = cens1, cens2 = cens2, model = "B",
                                                   upperBt1 = upperBt1, upperBt2 = upperBt2))
      } else {
        met_candidates[i] <- stats::BIC(GJRM::gjrm(f.l, data = data, surv = TRUE,
                                                   copula = copula, margins = margins,
                                                   cens1 = cens1, cens2 = cens2, model = "B",
                                                   upperBt1 = upperBt1, upperBt2 = upperBt2))
      }
    }

    if (min(met_candidates) < MeasureMO) {
      MeasureMO <- min(met_candidates)
      best_candidate <- candidates[which.min(met_candidates)]

      best_candidate_names <- c(best_candidate_names, best_candidate)
      Measure_best <- c(Measure_best, min(met_candidates))

      included <- c(included, best_candidate)

      eta1 <- stats::as.formula(paste(deparse(eta1), '+', best_candidate, sep = ''))
      eta2 <- stats::as.formula(paste(deparse(eta2), '+', best_candidate, sep = ''))

      if (is.null(eta3)) {
        eta3 <- stats::as.formula(paste('~', best_candidate))
      } else {
        eta3 <- stats::as.formula(paste(deparse(eta3), '+', best_candidate, sep = ''))
      }

      candidates <- setdiff(candidates, best_candidate)
    } else {
      best_candidate <- candidates[which.min(met_candidates)]
      candidates <- setdiff(candidates, best_candidate)
    }
  }

  return(list(
    Results = setNames(data.frame(Step = 1:length(best_candidate_names), Model = best_candidate_names, Measure_best),
                       c("Step", "Model", measure)),
    Equations = list(eta1, eta2, eta3)
  ))
}


