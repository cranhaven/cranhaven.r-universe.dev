#' Endogenous Switching Regression Models
#'
#' This is the main interface for the endoSwitch package to estimate the endogenous switching regression
#' models (Heckman, 1979).
#'
#' This function estimates the endogenous switching regression model using the full maximum likelihood estimation
#' method. In this model, a selection equation sorts observation units over two different regimes (e.g., treated and
#' not-treated, or adopter and non-adopter), and two outcome equations that determine the
#' outcome. Estimation of the model relies on joint normality of the error terms in the three-equation system (the selection
#' equation plus two outcome equations). The model is estimated by maximizing the joint likelihood function that is provided in
#' Lokshin and Sajaia (2004).
#'
#' The \code{endoSwitch} uses the \code{maxLik} function in the maxLik package to do the optimization. The function automatically
#' searches for starting values for maximization using the results from two-stage estimation following Maddala (1986, chapter 8).
#' Though not recommended, users may provide starting values manually. Assume that you have M variables (including the constant) in
#' the selection equation, and N variables (including the constant) in each outcome equation.
#' Then you need (M + 2*N + 4) starting values. The first M values are for the variables in the
#' selection equation (last value for the constant), followed by N values for the outcome equation for the non-treated individuals
#' (SelectDep = 0), and another N values for the outcome equation for the treated individuals (SelectDep = 1).
#' The last four values are: sigma in outcome equation for the non-treated, sigma in outcome equation for the treated,
#' rho in outcome equation for the non-treated, rho in outcome equation for the treated.
#'
#' If \code{treatEffect} = \code{TRUE}, the \code{endoSwitch} function will report average treatment effects (for the treated or untreated) as well as heterogeneity effects.
#' A detailed description of these effects is provided in Di Falco, Veronesi, and Yesuf (2011, p.837). If \code{treatEffect} = \code{FALSE},
#' the \code{endoSwitch} function will report expected outcome values in a list of two dataframes:
#' dataframe EYA1 reports actual (column EY1.A1) and counterfactual (column EY0.A1) expected outcome values for the treated;
#' dataframe EYA0 reports actual (column EY0.A0) and counterfactual (column EY1.A0) expected outcome values for the untreated.
#'
#'
#' @param data a data frame. Data for running the regression analysis.
#' @param OutcomeDep character. Dependent variable in the outcome equation.
#' @param SelectDep character. Dependent variable in the Selection model. The variable must be binary (0 or 1).
#' @param OutcomeCov character vector. Covariates in the outcome equation.
#' @param SelectCov character vector. Covariates in the selection equation.
#' @param Weight optional character. Name of the weight variable in the dataset, or NA (equal weight).
#' @param treatEffect \code{TRUE}/\code{FALSE}. If \code{TRUE}, average treatment effects will be calculated and returned.
#' If \code{FALSE}, expected outcome values will be calculated and returned.
#' @param method character. Maximization method to be used. The default is "BFGS" (for Broyden-Fletcher-Goldfarb-Shanno).
#' Other methods can also be used. See \code{\link{maxLik}}.
#' @param start optional numeric vector. Used as initial values of parameters for maximization purpose.
#' If NULL, the coefficient estimates from the two-stage estimation will be used.
#' @param verbose TRUE/FALSE. Choose to show the status of optimization or not.
#' @param ... Other parameters to be passed to the selected maximization routine. See \code{\link{maxLik}}.
#'
#' @return A list containing three elements. The first element is an object of class "maxLik", which includes parameters
#' in the selection equation, parameters in the outcome equations, and the transformed distributional parameters (parameters are
#' transformed to faciliate maximization, as recommended by Lokshin and Sajaia (2004)). The second element contains the estimates of
#' original distributional parameters (transformed back via the delta method). The third element contains a table reporting
#' average treatment effects or a list of expected outcome values, depending on users' choice of \code{treatEffect}.
#'
#'
#' @references Lokshin, Michael, and Roger B. Newson. “Impact of Interventions on Discrete Outcomes: Maximum Likelihood Estimation
#' of the Binary Choice Models with Binary Endogenous Regressors.” Stata Journal 11, no. 3 (2011): 368–85.
#'
#' Heckman, James J. “Sample Selection Bias as a Specification Error.” Econometrica 47, no. 1 (1979): 153–61. https://doi.org/10.2307/1912352.
#'
#' Maddala, G. S. “Limited-Dependent and Qualitative Variables in Econometrics.” Cambridge Books. Cambridge University Press, 1986.
#'
#' Di Falco, Salvatore, Marcella Veronesi, and Mahmud Yesuf. “Does Adaptation to Climate Change Provide Food Security? A Micro-Perspective from Ethiopia.”
#' American Journal of Agricultural Economics 93, no. 3 (2011): 829–46. https://doi.org/10.1093/ajae/aar006.
#'
#' Abdulai, Abdul Nafeo. “Impact of Conservation Agriculture Technology on Household Welfare in Zambia.”
#' Agricultural Economics 47, no. 6 (2016): 729–41. https://doi.org/10.1111/agec.12269.
#'
#' @export
#'
#' @examples
#' data(ImpactData) # Data are from Abdulai (2016)
#' OutcomeDep <- 'Output'
#' SelectDep <- 'CA'
#' OutcomeCov <- c('Age')
#' SelectCov <- c('Age', 'Perception')
#' endoReg <- endoSwitch(ImpactData, OutcomeDep, SelectDep, OutcomeCov, SelectCov)
#'
#' summary(endoReg) # Summarize the regression results

endoSwitch <- function(data, OutcomeDep, SelectDep, OutcomeCov, SelectCov, Weight = NA,
                       treatEffect = TRUE, method = 'BFGS', start = NULL,
                       verbose = FALSE, ...){

  if(sum(OutcomeDep %in% colnames(data)) != length(OutcomeDep))
    stop("OutcomeDep must be valid column names in the dataset.")

  if(sum(OutcomeCov %in% colnames(data)) != length(OutcomeCov))
    stop("OutcomeCov must be valid column names in the dataset.")

  if(sum(SelectDep %in% colnames(data)) != length(SelectDep))
    stop("SelectDep must be valid column names in the dataset.")

  if(sum(SelectCov %in% colnames(data)) != length(SelectCov))
    stop("SelectCov must be valid column names in the dataset.")

  if(!is.na(Weight)){
    if(sum(Weight %in% colnames(data)) != length(Weight))
    stop("Weight must be valid column names in the dataset.")
  }

  if(nrow(data) < length(OutcomeCov) + length(SelectCov))
    stop('Too few observations.')

  if(identical(OutcomeCov, SelectCov))
    stop('OutcomeCov can not be identical to SelectCov.')

  if(is.na(Weight)){
    covNames <- base::unique(c(OutcomeDep, SelectDep, OutcomeCov, SelectCov))
  }else{
    covNames <- base::unique(c(OutcomeDep, SelectDep, OutcomeCov, SelectCov, Weight))
  }

  data <- data.table::as.data.table(data)
  data <- data[, covNames, with = FALSE]
  nrowData <- nrow(data)

  data <- stats::na.omit(data)
  if(nrow(data) < nrowData) cat('Observations with NA values have been discarded.')

  if(sum(apply(data, 2, is.numeric)) < length(covNames))
    stop('All Selected columns must be numeric.')

  SelectValue <- unlist(data[, SelectDep, with = FALSE])

  if(length(SelectValue[-c(which(SelectValue == 0), which(SelectValue == 1))]) >= 1)
    stop('The dependent iable (SelectDep) must be 0 or 1.')

  envir = new.env()
  environment(maxLogFunc) = envir

  ParNames <- c(paste0('Select.', SelectCov), 'Select.Const',
                paste0('Outcome.0.', OutcomeCov), 'Outcome.0.Const',
                paste0('Outcome.1.', OutcomeCov), 'Outcome.1.Const',
                'Outcome.0.SigmaX', 'Outcome.1.SigmaX',
                'Outcome.0.RhoX', 'Outcome.1.RhoX')

  assign('data', data, envir)
  assign('OutcomeDep', OutcomeDep, envir)
  assign('SelectDep', SelectDep, envir)
  assign('OutcomeCov', OutcomeCov, envir)
  assign('SelectCov', SelectCov, envir)
  assign('Weight', Weight, envir)

  assign('OutcomeParNum', length(OutcomeCov) + 1, envir)
  assign('SelectParNum', length(SelectCov) + 1, envir)
  assign('TotParNum', 2*(length(OutcomeCov) + 1) + length(SelectCov) + 1 + 4, envir)

  assign('verbose', verbose, envir)

  if(is.null(start)){
    twostage.results <- endoSwitch2Stage(data, OutcomeDep, SelectDep, OutcomeCov, SelectCov)
    rho0EstX <- ifelse(twostage.results$distParEst['rho0'] > 1, 0.9,
                                                  ifelse(twostage.results$distParEst['rho0'] < -1, -0.9, twostage.results$distParEst['rho0']))
    rho1EstX <- ifelse(twostage.results$distParEst['rho1'] > 1, 0.9,
                                                  ifelse(twostage.results$distParEst['rho1'] < -1, -0.9, twostage.results$distParEst['rho1']))

    start <- c(stats::coef(twostage.results$FirstStageReg)[-1], stats::coef(twostage.results$FirstStageReg)[1],
               stats::coef(twostage.results$SecondStageReg.0)[2:(length(OutcomeCov)+1)],
               stats::coef(twostage.results$SecondStageReg.0)[1],
               stats::coef(twostage.results$SecondStageReg.1)[2:(length(OutcomeCov)+1)],
               stats::coef(twostage.results$SecondStageReg.1)[1],
               log(twostage.results$distParEst['sigma0']),  log(twostage.results$distParEst['sigma1']),
               .5*log((1 + rho0EstX)/(1 - rho0EstX)),
               .5*log((1 + rho1EstX)/(1 - rho1EstX)))
  }

  if(length(start) != (length(SelectCov) + 1 + 2*(length(OutcomeCov) + 1) + 4))
    stop("wrong number of starting values")
  #if(isTRUE(verbose)) cat('Searching for optimal values.', '\r')

  mle.results <- maxLik::maxLik(maxLogFunc, start = start, method = method, ...)
  names(mle.results$estimate) <- names(mle.results$gradient) <- ParNames
  rownames(mle.results$hessian) <- colnames(mle.results$hessian) <- ParNames
  #if(isTRUE(verbose)) cat('Searching completed.', '\r')

  treatEffectResult <- treatmentEffect(mle.results, data, OutcomeDep, SelectDep, OutcomeCov, SelectCov, treatEffect)

  distPar = calcPar(mle.results)

  out <- list(MLE.Results = mle.results, distPar = distPar, treatEffect = treatEffectResult)
  class(out) <- 'endoSwitch'
  out
}
