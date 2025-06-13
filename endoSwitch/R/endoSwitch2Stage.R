#' Endogenous switching regression
#'
#' This function estimates the endogenous switching regression model via two-stage estimations (Maddala, 1986)
#'
#' The first stage uses a probit model to estimate the selection equation.
#' The second stage uses ordinary least squares including the inverse mills ratios computed from the first stage
#' estimation results to estimate the outcome equations.
#'
#' @param data a data frame. Data for running the regression analysis.
#' @param OutcomeDep character. Dependent variable in the outcome equation.
#' @param SelectDep character. Dependent variable in the Selection model. The variable must be binary (0 or 1).
#' @param OutcomeCov character vector. Covariates in the outcome equation.
#' @param SelectCov character vector. Covariates in the selection equation.
#'
#' @references Maddala, G. S. “Limited-Dependent and Qualitative Variables in Econometrics.” Cambridge Books. Cambridge University Press, 1986.
#' @return A list containing regression results.
#'
#' @export
#' @examples
#'
#' data(ImpactData)
#' OutcomeDep <- 'Output'
#' SelectDep <- 'CA'
#' OutcomeCov <- c('Age')
#' SelectCov <- c('Age', 'Perception')
#' Results <- endoSwitch2Stage(ImpactData, OutcomeDep, SelectDep, OutcomeCov, SelectCov)
#' # First stage regression results
#' summary(Results$FirstStageReg)
#' # Second stage regression results -- non-adopter
#' summary(Results$SecondStageReg.0)
#' # Second stage regression results -- adopter
#' summary(Results$SecondStageReg.1)

endoSwitch2Stage <- function(data, OutcomeDep, SelectDep, OutcomeCov, SelectCov){
  data <- data.table::as.data.table(data)
  # Adopters
  AdtObs <- which(data[, SelectDep, with = F] == 1)

  # First stage estimation
  RegS1Formula <- stats::as.formula(paste0(SelectDep, '~', paste(SelectCov, collapse = '+')))

  RegS1 <- tryCatch(
    { # Ignore warnings, mostly harmless
      suppressWarnings(stats::glm(RegS1Formula, data = data, family = stats::binomial(link = 'probit')))
    }, error = function(cond){
      message('Error in searching for starting values. Please provide start values manually.')
      message(paste0('The error message is: ', cond))
    }
  )

  # Second stage estimation
  Pred1 <- suppressWarnings(stats::predict(RegS1, data[AdtObs, ]))
  Pred0 <- suppressWarnings(stats::predict(RegS1, data[-AdtObs, ]))

  MillsRatioAdt1 <- stats::dnorm(Pred1)/stats::pnorm(Pred1)
  MillsRatioAdt0 <- stats::dnorm(Pred0)/(1-stats::pnorm(Pred0))

  RegS2Formula_Adt1 <- stats::as.formula(paste0(OutcomeDep, '~', paste(OutcomeCov, collapse = '+'), '+', 'MillsRatioAdt1'))
  RegS2_Adt1 <- stats::lm(RegS2Formula_Adt1, data = data[AdtObs, ])

  RegS2Formula_Adt0 <- stats::as.formula(paste0(OutcomeDep, '~', paste(OutcomeCov, collapse = '+'), '+', 'MillsRatioAdt0'))
  RegS2_Adt0 <- stats::lm(RegS2Formula_Adt0, data = data[-AdtObs, ])

  sigma1Est <- (sum(RegS2_Adt1$residuals^2 + stats::coef(RegS2_Adt1)['MillsRatioAdt1']^2*MillsRatioAdt1*Pred1))/length(MillsRatioAdt1)
  if(sigma1Est <= 0) {
    warning('2 stage estimation failed, negative sigma1 found. Set to be 0.1.')
    sigma1Est <- 0.1
  }
  sigma1Est <- sigma1Est^.5

  sigma0Est <- (sum(RegS2_Adt0$residuals^2 + stats::coef(RegS2_Adt0)['MillsRatioAdt0']^2*MillsRatioAdt0*Pred0))/length(MillsRatioAdt0)
  if(sigma0Est <= 0) {
    warning('2 stage estimation failed, negative sigma0 found. Set to be 0.1.')
    sigma0Est <- 0.1
  }
  sigma0Est <- sigma0Est^.5

  rho1Est <- stats::coef(RegS2_Adt1)['MillsRatioAdt1']/sigma1Est
  rho0Est <- -stats::coef(RegS2_Adt0)['MillsRatioAdt0']/sigma0Est

  endoSwitch2SResults <- list(FirstStageReg = RegS1, SecondStageReg.0 = RegS2_Adt0, SecondStageReg.1 = RegS2_Adt1,
                  distParEst = c(sigma0 = as.numeric(sigma0Est), sigma1 = as.numeric(sigma1Est),
                                 rho0 = as.numeric(rho0Est), rho1 = as.numeric(rho1Est)))
  return(endoSwitch2SResults)
}


