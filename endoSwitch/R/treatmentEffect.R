#' Endogenous switching regression
#'
#' This function calculates average treatment effects and heterogeneity effects from an estimated endogenous switching regression model.
#'
#' @param Results Estimated endogenous switching regression model.
#' @param data a data frame. Data for running the regression analysis.
#' @param OutcomeDep character. Dependent variable in the outcome equation.
#' @param SelectDep character. Dependent variable in the Selection model. The variable must be binary (0 or 1).
#' @param OutcomeCov character vector. Covariates in the outcome equation.
#' @param SelectCov character vector. Covariates in the selection equation.
#' @param treatEffect TRUE/FALSE. Show average treatment effects or expected outcome values.
#'
#'
#' @return A data table that reports the average treatment effects or a list of two tables reporting expected outcome values.


treatmentEffect <- function(Results, data, OutcomeDep, SelectDep, OutcomeCov, SelectCov, treatEffect){
  # Estimate treatment effects.

  # 1. Treatment effects
  CovData <- as.matrix(data[, OutcomeCov, with = F])
  CovData <- cbind(CovData, matrix(1, nrow = nrow(CovData), 1))

  ParApt0 <- Results$estimate[paste0('Outcome.0.', c(OutcomeCov, 'Const'))]
  ParApt1 <- Results$estimate[paste0('Outcome.1.', c(OutcomeCov, 'Const'))]

  DistParEst <- calcPar(Results)

  # Treatment effect on the treated
  TreatedObs <- which(data[, SelectDep, with = F] == 1)
  SelectPar <- matrix(Results$estimate[paste0('Select.', c(SelectCov, 'Const'))], ncol = 1)

  SelectCovDataTreated <- as.matrix(data[TreatedObs, SelectCov, with = F])
  SelectCovDataTreated <- cbind(SelectCovDataTreated, matrix(1, nrow = nrow(SelectCovDataTreated), 1))
  MillsRatioTreated <- stats::dnorm(SelectCovDataTreated %*% SelectPar)/stats::pnorm(SelectCovDataTreated %*% SelectPar)

  SelectCovDataUnTreated <- as.matrix(data[-TreatedObs, SelectCov, with = F])
  SelectCovDataUnTreated <- cbind(SelectCovDataUnTreated, matrix(1, nrow = nrow(SelectCovDataUnTreated), 1))
  MillsRatioUnTreated <- stats::dnorm(SelectCovDataUnTreated %*% SelectPar)/(1-stats::pnorm(SelectCovDataUnTreated %*% SelectPar))

  # Treatment effect
  EY1.A1 <- CovData[TreatedObs, ] %*% matrix(ParApt1) + DistParEst['Outcome.1.Sigma', 1]*DistParEst['Outcome.1.Rho', 1]*MillsRatioTreated
  EY0.A0 <- CovData[-TreatedObs, ] %*% matrix(ParApt0) - DistParEst['Outcome.0.Sigma', 1]*DistParEst['Outcome.0.Rho', 1]*MillsRatioUnTreated

  EY0.A1 <- CovData[TreatedObs, ] %*% matrix(ParApt0) + DistParEst['Outcome.0.Sigma', 1]*DistParEst['Outcome.0.Rho', 1]*MillsRatioTreated
  EY1.A0 <- CovData[-TreatedObs, ] %*% matrix(ParApt1) - DistParEst['Outcome.1.Sigma', 1]*DistParEst['Outcome.1.Rho', 1]*MillsRatioUnTreated

  effectM <- data.frame(matrix(c(round(mean(EY1.A1),4), round(mean(EY0.A1), 4), NA,
                                 paste0('(', round(stats::sd(EY1.A1), 4), ')'), paste0('(', round(stats::sd(EY0.A1), 4), ')'), NA,
                                 round(mean(EY1.A0), 4), round(mean(EY0.A0), 4), NA,
                                 paste0('(', round(stats::sd(EY1.A0), 4), ')'), paste0('(', round(stats::sd(EY0.A0), 4), ')'), NA,
                                 NA, NA, NA,
                                 NA, NA, NA),
                               nrow = 6, byrow = T), stringsAsFactors = F)
  colnames(effectM) <- c('Y1 (Choose to adopt)', 'Y0 (Choose not to adopt)', 'TreatmentEffect')
  row.names(effectM) <- c('A1', '(Adopted)','A0', '(Not adopted)', 'Heterogeneity', 'Effect')

  effectM[c(1, 3), 3] <- round(c(mean(EY1.A1) - mean(EY0.A1), mean(EY1.A0) - mean(EY0.A0)), 4)
  effectM[c(2, 4), 3] <- c(paste0('(', round(stats::t.test(EY1.A1, EY0.A1, paired = T)$stderr, 4), ')'),
                           paste0('(', round(stats::t.test(EY1.A0, EY0.A0, paired = T)$stderr, 4), ')'))

  effectM[5, ] <- c(round(mean(EY1.A1) - mean(EY1.A0), 4),
                    round(mean(EY0.A1) - mean(EY0.A0), 4),
                    round(mean(EY1.A1) - mean(EY0.A1) - (mean(EY1.A0) - mean(EY0.A0)), 4))
  effectM[6, ] <- c(paste0('(', round(stats::t.test(EY1.A1, EY1.A0, paired = F, var.equal = F)$stderr, 4), ')'),
                    paste0('(', round(stats::t.test(EY0.A1, EY0.A0, paired = F, var.equal = F)$stderr, 4), ')'),
                    paste0('(', round(stats::t.test(EY1.A1 - EY0.A1, EY1.A0 - EY0.A0, paired = F, var.equal = F)$stderr, 4), ')'))

  effectM$Label <- c('ATT', '', 'ATU', '', 'ATH', '')

  if(isTRUE(treatEffect)){
    treatEffectOut <- effectM
  }else{
    treatEffectOut <- list(EYA1 = data.frame(EY1.A1, EY0.A1), EYA0 = data.frame(EY1.A0, EY0.A0))
  }
  treatEffectOut
}





