#' Endogenous switching regression
#'
#' This R function provides estimates of original distribution parameters (sigma and rho) from the estimates of
#' transformed distribution parameters using the delta method.
#'
#' @param Results A maxLik object that is estimated by the \code{endoSwitch} function.
#'
#' @return A matrix that reports the estimates of original distribution parameters. sigma is standard deviation,
#' and rho is correlation coefficient.
#'
#'
calcPar <- function(Results){

  VarCov <- base::solve(-Results$hessian)
  coefEst <- stats::coef(Results)

  SigmaNum <- grep('Sigma', names(coefEst))
  SigmaSD <- sqrt(diag(VarCov)[SigmaNum])

  RhoNum <- grep('Rho', names(coefEst))
  RhoSD <- sqrt(diag(VarCov)[RhoNum])

  outM <- cbind(Estimates = c(coefEst[SigmaNum], coefEst[RhoNum]),
                Std.error = c(SigmaSD, RhoSD))

  expFunc <- function(x) (exp(2*x) - 1)/(exp(2*x) + 1)

  parEst <- c(exp(outM[1, 1]), exp(outM[2, 1]), expFunc(outM[3, 1]), expFunc(outM[4, 1]))
  parSD <- c(msm::deltamethod (~ exp(x1), outM[1, 1], outM[1, 2]^2),
             msm::deltamethod (~ exp(x1), outM[2, 1], outM[2, 2]^2),
             msm::deltamethod (~ (exp(2*x1) - 1)/(exp(2*x1) + 1), outM[3, 1], outM[3, 2]^2),
             msm::deltamethod (~ (exp(2*x1) - 1)/(exp(2*x1) + 1), outM[4, 1], outM[4, 2]^2))

  parMatrix <- matrix(c(parEst, parSD, parEst/parSD), nrow = length(parEst))
  row.names(parMatrix) <- c('Outcome.0.Sigma', 'Outcome.1.Sigma', 'Outcome.0.Rho', 'Outcome.1.Rho')
  colnames(parMatrix) <- c('Estimate', 'Std. error', 'z')
  parMatrix
}



