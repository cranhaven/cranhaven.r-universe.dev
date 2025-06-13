maxLogFunc <- function(param){
  sigma0 <- exp(param[TotParNum - 3]); sigma1 <- exp(param[TotParNum - 2])
  rho0 <- (exp(2*param[TotParNum - 1]) - 1)/(exp(2*param[TotParNum - 1]) + 1)
  rho1 <- (exp(2*param[TotParNum]) - 1)/(exp(2*param[TotParNum]) + 1)

  SelectParM <- matrix(param[1:SelectParNum], SelectParNum, 1)
  SelectData <- cbind(as.matrix(data[, SelectCov, with = F]), matrix(1, nrow(data), 1))
  SelectCovSum <- SelectData %*% SelectParM

  SelectLabel <- which(data[, SelectDep, with = F] == 1)
  # Not treated
  OutcomeParM0 <- matrix(param[(SelectParNum + 1): (SelectParNum + OutcomeParNum)], OutcomeParNum, 1)
  OutcomeData0 <- cbind(as.matrix(data[-SelectLabel, OutcomeCov, with = F]),
                    matrix(1, nrow(data)-length(SelectLabel), 1))
  Outcome0Sum <- as.vector(OutcomeData0 %*% OutcomeParM0)

  # Treated
  OutcomeParM1 <- matrix(param[(SelectParNum + OutcomeParNum + 1): (SelectParNum + 2*OutcomeParNum)], OutcomeParNum, 1)
  OutcomeData1 <- cbind(as.matrix(data[SelectLabel, OutcomeCov, with = F]),
                    matrix(1, length(SelectLabel), 1))
  Outcome1Sum <- as.vector(OutcomeData1 %*% OutcomeParM1)

  SelectCovSum0 <- unlist(SelectCovSum[-SelectLabel, 1]) # Not Treated
  SelectCovSum1 <- unlist(SelectCovSum[SelectLabel, 1]) # Treated

  OutcomeRes0 <- unlist(data[-SelectLabel, OutcomeDep, with = F]) - Outcome0Sum
  OutcomeRes1 <- unlist(data[SelectLabel, OutcomeDep, with = F]) - Outcome1Sum

  eta1 <- (SelectCovSum1 + rho1*OutcomeRes1/sigma1)/sqrt(1-rho1^2)
  eta0 <- (SelectCovSum0 + rho0*OutcomeRes0/sigma0)/sqrt(1-rho0^2)

  if(is.na(Weight)){
    WeightValue <- rep(1, nrow(data))
  }else{
    WeightValue <- unlist(data[, Weight, with = F])
  }

  LogLike <- sum(WeightValue[SelectLabel]*(log(stats::pnorm(eta1)) + log(stats::dnorm(OutcomeRes1/sigma1)/sigma1))) +
    sum(WeightValue[-SelectLabel]*(log(1 - stats::pnorm(eta0)) + log(stats::dnorm(OutcomeRes0/sigma0)/sigma0)))

  if(isTRUE(envir$verbose)) cat('Searching for maximum value of the log-likelihood function:', LogLike, '\r')
  return(LogLike)
}
