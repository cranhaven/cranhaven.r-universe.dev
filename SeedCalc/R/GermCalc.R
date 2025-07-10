#funcao para calcular todos as variaveis

GermCalc <- function(germdata, NSeeds){

  result_FGP <- data.frame()
  result_GSI <- data.frame()
  result_T10 <- data.frame()
  result_T50 <- data.frame()
  result_T90 <- data.frame()
  result_UnifG <- data.frame()
  result_MGT <- data.frame()
  result_MGR <- data.frame()
  result_VarGer <- data.frame()
  result_CVt <- data.frame()
  result_Sinc <- data.frame()
  result_Unc <- data.frame()
  result_CVG <- data.frame()

  for (i in 2:ncol(germdata)) {
    result_FGP <- rbind(result_FGP, FGP(germdata[[i]], NSeeds))
  }

  for (i in 2:ncol(germdata)) {
    result_GSI <- rbind(result_GSI, GSI(germdata[[1]],germdata[[i]]))
  }

  for (i in 2:ncol(germdata)) {
    result_T10 <- rbind(result_T10, T10(germdata[[1]],germdata[[i]]))
  }

  for (i in 2:ncol(germdata)) {
    result_T50 <- rbind(result_T50, T50(germdata[[1]],germdata[[i]]))
  }

  for (i in 2:ncol(germdata)) {
    result_T90 <- rbind(result_T90, T90(germdata[[1]],germdata[[i]]))
  }

  for (i in 2:ncol(germdata)) {
    result_UnifG <- rbind(result_UnifG, UnifG(germdata[[1]],germdata[[i]]))
  }

  for (i in 2:ncol(germdata)) {
    result_MGT <- rbind(result_MGT, MGT(germdata[[1]],germdata[[i]]))
  }

  for (i in 2:ncol(germdata)) {
    result_MGR <- rbind(result_MGR, MGR(germdata[[1]],germdata[[i]]))
  }

  for (i in 2:ncol(germdata)) {
    result_VarGer <- rbind(result_VarGer, VarGer(germdata[[1]],germdata[[i]]))
  }

  for (i in 2:ncol(germdata)) {
    result_CVt <- rbind(result_CVt, CVt(germdata[[1]],germdata[[i]]))
  }

  for (i in 2:ncol(germdata)) {
    result_Sinc <- rbind(result_Sinc, Sinc(germdata[[1]],germdata[[i]]))
  }

  for (i in 2:ncol(germdata)) {
    result_Unc <- rbind(result_Unc, Unc(germdata[[1]],germdata[[i]]))
  }

  for (i in 2:ncol(germdata)) {
    result_CVG <- rbind(result_CVG, CVG(germdata[[1]],germdata[[i]]))
  }

  result <- data.frame(result_FGP, result_GSI, result_T10, result_T50, result_T90, result_UnifG, result_MGT, result_MGR, result_VarGer, result_CVt, result_Sinc, result_Unc, result_CVG)

  rownames(result) <- colnames(germdata[-1])
  colnames(result) <- c('FGP', 'GSI', 'T10', 'T50', 'T90', 'UnifG', 'MGT', 'MGR', 'VarGer', 'CVt', 'Sinc', 'Unc', 'CVG')

  return(result)
}
