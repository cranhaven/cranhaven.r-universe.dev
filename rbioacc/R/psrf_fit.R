#' Potential Scale Reduction Factors (PSRF) of the parameters 
#' 
#' @param fit An object of class \code{fitTK}
#' @return An object of class \code{data.frame} with two columns: PSRF and parameter
#' 
#' @return a data frame with Potential Scale Reduction Factors
#' 
#' @export
#'

psrf <- function(fit){
  fitMCMC = rstan::extract(fit[["stanfit"]])
  
  ls <- list()
  
  ls$ku <- lapply(1:ncol(fitMCMC$ku), function(i) rstan::Rhat(fitMCMC$ku[, i]))
  ls$kee <- rstan::Rhat(fitMCMC$ke[, 1])
  ls$sigmaConc <- rstan::Rhat(fitMCMC$sigmaCGpred[, 1])
  if(ncol(fitMCMC$ke) == 2){
    ls$keg <- rstan::Rhat(fitMCMC$ke[, 2])
    ls$sigmaGrowth <- rstan::Rhat(fitMCMC$sigmaCGpred[, 2])
    ls$gmax <- rstan::Rhat(fitMCMC$gmax)
    ls$G0 <- rstan::Rhat(fitMCMC$G0)
  }
  if("km" %in% names(fitMCMC)){
    ls$km <- lapply(1:ncol(fitMCMC$km), function(i) rstan::Rhat(fitMCMC$km[, i]))
    ls$sigmaCmet <- lapply(1:ncol(fitMCMC$sigmaCmetpred), function(i) rstan::Rhat(fitMCMC$sigmaCmetpred[, i]))
  }
  if("kem" %in% names(fitMCMC)){
    ls$kem <- lapply(1:ncol(fitMCMC$kem), function(i) rstan::Rhat(fitMCMC$kem[, i]))
  }
  
  ls_out <- do.call("c", ls)
  ls_out <- lapply(ls_out, round, digits = 3)
  df <- as.data.frame(do.call("rbind", ls_out))
  df$parameter <- as.factor(names(ls_out))
  colnames(df)[1] <- "PSRF"
  
  return(df)
}