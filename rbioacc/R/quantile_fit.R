#' Quantiles of parameters
#' 
#' @param fit An object of class \code{fitTK}
#' @param probs Scalar or Vector of quantiles.
#' Default is 0.025, 0.5 and 0.975 giving median and 95% credible interval
#' 
#' @return A data frame with quantiles
#' 
#' @export
#'
quantile_table <- function(fit, probs = c(0.025, 0.5, 0.975)){

  fitMCMC = rstan::extract(fit[["stanfit"]])

  ls <- list()
  
  ls$ku <- lapply(1:ncol(fitMCMC$ku), function(i) quantile(fitMCMC$ku[, i],probs ))
  ls$kee <- list(quantile(fitMCMC$ke[, 1],probs ))
  ls$sigmaConc <- list(quantile(fitMCMC$sigmaCGpred[, 1],probs ))
  if(ncol(fitMCMC$ke) == 2){
    ls$keg <- list(quantile(fitMCMC$ke[, 2],probs ))
    ls$sigmaGrowth <- list(quantile(fitMCMC$sigmaCGpred[, 2],probs ))
    ls$gmax <- list(quantile(fitMCMC$gmax,probs))
    ls$G0 <- list(quantile(fitMCMC$G0,probs))
  }
  if("km" %in% names(fitMCMC)){
    ls$km <- lapply(1:ncol(fitMCMC$km), function(i) quantile(fitMCMC$km[, i],probs ))
    ls$sigmaCmet <- lapply(1:ncol(fitMCMC$sigmaCmetpred), function(i) quantile(fitMCMC$sigmaCmetpred[, i],probs ))
  }
  if("kem" %in% names(fitMCMC)){
    ls$kem <- lapply(1:ncol(fitMCMC$kem), function(i) quantile(fitMCMC$kem[, i],probs ))
  }
  
  ls_out <- do.call("c", ls)
  df <- as.data.frame(do.call("rbind", ls_out))
  df$parameter <- as.factor(names(ls_out))
  
  return(df)
  
}
