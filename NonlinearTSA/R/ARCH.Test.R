#'ARCH Test for time series
#'
#' This function allows you to make ARCH Test for residuals
#' @param x residual series name,
#' @param lags lags
#' @keywords ARCH test
#' @export
#' @importFrom stats embed lm pchisq
#' @examples
#' set.seed(12345)
#' x <- rnorm(1000)
#' ARCH.Test(x,3)
#'


ARCH.Test <- function(x,lags){
  residsq <- x^2
  reg_aux_data <- embed(residsq,lags+1)
  kk = lags + 1
  n <- length(residsq) - lags
  reg_aux <- lm(reg_aux_data[,1]~reg_aux_data[,2:kk])
  ARCHstat <- summary(reg_aux)$r.square * n
  pval <- pchisq(ARCHstat,lags, lower.tail = FALSE)
  result <- c(ARCHstat,pval)
  cat("Test Stat","p-value","\n")
  return(result)
}


