#' Fast calculation of the variance inflation factor (VIF)
#'
#' @description Function for fast calculation of the variance inflation factor (VIF)
#'
#' @usage vif(x)
#'
#' @param x A data.frame of explanatory variables
#'
#' @return Variance inflation factor (VIF) values of variables
#'
#' @importFrom RcppArmadillo fastLm
#' @importFrom stats as.formula
#'
#' @examples
#' data("sample_vars_sda")
#' x <- sample_vars_sda$Elevation[, sample(55, 10)]
#' vif(x)
#'
#' @export
#'

vif <- function(x){
  nx <- ncol(x)
  vx <- names(x)
  vif1 <- c()
  R2 <- function(o, p) 1 - sum((o-p)^2)/sum((o-mean(o))^2)
  for (i in 1:nx){
    if (nx == 2){
      f1 <- as.formula(paste(as.character(vx[i]), as.character(vx[-i]), sep = "~"))
    } else {
      f1 <- as.formula(paste(as.character(vx[i]),
                             paste(as.character(vx[-i]), collapse = "+"),
                             sep = "~"))
    }
    m1 <- RcppArmadillo::fastLm(f1, x)
    vif1[i] <- 1/(1 - R2(m1$fitted.values, x[, i]))
  }
  return(vif1)
}
