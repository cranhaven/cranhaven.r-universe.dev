#' Perform association tests between phenotype and covariates
#'
#' @param pheno phenotype dataframe - output from makePhenotypes function
#' @param covariates covariate dataframe - output from risk_factor function.
#' @param phe.name Phenotype name in the data.
#' @param cov.name Selected covariate names in the data. By default, cov.name=c("sex","age","bmi"), covariates include sex, age and BMI.
#' @export log_cov
#' @return Outputs association test results with OR, 95% CI, and p-value.
#' @import questionr
#' @import stats
#' @import utils
#' @examples
#' \dontrun{
#' log_cov(pheno=phe, covariates=covar, phe.name="hospitalisation", cov.name=c("sex","age","bmi"))
#' }

log_cov <- function(pheno, covariates, phe.name, cov.name = c("sex","age","bmi")){
  data <- inner_join(pheno, covariates, by="ID")
  y <- data[,c(phe.name,cov.name)]
  y <- na.omit(y)
  colnames(y)[1] <- "phe"
  m <- glm(phe ~ ., data=y, family="binomial")
  log.reg <- summary(m)
  OR <- odds.ratio(m)
  asso <- as.data.frame(cbind(log.reg$coefficients[,1],OR[,c(1:4)]))
  colnames(asso)[1] <- "Estimate"
  attr(asso, "class") <- "data.frame"
  asso
}
