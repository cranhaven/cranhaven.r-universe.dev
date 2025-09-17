#' @title Same Species Sample Contamination
#' @description Detects whether a sample is contaminated another sample of its same species. The input file should be in vcf format.
#'
#' @param file VCF input object
#' @param rmCNV Remove CNV regions, default is FALSE
#' @param cnvobj cnv object, default is NULL
#' @param config config information of parameters. A default set is generated as part of the model and is included in a model object, which contains
#' @param class_model An SVM classification model
#' @param regression_model An SVM regression model
#'
#' @return A list containing (1) stat: a data frame with all statistics for contamination estimation; (2) result: contamination estimation (Class = 0, pure; Class = 1, contaminated)
#'
#' @export
#' @import e1071
#' @import utils
#'
#' @examples
#' data(vcf_example)
#' result <- sssc(file = vcf_example)
sssc <- function(file, rmCNV = FALSE, cnvobj = NULL, config = NULL, class_model = NULL, regression_model = NULL) {
  config_df <- svm_class_model <- svm_regression_model <- NULL
  data("config_df", envir = environment())
  data("svm_class_model", envir = environment())
  data("svm_regression_model", envir = environment())

  if (is.null(config)) {
    config <- config_df #config_df contains default parameters.
  }
  vcf <- file$VCF
  vcf <- update_vcf(rmCNV = rmCNV, vcf = vcf, cnvobj = cnvobj, config$threshold, config$skew, config$lower, config$upper)
  Name <- file$file_sample_name[1]

  LOH <- nrow(vcf[vcf$ZG == "het",])/nrow(vcf[vcf$ZG == "hom",])
  HomVar <- getVar(df = vcf, state = "hom", config$hom_mle, config$het_mle)
  HetVar <- getVar(df = vcf, state = "het", config$hom_mle, config$het_mle)
  HomRate <- nrow(vcf[(vcf$AF > config$Hom_thred),]) / nrow(vcf)
  HighRate <- nrow(vcf[((vcf$AF > config$High_thred) & (vcf$AF < config$Hom_thred)),]) / nrow(vcf)
  HetRate <- nrow(vcf[((vcf$AF > config$Het_thred) & (vcf$AF < config$High_thred)),]) / nrow(vcf)
  LowRate <- nrow(vcf[(vcf$AF < config$Het_thred),]) / nrow(vcf)
  AvgLL <- getAvgLL(vcf, config$hom_mle, config$het_mle, config$hom_rho, config$het_rho)

  res_df <- data.frame(Name, LOH, HomVar, HetVar, HomRate, HighRate, HetRate, LowRate, AvgLL)

  if (is.null(class_model)) {
    Class <- predict(object = svm_class_model, newdata = res_df) # svm_class_model is the default classification model.
  } else {
    Class <- predict(object = class_model, newdata = res_df)
  }

  if (is.null(regression_model)) {
    Regression <- predict(object = svm_regression_model, newdata = res_df) # svm_regression_model is the default regression model.
  } else {
    Regression <- predict(object = regression_model, newdata = res_df)
  }

  res <- data.frame(Name, Class, Regression)

  res_list <- list("stat" = res_df, "result" = res)
  return(res_list)
}
