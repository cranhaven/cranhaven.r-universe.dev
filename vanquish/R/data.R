#' Default parameters of config.
#'
#' A dataframe containing default parameters.
#'
#' @format A data frame with 12 variables:
#' \describe{
#'   \item{\code{threshold}}{Threshold for allele frequency}
#'   \item{\code{skew}}{Skewness for allele frequency}
#'   \item{\code{lower}}{Lower bound for allele frequency region}
#'   \item{\code{upper}}{Upper bound for allele frequency region}
#'   \item{\code{ldpthred}}{Threshold to determine low depth}
#'   \item{\code{hom_mle}}{Hom MLE of p in Beta-Binomial model}
#'   \item{\code{het_mle}}{Het MLE of p in Beta-Binomial model}
#'   \item{\code{Hom_thred}}{Threshold between hom and high}
#'   \item{\code{High_thred}}{Threshold between high and het}
#'   \item{\code{Het_thred}}{Threshold between het and low}
#'   \item{\code{hom_rho}}{Hom MLE of rho in Beta-Binomial model}
#'   \item{\code{het_rho}}{Het MLE of rho in Beta-Binomial model}
#' }
#' @source Created by Tao Jiang
"config_df"

#' Default svm classification model.
#'
#' An svm object containing default svm classification model.
#'
#' @format An svm object:
#' @source Created by Tao Jiang
"svm_class_model"

#' Default svm regression model.
#'
#' An svm object containing default svm regression model.
#'
#' @format An svm object:
#' @source Created by Tao Jiang
"svm_regression_model"

#' VCF example file.
#'
#' An example containing a list of 4 data frames.
#'
#' @format A list of 4 data frames:
#' @source Created by Tao Jiang
"vcf_example"
