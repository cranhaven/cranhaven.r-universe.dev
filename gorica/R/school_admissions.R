#' High School Admissions Data
#'
#' This dataset, provided by the UCLA Statistical Consulting Group (2021),
#' contains information on factors that influence whether or not a high school
#' senior is admitted into a very competitive engineering school. The dataset
#' includes the following variables:
#'
#' \tabular{lll}{
#' \strong{female} \tab \code{binary} \tab Binary variable indicating the gender of the student. \cr
#' \strong{apcalc} \tab \code{binary} \tab Binary variable indicating whether or not the student took Advanced Placement calculus in high school. \cr
#' \strong{admit} \tab \code{binary} \tab Binary outcome variable indicating admission status, where 1 represents admission and 0 represents non-admission.
#' }
#'
#' The dataset is used for exact logistic regression analysis due to the binary outcome variable. It aims to identify the factors that contribute to admission decisions in a highly competitive engineering school. Since the dataset has a small sample size, specialized procedures are required for accurate estimation.
#'
#' @docType data
#' @keywords datasets
#' @name school_admissions
#' @usage data(school_admissions)
#' @references Introduction to SAS. UCLA: Statistical Consulting Group. from
#' https://stats.oarc.ucla.edu/sas/modules/introduction-to-the-features-of-sas/
#' (accessed August 22, 2021).
#' @format A data frame with 30 rows and 3 variables.
NULL
