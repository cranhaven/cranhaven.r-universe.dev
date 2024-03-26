#' @docType data
#' @name data_DF1
#'@aliases  data_DF1
#' @title Data of 96 well plate.
#'
#' @format { A data frame with 96 rows and 10 variables
#' \describe{
#' \item{row}{Row number of multi well well plate.}
#' \item{col}{Column number of multi well plate.}
#' \item{position}{Well position address of multi well plate.}
#' \item{value}{Spectrophotometer reading (OD).}
#' \item{id}{Type of sample.'STD' represent standards and 'sample' represent samples.}
#' \item{type}{Type of sample.'STD1','STD2','STD3' etc represent different standards.'S1','S2','S3' represent different samples.}
#' \item{dilution}{dilution of samples used for the assay.}
#' \item{concentration}{Concentration of respective standards.}
#' \item{compound}{Compound used for the assay.}
#' \item{blankminus}{Blank reduced OD (value - mean(blank))}
#' }
#' }
#'
#' @source User generated dataframe of the 96 well plate.
#'
#' @usage data_DF1
#'
#' @description A complete dataset containing both metadata and spectrophotometer reading
#'
#'@keywords datasets
NULL
