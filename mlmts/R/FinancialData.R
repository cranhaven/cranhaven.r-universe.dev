
#'
#' @title FinancialData
#' @description Dataset containing 50 financial MTS associated with
#' companies in the S&P 500 index.
#' @usage data(FinancialData)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 50 MTS.}
#' \item{\code{classes}}{A character vector indicating the abbreviations associated with the
#' series (companies) in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 654 rows (series length)
#' and 2 columns (dimensions). Each MTS represents a company in the top 50 of the
#' S&P 500 index according to market capitalization. One dimension measures the
#' daily returns of the company, whereas the other measures the daily change in
#' trading volume. The sample period spans from 6th July 2015 to 7th February
#' 2018.
"FinancialData"
