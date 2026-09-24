
#' @title TJU Fiscal Year
#' 
#' @description ..
#' 
#' @param x \link[base]{integer} scalar
#' 
#' @returns 
#' Function [TJU_Fiscal_Year] returns a length-two \link[base]{Date} \link[base]{vector},
#' indicating the start (July 1 of the previous calendar year) and end date (June 30) of a fiscal year.
#' 
#' @examples 
#' TJU_Fiscal_Year(2022L)
#' 
#' @importFrom zoo as.yearmon
#' @export
TJU_Fiscal_Year <- function(x) {
  # www.jefferson.edu/finance/finance-for-staff/transaction-processing-deadlines.html 
  range(allDates.yearmon(as.yearmon(paste0(x-1, '-07'))), 
        allDates.yearmon(as.yearmon(paste0(x, '-06'))))
}