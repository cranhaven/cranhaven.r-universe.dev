#' Mapping phenodates to days of year (DoY)
#' 
#' This function maps estimated phenological dates to days of a year. 
#'
#' @param      start numeric, first month in mapping range. Default is 1. 
#' @param        end numeric, last month in mapping range. Default is 12.
#' @param phenodates numeric vector of length 6 containing estimates of phenological
#'                   dates (green up, start of season, maturity, senescence, end of season and
#'                   dormancy)
#' @param   totalDoY numeric vector, each entry (except for the first) gives a month's
#'                   total number of days 
#' 
#' @export
#' 
#' @details Length of \code{start:end} must be equal to \code{length(totalDoY)-1}.
#' 
#' @examples 
#' x <- c(102,140,177,301,339,242)
#' names(x) <- c("GU", "SoS", "Mat", "Sen", "EoS", "Dor")
#' datesToDoY(phenodates = x)
#' 
#' @return A \code{data.frame} with variables \code{month} and \code{day}
datesToDoY <- function(start=1, end=12, phenodates, 
                       totalDoY=c(0,cumsum(c(31,28,31,30,31,30,31,31,30,31,30,31)))){
  
  MoY <- numeric(length(phenodates))
  DoM <- numeric(length(phenodates))
  period <- start:end
  for( i in 1:length(MoY) ){
    MoY[i] <- period[ sum( totalDoY - phenodates[i] < 0 ) ]
    
    DoM[i] <- phenodates[i] - totalDoY[MoY[i]]
  }
  
  as.data.frame(cbind(month=MONTHS[MoY], day=DoM))
}
