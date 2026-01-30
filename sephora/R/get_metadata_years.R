#' Returns metadata to construct x-axis and legend of \code{\link[sephora]{plot.sephora}}
#' 
#' Metadata either from a numeric vector or a \code{\link[sephora]{sephora-class}} object
#' 
#' @param         x numeric vector or \code{\link[sephora]{sephora-class}} object
#' @param startYear integer, \code{x} initial year
#' @param   endYear integer, \code{x} final year
#' @param frequency integer giving number of observations per season. 
#'                  Default is 23.
#'                  
#' @export 
#' 
#' @examples 
#' x <- deciduous_polygon[1,]
#' y <- get_metadata_years(x=x)
#' str(y)
#' 
#' @return A list of 2 components: 
#' \item{xDates}{date vector containing DoY (acquisition date) using format yyyy-mm-dd}
#' \item{xLabels}{character vector containing period of study years using format "'YY"}
#' 
get_metadata_years <- function(x, startYear=2000, endYear=2021, frequency=23){
  years <- startYear:endYear
  initial_dates <- sapply(1:length(years), function(s) paste0( years[s], "-01-01" ) )
  
  x_dates <- as.Date(c())
  for(i in 1:(length(x)/frequency)){
    x_dates <- c(x_dates, as.Date(initial_dates[i]) + seq(1,365,by=365/frequency)-1)
  }
  
  startLabel <- startYear %% 100
  endLabel <- endYear %% 100
  
  labels <- startLabel:endLabel
  
  for(i in 1:length(labels)){
    if( nchar(labels[i]) == 1 ){
      labels[i] <- paste0(0, labels[i])
    }
  }
  
  list(xDates=x_dates, xLabels=labels)  
}
