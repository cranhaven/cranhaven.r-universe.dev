#' @title Convert Months to Seasons
#' @description Convert numeric month to season: Winter = Dec-Feb, 
#'   Spring = Mar-May, Summer = Jun-Aug, Fall = Sep-Nov
#' 
#' @param x a vector of months from 1:12
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' months <- sample(1:12, 10, rep = TRUE)
#' months
#' month2Season(months)
#' 
#' @export
#' 
month2Season <- function(x) {
  ssn <- rep(as.character(NA), length(x))
  ssn[x %in% c(12, 1, 2)] <- "Winter"
  ssn[x %in% 3:5] <- "Spring"
  ssn[x %in% 6:8] <- "Summer"
  ssn[x %in% 9:11] <- "Fall"
  factor(ssn, levels = c("Winter", "Spring", "Summer", "Fall"))
}