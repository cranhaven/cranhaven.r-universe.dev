#' Create Gold price data
#' @export
#' @param data dataframes
#' @return A dataframes of gold price data
#' @import readxl
#'
#'
#'
GOLDprice <-function(data){
  ringkasan<-summary(data);
  return(ringkasan);
}

