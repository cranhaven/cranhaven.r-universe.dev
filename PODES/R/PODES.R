#' Create Village Potential Statistics of Indonesia
#' @export
#' @param data dataframes
#' @return A dataframes of Village Potential Statistics of Indonesia
#' @import readxl
#'
#'
#' @examples
#' PODES(PODES2019)
#'
PODES <-function(data){
  ringkasan<-summary(data);
  return(ringkasan);
}

