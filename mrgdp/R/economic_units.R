#' Economic units
#'
#' @param data data downloaded with economic_census() function
#'
#' @export
#' @return a data.frame
#'

economic_units <- function(data=NA){
  data <- data[,1:6]
  data <- as.data.frame(data)
  data <- data[is.na(data[,4]), ]
  data <- data[is.na(data[,3]), ]
  data[,2]<- as.numeric(data[,2])
  nmun<-max(data[,2], na.rm = TRUE)
  data <- data[data[,2] %in% 1:nmun, ]
  data <- data[,c(1,2,5)]
}
