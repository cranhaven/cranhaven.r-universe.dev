#' @name matchMeanSD
#' @title Transform Data to Desired Mean and Standard Deviation 
#' @description  Transform Data to Desired Mean and Standard Deviation
#' @param data a vector includeing data to be transformed
#' @param mean a value of desired mean
#' @param sd a value of desirred SD
#' @return a vector of transformed vector
#' 
#' @author Jiangtao Gou
#' @author Fengqing Zhang
#' @export
#' @import stats
#' @examples
#' orig_data <- c(1,3,5,10)
#' trans_data <- matchMeanSD(data=orig_data, mean=100, sd=15)
#' print(trans_data)
#' 
#' 
matchMeanSD <- function(data, mean=0, sd=1) {
  meanX <- mean(data)
  sdX <- sd(data)
  b1 <- sd/sdX
  b0 <- mean - b1*meanX
  result <- b0 + b1*data
  return (result)
}