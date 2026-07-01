#' trapezoidal.rule
#'
#' Trapezoidal rule for numerical integration of a function f over a threshold
#'
#' @param f Function to be integrated
#' @param step Distance between points for integration
#' @param y Threshold
trapezoidal.rule<-function(f,step,y){
	f.y<-f[(f>=y)]
      integral<-(sum(f.y)+sum(f.y[-c(1,length(f.y))]))*(step/2)
return(integral)}
#' @noRd
#' @keywords internal
