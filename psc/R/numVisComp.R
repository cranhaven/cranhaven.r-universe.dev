#' Visualising Categorical Data
#'
#' A function which summarises numerical data using a density plots
#'
#' @param p a ggplot object
#' @param x a numeric vector
#' @return a ggplot object
#' @export
numVisComp <- function(p,x){
  xnew <- NULL
  dnew <- data.frame("xnew"=x)
  p + geom_density(aes(x=xnew,y=-after_stat(density)),data=dnew, fill="#404080",color="#404080" )
}
