#' @title SumOfPs
#'
#' @description Converts a list of p-values into a list, n= 2,3,...,k
#'
#' @param x #' Input n p-values  n = 2,3,...,k
#' @param ... #list of p values
#' @return List of p-values
#' @examples
#' Output <- SumOfPs(0.1,0.3,.7)
#' @export
#' @importFrom dplyr "%>%"
#'

SumOfPs= function(x,...){
    kwargs<-list(...)
    pos <- 1
    envir = as.environment(pos)
    output <- (c(x,kwargs))
    assign("output",output, envir = envir)
    return(output)
}

