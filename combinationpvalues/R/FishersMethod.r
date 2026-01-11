#' @title FishersMethod
#'
#' @description #' Combination p-value method that uses Fishers statistic
#' Summation i=1 to n log of pi where p equals p-value
#'
#' @param x #' SumOfPs
#'
#' @return Combined P-value
#' @examples
#' Output <- SumOfPs(0.1,0.3,.7)
#' Final <- FishersMethod(Output)
#' @export
#' @importFrom dplyr "%>%"
#'
FishersMethod = function(x) {
    k <- 1
    Len<- length(x)
    temp <-vector("list",Len)
    for (i in x) {
    temp[[k]]<-log(i)
    k <- k + 1
    }
    temp1 <- Reduce("+",temp)
    output <- -2 * temp1
    return(output)
    }
