#' @title Edgington Method
#'
#' @description #' Combination p-value method that uses Edgington statistic
#' Summation i=1 to n pi where p equals p-value
#'
#' @param x #' SumOfPs
#'
#' @return Combined P-value
#' @examples
#' Output <- SumOfPs(0.1,0.3,.7)
#' Final <- EdMethod(Output)
#' @export
#' @importFrom dplyr "%>%"
#'
        EdMethod = function(x) {
            k <- 1
            Len<- length(x)
            temp <-vector("list",Len)
            for (i in x) {
            temp[[k]]<- i
            k <- k + 1
            }
            temp1 <- Reduce("+",temp)
            output <- temp1
            return(output)
            }
