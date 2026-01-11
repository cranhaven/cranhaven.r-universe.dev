#' @title PearsonsMethod
#'
#' @description #' Combination p-value method that uses Pearson statistic
#' -Summation i= 1 to n log(1-pi) where p equals p value
#'
#' @param x #' InfinitePs
#'
#' @return Combined P-value
#' @examples
#' Output <- SumOfPs(0.1,0.3,.7)
#' Final <- PearsonsMethod(Output)
#' @export
#' @importFrom dplyr "%>%"
#'

PearsonsMethod = function(x) {
            k <- 1
            Len<- length(x)
            temp <-vector("list",Len)
            for (i in x) {
            temp[[k]]<- ((log(1-i)))
            k <- k + 1
            }
            temp1 <- Reduce("+",temp)
            output <- -2 * temp1
            return(output)
            }
