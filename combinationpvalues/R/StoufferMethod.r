#' @title StoufferMethod
#'
#' @description #' Combination p-value method that uses Stouffer statistic
#' Summation i=1 to n inverse CDF of N(0,1)(pi) where p equals p-value
#'
#' @param x #' SumOfPs
#'
#' @return Combined P-value
#' @examples
#' Output <- SumOfPs(0.1,0.3,.7)
#' Final <- StoufferMethod(Output)
#' @export
#' @importFrom qnorm "%>%"
#' @importFrom stats "%>%"
#'
#'
StoufferMethod = function(x){
            k <- 1
            Len<- length(x)
            temp <-vector("list",Len)
            for (i in x) {
            temp[[k]]<- qnorm(i) #inverse CDF
            k <- k + 1
            }
            temp1 <- Reduce("+",temp)
            output <- temp1
            return(output)
            }
