#' @title CombinedPValueMethod
#'
#' @description #' Input is the test statistic of the previous method selected and it returns the combined p-value
#'
#' @param x #' test statistic of method used (i.e., Tippett, Stouffer, etc.)
#' @param name # name of method using
#' @return Combined P-value
#' @examples
#' Output <- SumOfPs(0.1,0.3,.7)
#' Final <- TippettMethod(Output)
#' Combined <- CombinedPValueMethod(Final,"Tippett")
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom stats "%>%"
#'
CombinedPValueMethod = function(x,name){
  output = get("output",envir = .GlobalEnv)
  n <- length(output) #how to get the length from InfinitePs into this function


  if (name == "Tippett"){
    outputs <- 1-(1-x)**n#pbeta(x, shape1 = 1, shape2 = n)
    return(outputs)
  }
  else if (name == "George"){
    outputs <- dt(x,df=n)
    return(outputs)
  }
  else if (name == "Pearson"){
    outputs <- dchisq(x,2*n)
    return(outputs)
  }
  else if (name == "Ed"){
    outputs <- dgamma(x,shape=n)
    return(outputs)
  }
  else if (name == "Stouffer"){
    outputs <- dnorm(x,sd=n)
    return(outputs)
  }
  else if (name == "Fisher"){
    outputs <- dchisq(x,2*n)
    return(outputs)
  }
}
