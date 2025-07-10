#' Applicable dimensions and copula for each test
#' 
#' \code{\link{CopulaTestTable}} returns a table which shows the applicable
#' dimensions and copula for each test.
#' 
#' Before performing a gof test with the package on a dataset, it pays out to 
#' know the implemented copula and dimensions for each test. This
#' function is dedicated to help finding the applicable tests depending on the 
#' copula and dimensions available.
#' 
#' @return A character matrix which consists of dimensions for the 
#' combination of tests and copula.
#' @examples
#' 
#' CopulaTestTable()
#' 
#' @export CopulaTestTable
CopulaTestTable = function(){
  copula = c("normal", "t", "clayton", "gumbel", "frank", "joe", "amh", 
             "galambos", "huslerReiss", "tawn", "tev", "fgm", "plackett")
  tests = c("gofCvM", "gofKS", "gofKendallCvM", "gofKendallKS", 
            "gofRosenblattSnB", "gofRosenblattSnC","gofRosenblattGamma", 
            "gofRosenblattChisq", "gofKernel", "gofWhite", "gofPIOSTn",
            "gofPIOSRn", "gofArchmSnB",  "gofArchmSnC", "gofArchmGamma",
            "gofArchmChisq")
  
  tbl = matrix(rbind(c(">=2", ">=2", ">=2", ">=2", ">=2", ">=2", "2", "2", "2", 
                       "2", "2", "2", "2"),
                     c(">=2", ">=2", ">=2", ">=2", ">=2", ">=2", "2", "2", "2", 
                       "2", "2", "2", "2"),   
                     c(">=2", ">=2", ">=2", ">=2", ">=2", ">=2", "2", "2", "2", 
                       "2", "2", "2", "2"),
                     c(">=2", ">=2", ">=2", ">=2", ">=2", ">=2", "2", "2", "2", 
                       "2", "2", "2", "2"),
                     c(">=2", ">=2", ">=2", ">=2", ">=2", ">=2", "2", "2", "-", 
                       "-", "-", "2", "2"),  
                     c(">=2", ">=2", ">=2", ">=2", ">=2", ">=2", "2", "2", "-", 
                       "-", "-", "2", "2"), 
                     c(">=2", ">=2", ">=2", ">=2", ">=2", ">=2", "2", "2", "-", 
                       "-", "-", "2", "2"),  
                     c(">=2", ">=2", ">=2", ">=2", ">=2", ">=2", "2", "2", "-", 
                       "-", "-", "2", "2"), 
                     c("2", "2", "2", "2", "2", "2", "2", "2", "2", "2", "2", 
                       "2", "2"),
                     c("2", "2", "2", "2", "2", "2", "-", "-", "-", "-", "-", 
                       "-", "-"), 
                     c("3", "2", "3", "3", "3", "3", "2", "2", "-", "-", "-", 
                       "2", "2"), 
                     c("3", "2", "3", "3", "3", "3", "2", "2", "-", "-", "-", 
                       "2", "2"), 
                     c("-", "-", ">=2", ">=2", ">=2", ">=2", "2", "-", "-", 
                       "-", "-", "-", "-"), 
                     c("-", "-", ">=2", ">=2", ">=2", ">=2", "2", "-", "-", 
                       "-", "-", "-", "-"),
                     c("-", "-", ">=2", ">=2", ">=2", ">=2", "2", "-", "-", 
                       "-", "-", "-", "-"),
                     c("-", "-", ">=2", ">=2", ">=2", ">=2", "2", "-", "-", 
                       "-", "-", "-", "-")),
               dimnames = list(tests, copula), nrow = length(tests), 
               ncol = length(copula))
  return(data.frame(tbl))
}
