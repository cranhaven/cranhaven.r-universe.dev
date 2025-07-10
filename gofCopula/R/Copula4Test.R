#' Implemented copula for a certain test
#' 
#' \code{\link{gofCopula4Test}} returns for a given test the applicable
#' implemented copula.
#' 
#' In case that the decision for a certain gof test was already done, it is
#' interesting to know which copula can be used with this test.
#' 
#' @param test The test to search for copula.
#' @return A character vector which consists of the names of the copula.
#' @examples
#' 
#' gofCopula4Test("gofRosenblattSnB")
#' 
#' gofCopula4Test("gofPIOSTn")
#' 
#' @export gofCopula4Test
 gofCopula4Test <- function(test) {
  if (!is.character(test)) {
stop(
"The argument 'test' has to be a character."
)
  }
  if (length(test) > 1) {
stop(
"The argument 'test' has to be of length 1."
)
  }
    res <- try(eval(formals(eval(parse(text = test)))$copula), silent = TRUE)
    if (inherits(res, "try-error")) {
      cat("The test is not implemented.", fill = TRUE)
    } else {
      return(res)
    }
}

# depreciated function from earlier package versions. 


#' The function \code{gofWhichCopula} was renamed to \code{gofCopula4Test}.
#' Please use \code{gofCopula4Test}.
#' 
#' Please see and use the function \code{\link{gofCopula4Test}}.
#' 
#' @param test The test to search for copula.
#' @return A character vector which consists of the names of the copula.
#' 
#' @export gofWhichCopula
gofWhichCopula <- function(test) {
warning(
"The function 'gofWhichCopula' was renamed to 'gofCopula4Test'. Please use 
'gofCopula4Test'."
)
  res.f <- gofCopula4Test(test = test)
  return(res.f)
}
