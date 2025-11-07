#' @title Print a contents object
#' @description
#' \code{print.contents} prints the results of the content function.
#' @param x a object of class \code{contents}
#' @param ... not used.
#' @return 
#' No return value, called for side effects.
#'
#' @examples
#' testdata <- data.frame(height=c(4, 5, 3, 2, 100),
#'                        weight=c(39, 88, NA, 15, -2),
#'                        names=c("Bill","Dean", "Sam", NA, "Jane"),
#'                        race=c('b', 'w', 'w', 'o', 'b'))
#'
#' x <- contents(testdata)
#' print(x)
#'
#' @rdname print.contents
#' @importFrom crayon blue
#' @export

print.contents <- function(x, ...){
  if(!inherits(x, "contents")) stop("Must be class 'tab'")

  cat("\nThe data frame", x$dfname, "has",
      format(x$nrow, big.mark=","), "observations and",
      format(x$ncol, big.mark=","), "variables.\n")

  if(x$nrow == 0 | x$ncol == 0)return(NULL)

  cat("\n", crayon::blue$underline$bold('Overall'), "\n", sep="")
  print(x$overall, row.names=FALSE, right=FALSE)

  if(!is.null(x$qvars)){
      cat("\n", crayon::blue$underline$bold('Numeric Variables'),
      "\n", sep="")
      print(x$qvars)
  }


  if(!is.null(x$cvars)){
      cat("\n",
      crayon::blue$underline$bold('Categorical Variables'),
      "\n", sep="")
      print.data.frame(x$cvars, right=FALSE, row.names=FALSE)
  }
  return(NULL)
}
