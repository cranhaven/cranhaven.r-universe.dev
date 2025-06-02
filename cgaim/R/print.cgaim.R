#' Print a cgaim object
#'
#' Default method to print the results from a call to \code{cgaim}. 
#' Conveniently prints the formula, \code{beta} and \code{alpha} coefficients
#'    as well as the residual sum of squares.
#'    
#' @param x A \code{cgaim} object.
#' @param ... For compatibility with the default \code{print} method. Unused
#'    at the moment.
#'    
#' @returns Function called for its side effect of printing a \code{cgaim} object. Returns no value.
#' 
#' @seealso The main fitting function \code{\link{cgaim}}.
#' 
#' @export
print.cgaim <- function(x, ...){
  cat("Formula:\n")
  trms <- x$terms
  attributes(trms) <- NULL
  print(trms)
  cat("\nCoefficients:\n")
  print(x$beta)
  cat("\nIndices weights:\n")
  for (j in seq_along(x$alpha)){
    cat(names(x$alpha)[j], "\n")
    print(x$alpha[[j]])
  }
  cat("\nResidual sum of squares: ")
  cat(x$rss)
  cat("\n")
  invisible(x)
}