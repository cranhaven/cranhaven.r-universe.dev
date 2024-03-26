#' Print a vlda object
#'
#' Print method for vlda
#'
#' @param x A vlda object to print
#' @param ... Other arguments not used by this method
#' @method print vlda
#' @aliases print.vlda
#' @export
#' @rdname print.vlda
#' @return Invisibly returns the result of vlda, which is a list of components that contain the data itself, information etc.
print.vlda <- function(x, ...){
  hidden <- attr(x, "hidden")

  print(x[!names(x) %in% hidden])

}

