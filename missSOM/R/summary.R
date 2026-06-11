#' Summary and print methods for missSOM objects
#'
#' @param object a \code{missSOM} object
#' @param ... Not used.
#'
#' @usage 
#' \method{summary}{missSOM}(object, \dots)
#' \method{print}{missSOM}(x, \dots)
#' @export
#' @description Summary and print methods for \code{missSOM} objects. The \code{print} 
#' method shows the dimensions and the topology of the map; if information on the training data is included, the \code{summary} 
#' method additionally prints information on the size of the data, the distance functions used, and the mean distance of an 
#' object to its closest codebookvector, which is an indication of the quality of the mapping.
#' @seealso \code{\link{imputeSOM}}
#' @return No return a value.
#' 
#' @examples 
#' data(wines)
#' som.wines <- imputeSOM(scale(wines), grid = somgrid(5, 5, "hexagonal"))
#' som.wines
#' summary(som.wines)
#'
summary.missSOM <- function(object, ...)
{
  cat("SOM of size ", object$grid$xdim, "x", object$grid$ydim,
      " with a ", object$grid$topo,
      if (object$grid$toroidal) "toroidal", " topology and a ",
      as.character(object$grid$neighbourhood.fct),
      " neighbourhood function.", sep="")

  cat("\nDistance measure(s) used: ",
      paste(object$dist.fcts, collapse = ", "), ".", sep = "")
  
  if (!is.null(object$data)) {
    cat("\nTraining data included:",
        nrow(object$ximp), "objects.")
    cat("\nMean distance to the closest unit in the map: ",
        round(mean(object$distances, na.rm = TRUE), 3), ".", sep = "")
  } else {
    cat("\nNo training data included in the object.")
  }
  
  cat("\n")
  
  invisible()
}

#' @param x a \code{kohonen} object
#'
#' @param ... Not used.
#' @rdname summary.missSOM
#' @export
#' 
print.missSOM <- function(x, ...)
{
  cat("SOM of size ", x$grid$xdim, "x", x$grid$ydim,
      " with a ", x$grid$topo, if (x$grid$toroidal) " toroidal",
      " topology.", sep="")
  if (!is.null(x$ximp))
    cat("\nTraining data included.")
  cat("\n")
  
  invisible()
}
