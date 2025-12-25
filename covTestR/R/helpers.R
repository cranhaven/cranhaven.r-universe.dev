#' Paste Wrapper
#'
#' @keywords internal
past <- function(xmin, ..., xmax){
  x <- c(list(...))

  if(length(x[[1]]) > 0){
    namevec <- c(xmin, paste0(", ", x[[1]]), paste0(" and ", xmax))
  }else{
    namevec <- c(xmin, paste0(" and ", xmax))
  }
  namevec
}


#' Trace of Matrix
#'
#' @keywords internal
tr <- function(x){
  sum(diag(x))
}
