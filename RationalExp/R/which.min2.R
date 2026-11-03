#' Find the min of a list starting from the end
#'
#' @param x list of elements
#' @param last.index starting from the last index (=TRUE). Default is false
#' @param ... hypotetical additional elements
#'
#' @return
#' @export
#'
#' @examples
which.min2 <- function(x, last.index = FALSE, ...){
  if(last.index) max(which(x == min(x, ...))) else which.min(x)
}
