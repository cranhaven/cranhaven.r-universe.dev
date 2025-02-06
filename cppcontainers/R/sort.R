#' Sort elements
#' 
#' Sorts the elements in a container by reference.
#' 
#' @param x A CppForwardList or CppList object.
#' @param decreasing Ignored.
#' @param ... Ignored.
#' 
#' @details \code{decreasing} and \code{...} are only included for compatibility with the generic \code{base::sort} method and have no effect.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{sorting}, \link{unique}.
#' 
#' @examples
#' l <- cpp_forward_list(c(3, 2, 4))
#' l
#' # 3 2 4
#' 
#' sort(l)
#' l
#' # 2 3 4
#' 

#' @aliases sort,CppForwardList-method sort,CppList-method

#' @usage sort(x, decreasing, ...)

methods::setGeneric("sort", methods::getGeneric("sort", package = "base"))

#' @include classes.R

#' @export
methods::setMethod("sort", methods::signature(x = "CppForwardList"), function(x) {
  return(switch(x@type,
    integer = forward_list_sort_i(x@pointer),
    double = forward_list_sort_d(x@pointer),
    string = forward_list_sort_s(x@pointer),
    boolean = forward_list_sort_b(x@pointer)
  ))
})

#' @export
methods::setMethod("sort", methods::signature(x = "CppList"), function(x) {
  return(switch(x@type,
    integer = list_sort_i(x@pointer),
    double = list_sort_d(x@pointer),
    string = list_sort_s(x@pointer),
    boolean = list_sort_b(x@pointer)
  ))
})
