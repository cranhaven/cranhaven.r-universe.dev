#' Delete consecutive duplicates
#' 
#' Erases consecutive duplicated values from the container by reference.
#' 
#' @param x A CppForwardList or CppList object.
#' @param incomparables Ignored.
#' @param ... Ignored.
#' 
#' @details Duplicated, non-consecutive elements are not removed.
#' 
#' \code{incomparables} and \code{...} are only included for compatibility with the generic \code{base::unique} method and have no effect.
#' 
#' @returns Returns the number of deleted elements.
#' 
#' @seealso \link{erase}, \link{remove.}, \link{sort}.
#' 
#' @examples
#' l <- cpp_forward_list(c(4, 5, 6, 6, 4))
#' l
#' # 4 5 6 6 4
#' 
#' unique(l)
#' # [1] 1
#' l
#' # 4 5 6 4
#' 

#' @aliases unique,CppForwardList-method unique,CppList-method

#' @usage unique(x, incomparables, ...)

methods::setGeneric("unique", methods::getGeneric("unique", package = "base"))

#' @include classes.R

#' @export
methods::setMethod("unique", methods::signature(x = "CppForwardList"), function(x) {
  return(switch(x@type,
    integer = forward_list_unique_i(x@pointer),
    double = forward_list_unique_d(x@pointer),
    string = forward_list_unique_s(x@pointer),
    boolean = forward_list_unique_b(x@pointer)
  ))
})

#' @export
methods::setMethod("unique", methods::signature(x = "CppList"), function(x) {
  return(switch(x@type,
    integer = list_unique_i(x@pointer),
    double = list_unique_d(x@pointer),
    string = list_unique_s(x@pointer),
    boolean = list_unique_b(x@pointer)
  ))
})
