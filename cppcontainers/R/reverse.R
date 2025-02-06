#' Reverse element order
#' 
#' Reverses the order of the elements in the container by reference.
#' 
#' @param x A CppForwardList or CppList object.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @examples
#' l <- cpp_forward_list(4:9)
#' l
#' # 4 5 6 7 8 9
#' 
#' reverse(l)
#' l
#' # 
#' 

#' @aliases reverse,CppForwardList-method reverse,CppList-method

#' @export
methods::setGeneric("reverse", function(x) standardGeneric("reverse"))

#' @include classes.R

#' @export
methods::setMethod("reverse", methods::signature(x = "CppForwardList"), function(x) {
  return(switch(x@type,
    integer = forward_list_reverse_i(x@pointer),
    double = forward_list_reverse_d(x@pointer),
    string = forward_list_reverse_s(x@pointer),
    boolean = forward_list_reverse_b(x@pointer)
  ))
})

#' @export
methods::setMethod("reverse", methods::signature(x = "CppList"), function(x) {
  return(switch(x@type,
    integer = list_reverse_i(x@pointer),
    double = list_reverse_d(x@pointer),
    string = list_reverse_s(x@pointer),
    boolean = list_reverse_b(x@pointer)
  ))
})
