#' Alter the container size
#' 
#' Alter the size of the container by reference.
#' 
#' @param x A \code{cppcontainers} object.
#' @param size The new size of the container.
#' @param value The value of new elements. It defaults to \code{0} for integers and doubles, to \code{""} for strings, and to \code{FALSE} for booleans.
#' 
#' @details If the new size is larger than the former size, the function sets newly added elements in the back to \code{value}.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @examples
#' v <- cpp_vector(4:9)
#' v
#' # 4 5 6 7 8 9
#' 
#' size(v)
#' # 6
#' 
#' resize(v, 10)
#' v
#' # 4 5 6 7 8 9 0 0 0 0
#' 
#' resize(v, 3)
#' v
#' # 4 5 6
#' 

#' @aliases resize,CppSet-method resize,CppUnorderedSet-method resize,CppMultiset-method resize,CppUnorderedMultiset-method resize,CppMap-method 
#' resize,CppUnorderedMap-method resize,CppMultimap-method resize,CppUnorderedMultimap-method resize,CppStack-method resize,CppQueue-method 
#' resize,CppPriorityQueue-method resize,CppVector-method resize,CppDeque-method resize,CppForwardList-method resize,CppList-method

#' @export
methods::setGeneric("resize", function(x, size, value = NULL) standardGeneric("resize"))

#' @include classes.R

#' @export
methods::setMethod("resize", methods::signature(x = "CppVector"), function(x, size, value = NULL) {
  if(is.null(value)) {
    value <- switch(x@type,
      integer = 0L,
      double = 0,
      string = "",
      boolean = FALSE
    )
  } else if(is.numeric(value) && !is.finite(value[1L])) {
    stop("If x is of type integer or double, value must be NULL or a finite number.")
  }
  if(!is.finite(size[1L])) {
    stop("size must be a weakly positive finite number.")
  }
  return(switch(x@type,
    integer = vector_resize_i(x@pointer, size, value),
    double = vector_resize_d(x@pointer, size, value),
    string = vector_resize_s(x@pointer, size, value),
    boolean = vector_resize_b(x@pointer, size, value)
  ))
})

#' @export
methods::setMethod("resize", methods::signature(x = "CppDeque"), function(x, size, value = NULL) {
  if(is.null(value)) {
    value <- switch(x@type,
      integer = 0L,
      double = 0,
      string = "",
      boolean = FALSE
    )
  } else if(is.numeric(value) && !is.finite(value[1L])) {
    stop("If x is of type integer or double, value must be NULL or a finite number.")
  }
  if(!is.finite(size[1L])) {
    stop("size must be a weakly positive finite number.")
  }
  return(switch(x@type,
    integer = deque_resize_i(x@pointer, size, value),
    double = deque_resize_d(x@pointer, size, value),
    string = deque_resize_s(x@pointer, size, value),
    boolean = deque_resize_b(x@pointer, size, value)
  ))
})

#' @export
methods::setMethod("resize", methods::signature(x = "CppForwardList"), function(x, size, value = NULL) {
  if(is.null(value)) {
    value <- switch(x@type,
      integer = 0L,
      double = 0,
      string = "",
      boolean = FALSE
    )
  } else if(is.numeric(value) && !is.finite(value[1L])) {
    stop("If x is of type integer or double, value must be NULL or a finite number.")
  }
  if(!is.finite(size[1L])) {
    stop("size must be a weakly positive finite number.")
  }
  return(switch(x@type,
    integer = forward_list_resize_i(x@pointer, size, value),
    double = forward_list_resize_d(x@pointer, size, value),
    string = forward_list_resize_s(x@pointer, size, value),
    boolean = forward_list_resize_b(x@pointer, size, value)
  ))
})

#' @export
methods::setMethod("resize", methods::signature(x = "CppList"), function(x, size, value = NULL) {
  if(is.null(value)) {
    value <- switch(x@type,
      integer = 0L,
      double = 0,
      string = "",
      boolean = FALSE
    )
  } else if(is.numeric(value) && !is.finite(value[1L])) {
    stop("If x is of type integer or double, value must be NULL or a finite number.")
  }
  if(!is.finite(size[1L])) {
    stop("size must be a weakly positive finite number.")
  }
  return(switch(x@type,
    integer = list_resize_i(x@pointer, size, value),
    double = list_resize_d(x@pointer, size, value),
    string = list_resize_s(x@pointer, size, value),
    boolean = list_resize_b(x@pointer, size, value)
  ))
})
