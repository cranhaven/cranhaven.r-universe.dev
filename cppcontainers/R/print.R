#' @include classes.R

process_print_ellipsis_ordered <- function(x, e, index) {
  n <- e[["n"]]
  from <- e[["from"]]
  to <- e[["to"]]
  use_n <- FALSE
  reverse <- FALSE
  if(is.null(n)) {
    if(is.null(from) && is.null(to)) {
      n <- 100L
      use_n <- TRUE
    }
  } else {
    check_length(n)
    if(!is.numeric(n)) {
      stop("n must be a number.")
    }
    if(n == 0L) {
      stop("n must be NULL or a non-zero number.")
    }
    if(!is.finite(n)) {
      n <- 0L
    }
    use_n <- TRUE
    if(n < 0L) {
      reverse <- TRUE
      n <- n * (-1)
    } else {
      reverse <- FALSE
    }
  }
  if(use_n) {
    use_from <- FALSE
    use_to <- FALSE
    if(index) {
      from <- to <- NA_integer_
    } else {
      from <- to <- assign_na(get_type(x))
    }
  } else {
    n <- NA_integer_
    if(is.null(from)) {
      use_from <- FALSE
      if(index) {
        from <- NA_integer_
      } else {
        from <- assign_na(get_type(x))
      }
    } else {
      check_length(from)
      check_type(get_type(x), from, index)
      use_from <- TRUE
    }
    if(is.null(to)) {
      use_to <- FALSE
      if(index) {
        to <- NA_integer_
      } else {
        to <- assign_na(get_type(x))
      }
    } else {
      check_length(to)
      check_type(get_type(x), to, index)
      use_to <- TRUE
    }
  }
  return(list(use_n = use_n, n = n, reverse = reverse, use_from = use_from, from = from, use_to = use_to, to = to))
}

process_print_ellipsis_unordered <- function(n) {
  if(is.null(n)) {
    n <- 100L
  } else {
    check_length(n)
    if(!is.numeric(n)) {
      stop("n must be a number.")
    }
    if(n <= 0L) {
      stop("n must be NULL or a strictly positive number.")
    }
    if(!is.finite(n)) {
      n <- 0L
    }
  }
  return(n)
}

#' Print container data
#' 
#' Print the data in a container.
#' 
#' @param x A \code{cppcontainers} object.
#' @param ... An ellipsis for compatibility with the generic method. Accepts the parameters \code{n}, \code{from}, and \code{to}. See \link{to_r} for their 
#' effects. A difference to \link{to_r} is that their omission does not induce the function to print all elements, but to print the first 100 elements. 
#' Stacks, queues, and priority queues ignore the ellipsis and only print the top or first element.
#' 
#' @details \code{print} has no side effects. Unlike \link{to_r}, it does not remove elements from stacks or queues.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{sorting}, \link{to_r}, \link{type}.
#' 
#' @examples
#' s <- cpp_set(4:9)
#' 
#' print(s)
#' # 4 5 6 7 8 9
#' 
#' print(s, n = 3)
#' # 4 5 6
#' 
#' print(s, n = -3)
#' # 9 8 7
#' 
#' print(s, from = 5, to = 7)
#' # 5 6 7
#' 
#' v <- cpp_vector(4:9)
#' 
#' print(v, n = 2)
#' # 4 5
#' 
#' print(v, from = 2, to = 3)
#' # 5 6
#' 
#' print(v, from = 3)
#' # 6 7 8 9
#' 

#' @rdname print
#' @name print
#' @usage print(x, ...)

#' @aliases print,CppSet-method print,CppUnorderedSet-method print,CppMultiset-method print,CppUnorderedMultiset-method print,CppMap-method 
#' print,CppUnorderedMap-method print,CppMultimap-method print,CppUnorderedMultimap-method print,CppStack-method print,CppQueue-method 
#' print,CppPriorityQueue-method print,CppVector-method print,CppDeque-method print,CppForwardList-method print,CppList-method

methods::setGeneric("print", methods::getGeneric("print", package = "base"))

#' @export
methods::setMethod("print", methods::signature(x = "CppSet"), function(x, ...) {
  a <- process_print_ellipsis_ordered(x, list(...), FALSE)
  return(switch(x@type,
    integer = set_print_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    double = set_print_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    string = set_print_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    boolean = set_print_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppUnorderedSet"), function(x, ...) {
  n <- process_print_ellipsis_unordered(list(...)[["n"]])
  return(switch(x@type,
    integer = unordered_set_print_i(x@pointer, n),
    double = unordered_set_print_d(x@pointer, n),
    string = unordered_set_print_s(x@pointer, n),
    boolean = unordered_set_print_b(x@pointer, n)
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppMultiset"), function(x, ...) {
  a <- process_print_ellipsis_ordered(x, list(...), FALSE)
  return(switch(x@type,
    integer = multiset_print_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    double = multiset_print_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    string = multiset_print_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    boolean = multiset_print_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppUnorderedMultiset"), function(x, ...) {
  n <- process_print_ellipsis_unordered(list(...)[["n"]])
  return(switch(x@type,
    integer = unordered_multiset_print_i(x@pointer, n),
    double = unordered_multiset_print_d(x@pointer, n),
    string = unordered_multiset_print_s(x@pointer, n),
    boolean = unordered_multiset_print_b(x@pointer, n)
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppMap"), function(x, ...) {
  a <- process_print_ellipsis_ordered(x, list(...), FALSE)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_print_i_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = map_print_i_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = map_print_i_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = map_print_i_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])),
    double = switch(x@value_type,
      integer = map_print_d_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = map_print_d_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = map_print_d_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = map_print_d_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])),
    string = switch(x@value_type,
      integer = map_print_s_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = map_print_s_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = map_print_s_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = map_print_s_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])),
    boolean = switch(x@value_type,
      integer = map_print_b_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = map_print_b_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = map_print_b_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = map_print_b_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]))
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppUnorderedMap"), function(x, ...) {
  n <- process_print_ellipsis_unordered(list(...)[["n"]])
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_print_i_i(x@pointer, n),
      double = unordered_map_print_i_d(x@pointer, n),
      string = unordered_map_print_i_s(x@pointer, n),
      boolean = unordered_map_print_i_b(x@pointer, n)),
    double = switch(x@value_type,
      integer = unordered_map_print_d_i(x@pointer, n),
      double = unordered_map_print_d_d(x@pointer, n),
      string = unordered_map_print_d_s(x@pointer, n),
      boolean = unordered_map_print_d_b(x@pointer, n)),
    string = switch(x@value_type,
      integer = unordered_map_print_s_i(x@pointer, n),
      double = unordered_map_print_s_d(x@pointer, n),
      string = unordered_map_print_s_s(x@pointer, n),
      boolean = unordered_map_print_s_b(x@pointer, n)),
    boolean = switch(x@value_type,
      integer = unordered_map_print_b_i(x@pointer, n),
      double = unordered_map_print_b_d(x@pointer, n),
      string = unordered_map_print_b_s(x@pointer, n),
      boolean = unordered_map_print_b_b(x@pointer, n))
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppMultimap"), function(x, ...) {
  a <- process_print_ellipsis_ordered(x, list(...), FALSE)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = multimap_print_i_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = multimap_print_i_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = multimap_print_i_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = multimap_print_i_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])),
    double = switch(x@value_type,
      integer = multimap_print_d_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = multimap_print_d_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = multimap_print_d_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = multimap_print_d_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])),
    string = switch(x@value_type,
      integer = multimap_print_s_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = multimap_print_s_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = multimap_print_s_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = multimap_print_s_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])),
    boolean = switch(x@value_type,
      integer = multimap_print_b_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = multimap_print_b_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = multimap_print_b_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = multimap_print_b_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]))
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppUnorderedMultimap"), function(x, ...) {
  n <- process_print_ellipsis_unordered(list(...)[["n"]])
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_print_i_i(x@pointer, n),
      double = unordered_multimap_print_i_d(x@pointer, n),
      string = unordered_multimap_print_i_s(x@pointer, n),
      boolean = unordered_multimap_print_i_b(x@pointer, n)),
    double = switch(x@value_type,
      integer = unordered_multimap_print_d_i(x@pointer, n),
      double = unordered_multimap_print_d_d(x@pointer, n),
      string = unordered_multimap_print_d_s(x@pointer, n),
      boolean = unordered_multimap_print_d_b(x@pointer, n)),
    string = switch(x@value_type,
      integer = unordered_multimap_print_s_i(x@pointer, n),
      double = unordered_multimap_print_s_d(x@pointer, n),
      string = unordered_multimap_print_s_s(x@pointer, n),
      boolean = unordered_multimap_print_s_b(x@pointer, n)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_print_b_i(x@pointer, n),
      double = unordered_multimap_print_b_d(x@pointer, n),
      string = unordered_multimap_print_b_s(x@pointer, n),
      boolean = unordered_multimap_print_b_b(x@pointer, n))
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppStack"), function(x, ...) {
  return(switch(x@type,
    integer = stack_print_i(x@pointer),
    double = stack_print_d(x@pointer),
    string = stack_print_s(x@pointer),
    boolean = stack_print_b(x@pointer)
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppQueue"), function(x, ...) {
  return(switch(x@type,
    integer = queue_print_i(x@pointer),
    double = queue_print_d(x@pointer),
    string = queue_print_s(x@pointer),
    boolean = queue_print_b(x@pointer)
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppPriorityQueue"), function(x) {
  if(x@ascending) {
    return(switch(x@type,
      integer = priority_queue_print_i_a(x@pointer),
      double = priority_queue_print_d_a(x@pointer),
      string = priority_queue_print_s_a(x@pointer),
      boolean = priority_queue_print_b_a(x@pointer)
    ))
  } else {
    return(switch(x@type,
      integer = priority_queue_print_i_d(x@pointer),
      double = priority_queue_print_d_d(x@pointer),
      string = priority_queue_print_s_d(x@pointer),
      boolean = priority_queue_print_b_d(x@pointer)
    ))
  }
})

#' @export
methods::setMethod("print", methods::signature(x = "CppVector"), function(x, ...) {
  a <- process_print_ellipsis_ordered(x, list(...), TRUE)
  return(switch(x@type,
    integer = vector_print_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    double = vector_print_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    string = vector_print_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    boolean = vector_print_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppDeque"), function(x, ...) {
  a <- process_print_ellipsis_ordered(x, list(...), TRUE)
  return(switch(x@type,
    integer = deque_print_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    double = deque_print_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    string = deque_print_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    boolean = deque_print_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppForwardList"), function(x, ...) {
  n <- process_print_ellipsis_unordered(list(...)[["n"]])
  return(switch(x@type,
    integer = forward_list_print_i(x@pointer, n),
    double = forward_list_print_d(x@pointer, n),
    string = forward_list_print_s(x@pointer, n),
    boolean = forward_list_print_b(x@pointer, n)
  ))
})

#' @export
methods::setMethod("print", methods::signature(x = "CppList"), function(x, ...) {
  a <- process_print_ellipsis_ordered(NULL, list(...), TRUE)
  return(switch(x@type,
    integer = list_print_i(x@pointer, a[["n"]], a[["reverse"]]),
    double = list_print_d(x@pointer, a[["n"]], a[["reverse"]]),
    string = list_print_s(x@pointer, a[["n"]], a[["reverse"]]),
    boolean = list_print_b(x@pointer, a[["n"]], a[["reverse"]])
  ))
})
