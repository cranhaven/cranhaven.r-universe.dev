#' Export data to R
#' 
#' Export C++ data to an R object.
#' 
#' @param x A \code{cppcontainers} object.
#' @param n The number of elements to export. If \code{n} is positive it exports elements starting at the front of the container. If \code{n} is negative, 
#' it starts at the back. Negative values only work on CppSet, CppMultiset, CppMap, CppMultimap, CppVector, CppDeque, and CppList objects.
#' @param from The first value in CppSet, CppMultiset, CppMap, CppMultimap objects to export. If it is not a member of \code{x}, the export starts at the 
#' subsequent value. In a CppVector or CppDeque object, \code{from} marks the index of the first element to export. Ignored for other classes.
#' @param to The last value in CppSet, CppMultiset, CppMap, CppMultimap objects to export. If it is not a member of \code{x}, the export ends with the 
#' prior value. In a CppVector or CppDeque object, \code{from} marks the index of the last element to export. Ignored for other classes.
#' 
#' @details \code{to_r} has side effects, when applied to stacks, queues, or priority queues. These container types are not iterable. Hence, 
#' \strong{\code{to_r} removes elements from the CppStack, CppQueue, and CppPriorityQueue objects when exporting them to R}. When \code{n} is specified, 
#' the method removes the top \code{n} elements from a stack or priority queue or the first \code{n} elements from a queue. Otherwise, it removes all 
#' elements. Other container types, like sets, etc., are unaffected.
#' 
#' @returns Returns a vector in case of CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppStack, CppQueue, CppPriorityQueue, CppVector, 
#' CppDeque, CppForwardList, and CppList objects. Returns a data frame in case of CppMap, CppUnorderedMap, CppMultimap, and CppUnorderedMultimap objects.
#' 
#' @seealso \link{print}, \link{sorting}, \link{type}.
#' 
#' @examples
#' s <- cpp_set(11:20)
#' to_r(s)
#' # [1] 11 12 13 14 15 16 17 18 19 20
#' 
#' to_r(s, n = 4)
#' # [1] 11 12 13 14
#' 
#' to_r(s, n = -4)
#' # [1] 20 19 18 17
#' 
#' to_r(s, from = 14)
#' # [1] 14 15 16 17 18 19 20
#' 
#' to_r(s, to = 18)
#' # [1] 11 12 13 14 15 16 17 18
#' 
#' to_r(s, from = 14, to = 18)
#' # [1] 14 15 16 17 18
#' 
#' m <- cpp_unordered_multimap(c("hello", "hello", "there"), 4:6)
#' to_r(m)
#' #     key value
#' # 1 there     6
#' # 2 hello     4
#' # 3 hello     5
#' 
#' s <- cpp_stack(11:20)
#' to_r(s, n = 3)
#' # [1] 20 19 18
#' s
#' # Top element: 17
#' 

#' @aliases to_r,CppSet-method to_r,CppUnorderedSet-method to_r,CppMultiset-method to_r,CppUnorderedMultiset-method to_r,CppMap-method 
#' to_r,CppUnorderedMap-method to_r,CppMultimap-method to_r,CppUnorderedMultimap-method to_r,CppStack-method to_r,CppQueue-method 
#' to_r,CppPriorityQueue-method to_r,CppVector-method to_r,CppDeque-method to_r,CppForwardList-method to_r,CppList-method

#' @export
methods::setGeneric("to_r", function(x, n = NULL, from = NULL, to = NULL) standardGeneric("to_r"))

#' @include classes.R

process_to_r_args_ordered <- function(x, n, from, to, index) {
  use_n <- !is.null(n) && is.finite(n)
  reverse <- FALSE
  if(use_n) {
    check_length(n)
    if(!is.numeric(n)) {
      stop("n must be a number.")
    }
    if(n == 0L) {
      stop("n must be NULL or a non-zero number.")
    }
    if(n < 0L) {
      reverse <- TRUE
      n <- n * (-1)
    } else {
      reverse <- FALSE
    }
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

process_to_r_args_unordered <- function(n) {
  if(is.null(n)) {
    n <- 0L
  } else {
    check_length(n)
    if(!is.numeric(n) || !is.finite(n) || n <= 0L) {
      stop("n must be a finite, strictly positive number.")
    }
  }
  return(n)
}

#' @export
methods::setMethod("to_r", methods::signature(x = "CppSet"), function(x, n = NULL, from = NULL, to = NULL) {
  a <- process_to_r_args_ordered(x, n, from, to, FALSE)
  return(switch(x@type,
    integer = set_to_r_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    double = set_to_r_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    string = set_to_r_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    boolean = set_to_r_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppUnorderedSet"), function(x, n = NULL) {
  n <- process_to_r_args_unordered(n)
  return(switch(x@type,
    integer = unordered_set_to_r_i(x@pointer, n),
    double = unordered_set_to_r_d(x@pointer, n),
    string = unordered_set_to_r_s(x@pointer, n),
    boolean = unordered_set_to_r_b(x@pointer, n)
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppMultiset"), function(x, n = NULL, from = NULL, to = NULL) {
  a <- process_to_r_args_ordered(x, n, from, to, FALSE)
  return(switch(x@type,
    integer = multiset_to_r_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    double = multiset_to_r_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    string = multiset_to_r_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    boolean = multiset_to_r_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppUnorderedMultiset"), function(x, n = NULL) {
  n <- process_to_r_args_unordered(n)
  return(switch(x@type,
    integer = unordered_multiset_to_r_i(x@pointer, n),
    double = unordered_multiset_to_r_d(x@pointer, n),
    string = unordered_multiset_to_r_s(x@pointer, n),
    boolean = unordered_multiset_to_r_b(x@pointer, n)
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppMap"), function(x, n = NULL, from = NULL, to = NULL) {
  a <- process_to_r_args_ordered(x, n, from, to, FALSE)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_to_r_i_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = map_to_r_i_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = map_to_r_i_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = map_to_r_i_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])),
    double = switch(x@value_type,
      integer = map_to_r_d_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = map_to_r_d_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = map_to_r_d_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = map_to_r_d_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])),
    string = switch(x@value_type,
      integer = map_to_r_s_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = map_to_r_s_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = map_to_r_s_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = map_to_r_s_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])),
    boolean = switch(x@value_type,
      integer = map_to_r_b_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = map_to_r_b_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = map_to_r_b_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = map_to_r_b_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]))
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppUnorderedMap"), function(x, n = NULL) {
  n <- process_to_r_args_unordered(n)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_to_r_i_i(x@pointer, n),
      double = unordered_map_to_r_i_d(x@pointer, n),
      string = unordered_map_to_r_i_s(x@pointer, n),
      boolean = unordered_map_to_r_i_b(x@pointer, n)),
    double = switch(x@value_type,
      integer = unordered_map_to_r_d_i(x@pointer, n),
      double = unordered_map_to_r_d_d(x@pointer, n),
      string = unordered_map_to_r_d_s(x@pointer, n),
      boolean = unordered_map_to_r_d_b(x@pointer, n)),
    string = switch(x@value_type,
      integer = unordered_map_to_r_s_i(x@pointer, n),
      double = unordered_map_to_r_s_d(x@pointer, n),
      string = unordered_map_to_r_s_s(x@pointer, n),
      boolean = unordered_map_to_r_s_b(x@pointer, n)),
    boolean = switch(x@value_type,
      integer = unordered_map_to_r_b_i(x@pointer, n),
      double = unordered_map_to_r_b_d(x@pointer, n),
      string = unordered_map_to_r_b_s(x@pointer, n),
      boolean = unordered_map_to_r_b_b(x@pointer, n))
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppMultimap"), function(x, n = NULL, from = NULL, to = NULL) {
  a <- process_to_r_args_ordered(x, n, from, to, FALSE)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = multimap_to_r_i_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = multimap_to_r_i_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = multimap_to_r_i_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = multimap_to_r_i_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])),
    double = switch(x@value_type,
      integer = multimap_to_r_d_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = multimap_to_r_d_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = multimap_to_r_d_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = multimap_to_r_d_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])),
    string = switch(x@value_type,
      integer = multimap_to_r_s_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = multimap_to_r_s_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = multimap_to_r_s_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = multimap_to_r_s_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])),
    boolean = switch(x@value_type,
      integer = multimap_to_r_b_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      double = multimap_to_r_b_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      string = multimap_to_r_b_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
      boolean = multimap_to_r_b_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]))
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppUnorderedMultimap"), function(x, n = NULL) {
  n <- process_to_r_args_unordered(n)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_to_r_i_i(x@pointer, n),
      double = unordered_multimap_to_r_i_d(x@pointer, n),
      string = unordered_multimap_to_r_i_s(x@pointer, n),
      boolean = unordered_multimap_to_r_i_b(x@pointer, n)),
    double = switch(x@value_type,
      integer = unordered_multimap_to_r_d_i(x@pointer, n),
      double = unordered_multimap_to_r_d_d(x@pointer, n),
      string = unordered_multimap_to_r_d_s(x@pointer, n),
      boolean = unordered_multimap_to_r_d_b(x@pointer, n)),
    string = switch(x@value_type,
      integer = unordered_multimap_to_r_s_i(x@pointer, n),
      double = unordered_multimap_to_r_s_d(x@pointer, n),
      string = unordered_multimap_to_r_s_s(x@pointer, n),
      boolean = unordered_multimap_to_r_s_b(x@pointer, n)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_to_r_b_i(x@pointer, n),
      double = unordered_multimap_to_r_b_d(x@pointer, n),
      string = unordered_multimap_to_r_b_s(x@pointer, n),
      boolean = unordered_multimap_to_r_b_b(x@pointer, n))
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppStack"), function(x, n = NULL) {
  n <- process_to_r_args_unordered(n)
  return(switch(x@type,
    integer = stack_to_r_i(x@pointer, n),
    double = stack_to_r_d(x@pointer, n),
    string = stack_to_r_s(x@pointer, n),
    boolean = stack_to_r_b(x@pointer, n)
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppQueue"), function(x, n = NULL) {
  n <- process_to_r_args_unordered(n)
  return(switch(x@type,
    integer = queue_to_r_i(x@pointer, n),
    double = queue_to_r_d(x@pointer, n),
    string = queue_to_r_s(x@pointer, n),
    boolean = queue_to_r_b(x@pointer, n)
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppPriorityQueue"), function(x, n = NULL) {
  n <- process_to_r_args_unordered(n)
  if(x@ascending) {
    return(switch(x@type,
      integer = priority_queue_to_r_i_a(x@pointer, n),
      double = priority_queue_to_r_d_a(x@pointer, n),
      string = priority_queue_to_r_s_a(x@pointer, n),
      boolean = priority_queue_to_r_b_a(x@pointer, n)
    ))
  } else {
    return(switch(x@type,
      integer = priority_queue_to_r_i_d(x@pointer, n),
      double = priority_queue_to_r_d_d(x@pointer, n),
      string = priority_queue_to_r_s_d(x@pointer, n),
      boolean = priority_queue_to_r_b_d(x@pointer, n)
    ))
  }
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppVector"), function(x, n = NULL, from = NULL, to = NULL) {
  a <- process_to_r_args_ordered(x, n, from, to, TRUE)
  return(switch(x@type,
    integer = vector_to_r_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    double = vector_to_r_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    string = vector_to_r_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    boolean = vector_to_r_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppDeque"), function(x, n = NULL, from = NULL, to = NULL) {
  a <- process_to_r_args_ordered(x, n, from, to, TRUE)
  return(switch(x@type,
    integer = deque_to_r_i(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    double = deque_to_r_d(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    string = deque_to_r_s(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]]),
    boolean = deque_to_r_b(x@pointer, a[["use_n"]], a[["n"]], a[["reverse"]], a[["use_from"]], a[["from"]], a[["use_to"]], a[["to"]])
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppForwardList"), function(x, n = NULL) {
  n <- process_to_r_args_unordered(n)
  return(switch(x@type,
    integer = forward_list_to_r_i(x@pointer, n),
    double = forward_list_to_r_d(x@pointer, n),
    string = forward_list_to_r_s(x@pointer, n),
    boolean = forward_list_to_r_b(x@pointer, n)
  ))
})

#' @export
methods::setMethod("to_r", methods::signature(x = "CppList"), function(x, n = NULL) {
  if(is.null(n) || !is.finite(n[1L])) {
    n <- 0L
    reverse <- FALSE
  } else {
    check_length(n)
    if(n == 0L) {
      stop("n must be NULL or a non-zero number.")
    }
    reverse <- n < 0L
    if(reverse) {
      n <- n * (-1)
    }
  }
  return(switch(x@type,
    integer = list_to_r_i(x@pointer, n, reverse),
    double = list_to_r_d(x@pointer, n, reverse),
    string = list_to_r_s(x@pointer, n, reverse),
    boolean = list_to_r_b(x@pointer, n, reverse)
  ))
})
