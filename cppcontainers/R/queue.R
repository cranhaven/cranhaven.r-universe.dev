#' Create queue
#' 
#' Create a queue. Queues are first-in, first-out containers.
#' 
#' @param x An integer, numeric, character, or logical vector.
#' 
#' @details The first element added to a queue is the first one to remove.
#' 
#' C++ queue methods implemented in this package are \link{back}, \link{emplace}, \link{empty}, \link{front}, \link{pop}, \link{push}, and \link{size}. The 
#' package also adds the \link{==} operator and various helper functions (\link{print}, \link{to_r}, \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppQueue object referencing a queue in C++.
#' 
#' @examples
#' q <- cpp_queue(1:4)
#' q
#' # First element: 1
#' 
#' push(q, 9L)
#' q
#' # First element: 1
#' back(q)
#' # [1] 9
#' 
#' emplace(q, 10L)
#' back(q)
#' # [1] 10
#' 
#' @include classes.R

#' @export
cpp_queue <- function(x) {
  check_insert_values(x)
  if(is.integer(x)) {
    q <- methods::new("CppQueue", pointer = queue_i(x), type = "integer")
  } else if(is.numeric(x)) {
    q <- methods::new("CppQueue", pointer = queue_d(x), type = "double")
  } else if(is.character(x)) {
    q <- methods::new("CppQueue", pointer = queue_s(x), type = "string")
  } else if(is.logical(x)) {
    q <- methods::new("CppQueue", pointer = queue_b(x), type = "boolean")
  } else {
    stop("x must an integer, numeric, character, or logical vector")
  }
  return(q)
}
