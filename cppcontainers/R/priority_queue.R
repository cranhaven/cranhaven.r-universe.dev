#' Create priority queue
#' 
#' Create a priority queue. Priority queues are hold ordered, non-unique elements.
#' 
#' @param x An integer, numeric, character, or logical vector.
#' @param sorting \code{"descending"} (default) arranges elements in descending order with the largest element at the top. \code{"ascending"} sorts the 
#' elements in the opposite direction, with the smallest element at the top.
#' 
#' @details A priority queue is a container, in which the order of the elements depends on their size rather than their time of insertion. As in a stack, 
#' elements are removed from the top.
#' 
#' C++ priority queue methods implemented in this package are \link{emplace}, \link{empty}, \link{pop}, \link{push}, \link{size}, and \link{top}. The 
#' package also adds various helper functions (\link{print}, \link{sorting}, \link{to_r}, \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppPriorityQueue object referencing a priority_queue in C++.
#' 
#' @seealso \link{cpp_queue}, \link{cpp_stack}.
#' 
#' @examples
#' q <- cpp_priority_queue(4:6)
#' q
#' # First element: 6
#' 
#' emplace(q, 10L)
#' q
#' # First element: 10
#' 
#' emplace(q, 3L)
#' q
#' # First element: 10
#' 
#' top(q)
#' # [1] 10
#' 
#' q <- cpp_priority_queue(4:6, "ascending")
#' q
#' # First element: 4
#' 
#' push(q, 10L)
#' q
#' # First element: 4
#' 
#' @include classes.R

#' @export
cpp_priority_queue <- function(x, sorting = c("descending", "ascending")) {
  check_insert_values(x)
  ascending <- match.arg(sorting) == "ascending"
  if(ascending) {
    if(is.integer(x)) {
      q <- methods::new("CppPriorityQueue", pointer = priority_queue_i_a(x), type = "integer", ascending = TRUE)
    } else if(is.numeric(x)) {
      q <- methods::new("CppPriorityQueue", pointer = priority_queue_d_a(x), type = "double", ascending = TRUE)
    } else if(is.character(x)) {
      q <- methods::new("CppPriorityQueue", pointer = priority_queue_s_a(x), type = "string", ascending = TRUE)
    } else if(is.logical(x)) {
      q <- methods::new("CppPriorityQueue", pointer = priority_queue_b_a(x), type = "boolean", ascending = TRUE)
    } else {
      stop("x must an integer, numeric, character, or logical vector")
    }
  } else {
    if(is.integer(x)) {
      q <- methods::new("CppPriorityQueue", pointer = priority_queue_i_d(x), type = "integer", ascending = FALSE)
    } else if(is.numeric(x)) {
      q <- methods::new("CppPriorityQueue", pointer = priority_queue_d_d(x), type = "double", ascending = FALSE)
    } else if(is.character(x)) {
      q <- methods::new("CppPriorityQueue", pointer = priority_queue_s_d(x), type = "string", ascending = FALSE)
    } else if(is.logical(x)) {
      q <- methods::new("CppPriorityQueue", pointer = priority_queue_b_d(x), type = "boolean", ascending = FALSE)
    } else {
      stop("x must an integer, numeric, character, or logical vector")
    }
  }
  return(q)
}
