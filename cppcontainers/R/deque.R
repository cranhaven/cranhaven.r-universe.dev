#' Create deque
#' 
#' Create a deque, i.e. a double-ended queue.
#' 
#' @param x An integer, numeric, character, or logical vector.
#' 
#' @details C++ deque methods implemented in this package are \link{assign}, \link{at}, \link{back}, \link{clear}, \link{emplace}, \link{emplace_back}, 
#' \link{emplace_front}, \link{empty}, \link{erase}, \link{front}, \link{insert}, \link{max_size}, \link{pop_back}, \link{pop_front}, \link{push_back}, 
#' \link{push_front}, \link{resize}, \link{shrink_to_fit}, and \link{size}. The package also adds the \link{==} and \link{[} operators and various helper 
#' functions (\link{print}, \link{to_r}, \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppDeque object referencing a deque in C++.
#' 
#' @seealso \link{cpp_vector}, \link{cpp_forward_list}, \link{cpp_list}.
#' 
#' @examples
#' d <- cpp_deque(4:6)
#' d
#' # 4 5 6
#' 
#' push_back(d, 1L)
#' d
#' # 4 5 6 1
#' 
#' push_front(d, 2L)
#' d
#' # 2 4 5 6 1
#' 
#' @include classes.R

#' @export
cpp_deque <- function(x) {
  check_insert_values(x)
  if(is.integer(x)) {
    s <- methods::new("CppDeque", pointer = deque_i(x), type = "integer")
  } else if(is.numeric(x)) {
    s <- methods::new("CppDeque", pointer = deque_d(x), type = "double")
  } else if(is.character(x)) {
    s <- methods::new("CppDeque", pointer = deque_s(x), type = "string")
  } else if(is.logical(x)) {
    s <- methods::new("CppDeque", pointer = deque_b(x), type = "boolean")
  } else {
    stop("x must an integer, numeric, character, or logical vector")
  }
  return(s)
}
