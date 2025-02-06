#' Create list
#' 
#' Create a list, i.e. a doubly-linked list.
#' 
#' @param x An integer, numeric, character, or logical vector.
#' 
#' @details Doubly-linked means that list elements store a reference both to the previous element and to the following element. This container type, thus, 
#' requires more RAM than a singly-linked list does, but can be iterated in both directions.
#' 
#' C++ list methods implemented in this package are \link{assign}, \link{back}, \link{clear}, \link{emplace}, \link{emplace_back}, \link{emplace_front}, 
#' \link{empty}, \link{erase}, \link{front}, \link{insert}, \link{max_size}, \link{merge}, \link{pop_back}, \link{pop_front}, \link{push_back}, 
#' \link{push_front}, \link{remove.}, \link{resize}, \link{reverse}, \link{size}, \link{sort}, \link{splice}, and \link{unique}. The package also adds the 
#' \link{==} operator and various helper functions (\link{print}, \link{to_r}, \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppList object referencing a list in C++.
#' 
#' @seealso \link{cpp_vector}, \link{cpp_deque}, \link{cpp_forward_list}.
#' 
#' @examples
#' l <- cpp_list(4:6)
#' l
#' # 4 5 6
#' 
#' push_back(l, 1L)
#' l
#' # 4 5 6 1
#' 
#' push_front(l, 2L)
#' l
#' # 2 4 5 6 1
#' 
#' @include classes.R

#' @export
cpp_list <- function(x) {
  check_insert_values(x)
  if(is.integer(x)) {
    s <- methods::new("CppList", pointer = list_i(x), type = "integer")
  } else if(is.numeric(x)) {
    s <- methods::new("CppList", pointer = list_d(x), type = "double")
  } else if(is.character(x)) {
    s <- methods::new("CppList", pointer = list_s(x), type = "string")
  } else if(is.logical(x)) {
    s <- methods::new("CppList", pointer = list_b(x), type = "boolean")
  } else {
    stop("x must an integer, numeric, character, or logical vector")
  }
  return(s)
}
