#' Create forward list
#' 
#' Create a forward list, i.e. a singly-linked list.
#' 
#' @param x An integer, numeric, character, or logical vector.
#' 
#' @details @details Singly-linked means that list elements store a reference only to the following element. This container type, thus, requires less RAM 
#' than a doubly-linked list does, but can only be iterated in the forward direction.
#' 
#' C++ forward_list methods implemented in this package are \link{assign}, \link{clear}, \link{emplace_after}, \link{emplace_front}, \link{empty}, 
#' \link{erase_after}, \link{front}, \link{insert_after}, \link{max_size}, \link{pop_front}, \link{push_front}, \link{remove.}, \link{resize}, 
#' \link{reverse}, \link{sort}, \link{splice_after}, and \link{unique}. The package also adds the \link{==} operator and various helper functions 
#' (\link{print}, \link{to_r}, \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppForwardList object referencing a forward_list in C++.
#' 
#' @seealso \link{cpp_vector}, \link{cpp_deque}, \link{cpp_list}.
#' 
#' @examples
#' v <- cpp_forward_list(4:6)
#' v
#' # 4 5 6
#' 
#' push_front(v, 10L)
#' v
#' # 10 4 5 6
#' 
#' pop_front(v)
#' v
#' # 4 5 6
#' 
#' @include classes.R

#' @export
cpp_forward_list <- function(x) {
  check_insert_values(x)
  if(is.integer(x)) {
    s <- methods::new("CppForwardList", pointer = forward_list_i(x), type = "integer")
  } else if(is.numeric(x)) {
    s <- methods::new("CppForwardList", pointer = forward_list_d(x), type = "double")
  } else if(is.character(x)) {
    s <- methods::new("CppForwardList", pointer = forward_list_s(x), type = "string")
  } else if(is.logical(x)) {
    s <- methods::new("CppForwardList", pointer = forward_list_b(x), type = "boolean")
  } else {
    stop("x must an integer, numeric, character, or logical vector")
  }
  return(s)
}
