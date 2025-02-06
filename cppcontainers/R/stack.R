#' Create stack
#' 
#' Create a stack. Stacks are last-in, first-out containers.
#' 
#' @param x An integer, numeric, character, or logical vector.
#' 
#' @details The last element added to a stack is the first one to remove.
#' 
#' C++ stack methods implemented in this package are \link{emplace}, \link{empty}, \link{pop}, \link{push}, \link{size}, and \link{top}. The package also 
#' adds the \link{==} operator and various helper functions (\link{print}, \link{to_r}, \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppStack object referencing a stack in C++.
#' 
#' @seealso \link{cpp_queue}, \link{cpp_priority_queue}.
#' 
#' @examples
#' s <- cpp_stack(4:6)
#' s
#' # Top element: 6
#' 
#' emplace(s, 3L)
#' s
#' # Top element: 3
#' 
#' push(s, 9L)
#' s
#' # Top element: 9
#' 
#' pop(s)
#' s
#' # Top element: 3
#' 
#' @include classes.R

#' @export
cpp_stack <- function(x) {
  check_insert_values(x)
  if(is.integer(x)) {
    s <- methods::new("CppStack", pointer = stack_i(x), type = "integer")
  } else if(is.numeric(x)) {
    s <- methods::new("CppStack", pointer = stack_d(x), type = "double")
  } else if(is.character(x)) {
    s <- methods::new("CppStack", pointer = stack_s(x), type = "string")
  } else if(is.logical(x)) {
    s <- methods::new("CppStack", pointer = stack_b(x), type = "boolean")
  } else {
    stop("x must an integer, numeric, character, or logical vector")
  }
  return(s)
}
