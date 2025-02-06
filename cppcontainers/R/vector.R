#' Create vector
#' 
#' Create a vector. Vectors are dynamic, contiguous arrays.
#' 
#' @param x An integer, numeric, character, or logical vector.
#' 
#' @details R vectors are similar to C++ vectors. These sequence containers allow for random access. I.e., you can directly access the fourth element via 
#' its index \code{x[4]}, without iterating through the first three elements before. Vectors are comparatively space-efficient, requiring less RAM per 
#' element than many other container types.
#' 
#' One advantage of C++ vectors over R vectors is their ability to reduce the number of copies made during modifications. \link{reserve}, e.g., reserves 
#' space for future vector extensions.
#' 
#' C++ vector methods implemented in this package are \link{assign}, \link{at}, \link{back}, \link{capacity}, \link{clear}, \link{emplace}, 
#' \link{emplace_back}, \link{empty}, \link{erase}, \link{flip}, \link{front}, \link{insert}, \link{max_size}, \link{pop_back}, \link{push_back}, 
#' \link{reserve}, \link{resize}, \link{shrink_to_fit}, and \link{size}. The package also adds the \link{==} and \link{[} operators and various helper 
#' functions (\link{print}, \link{to_r}, \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppVector object referencing a vector in C++.
#' 
#' @seealso \link{cpp_deque}, \link{cpp_forward_list}, \link{cpp_list}.
#' 
#' @examples
#' v <- cpp_vector(4:6)
#' v
#' # 4 5 6
#' 
#' push_back(v, 3L)
#' v
#' # 4 5 6 3
#' 
#' print(v, from = 3)
#' # 6 3
#' 
#' print(v, n = -2)
#' # 3 6
#' 
#' pop_back(v)
#' v
#' # 4 5 6
#' 
#' @include classes.R

#' @export
cpp_vector <- function(x) {
  check_insert_values(x)
  if(is.integer(x)) {
    s <- methods::new("CppVector", pointer = vector_i(x), type = "integer")
  } else if(is.numeric(x)) {
    s <- methods::new("CppVector", pointer = vector_d(x), type = "double")
  } else if(is.character(x)) {
    s <- methods::new("CppVector", pointer = vector_s(x), type = "string")
  } else if(is.logical(x)) {
    s <- methods::new("CppVector", pointer = vector_b(x), type = "boolean")
  } else {
    stop("x must an integer, numeric, character, or logical vector")
  }
  return(s)
}
