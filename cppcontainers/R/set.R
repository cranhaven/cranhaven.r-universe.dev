#' Create set
#' 
#' Create a set. Sets are containers of unique, sorted elements.
#' 
#' @param x An integer, numeric, character, or logical vector.
#' 
#' @details Sets are associative containers. They do not provide random access through an index. I.e., \code{s[2]} does not return the second element.
#' 
#' C++ set methods implemented in this package are \link{clear}, \link{contains}, \link{count}, \link{emplace}, \link{empty}, \link{erase}, \link{insert}, 
#' \link{max_size}, \link{merge}, and \link{size}. The package also adds the \link{==} operator and various helper functions (\link{print}, \link{to_r}, 
#' \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppSet object referencing a set in C++.
#' 
#' @seealso \link{cpp_unordered_set}, \link{cpp_multiset}, \link{cpp_unordered_multiset}.
#' 
#' @examples
#' s <- cpp_set(6:9)
#' s
#' # 6 7 8 9
#' 
#' insert(s, 4:7)
#' s
#' # 4 5 6 7 8 9
#' 
#' print(s, from = 6)
#' # 6 7 8 9
#' 
#' s <- cpp_set(c("world", "hello", "there"))
#' s
#' # "hello" "there" "world"
#' 
#' erase(s, "there")
#' s
#' # "hello" "world"
#' 
#' @include classes.R

#' @export
cpp_set <- function(x) {
  check_insert_values(x)
  if(is.integer(x)) {
    s <- methods::new("CppSet", pointer = set_i(x), type = "integer")
  } else if(is.numeric(x)) {
    s <- methods::new("CppSet", pointer = set_d(x), type = "double")
  } else if(is.character(x)) {
    s <- methods::new("CppSet", pointer = set_s(x), type = "string")
  } else if(is.logical(x)) {
    s <- methods::new("CppSet", pointer = set_b(x), type = "boolean")
  } else {
    stop("x must an integer, numeric, character, or logical vector")
  }
  return(s)
}
