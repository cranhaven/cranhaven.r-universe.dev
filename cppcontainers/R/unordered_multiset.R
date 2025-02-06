#' Create unordered multiset
#' 
#' Create an unordered multiset. Unordered multisets are containers of non-unique, unsorted elements.
#' 
#' @param x An integer, numeric, character, or logical vector.
#' 
#' @details Unordered sets are associative containers. They do not provide random access through an index. I.e. \code{s[2]} does not return the second 
#' element.
#' 
#' Unordered means that the container does not enforce elements to be stored in a particular order. This makes unordered multisets in some applications 
#' faster than multisets. I.e., elements in unordered multisets are neither unique nor sorted.
#' 
#' C++ unordered_multiset methods implemented in this package are \link{bucket_count}, \link{clear}, \link{contains}, \link{count}, \link{emplace}, 
#' \link{empty}, \link{erase}, \link{insert}, \link{load_factor}, \link{max_bucket_count}, \link{max_load_factor}, \link{max_size}, \link{merge}, 
#' \link{rehash}, \link{reserve}, and \link{size}. The package also adds the \link{==} operator and various helper functions (\link[=print]{print}, 
#' \link{to_r}, \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppUnorderedMultiset object referencing an unordered_multiset in C++.
#' 
#' @seealso \link{cpp_set}, \link{cpp_unordered_set}, \link{cpp_multiset}.
#' 
#' @examples
#' s <- cpp_unordered_multiset(c(6:10, 7L))
#' s
#' # 10 9 8 7 7 6
#' 
#' insert(s, 4:7)
#' s
#' # 5 4 6 6 7 7 7 8 9 10
#' 
#' print(s, n = 3L)
#' # 5 4 6
#' 
#' erase(s, 6L)
#' s
#' # 5 4 7 7 7 8 9 10
#' 
#' @include classes.R

#' @export
cpp_unordered_multiset <- function(x) {
  check_insert_values(x)
  if(is.integer(x)) {
    s <- methods::new("CppUnorderedMultiset", pointer = unordered_multiset_i(x), type = "integer")
  } else if(is.numeric(x)) {
    s <- methods::new("CppUnorderedMultiset", pointer = unordered_multiset_d(x), type = "double")
  } else if(is.character(x)) {
    s <- methods::new("CppUnorderedMultiset", pointer = unordered_multiset_s(x), type = "string")
  } else if(is.logical(x)) {
    s <- methods::new("CppUnorderedMultiset", pointer = unordered_multiset_b(x), type = "boolean")
  } else {
    stop("x must an integer, numeric, character, or logical vector")
  }
  return(s)
}
