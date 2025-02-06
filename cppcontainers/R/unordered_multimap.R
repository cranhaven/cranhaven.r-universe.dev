#' Create unordered multimap
#' 
#' Create an unordered multimap. Unordered multimaps are key-value pairs with non-unique keys.
#' 
#' @param keys An integer, numeric, character, or logical vector.
#' @param values An integer, numeric, character, or logical vector.
#' 
#' @details Unordered multimaps are associative containers. They do not provide random access through an index. I.e. \code{m[2]} does not return the second 
#' element.
#' 
#' Unordered means that the container does not enforce elements to be stored in a particular order. This makes unordered multimaps in some applications 
#' faster than multimaps.
#' 
#' C++ unordered_multimap methods implemented in this package are \link{bucket_count}, \link{clear}, \link{contains}, \link{count}, \link{emplace}, 
#' \link{empty}, \link{erase}, \link{insert}, \link{load_factor}, \link{max_bucket_count}, \link{max_load_factor}, \link{max_size}, \link{merge}, 
#' \link{rehash}, \link{reserve}, and \link{size}. The package also adds the \link{==} operator and various helper functions (\link{print}, \link{to_r}, 
#' \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppUnorderedMultimap object referencing an unordered_multimap in C++.
#' 
#' @seealso \link{cpp_map}, \link{cpp_unordered_map}, \link{cpp_multimap}.
#' 
#' @examples
#' m <- cpp_unordered_multimap(c("world", "hello", "there", "hello"), 4:7)
#' m
#' # ["there",6] ["hello",5] ["hello",7] ["world",4]
#' 
#' print(m, n = 2)
#' # 
#' 
#' erase(m, "hello")
#' m
#' # ["there",6] ["world",4]
#' 
#' contains(m, "there")
#' # [1] TRUE
#' 
#' @include classes.R

#' @export
cpp_unordered_multimap <- function(keys, values) {
  check_insert_values(keys)
  check_insert_values(values)
  if(length(keys) != length(values)) {
    stop("keys and values must be vectors of the same length.")
  }
  if(is.integer(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_i_i(keys, values), key_type = "integer", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_i_d(keys, values), key_type = "integer", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_i_s(keys, values), key_type = "integer", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_i_b(keys, values), key_type = "integer", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else if(is.numeric(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_d_i(keys, values), key_type = "double", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_d_d(keys, values), key_type = "double", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_d_s(keys, values), key_type = "double", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_d_b(keys, values), key_type = "double", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else if(is.character(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_s_i(keys, values), key_type = "string", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_s_d(keys, values), key_type = "string", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_s_s(keys, values), key_type = "string", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_s_b(keys, values), key_type = "string", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else if(is.logical(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_b_i(keys, values), key_type = "boolean", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_b_d(keys, values), key_type = "boolean", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_b_s(keys, values), key_type = "boolean", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppUnorderedMultimap", pointer = unordered_multimap_b_b(keys, values), key_type = "boolean", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else {
    stop("keys must an integer, numeric, character, or logical vector")
  }
  return(m)
}
