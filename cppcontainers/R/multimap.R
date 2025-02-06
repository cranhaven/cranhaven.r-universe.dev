#' Create multimap
#' 
#' Create a multimap. Multimaps are key-value pairs sorted by non-unique keys.
#' 
#' @param keys An integer, numeric, character, or logical vector.
#' @param values An integer, numeric, character, or logical vector.
#' 
#' @details Multimaps are associative containers. They do not provide random access through an index. I.e. \code{m[2]} does not return the second element.
#' 
#' C++ multimap methods implemented in this package are \link{clear}, \link{contains}, \link{count}, \link{emplace}, \link{empty}, \link{erase}, 
#' \link{insert}, \link{max_size}, \link{merge}, and \link{size}. The package also adds the \link{==} operator and various helper functions (\link{print}, 
#' \link{to_r}, \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppMultimap object referencing a multimap in C++.
#' 
#' @seealso \link{cpp_map}, \link{cpp_unordered_map}, \link{cpp_unordered_multimap}.
#' 
#' @examples
#' m <- cpp_multimap(4:6, seq.int(1, by = 0.5, length.out = 3L))
#' m
#' # [4,1] [5,1.5] [6,2]
#' 
#' insert(m, seq.int(100, by = 0.1, length.out = 3L), 5:7)
#' m
#' # [4,1] [5,1.5] [5,100] [6,2] [6,100.1] [7,100.2]
#' 
#' print(m, from = 6)
#' # [6,2] [6,100.1] [7,100.2]
#' 
#' m <- cpp_multimap(c("world", "hello", "there", "world"), 3:6)
#' m
#' # ["hello",4] ["there",5] ["world",3] ["world",6]
#' 
#' erase(m, "world")
#' m
#' # ["hello",4] ["there",5]
#' 
#' @include classes.R

#' @export
cpp_multimap <- function(keys, values) {
  check_insert_values(keys)
  check_insert_values(values)
  if(length(keys) != length(values)) {
    stop("keys and values must be vectors of the same length.")
  }
  if(is.integer(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_i_i(keys, values), key_type = "integer", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_i_d(keys, values), key_type = "integer", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_i_s(keys, values), key_type = "integer", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_i_b(keys, values), key_type = "integer", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else if(is.numeric(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_d_i(keys, values), key_type = "double", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_d_d(keys, values), key_type = "double", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_d_s(keys, values), key_type = "double", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_d_b(keys, values), key_type = "double", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else if(is.character(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_s_i(keys, values), key_type = "string", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_s_d(keys, values), key_type = "string", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_s_s(keys, values), key_type = "string", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_s_b(keys, values), key_type = "string", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else if(is.logical(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_b_i(keys, values), key_type = "boolean", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_b_d(keys, values), key_type = "boolean", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_b_s(keys, values), key_type = "boolean", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppMultimap", pointer = multimap_b_b(keys, values), key_type = "boolean", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else {
    stop("keys must an integer, numeric, character, or logical vector")
  }
  return(m)
}
