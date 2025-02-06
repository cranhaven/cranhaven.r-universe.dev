#' Create map
#' 
#' Create a map. Maps are key-value pairs sorted by unique keys.
#' 
#' @param keys An integer, numeric, character, or logical vector.
#' @param values An integer, numeric, character, or logical vector.
#' 
#' @details Maps are associative containers. They do not provide random access through an index. I.e. \code{m[2]} does not return the second element.
#' 
#' C++ map methods implemented in this package are \link{at}, \link{clear}, \link{contains}, \link{count}, \link{emplace}, \link{empty}, \link{erase}, 
#' \link{insert}, \link{insert_or_assign}, \link{max_size}, \link{merge}, \link{size}, and \link{try_emplace}. The package also adds the \link{==} and 
#' \link{[} operators and various helper functions (\link{print}, \link{to_r}, \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppMap object referencing a map in C++.
#' 
#' @seealso \link{cpp_unordered_map}, \link{cpp_multimap}, \link{cpp_unordered_multimap}.
#' 
#' @examples
#' m <- cpp_map(4:6, seq.int(1, by = 0.5, length.out = 3L))
#' m
#' # [4,1] [5,1.5] [6,2]
#' 
#' insert(m, seq.int(100, by = 0.1, length.out = 3L), 14:16)
#' m
#' # [4,1] [5,1.5] [6,2] [14,100] [15,100.1] [16,100.2]
#' 
#' print(m, from = 6L)
#' # [6,2] [14,100] [15,100.1] [16,100.2]
#' 
#' m <- cpp_map(c("world", "hello", "there"), 4:6)
#' m
#' # ["hello",5] ["there",6] ["world",4]
#' 
#' erase(m, "there")
#' m
#' # ["hello",5] ["world",4] 
#' 
#' @include classes.R

#' @export
cpp_map <- function(keys, values) {
  check_insert_values(keys)
  check_insert_values(values)
  if(length(keys) != length(values)) {
    stop("keys and values must be vectors of the same length.")
  }
  if(is.integer(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppMap", pointer = map_i_i(keys, values), key_type = "integer", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppMap", pointer = map_i_d(keys, values), key_type = "integer", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppMap", pointer = map_i_s(keys, values), key_type = "integer", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppMap", pointer = map_i_b(keys, values), key_type = "integer", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else if(is.numeric(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppMap", pointer = map_d_i(keys, values), key_type = "double", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppMap", pointer = map_d_d(keys, values), key_type = "double", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppMap", pointer = map_d_s(keys, values), key_type = "double", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppMap", pointer = map_d_b(keys, values), key_type = "double", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else if(is.character(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppMap", pointer = map_s_i(keys, values), key_type = "string", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppMap", pointer = map_s_d(keys, values), key_type = "string", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppMap", pointer = map_s_s(keys, values), key_type = "string", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppMap", pointer = map_s_b(keys, values), key_type = "string", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else if(is.logical(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppMap", pointer = map_b_i(keys, values), key_type = "boolean", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppMap", pointer = map_b_d(keys, values), key_type = "boolean", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppMap", pointer = map_b_s(keys, values), key_type = "boolean", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppMap", pointer = map_b_b(keys, values), key_type = "boolean", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else {
    stop("keys must an integer, numeric, character, or logical vector")
  }
  return(m)
}
