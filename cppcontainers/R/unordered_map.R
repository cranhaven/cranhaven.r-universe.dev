#' Create unordered map
#' 
#' Create an unordered map. Unordered maps are key-value pairs with unique keys.
#' 
#' @param keys An integer, numeric, character, or logical vector.
#' @param values An integer, numeric, character, or logical vector.
#' 
#' @details Unordered maps are associative containers. They do not provide random access through an index. I.e. \code{m[2]} does not return the second 
#' element.
#' 
#' Unordered means that the container does not enforce elements to be stored in a particular order. This makes unordered maps in some applications faster 
#' than maps.
#' 
#' C++ unordered_map methods implemented in this package are \link{at}, \link{bucket_count}, \link{clear}, \link{contains}, \link{count}, \link{emplace}, 
#' \link{empty}, \link{erase}, \link{insert}, \link{insert_or_assign}, \link{load_factor}, \link{max_bucket_count}, \link{max_load_factor}, 
#' \link{max_size}, \link{merge}, \link{rehash}, \link{reserve}, \link{size}, and \link{try_emplace}. The package also adds the \link{==} and \link{[} 
#' operators and various helper functions (\link{print}, \link{to_r}, \link{type}).
#' 
#' All object-creating methods in this package begin with \code{cpp_} to avoid clashes with functions from other packages, such as \code{utils::stack} and 
#' \code{base::vector}.
#' 
#' @returns Returns a CppUnorderedMap object referencing an unordered_map in C++.
#' 
#' @seealso \link{cpp_map}, \link{cpp_multimap}, \link{cpp_unordered_multimap}.
#' 
#' @examples
#' m <- cpp_unordered_map(4:6, seq.int(1, by = 0.5, length.out = 3L))
#' m
#' # [6,2] [5,1.5] [4,1]
#' 
#' insert(m, seq.int(100, by = 0.1, length.out = 3L), 14:16)
#' m
#' # [16,100.2] [15,100.1] [14,100] [6,2] [5,1.5] [4,1]
#' 
#' print(m, n = 3)
#' # [16,100.2] [15,100.1] [14,100]
#' 
#' m <- cpp_unordered_map(c("world", "hello", "there"), 4:6)
#' m
#' # ["there",6] ["hello",5] ["world",4]
#' 
#' erase(m, "there")
#' m
#' # ["hello",5] ["world",4]
#' 
#' @include classes.R

#' @export
cpp_unordered_map <- function(keys, values) {
  check_insert_values(keys)
  check_insert_values(values)
  if(length(keys) != length(values)) {
    stop("keys and values must be vectors of the same length.")
  }
  if(is.integer(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_i_i(keys, values), key_type = "integer", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_i_d(keys, values), key_type = "integer", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_i_s(keys, values), key_type = "integer", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_i_b(keys, values), key_type = "integer", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else if(is.numeric(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_d_i(keys, values), key_type = "double", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_d_d(keys, values), key_type = "double", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_d_s(keys, values), key_type = "double", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_d_b(keys, values), key_type = "double", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else if(is.character(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_s_i(keys, values), key_type = "string", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_s_d(keys, values), key_type = "string", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_s_s(keys, values), key_type = "string", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_s_b(keys, values), key_type = "string", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else if(is.logical(keys)) {
    if(is.integer(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_b_i(keys, values), key_type = "boolean", value_type = "integer")
    } else if(is.numeric(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_b_d(keys, values), key_type = "boolean", value_type = "double")
    } else if(is.character(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_b_s(keys, values), key_type = "boolean", value_type = "string")
    } else if(is.logical(values)) {
      m <- methods::new("CppUnorderedMap", pointer = unordered_map_b_b(keys, values), key_type = "boolean", value_type = "boolean")
    } else {
      stop("values must an integer, numeric, character, or logical vector")
    }
  } else {
    stop("keys must an integer, numeric, character, or logical vector")
  }
  return(m)
}
