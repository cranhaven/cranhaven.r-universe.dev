# Treat list as a dict key -> value with name -> value.
# Return value for key (if multiple, first occurence)
# default can be a value, or a function.
# If you want the key to be returned if not in the dict, give default = identity.
# You can either choose that the key(s) are regexes, or that the dictionary keys are regexes.
# (as one is matched by the other, regexes in both is nonsense)
#' Lookup in a dictionary
#'
#' Looks up all values as keys of the dictionary and returns the values.
#'
#' @param dict A dictionaryish vector (named: key -> value)
#' @param ... Keys to lookup in the dictionary
#' @param default Default value to return if key is not found. Can be a value or function (called with the key).
#'                Note: default is to return NA; another very intuitive case is to return the key itself.
#'                To achieve this, pass \code{default = identity}.
#' @param dict_key_is_regex Should the dictionary keys, the names of dict,
#'                          be regarded as regular expressions? (excludes key_is_regex)
#' @param key_is_regex Should the keys to lookup be regarded as regular expressions? (excludes dict_key_is_regex)
#'
#' @return A list of the same size as ..., containing the lookup results. For the type-specific functions,
#'         returns a vector typed as requested, requiring all lookup results to have matching type.
#' @export
#'
#' @examples
#' a <- list("x", "y", "z")
#' dict <- c(x="xc", y="yv")
#' # returns c("xc", "yv", na_chr)
#' lookup_chr(dict, a)#'
#' # returns c("xc", "yv", "z")
#' lookup_chr(dict, "x", "y", "z", default=identity)
lookup <- function(dict, ..., default = NA, dict_key_is_regex = F, key_is_regex = F)
{
  return(.lookupImpl(dict, default, dict_key_is_regex, key_is_regex, map2, ...))
}

#' @export
#' @rdname lookup
lookup_int <- function(dict, ..., default = NA, dict_key_is_regex = F, key_is_regex = F)
{
  return(.lookupImpl(dict, default, dict_key_is_regex, key_is_regex, map2_int, ...))
}

#' @export
#' @rdname lookup
lookup_chr <- function(dict, ..., default = NA, dict_key_is_regex = F, key_is_regex = F)
{
  return(.lookupImpl(dict, default, dict_key_is_regex, key_is_regex, map2_chr, ...))
}

#' @export
#' @rdname lookup
lookup_lgl <- function(dict, ..., default = NA, dict_key_is_regex = F, key_is_regex = F)
{
  return(.lookupImpl(dict, default, dict_key_is_regex, key_is_regex, map2_lgl, ...))
}

#' @export
#' @rdname lookup
lookup_dbl <- function(dict, ..., default = NA, dict_key_is_regex = F, key_is_regex = F)
{
  return(.lookupImpl(dict, default, dict_key_is_regex, key_is_regex, map2_dbl, ...))
}
#' @export
#' @rdname lookup
lookup_num <- lookup_dbl

.lookupImpl <- function(dict, default, dict_key_is_regex, key_is_regex, map_func, ...)
{
  args <- dots_splice(..., .ignore_empty = "all")

  if (has_length(args, 1) && is_atomic(args[[1]]))
  {
    args <- args[[1]]
  }

  # empty dict?
  if (is_empty(dict))
  {
    match_func_empty <- function(arg, ...)
    {
      if (is_function(default))
        return(default(arg))
      else
        return(default)
    }
    # map_func is a map2 variant
    values <- map_func(args, args, match_func_empty)
  }
  else
  {
    dict_keys <- names(dict)

    if (dict_key_is_regex)
    {
      matches <- rep_along(args, na_int)
      for (i in seq_along(args))
      {
        matches_per_key <- str_locate_match(dict_keys, args[[i]])
        matches[[i]] <- which.min(!is.na(matches_per_key))
      }
    }
    else if (key_is_regex)
    {
      matches <- str_locate_match(args, dict_keys)
    }
    else
    {
      matches <- match(args, dict_keys)
    }

    match_func <- function(match, arg) {
      if (is.na(match))
        if (is_function(default))
          return(default(arg))
      else
        return(default)
      else
        return(dict[[match]])
    }
    values <- map_func(matches, args, match_func)
  }

  names(values) <- names(args)
  return(values)
}

#' Detect sequential duplicates
#'
#' @param strings Character vector
#' @param ordering Optional: treat strings as if ordered like strings[ordering], or,
#'                 if a function, strings[ordering(strings)]
#'
#' @return A logical vector which indicates if a string is identical to the previous string.
#' @export
#'
#' @examples
#' # return c(F, T, F, T, T, F)
#' sequential_duplicates(c("a", "a", "b", "b", "b", "a"))
sequential_duplicates <- function(strings, ordering = NULL)
{
  if (is.null(ordering))
  {
    permutation <- seq_along(strings)
  }
  else if (is_function(ordering))
  {
    permutation <- ordering(strings)
  }
  else
  {
    permutation <- ordering
  }
  isduplicate <- rep_along(strings, na_lgl)
  previous <- NULL
  for (i in seq_along(permutation))
  {
    currentIndex <- permutation[[i]]
    current <- strings[[currentIndex]]
    isduplicate[[currentIndex]] <- !invalid(previous) && previous == current
    previous <- current
  }
  return(isduplicate)

}

# Replace all duplicates in immediate sequence in the strings vector by replace_with, keeping the first occurence as is.
# If ordering is null, strings are used as is; if order is a vector or a function, reorders strings first,
# then replaces, but the returned vector is ordered as initially.
#' Replace sequential duplicates
#'
#' @param strings Character vector
#' @param replace_with Replacement string
#' @param ordering Optional: treat strings as if ordered like strings[ordering], or,
#'                 if a function, strings[ordering(strings)]
#'
#' @return A character vector with strings identical to the previous string replaced with replace_with
#' @export
#'
#' @examples
#' # returns c("a", "", "b", "", "", "a")
#' replace_sequential_duplicates(c("a", "a", "b", "b", "b", "a"))
replace_sequential_duplicates <- function(strings, replace_with = "", ordering = NULL)
{
  strings[sequential_duplicates(strings, ordering)] <- replace_with
  return(strings)
}

#' Creating a lookup function from dictionary
#'
#' @param dict A dictionaryish character vector (named: key -> value)
#' @param default Value to return if key is not found, or function to evaluate with key as argument
#' @param dict_key_is_regex If True, treats dictionary keys are regular expressions when matching
#'
#' @return A function which can be called with keys and performs the described lookup, returning the value (string)
#' @export
lookup_function_from_dict <- function(dict,
                                      default = identity,
                                      dict_key_is_regex = F)
{
  if (invalid(dict))
  {
    return(identity)
  }
  stopifnot(is_vector(dict) && is_dictionaryish(dict))
  return(function(...) lookup_chr(dict, ..., default = default, dict_key_is_regex = dict_key_is_regex))
}

#' Orderer function for complex sorting
#'
#' If you want to order by multiple features and have sorted vectors for each feature
#' which describe the intended order
#'
#' @param ... k sorted vectors, in order of priority
#'
#' @return A function which takes (at least) k vectors
#'         This function will return an order for these vectors determined by the sorted vectors
#' @export
orderer_function_from_sorted_vectors <- function(...)
{
  sorted_lists <- dots_list(...)
  return(
    function(...)
    {
      data_vectors <- dots_list(...)
      stopifnot(length(sorted_lists) <= length(data_vectors))
      orders <- list()
      for (i in seq_along(sorted_lists))
      {
        if (invalid(sorted_lists[[i]]))
          next
        # as there is no guarantee the sorted_lists contain all factor, we cannot use plain factor(., levels=..)
        orderedFactor <- fct_relevel(factor(data_vectors[[i]]), sorted_lists[[i]])
        orders <- append(orders, list(orderedFactor))
      }
      return (do.call(order, orders))
    }
  )
}

# An implementation of order() which retains the order of the given values
#' Ordering function: identity order
#'
#' This can be used in a place where a function with a signature like \code{\link{order}} is required.
#' It simply retains the original order.
#'
#' @param x a vector
#' @param ... Effectively ignored
#'
#' @return An integer vector
identity_order <- function(x, ...)
{
  seq(1, length(x))
}

#' Appending in a pipe, never unlisting
#'
#' Append to a given list, while considering as a single object and not unlisting as base::append does.
#' Argument order is reversed compared to base::append to allow a different pattern of use in a pipe.
#'
#' @param x Object to append. If the object is a list, then it is appended as-is, and not unlisted.
#' @param .l The list to append to.
#'           Special case handling applies if .l does not exist: then an empty list is used.
#'           This alleviates the need for an initial mylist <- list()
#' @param name Will be used as name of the object in the list
#'
#' @return The list .l with x appended
#' @export
#'
#' @examples
#' library(magrittr)
#' results <- list(first=c(3,4), second=list(5,6))
#' list(7,8) %>%
#'   append_object(results, "third result") ->
#' results
#' # results has length 1, containing one list named "first"
append_object <- function(x, .l, name = NULL)
{
  l_quo <- rlang::enquo(.l)
  l_env <- rlang::quo_get_env(l_quo)
  if (rlang::quo_is_symbol(l_quo) && !rlang::env_has(l_env, rlang::quo_name(l_quo), inherit = T))
    .l <- list()
  .l <- append(.l, list(x))
  if (!is_null(name))
    names(.l)[[length(.l)]] <- name
  return (.l)
}

#' Prepending in a pipe, never unlisting
#'
#' Prepend to a given list, while considering as a single object and not unlisting.
#' Argument order is reversed compared to base::append or purrr::prepend to allow a different pattern of use in a pipe.
#'
#' @param x Object to prepend. If the object is a list, then it is appended as-is, and not unlisted.
#' @param .l The list to append to.
#'           Special case handling applies if .l does not exist: then an empty list is used.
#'           This alleviates the need for an initial mylist <- list()
#' @param name Will be used as name of the object in the list
#' @param before Prepend before this index
#'
#' @return The list .l with x prepended
#' @export
#'
#' @examples
#' #' library(tibble)
#' library(magrittr)
#' library(dplyr)
#' results <- list(second=list(1,2), third=list(3))
#' list(-1, 1) %>%
#'   prepend_object(results, "first") ->
#' results
#' # results has length 3, containing three lists
prepend_object <- function(x, .l, name = NULL, before = 1)
{
  l_quo <- rlang::enquo(.l)
  l_env <- rlang::quo_get_env(l_quo)
  if (rlang::quo_is_symbol(l_quo) && !rlang::env_has(l_env, rlang::quo_name(l_quo), inherit = T))
    .l <- list()
  .l <- purrr::prepend(.l, list(x), before = before)
  if (!is_null(name))
    names(.l)[[before]] <- name
  return (.l)
}

# Returns a slice of v of all elements with the given name
#' Slice by name
#'
#' Slices of a vector with elements of given name, or containing given patterns.
#' Analogous accessor functions for \code{\link[purrr]{pluck}}
#'
#' @param v A vector
#' @param name Name of entry to pluck
#'
#' @return A slice from v containing all elements in v with the given name,
#'         or the name of which contains pattern
#' @export
with_name <- function(v, name)
{
  idc <- which(names(v) %in% name)
  v[idc]
}

#' @param pattern Pattern passed to \code{\link[stringr]{str_detect}}
#' @export
#' @rdname with_name
with_name_containing <- function(v, pattern)
{
  lgl <- str_detect(names(v), pattern)
  v[lgl]
}

# Creates an accessor function using with_name
# Useful for pluck()
#' @export
#' @rdname with_name
named <- function(name)
{
  function(v) with_name(v, name)
}

# Creates an accessor function using with_name
# Useful for pluck()
#' @export
#' @rdname with_name
name_contains <- function(pattern)
{
  function(v) with_name_containing(v, pattern)
}

# Returns a slice of character vector v
#' Slice by value
#'
#' Slices of a vector with elements containing given patterns.
#' Analogous accessor function for \code{\link[purrr]{pluck}}
#'
#' @param v A vector
#' @param pattern Pattern passed to \code{\link[stringr]{str_detect}}
#'
#' @return A slice from v containing all elements in v with the given name,
#'         or the name of which contains pattern
#' @export
with_value_containing <- function(v, pattern)
{
  lgl <- str_detect(v, pattern)
  v[lgl]
}

#' @export
#' @rdname with_value_containing
value_contains <- function(pattern)
{
  function(v) with_value_containing(v, pattern)
}

#' Pluck with simplified return value
#'
#' Like \code{\link[purrr]{pluck}()}, but will return simplify()'ed as a vector
#'
#' @param .x Container object
#' @param ... Accessor specification
#' @param .default Default value
#'
#' @return Result of \code{\link[purrr]{pluck}()}, transformed y \code{\link[purrr]{simplify}()}
#' @export
pluck_vector <- function(.x, ..., .default = NULL)
{
  purrr::simplify(purrr::pluck(.x, ..., .default = .default))
}

# Return container where names(v) are the values and the values of v are the names
#' Inverting name and value
#'
#' @param v A named vector
#'
#' @return A vector where names(v) are the values and the values of v are the names
#' @export
invert_value_and_names <- function(v)
{
  return(setNames(names(v), v))
}
