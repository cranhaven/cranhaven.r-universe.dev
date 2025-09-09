
# x is logical (or coerced to logical by rlang::as_logical())
# Returns TRUE if and only if x is TRUE, and FALSE if x is FALSE or NA
# is_true <- function(x)
# {
#   if(rlang::is_na(x))
#     F
#   else
#     rlang::as_logical(x)
# }

# Vectorised is_true
#' Vectorised conversion to logical, treating NA as False
#'
#' @param x A vector
#'
#' @return A logical vector of same size as x which is true where x is true (\code{\link[rlang]{as_logical}}) and not NA
#' @export
are_true <- function(x)
{
  dplyr::if_else(rlang::are_na(x), F, rlang::as_logical(x))
}

# REMOVE

b <- are_true


#' Compare vectors, treating NA like a value
#'
#' @param v1,v2 Vectors of equal size
#'
#' @return Returns a logical vector of the same size as v1 and v2, TRUE wherever elements are the same.
#'         NA is treated like a value level, i.e., NA == NA is true, NA == 1 is false.
#' @export
equal_including_na <- function(v1,v2) {
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  same
}

#' Test for logical true or NA
#'
#' @param x Logical
#'
#' @return True if and only if x is TRUE or x is NA, False otherwise.
#' @export
true_or_na <- function(x)
{
  x  |  is.na(x)
}

compareNA <- equal_including_na

#' Any() giving NA only if all values are NA
#'
#' @param ... Values
#'
#' @return NA if and only if all ... are NA, else any(...), ignoring NA values
#' @export
any_or_all_na <- function(...)
{

  if (all(map_lgl(list2(...), are_na)))
  {
    na_lgl
  }
  else
  {
    any(..., na.rm = T)
  }
}

anyOrAllNA <- any_or_all_na

#' All() giving NA only if all values are NA
#'
#' @param ... Values
#'
#' @return NA if and only if all ... are NA, else all(...), ignoring NA values
#' @export
all_or_all_na <- function(...)
{
  if (all(map_lgl(list2(...), are_na)))
  {
    na_lgl
  }
  else
  {
    all(..., na.rm = T)
  }
}

allOrAllNA <- all_or_all_na

#' First argument that is not NA
#'
#' @param ... Values
#'
#' @return The first argument that is not NA, or NA iff all are NA
#' @export
first_not_na <- function(...)
{
  dots <- dots_list(...)
  for (arg in dots)
  {
    if (!length(arg)==0 && !is.na(arg))
    {
      return(arg)
    }
  }
  return(NA)
}


firstNotNA <- first_not_na

#' First argument that does not equal a given value
#'
#' @param not Value: we look for the first value not equal to this one
#' @param ... Values
#'
#' @return The first value that does not equal "not", or NA iff all equal "not"
#' @export
#'
#' @examples
#' # 5
#' first_not(1, 1,1,1,5)
first_not <- function(not, ...)
{
  if (is.na(not))
  {
    return(first_not_na(...))
  }
  dots <- dots_list(...)
  for (arg in dots)
  {
    if (!is.na(arg) && arg != not)
    {
      return(arg)
    }
  }
  return(not)
}

#' First which() is not na
#'
#' @param ... Values; concatenated as given. Intended use is with one vector of length > 1 or multiple single arguments.
#'
#' @return The index of the first value which is not NA, or NA iff all elements are NA.
#' @export
#'
#' @examples
#' # 4
#' first_which_not_na(NA, NA, NA, 56)
first_which_not_na <- function(...)
{
  idx <- which(!is.na(c(...)))
  ifelse(has_length(idx), idx[[1]], NA)
}

#' Row-wise first value which is not NA
#'
#' This is useful in conjunction with dplyr's mutate to condense multiple columns to one,
#' where in each sample typically only one of n columns has a value, while the others are NA.
#' Returns one vector of the same length as each input vector containing the result.
#' Note that factors will be converted to character vectors (with a warning).
#'
#' @param ... multiple vectors of same type and size, regarded as columns
#'
#' @return Returns a vector of type and size as any of the given vectors
#'         (vectors regarded a column, number of rows is size of each vectors)
#'         For each "row", returns the first value that is not NA, or NA iff all values in the row are NA.
#' @export
#'
#' @examples
#' library(tibble)
#' library(magrittr)
#' library(dplyr)
#' # Creates a column containing (4, 2, 2)
#' tibble(a=c(NA, NA, 2), b=c(4, NA, 5), c=c(1, 2, 3)) %>%
#'   mutate(essence=first_non_nas(a, b, c))
first_non_nas <- function(...)
{
  dots <- dots_list(...)
  if (!has_length(dots))
    return()

  if (any(map_lgl(dots, is.factor)))
  {
    warning("Coercing factor to character vector")
    dots <- map(dots, function(x) { if (is.factor(x)) as.character(x) else x })
  }
  type <- map_chr(dots, rlang::type_of) %>% unique()
  if (!has_length(type, 1))
  {
    stop("Got vectors of differing types, ", str_c(type, collapse = ", "))
  }

  pmap_dict = c(
    "integer" = pmap_int,
    "double"  = pmap_dbl,
    "logical" = pmap_lgl,
    "string"  = pmap_chr,
    "character" = pmap_chr,
    "list" = pmap
  )
  na_dict = list(
    "integer" = na_int,
    "double"  = na_dbl,
    "logical" = na_lgl,
    "string"  = na_chr,
    "character"  = na_chr,
    "list" = na_chr
  )
  pmap_fn <- pmap_dict[[type]]
  na <- na_dict[[type]]

  pmap_fn(dots, function(...)
  {
    vec <- c(...)
    not_na <- which(!is.na(vec))
    if (has_length(not_na))
      vec[[ not_na[[1]] ]]
    else
      na
  })
}


# Given
# for each row, returns the indices of those value not NA,
# or an empty vector iff all values in the row are NA.
# Returns one list of the same length as the vectors containing an index vector for each row.
#' Get indices of non-NA values
#'
#' @param ... k vectors of the same length n, regarded as k columns with each n rows
#'
#' @return A list of n numerical vectors. Each numerical vector has a size between 0 and k and contains the
#'         indices of the vectors whose elements are not na in the corresponding row.
#' @export
#'
#' @examples
#' library(tibble)
#' library(magrittr)
#' library(dplyr)
#' # Creates a list column containing (2,3);(3);(1,2,3)
#' tibble(a=c(NA, NA, 2), b=c(4, NA, 5), c=c(1, 2, 3)) %>%
#'   mutate(non_na_idc=which_non_na(a, b, c))
which_non_na <- function(...)
{
  dots <- dots_list(...)
  if (!has_length(dots))
    return()

  pmap(dots, ~which(!is.na(c(...))))
}


#' Row-wise first value that is not NA
#'
#' @param .tbl A data frame
#' @param ... A column selection, as for \code{\link[dplyr]{select}}
#'
#' @return A vector of length nrow(.tbl) containing the first found non-na value
#' @export
first_non_nas_at <- function(.tbl, ...)
{
  vars <- quos(...)
  .tbl %>%
    select(!!!vars) ->
    subset

  do.call(first_non_nas, subset)
}

#' Row-wise first index of column that is not NA
#'
#' @param .tbl A data frame
#' @param ... A column selection, as for \code{\link[dplyr]{select}}
#'
#' @return A numeric vector of length nrow(.tbl) containing the index of the first found non-na value in the given columns.
#'    Possible values are NA (all values in that row are NA), and 1 ... number of columns in selection
#' @export
first_which_non_na_at <- function(.tbl, ...)
{
  vars <- quos(...)
  .tbl %>%
    select(!!!vars) ->
    subset

  do.call(which_non_na, subset) %>%
    map_int(function(indices)
    {
      if (is_empty(indices))
        na_int
      else
        indices[[1]]
    })
}


