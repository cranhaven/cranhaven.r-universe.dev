#' Iterator that filters elements not satisfying a predicate function
#'
#' `i_keep(iterable, predicate)` constructs an iterator that filters
#' elements from iterable returning only those for which the predicate
#' is \code{TRUE}.
#'
#' @export
#' @aliases ifilter
#' @seealso i_drop i_keepwhile i_dropwhile
#' @param iterable an iterable object.
#' @param predicate a function that determines whether an element is
#'   \code{TRUE} or \code{FALSE}. The function is assumed to take only
#'   one argument.
#' @param ... passed along to [iteror] constructor.
#' @details Originally called 'ifilter' from package
#'   `itertools`. Renamed because the order of arguments has changed
#'   to put the iterable in the first argument, the better to be used
#'   with the `|>` operator.
#' @return iterator object
#'
#' @examples
#' # Filters out odd numbers and retains only even numbers
#' is_even <- function(x) {
#'   x %% 2 == 0
#' }
#' it <- i_keep(1:10, is_even)
#' as.list(it)
#'
#' # Similar idea here but anonymous function is used to retain only odd
#' # numbers
#' it2 <- i_drop(1:10, function(x) x %% 2 == 0)
#' nextOr(it2, NA) # 1
#' nextOr(it2, NA) # 3
#' nextOr(it2, NA) # 5
#' nextOr(it2, NA) # 7
#' nextOr(it2, NA) # 9
#'
#' is_vowel <- function(x) {
#'   x %in% c('a', 'e', 'i', 'o', 'u')
#' }
#' it3 <- i_keep(letters, is_vowel)
#' as.list(it3)
i_keep <- function(iterable, predicate, ...) {
  if (!is.function(predicate)) {
    stop("The 'predicate' must be a function that returns TRUE or FALSE.")
  }

  iter_obj <- iteror(iterable, ...)

  nextOr_ <- function(or) {
    repeat {
      next_elem <- iter_obj(or = return(or))
      if (predicate(next_elem)) {
        return(next_elem)
      }
    }
  }

  iteror_internal(nextOr_)
}

#' `i_drop(iterable, predicate)` constructs an iterator that filters
#' elements from iterable returning only those for which the predicate
#' is \code{FALSE}.
#'
#' @export
#' @aliases ifilterfalse
#' @examples
#' # Filters out even numbers and retains only odd numbers
#' is_even <- function(x) {
#'   x %% 2 == 0
#' }
#' it <- i_drop(1:10, is_even)
#' as.list(it)
#'
#' # Similar idea here but anonymous function is used to filter out odd
#' # numbers
#' it2 <- i_drop(1:10, function(x) x %% 2 == 1)
#' as.list(it2)
#'
#' is_vowel <- function(x) {
#'   x %in% c('a', 'e', 'i', 'o', 'u')
#' }
#' it3 <- i_drop(letters, is_vowel)
#' nextOr(it3, NA) # b
#' nextOr(it3, NA) # c
#' nextOr(it3, NA) # d
#' nextOr(it3, NA) # f
#' nextOr(it3, NA) # g
#' # nextOr(it, NA) continues through the rest of the consonants
#'
#' @rdname i_keep
i_drop <- function(iterable, predicate, ...) {
  if (!is.function(predicate)) {
    stop("The 'predicate' must a function that returns TRUE or FALSE.")
  }

  iter_obj <- iteror(iterable, ...)

  nextOr_ <- function(or) {
    repeat {
      next_elem <- iter_obj(or = return(or))
      if (!predicate(next_elem)) {
        return(next_elem)
      }
    }
  }

  iteror_internal(nextOr_)
}
