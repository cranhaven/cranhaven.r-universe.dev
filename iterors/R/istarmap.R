#' Iteror that applies a given function to the elements of an iterable.
#'
#' Constructs an iteror that applies the function \code{f} concurrently to the
#' elements within the list \code{x}.
#'
#' The iteror returned is exhausted when the shortest element in \code{x}
#' is exhausted. Note that \code{i_starmap} does not recycle arguments as
#' \code{\link[base]{Map}} does.
#'
#' The primary difference between \code{i_starmap} and
#' \code{\link[iterors]{i_map}} is that the former expects an iterable object
#' whose elements are already grouped together, while the latter case groups the
#' arguments together before applying the given function. The choice is a matter
#' of style and convenience.
#'
#' @export
#' @param f a function to apply to the elements of \code{x}
#' @param x an iterable object
#' @return iterator that returns the values of \code{object} along with the
#' index of the object.
#'
#' @examples
#' pow <- function(x, y) {
#'   x^y
#' }
#' it <- i_starmap(pow, list(c(2, 3, 10), c(5, 2, 3)))
#' unlist(as.list(it)) == c(32, 9, 1000)
#'
#' # Similar to the above, but because the second vector is exhausted after two
#' # calls to `nextElem`, the iterator is exhausted.
#' it2 <- i_starmap(pow, list(c(2, 3, 10), c(5, 2)))
#' unlist(as.list(it2)) == c(32, 9)
#'
#' # Another similar example but with lists instead of vectors
#' it3 <- i_starmap(pow, list(list(2, 3, 10), list(5, 2, 3)))
#' as.list(it3)
#'
#' # Computes sum of each row in the iris data set
#' # Numerically equivalent to base::rowSums()
#' tolerance <- sqrt(.Machine$double.eps)
#' iris_x <- iris[, -5]
#' it4 <- i_starmap(sum, iris_x)
#' unlist(as.list(it4)) - rowSums(iris_x) < tolerance
i_starmap <- function(f, x) {
  f <- match.fun(f)
  iter_list <- lapply(x, iteror)

  nextOr_ <- function(or) {
    next_args <- lapply(iter_list, nextOr, or=return(or))
    do.call(f, next_args)
  }

  iteror_internal(nextOr_)
}

#' @rdname i_starmap
#' @export
i_star <- i_starmap
