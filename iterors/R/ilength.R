#' Consumes an iterator and computes its length
#'
#' Counts the number of elements in an iterator. NOTE: The iterator is consumed
#' in the process.
#'
#' @param object an iterable object
#' @return the number of elements in the iterator
#' @param ... passed along to [iteror] constructor.
#' @seealso take consume as.list.iteror
#'
#' @examples
#' count(1:5) == length(1:5)
#'
#' it <- iteror(1:5)
#' count(it) == length(1:5)
#'
#' it2 <- i_chain(1:3, 4:5, 6)
#' count(it2)
#'
#' it3 <- i_chain(1:3, levels(iris$Species))
#' count(it3)
#'
#' @export
count <- function(object, ...) {
  UseMethod("count")
}

#' @exportS3Method
count.default <- function(object, ...) {
  count.iteror(iteror(object, ...))
}

#' @exportS3Method
count.iteror <- function(object, ...) {
  stop_unused(...)
  i <- 0
  repeat{
    object(or = break)
    i <- i + 1
  }
  i
}
