#' Computes the dot product of two iterable objects.
#'
#' Returns the dot product of two numeric iterables of equal length
#'
#' @export
#' @param vec1 the first
#' @param vec2 the second iterable object
#' @return the dot product of the iterators
#'
#' @examples
#' it <- iteror(1:3)
#' it2 <- iteror(4:6)
#' dotproduct(it, it2) # 32
#'
#' it <- iteror(1:4)
#' it2 <- iteror(7:10)
#' dotproduct(1:4, 7:10) # 90
#'
dotproduct <- function(vec1, vec2) {
  vec1 <- iteror(vec1)
  vec2 <- iteror(vec2)
  it <- i_map(prod, vec1, vec2)
  sum(sapply(it, identity))
}
