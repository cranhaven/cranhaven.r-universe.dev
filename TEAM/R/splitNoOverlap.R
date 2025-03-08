#' splitNoOverlap function
#'
#' Split a vector into distinct chunks of specified size
#'
#' @param vec Numeric Vector
#' @param seg.length Number of distinct chunks to split vec
#'
#' @return NULL
#'
splitNoOverlap <- function(vec, seg.length) {
  out = unname(tapply(vec, (seq_along(vec)-1) %/% seg.length, c))
  #remove list elements with < seg.length items - make sure to remove from curr.indx.vec too
  return(list("kept"=out[lapply(out, length)>=seg.length],
              "removed"=unlist(out[lapply(out, length)<seg.length])))
}
