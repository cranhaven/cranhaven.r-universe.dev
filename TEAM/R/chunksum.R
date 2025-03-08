#' Chunk.Sum function
#'
#' Rolling Sum over distinct chunks
#'
#' @param v Numeric Vector
#' @param n Size of chunk
#' @param na.rm Remove NAs (default=TRUE)
#'
#' @return NULL
#'
chunk.sum <- function(v, n, na.rm=TRUE) {    # 'tapply'
  unname(tapply(v, (seq_along(v)-1) %/% n, sum, na.rm=na.rm))
}
