#' Iteror that traverses each given iterable in a roundrobin order
#'
#' Constructs an iterator that traverses each given iterable in a roundrobin
#' order. That is, the iterables are traversed in an alternating fashion such
#' that the each element is drawn from the next iterable. If an iterable has no
#' more available elements, it is skipped, and the next element is taken from
#' the next iterable having available elements.
#'
#' @export
#' @param ... multiple arguments to iterate through in roundrobin sequence
#' @return iterator that alternates through each argument in roundrobin sequence
#'
#' @examples
#' it <- iteror(c("A", "B", "C"))
#' it2 <- iteror("D")
#' it3 <- iteror(c("E", "F"))
#' as.list(i_roundrobin(it, it2, it3)) # A D E B F C
#'
#' it_rr <- i_roundrobin(1:3, 4:5, 7:10)
#' as.list(it_rr) # 1 4 7 2 5 8 3 9 10
#'
i_roundrobin <- function(...) {
  iter_list <- lapply(list(...), iteror)
  num_iters <- length(iter_list)

  has_elems <- rep(TRUE, num_iters)
  it_cycle <- icount(num_iters, recycle=TRUE)

  nextOr_ <- function(or) {
    repeat {
      if (!any(has_elems)) {
        return(or)
      }
      which_iter <- it_cycle()
      next_elem <- iter_list[[which_iter]](or = {
        has_elems[which_iter] <<- FALSE
        next
      })
      return(next_elem)
    }
  }

  iteror(nextOr_)
}
