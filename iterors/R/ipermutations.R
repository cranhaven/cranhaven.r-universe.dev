#' Iterator that generates all permutations of a vector.
#'
#' Constructs an iterator generates all permutations of an iterable object. By
#' default, full-length permutations are generated. If \code{m} is specified,
#' successive \code{m} length permutations are instead generated.
#'
#' The implementation is loosely based on that of Python's itertools.
#'
#' @export
#' @param object vector
#' @param m length of permutations. By default, full-length permutations are
#' generated.
#' @return iterator that generates all permutations of \code{object}
#'
#' @aliases ipermutations
#' @examples
#' it <- ipermutations(1:3)
#'
#' nextOr(it, NA) # c(1, 2, 3)
#' nextOr(it, NA) # c(1, 3, 2)
#' nextOr(it, NA) # c(3, 1, 2)
#' nextOr(it, NA) # c(3, 2, 1)
#' nextOr(it, NA) # c(2, 3, 1)
#' nextOr(it, NA) # c(2, 1, 3)
#'
#' it2 <- ipermutations(letters[1:4])
#' # 24 = 4! permutations of the letters a, b, c, and d
#' as.list(it2)
#'
i_permutations <- function(object, m=NULL) {
  object <- as.vector(object)
  n <- length(object)

  # By default, m == n for full-length permutations.
  if (is.null(m)) {
    m <- n
  } else {
    m <- as.integer(m)
  }

  # Traverses through the Cartesian product of indices
  # Skips any cases where the indices are not unique
  # The unique indices generate the permutations
  # This approach is similar to how Python's itertools works
  replicate_n <- replicate(n=m, seq_len(n), simplify=FALSE)
  iter_product <- do.call(igrid, c(replicate_n, list(rowMajor=FALSE)))

  nextOr_ <- function(or) {
    repeat {
       indices <- unique(iter_product(or = return(or)))
      if (length(indices) == m) {
        return(object[unlist(indices)])
      }
    }
  }

  iteror(nextOr_)
}
#' @export
ipermutations <- i_permutations
