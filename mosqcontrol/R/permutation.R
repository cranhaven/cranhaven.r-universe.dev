#' uperm
#'
#' \code{uperm} returns permutation matrix.
#'
#' For a given list of numbers, this function outputs a matrix, where each row
#' is a unique permutation of the list.
#'
#' @param d Vector
#' @export
#' @examples
#' uperm(c(1, 2))
uperm <- function(d) {
  dat <- factor(d)
  N <- length(dat)
  n <- tabulate(dat)
  ng <- length(n)

  if (ng == 1) {
    return(d)
  }

  a <- N - c(0, cumsum(n))[-(ng + 1)]
  foo <- lapply(
    1:ng,
    function(i) matrix(utils::combn(a[i], n[i]), nrow = n[i])
  )

  out <- matrix(NA, nrow = N, ncol = prod(sapply(foo, ncol)))
  xxx <- c(0, cumsum(sapply(foo, nrow)))
  xxx <- cbind(xxx[-length(xxx)] + 1, xxx[-1])
  miss <- matrix(1:N, ncol = 1)

  for (i in seq_len(length(foo) - 1)) {
    l1 <- foo[[i]]
    nn <- ncol(miss)
    miss <- matrix(rep(miss, ncol(l1)), nrow = nrow(miss))
    k <- (rep(0:(ncol(miss) - 1), each = nrow(l1))) * nrow(miss) +
      l1[, rep(1:ncol(l1), each = nn)]
    out[xxx[i, 1]:xxx[i, 2], ] <- matrix(miss[k], ncol = ncol(miss))
    miss <- matrix(miss[-k], ncol = ncol(miss))
  }

  k <- length(foo)

  out[xxx[k, 1]:xxx[k, 2], ] <- miss
  out <- out[rank(as.numeric(dat), ties.method = "first"), ]
  foo <- cbind(as.vector(out), as.vector(col(out)))

  out[foo] <- d

  t(out)
}
