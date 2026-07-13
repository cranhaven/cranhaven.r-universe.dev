#' Resample
#'
#' A version of sample that doesn't have awkward behaviour when `length(x) == 1`.
#' Vectorized version of `sample` using `Vectorize`
#'
#' Intended for internal use only. `size` is vectorized.
#'
#' @inheritParams base::sample
#'
#' @return A random permutation, as in `sample`, but with `size` vectorized.
#'
#' @keywords internal
sampleV <- Vectorize("sample", "size", SIMPLIFY = FALSE)

#' Adapted directly from the [base::sample()] help file.
#'
#' @inheritParams base::sample
#'
#' @param ... Passed to [base::sample()]
#'
#' @keywords internal
#' @rdname resample
resample <- function(x, ...) x[sample.int(length(x), ...)]

#' `resampleZeroProof` is a version that works even if sum of all
#' probabilities passed to `sample.int` is zero.
#' This causes an error in `sample.int`.
#'
#' @note Intended for internal use only.
#'
#' @inheritParams base::sample
#'
#' @param spreadProbHas0 logical. Does `spreadProb` have any zeros on it.
#'
#' @keywords internal
#' @rdname resample
resampleZeroProof <- function(spreadProbHas0, x, n, prob) {
  if (spreadProbHas0) {
    sm <- sum(prob, na.rm = TRUE)
    if (sum(prob > 0) <= n) {
      integer()
    } else {
      resample(x, n, prob = prob / sm)
    }
  } else {
    resample(x, n, prob = prob / sum(prob, na.rm = TRUE))
  }
}
