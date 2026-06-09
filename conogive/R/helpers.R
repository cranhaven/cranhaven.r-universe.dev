#' Thurstone weights
#'
#' @keywords internal
#' @param lambda Vector of loadings.
#' @param sigma Vector of standard deviations.
#' @return The Thurstone weights.

thurstone = function(lambda, sigma) {
  c(lambda/(sigma^2 * (1 + sum (lambda^2 / sigma^2))))
}

#' Trace of matrix
#' @keywords internal
#' @param A A square matrix.
#' @return Trace of the matrix.
tr <- function(A) sum(diag(A))

#' Transform y Into a Form Where Each Category is An Positive Integer
#'
#' @keywords internal
#' @param y An array or data frame of observations.
#' @return A matrix.

ordered_y = function(y) {
  k = ncol(y)
  y = data.frame(apply(data.frame(y), 2, as.factor))
  for(i in seq.int(k)) levels(y[, i]) = seq.int(length(levels(y[, i])))
  matrix(as.numeric(as.matrix(y)), ncol = k)
}

#' Remove Infinities from Vector, Append and Prepend `-Inf` and `Inf`, and Sort
#'
#' @keywords internal
#' @param x Numeric vector with no `NA` values.
#' @return Vector with `-Inf`` prepended and `Inf` appended

trim_vector = function(x) {
  checkmate::assert_atomic_vector(x, any.missing = FALSE)
  checkmate::assert_numeric(x)
  x = x[x != Inf & x != -Inf]
  c(-Inf, sort(x), Inf)
}

#' Massage Cuts to the Desired Shape
#'
#' @param cuts A matrix, list, or vector of cuts
#' @param k Optional `k` saying how many times the vector of cuts should be
#'    repeated. Only matters when `cuts` is a vector.

massage_cuts = function(cuts, k)  {

  if(!missing(k)) checkmate::assert_count(k)

  if (checkmate::testAtomicVector(cuts)) {
    if(missing(k)) k = 1
    cuts = t(matrix(rep(trim_vector(cuts), k), nrow = length(cuts)))
  }

  if(is.matrix(cuts)) {
    cuts = lapply(seq.int(nrow(cuts)), function(i) trim_vector(cuts[i, ]))
  }

  checkmate::assert_list(cuts)

  cuts

}

#' Calculate the Theoretical Xi
#'
#' @keywords internal
#' @param cuts A matrix, list, or vector of cuts.
#' @param rho The polychoric correlation matrix; a numeric matrix with no
#'    missing entries.
#' @return Theoretical xi matrix.

xi_theoretical = function(cuts, rho) {

  checkmate::assert_matrix(rho, mode = "numeric", any.missing = FALSE)

  items_k = nrow(rho)
  cuts = massage_cuts(cuts, items_k)
  probs = lapply(cuts, function(x) diff(stats::pnorm(x)))
  ms = sapply(probs, length)

  f = function(i, cut)
    -(stats::dnorm(cut[i + 1]) - stats::dnorm(cut[i])) / (stats::pnorm(cut[i + 1]) - stats::pnorm(cut[i]))

  etas = lapply(seq.int(probs), function(i) f(seq.int(ms[i]), cuts[[i]]))

  g = function(i, j, k, l) {
    lower = rep(-Inf, items_k)
    upper = rep(Inf, items_k)
    lower[c(i, j)] = c(cuts[[i]][k], cuts[[j]][l])
    upper[c(i, j)] = c(cuts[[i]][k + 1], cuts[[j]][l + 1])
    mvtnorm::pmvnorm(lower = lower, upper = upper, corr = rho)
  }

  FUN <- Vectorize(function(i, j) {

    if(i != j) {
      grid = as.matrix(expand.grid(seq.int(ms[i]), (seq.int(ms[j]))))
      sum(apply(grid, 1, function(y)
        g(i, j, y[1], y[2]) * etas[[i]][y[1]] * etas[[j]][y[2]]))
    } else {
      grid = seq.int(ms[i])
      sum(sapply(grid, function(y)
        g(i, i, y, y) * etas[[i]][y]^2))
    }

  })

  outer(X = seq.int(items_k), Y = seq.int(items_k), FUN = FUN)

}

#' Calculate Sample Xi
#'
#' @keywords internal
#' @param y An array or data frame of observations.
#' @param cuts A matrix, list, or vector of cuts.
#' @param use Passed to `stats::cov`; defaults to `"complete.obs"`.
#' @return Sample xi matrix.

xi_sample = function(y, cuts, use = "complete.obs") {
  k = ncol(y)

  n_categories = apply(y, 2, max, na.rm = TRUE)
  cuts = massage_cuts(cuts, k)
  lengths = sapply(cuts, length) - 1

  assertthat::assert_that(sum(n_categories - lengths) <= 0,
   msg = "Larger number of categories than length of cuts.")

  args = lapply(seq.int(k), function(i) x_hat(y[, i], cuts[[i]]))
  xhats = do.call(what = cbind, args = args)
  stats::cov(xhats, use = use)

}

#' Transform Likert data to X_hats.
#'
#' @keywords internal
#' @param y Vector of observations.
#' @param cuts Vector of cuts.
#' @return The X_hats associated with `y` and `cuts`.
x_hat = function(y, cuts) {

  checkmate::assert_atomic_vector(y)
  checkmate::assert_numeric(y)

  f = function(i) {
    -(stats::dnorm(cuts[i + 1]) - stats::dnorm(cuts[i]))/
      (stats::pnorm(cuts[i + 1]) - stats::pnorm(cuts[i]))
  }

  sapply(X = y, FUN = f)

}

#' Standardize Parameter Vectors
#'
#' The function `standardize_lambda` standardizes `lambda` and
#'    `standardize_sigma` standardizes `sigma`.
#'
#' @keywords internal
#' @param lambda Vector of loadings.
#' @param sigma Vector of standard deviations.
#' @return Standardized vector.
#' @name standardize
standardize_lambda = function(lambda, sigma) {
  checkmate::assert_atomic_vector(lambda, any.missing = FALSE)
  checkmate::assert_atomic_vector(sigma, any.missing = FALSE)
  checkmate::assert_numeric(lambda)
  checkmate::assert_numeric(sigma)
  lambda/sqrt(lambda^2 + sigma^2)
}

#' @rdname standardize
standardize_sigma = function(lambda, sigma) {
  checkmate::assert_atomic_vector(lambda, any.missing = FALSE)
  checkmate::assert_atomic_vector(sigma, any.missing = FALSE)
  checkmate::assert_numeric(lambda)
  checkmate::assert_numeric(sigma)
  sigma/sqrt(lambda^2 + sigma^2)
}
