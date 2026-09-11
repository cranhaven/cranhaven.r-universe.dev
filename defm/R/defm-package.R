## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

## usethis namespace: start
#' @useDynLib defm, .registration = TRUE
## usethis namespace: end
NULL

#' @export
print.DEFM <- function(x, ...) {
  print_defm_cpp(x)
}


#' Discrete Exponential Family Model (DEFM)
#'
#' Discrete Exponential Family Models (DEFMs) are models from the exponential
#' family that deal with discrete data. Here, we deal with binary arrays which
#' can be used to represent, among other things, networks and multinomial
#' binary Markov processes.
#'
#' @param id Integer vector of length `n`. Observation ids, for example, 
#' person id.
#' @param Y 0/1 matrix of responses of `n_y` columns and `n` rows.
#' @param X Numeric matrix of covariates of size `n_x` by `n`.
#' @param order Integer. Order of the markov process, by default, 1.
#' @param copy_data Logical, if `TRUE` (default) the data is copied to the
#' C++ side. If `FALSE`, the data is not copied, and the user must ensure
#' that the data is not modified in R while the model exists.
#'
#' @return An external pointer of class `DEFM.`
#'
#' @name DEFM
#' @aliases new_defm defm
#' @seealso [defm_mle()] for maximum likelihood estimation and [loglike_defm()] 
#' for the log-likelihood function.
#' @export
#' @references 
#' Vega Yon, G. G., Pugh, M. J., & Valente, T. W. (2022). Discrete Exponential-Family Models for Multivariate Binary Outcomes (arXiv:2211.00627). arXiv. \url{https://arxiv.org/abs/2211.00627}
new_defm <- function(
    id, Y, X, order = 1, copy_data = TRUE
) {

  m <- new_defm_cpp(id, Y, X, order, copy_data)

  cnames_y <- if (is.matrix(Y))
    colnames(Y)
  else
    stop("Y should be a matrix")

  cnames_x <- if (is.matrix(X))
    colnames(X)
  else
    stop("X should be a matrix")

  # If the names are empty, then error
  if (is.null(cnames_y) || any(cnames_y == ""))
    stop("Y should have column names.")

  # if (is.null(cnames_y))
  #   cnames_y <- paste0("y", 0:(ncol(Y) - 1))

  # if (is.null(cnames_x))
  #   cnames_x <- paste0("x", 0:(ncol(X) - 1))

  set_names(m, cnames_y, cnames_x)

  m

}

#' @export
#' @rdname defm_terms
#' @param e1,e2 e1 An object of class [DEFM] (e1) and a character (e2).
#' @details The `+` method is a shortcut for term_formula
`+.DEFM` <- function(e1, e2) {

  if (!inherits(e2, "character"))
    stop("The RHS should be a character")

  res <- strsplit(e2, split = "\\s*[+]\\s*")
  for (r in res)
    invisible(td_formula(e1, r))

  invisible(e1)

}

#' @export
#' @importFrom stats4 nobs
#' @method nobs DEFM
nobs.DEFM <- function(object, ...) {
  nobs_defm(object)
}

