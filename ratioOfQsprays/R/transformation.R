#' @include ratioOfQsprays.R
NULL

#' @title Partial derivative
#' @description Partial derivative of a \code{ratioOfQsprays}.
#'
#' @param roq object of class \code{ratioOfQsprays}
#' @param i integer, the dimension to differentiate with respect to, e.g.
#'   \code{2} to differentiate with respect to \eqn{y}
#' @param derivative integer, how many times to differentiate
#'
#' @return A \code{ratioOfQsprays} object.
#' @importFrom qspray derivQspray
#' @export
#'
#' @examples
#' library(ratioOfQsprays)
#' x <- qlone(1)
#' y <- qlone(2)
#' roq <- (2*x  + 3*x*y) / (x^2 + y^2)
#' derivRatioOfQsprays(roq, 2) # derivative w.r.t. y
derivRatioOfQsprays <- function(roq, i, derivative = 1) {
  stopifnot(inherits(roq, "ratioOfQsprays"))
  stopifnot(isNonnegativeInteger(i))
  if(derivative == 0L) {
    return(roq)
  }
  stopifnot(isPositiveInteger(derivative))
  droq <- roq
  for(. in seq_len(derivative)) {
    f  <- droq@numerator
    g  <- droq@denominator
    fp <- derivQspray(f, i, derivative = 1L)
    gp <- derivQspray(g, i, derivative = 1L)
    droq <- (fp*g - f*gp) / g^2
  }
  passShowAttributes(roq, droq)
}

#' @title Partial differentiation
#' @description Partial differentiation of a \code{ratioOfQsprays} polynomial.
#'
#' @param roq object of class \code{ratioOfQsprays}
#' @param orders integer vector, the orders of the differentiation; e.g.
#'   \code{c(2, 0, 1)} means that you differentiate two times with respect to
#'   \eqn{x}, you do not differentiate with respect to \eqn{y}, and you
#'   differentiate one time with respect to \eqn{z}
#'
#' @return A \code{ratioOfQsprays} object.
#' @export
#'
#' @examples
#' library(ratioOfQsprays)
#' x <- qlone(1)
#' y <- qlone(2)
#' roq <- (x + 2*y  + 3*x*y) / (x + 1)
#' dRatioOfQsprays(roq, c(1, 1))
#' derivRatioOfQsprays(derivRatioOfQsprays(roq, 1), 2)
dRatioOfQsprays <- function(roq, orders) {
  stopifnot(inherits(roq, "ratioOfQsprays"))
  for(i in seq_along(orders)) {
    stopifnot(isPositiveInteger(orders[i]))
  }
  orders <- removeTrailingZeros(orders)
  if(length(orders) > numberOfVariables(roq)) {
    return(as.ratioOfQsprays(0L))
  }
  ns <- do.call(c, lapply(seq_along(as.integer(orders)), function(i) {
    rep(i, orders[i])
  }))
  f <- function(r, i) {
    if(i != 0L) {
      derivRatioOfQsprays(r, i, derivative = 1L)
    } else {
      r
    }
  }
  passShowAttributes(roq, Reduce(f, ns, init = roq))
}

setGeneric("permuteVariables")

#' @name permuteVariables
#' @aliases permuteVariables,ratioOfQsprays,numeric-method
#' @docType methods
#' @title Permute variables
#' @description Permute the variables of a \code{ratioOfQsprays} fraction
#'   of polynomials.
#'
#' @param x a \code{ratioOfQsprays} object
#' @param permutation a permutation
#'
#' @return A \code{ratioOfQsprays} object.
#' @export
#' @importFrom qspray permuteVariables
#'
#' @examples
#' library(ratioOfQsprays)
#' f <- function(x, y, z) {
#'   (x^2 + 5*y + z - 1) / (x + 1)
#' }
#' x <- qlone(1)
#' y <- qlone(2)
#' z <- qlone(3)
#' R <- f(x, y, z)
#' permutation <- c(3, 1, 2)
#' S <- permuteVariables(R, permutation)
#' S == f(z, x, y) # should be TRUE
setMethod(
  "permuteVariables", c("ratioOfQsprays", "numeric"),
  function(x, permutation) {
    permuteVariables(x@numerator, permutation) /
      permuteVariables(x@denominator, permutation)
  }
)

setGeneric("swapVariables")

#' @name swapVariables
#' @aliases swapVariables,ratioOfQsprays,numeric,numeric-method
#' @docType methods
#' @title Swap variables
#' @description Swap two variables of a \code{ratioOfQsprays}.
#'
#' @param x a \code{ratioOfQsprays} object
#' @param i,j indices of the variables to be swapped
#'
#' @return A \code{ratioOfQsprays} object.
#' @export
#' @importFrom qspray swapVariables
#'
#' @examples
#' library(ratioOfQsprays)
#' f <- function(x, y, z) {
#'   (x^2 + 5*y + z - 1) / (x + 1)
#' }
#' x <- qlone(1)
#' y <- qlone(2)
#' z <- qlone(3)
#' R <- f(x, y, z)
#' S <- swapVariables(R, 2, 3)
#' S == f(x, z, y) # should be TRUE
setMethod(
  "swapVariables", c("ratioOfQsprays", "numeric", "numeric"),
  function(x, i, j) {
    swapVariables(x@numerator, i, j) /
      swapVariables(x@denominator, i, j)
  }
)

setGeneric("changeVariables")

#' @name changeVariables
#' @aliases changeVariables,ratioOfQsprays,list-method
#' @docType methods
#' @importFrom qspray changeVariables
#' @title Change of variables in a 'ratioOfQsprays' fraction of polynomials
#' @description Replaces the variables of a \code{ratioOfQsprays} fraction of
#'   polynomials with some \code{qspray} polynomials. E.g. you have a fraction
#'   of polynomials \eqn{R(x, y)} and you want the fraction of polynomials
#'   \eqn{R(x^2, x+y+1)}.
#'
#' @param x a \code{ratioOfQsprays} fraction of polynomials
#' @param listOfQsprays a list containing at least \code{n} \code{qspray}
#'   objects, or objects coercible to \code{qspray} objects, where
#'   \code{n} is the number of variables of the \code{ratioOfQsprays} fraction
#'   of polynomials given in the \code{x} argument; if this list is named,
#'   then its names will be used in the show options of the result
#'
#' @return The \code{ratioOfQsprays} fraction of polynomials obtained by
#'   replacing the variables of the fraction of polynomials given in the
#'   \code{x} argument with the \code{qspray} polynomials given in the
#'   \code{listOfQsprays} argument.
#' @export
#' @examples
#' library(ratioOfQsprays)
#' f <- function(x, y) {
#'   (x^2 + 5*y - 1) / (x + 1)
#' }
#' x <- qlone(1)
#' y <- qlone(2)
#' R <- f(x, y)
#' X <- x^2
#' Y <- x + y + 1
#' S <- changeVariables(R, list(X, Y))
#' S == f(X, Y) # should be TRUE
setMethod(
  "changeVariables", c("ratioOfQsprays", "list"),
  function(x, listOfQsprays) {
    if(length(listOfQsprays) < numberOfVariables(x)) {
      stop(
        "The `listOfQsprays` list is too short."
      )
    }
    num <- changeVariables(x@numerator, listOfQsprays)
    den <- changeVariables(x@denominator, listOfQsprays)
    as.ratioOfQsprays(num) / as.ratioOfQsprays(den)
  }
)
