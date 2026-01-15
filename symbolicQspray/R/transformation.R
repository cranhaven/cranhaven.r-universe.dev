#' @include symbolicQspray.R
NULL

setGeneric("permuteVariables")

#' @name permuteVariables
#' @aliases permuteVariables,symbolicQspray,numeric-method
#' @docType methods
#' @title Permute variables
#' @description Permute the variables of a \code{symbolicQspray} polynomial.
#'
#' @param x a \code{symbolicQspray} object
#' @param permutation a permutation
#'
#' @return A \code{symbolicQspray} object.
#' @export
#' @importFrom qspray permuteVariables
#'
#' @examples
#' f <- function(a1, a2, X, Y, Z) {
#'   (a1^2 + 5*a2) / (a1 + 1) * X^2*Y  +  (3*a1 - a2) / a2 * Y^3
#' }
#' a1 <- qlone(1)
#' a2 <- qlone(2)
#' X <- Qlone(1)
#' Y <- Qlone(2)
#' Z <- Qlone(3)
#' Qspray <- f(a1, a2, X, Y, Z)
#' perm <- c(3, 1, 2)
#' permuteVariables(Qspray, perm) == f(a1, a2, Z, X, Y) # should be TRUE
setMethod(
  "permuteVariables", c("symbolicQspray", "numeric"),
  function(x, permutation) {
    stopifnot(isPermutation(permutation))
    if(isConstant(x)) {
      return(x)
    }
    m <- numberOfVariables(x)
    n <- length(permutation)
    if(m > n) {
      stop("Invalid permutation.")
    }
    permutation[permutation] <- seq_along(permutation)
    M <- powersMatrix(x)
    for(. in seq_len(n - m)) {
      M <- cbind(M, 0L)
    }
    M <- M[, permutation, drop = FALSE]
    powers <- apply(M, 1L, removeTrailingZeros, simplify = FALSE)
    out <- new("symbolicQspray", powers = powers, coeffs = x@coeffs)
    passShowAttributes(x, out)
  }
)

setGeneric("swapVariables")

#' @name swapVariables
#' @aliases swapVariables,symbolicQspray,numeric,numeric-method
#' @docType methods
#' @title Swap variables
#' @description Swap two variables of a \code{symbolicQspray}.
#'
#' @param x a \code{symbolicQspray} object
#' @param i,j indices of the variables to be swapped
#'
#' @return A \code{symbolicQspray} object.
#' @export
#' @importFrom qspray swapVariables
#'
#' @examples
#' library(symbolicQspray)
#' f <- function(a1, a2, X, Y, Z) {
#'   (a1^2 + 5*a2) / (a1 + 1) * X^2*Y  +  (3*a1 - a2) / a2 * Y^3
#' }
#' a1 <- qlone(1)
#' a2 <- qlone(2)
#' X <- Qlone(1)
#' Y <- Qlone(2)
#' Z <- Qlone(3)
#' Qspray <- f(a1, a2, X, Y, Z)
#' swapVariables(Qspray, 2, 3) == f(a1, a2, X, Z, Y) # should be TRUE
setMethod(
  "swapVariables", c("symbolicQspray", "numeric", "numeric"),
  function(x, i, j) {
    stopifnot(isNonnegativeInteger(i), isNonnegativeInteger(j))
    if(isConstant(x)) {
      return(x)
    }
    m <- numberOfVariables(x)
    n <- max(m, i, j)
    permutation <- seq_len(n)
    permutation[i] <- j
    permutation[j] <- i
    M <- powersMatrix(x)
    for(. in seq_len(n - m)) {
      M <- cbind(M, 0L)
    }
    M <- M[, permutation, drop = FALSE]
    powers <- apply(M, 1L, removeTrailingZeros, simplify = FALSE)
    out <- new("symbolicQspray", powers = powers, coeffs = x@coeffs)
    passShowAttributes(x, out)
  }
)

#' @title Partial derivative
#' @description Partial derivative of a \code{symbolicQspray} polynomial.
#'
#' @param Qspray object of class \code{symbolicQspray}
#' @param i integer, the dimension to differentiate with respect to, e.g.
#'   \code{2} to differentiate w.r.t. \eqn{y}
#' @param derivative positive integer, how many times to differentiate
#'
#' @return A \code{symbolicQspray} object.
#' @export
derivSymbolicQspray <- function(Qspray, i, derivative = 1) {
  stopifnot(inherits(Qspray, "symbolicQspray"))
  stopifnot(isNonnegativeInteger(i))
  stopifnot(isPositiveInteger(derivative))
  if(i > arity(Qspray)) {
    dQspray <- Qzero()
  } else {
    n    <- integer(length = i)
    n[i] <- as.integer(derivative)
    drv  <- SymbolicQspray_deriv(
      Qspray@powers, lapply(Qspray@coeffs, ratioOfQsprays_as_list), n
    )
    dQspray <- symbolicQspray_from_list(drv)
  }
  passShowAttributes(Qspray, dQspray)
}

#' @title Partial differentiation
#' @description Partial differentiation of a \code{symbolicQspray}
#'   polynomial.
#'
#' @param Qspray object of class \code{symbolicQspray}
#' @param orders integer vector, the orders of the differentiation; e.g.
#'   \code{c(2, 0, 1)} means that you differentiate two times with respect to
#'   \eqn{x}, you do not differentiate with respect to \eqn{y}, and you
#'   differentiate one time with respect to \eqn{z}
#'
#' @return A \code{symbolicQspray} object.
#' @export
dSymbolicQspray <- function(Qspray, orders) {
  stopifnot(inherits(Qspray, "symbolicQspray"))
  for(i in seq_along(orders)) {
    stopifnot(isPositiveInteger(orders[i]))
  }
  orders <- removeTrailingZeros(orders)
  if(length(orders) > arity(Qspray)) {
    dQspray <- Qzero()
  } else {
    n    <- as.integer(orders)
    drv  <- SymbolicQspray_deriv(
      Qspray@powers, lapply(Qspray@coeffs, ratioOfQsprays_as_list), n
    )
    dQspray <- symbolicQspray_from_list(drv)
  }
  passShowAttributes(Qspray, dQspray)
}

setGeneric("changeVariables")

#' @name changeVariables
#' @aliases changeVariables,symbolicQspray,list-method
#' @docType methods
#' @importFrom qspray changeVariables showMonomialXYZ
#' @title Change of variables in a 'symbolicQspray' polynomial
#' @description Replaces the variables of a \code{symbolicQspray} polynomial
#'   with some \code{symbolicQspray} polynomials. E.g. you have a polynomial
#'   \eqn{P_a(x, y)} and you want the polynomial \eqn{P_a(x+a, y+a)} (see
#'   example).
#'
#' @param x a \code{symbolicQspray} polynomial
#' @param listOfQsprays a list containing at least \code{n}
#'   \code{symbolicQspray} objects, or objects coercible to
#'   \code{symbolicQspray} objects, where \code{n} is the number of
#'   variables in the polynomial given in the \code{x} argument; if
#'   this list is named, their its names will be used in the show
#'   options of the result
#'
#' @return The \code{symbolicQspray} polynomial obtained by replacing the
#'   variables of the polynomial given in the \code{x} argument with the
#'   polynomials given in the \code{listOfQsprays} argument.
#' @export
#'
#' @seealso If you want to change the parameters of a symbolic qspray, use
#'   \code{\link{changeParameters}}. If you want to assign some values to
#'   its variables, see \code{\link{substituteVariables}}.
#'
#' @examples
#' library(symbolicQspray)
#' f <- function(a, X, Y) {
#'   a^2 / (a + 1) * X^2*Y  +  (3*a - 2) / a * Y^2
#' }
#' a <- qlone(1)
#' X <- Qlone(1)
#' Y <- Qlone(2)
#' Qspray <- f(a, X, Y)
#' U <- X + a
#' V <- Y + a
#' changeVariables(Qspray, list(U, V)) == f(a, U, V) # should be TRUE
setMethod(
  "changeVariables", c("symbolicQspray", "list"),
  function(x, listOfQsprays) {
    if(isConstant(x)) {
      return(x)
    }
    n <- numberOfVariables(x)
    if(length(listOfQsprays) < n) {
      stop(
        sprintf(
          paste0(
            "The `listOfQsprays` argument must be a list containing ",
            "at least %d symbolic qspray polynomials."
          ), n
        )
      )
    }
    coeffs <- x@coeffs
    powers <- x@powers
    result <- Qzero()
    for(i in seq_along(powers)) {
      term <- Qone()
      pwr <- powers[[i]]
      for(j in seq_along(pwr)) {
        p <- pwr[j]
        if(p != 0L) {
          term <- term * as.symbolicQspray(listOfQsprays[[j]])^p
        }
      }
      result <- result + coeffs[[i]] * term
    }
    sSQ <- getShowSymbolicQspray(x)
    showSymbolicQsprayOption(result, "showRatioOfQsprays") <-
      attr(sSQ, "showRatioOfQsprays")
    notSymbolicQsprays <- all(vapply(listOfQsprays, function(x) {
      !inherits(x, "symbolicQspray")
    }, logical(1L)))
    if(notSymbolicQsprays) {
      return(getConstantTerm(result))
    }
    if(isNamedList(listOfQsprays)) {
      showSymbolicQsprayOption(result, "showMonomial") <-
        showMonomialXYZ(names(listOfQsprays))
    }
    result
  }
)

#' @title Change of parameters in a 'symbolicQspray' polynomial
#' @description Replaces the parameters of a \code{symbolicQspray} polynomial
#'   (which are \code{qspray} objects) with some \code{qspray} polynomials.
#'   E.g. you have a polynomial with two parameters \eqn{P_{a,b}(x)} and you
#'   want the polynomial \eqn{P_{a+1,b+1}(x)} (see example).
#'
#' @param Qspray a \code{symbolicQspray} polynomial
#' @param newParameters a list containing at least \code{n} \code{qspray}
#'   objects, or objects coercible to \code{qspray} objects, where \code{n} is
#'   the number of parameters in the symbolic polynomial given in the
#'   \code{Qspray} argument; if this list is named, then its names will be
#'   used in the show options of the result
#'
#' @return The \code{symbolicQspray} polynomial obtained by replacing the
#'   parameters of the symbolic polynomial given in the \code{Qspray} argument
#'   with the polynomials given in the \code{newParameters} argument.
#' @export
#'
#' @importFrom ratioOfQsprays showRatioOfQspraysXYZ
#'
#' @seealso If you want to change the variables of a symbolic qspray, use
#'   \code{\link{changeVariables}}. If you want to assign some values to
#'   its parameters, use \code{\link{substituteParameters}}.
#'
#' @examples
#' library(symbolicQspray)
#' ( JP <- JacobiPolynomial(2) ) # a univariate polynomial with two parameters
#' a1 <- qlone(1)
#' a2 <- qlone(2)
#' changeParameters(JP, list(a1, a2)) == JP # should be TRUE
#' changeParameters(JP, list(a1+1, a2+1))
changeParameters <- function(Qspray, newParameters) {
  if(length(newParameters) < numberOfParameters(Qspray)) {
    stop("Not enough new parameters provided.")
  }
  coeffs <- Qspray@coeffs
  newCoeffs <- lapply(coeffs, function(rOQ) {
    changeVariables(rOQ, newParameters)
  })
  toKeep <- vapply(newCoeffs, function(rOQ) {
    rOQ != 0L
  }, logical(1L))
  result <- new(
    "symbolicQspray",
    powers = Qspray@powers[toKeep],
    coeffs = newCoeffs[toKeep]
  )
  if(isNamedList(newParameters)) {
    showSymbolicQsprayOption(result, "showRatioOfQsprays") <-
      showRatioOfQspraysXYZ(names(newParameters))
  }
  result
}
