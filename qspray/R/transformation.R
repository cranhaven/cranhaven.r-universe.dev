#' @include qspray.R
NULL

#' @title Ordered 'qspray'
#' @description Reorders the terms of a \code{qspray} object according to the 
#'   lexicographic order of the powers. This function is rather used 
#'   internally only but it is exported for internal usage in other packages.
#'
#' @param qspray a \code{qspray} object
#'
#' @return A \code{qspray} object. It defines the same polynomial as the 
#'   input \code{qspray} object but it is ordered.
#' @export
#'
#' @examples
#' qspray <- rQspray()
#' qspray == orderedQspray(qspray) # should be TRUE
orderedQspray <- function(qspray) {
  M <- powersMatrix(qspray)
  if(ncol(M) > 0L) {
    lex <- lexorder(M)
    qspray@powers <- qspray@powers[lex]
    qspray@coeffs <- qspray@coeffs[lex]
  }
  qspray
}

#' @title Partial derivative
#' @description Partial derivative of a \code{qspray} polynomial.
#'
#' @param qspray object of class \code{qspray}
#' @param i integer, the dimension to differentiate with respect to, e.g. 
#'   \code{1} to differentiate with respect to \eqn{x}
#' @param derivative integer, how many times to differentiate
#'
#' @return A \code{qspray} object.
#' @export
#'
#' @examples
#' library(qspray)
#' x <- qlone(1)
#' y <- qlone(2)
#' qspray <- 2*x  + 3*x*y
#' derivQspray(qspray, 2) # derivative w.r.t. y
derivQspray <- function(qspray, i, derivative = 1) {
  stopifnot(inherits(qspray, "qspray"))
  stopifnot(isNonnegativeInteger(i))
  stopifnot(isPositiveInteger(derivative))
  if(derivative == 0L) {
    dqspray <- qspray
  } else if(i > numberOfVariables(qspray)) {
    dqspray <- qzero()
  } else {
    n    <- integer(length = i)
    n[i] <- as.integer(derivative)
    drv  <- qspray_deriv(qspray@powers, qspray@coeffs, n)
    dqspray <- qspray_from_list(drv)
  }
  passShowAttributes(qspray, dqspray)
}

#' @title Partial differentiation
#' @description Partial differentiation of a \code{qspray} polynomial.
#'
#' @param qspray object of class \code{qspray}
#' @param orders integer vector, the orders of the differentiation; e.g. 
#'   \code{c(2, 0, 1)} means that you differentiate two times with respect to 
#'   \eqn{x}, you do not differentiate with respect to \eqn{y}, and you 
#'   differentiate one time with respect to \eqn{z}
#'
#' @return A \code{qspray} object.
#' @export
#'
#' @examples
#' library(qspray)
#' x <- qlone(1)
#' y <- qlone(2)
#' qspray <- x + 2*y  + 3*x*y
#' dQspray(qspray, c(1, 1))
#' derivQspray(derivQspray(qspray, 1), 2)
dQspray <- function(qspray, orders) {
  stopifnot(inherits(qspray, "qspray"))
  for(i in seq_along(orders)) {
    stopifnot(isPositiveInteger(orders[i]))
  }
  orders <- removeTrailingZeros(orders)
  if(length(orders) == 0L) {
    dqspray <- qspray
  } else if(length(orders) > numberOfVariables(qspray)) {
    dqspray <- qzero()
  } else {
    n    <- as.integer(orders)
    drv  <- qspray_deriv(qspray@powers, qspray@coeffs, n)
    dqspray <- qspray_from_list(drv)
  }
  passShowAttributes(qspray, dqspray)
}

setGeneric(
  "permuteVariables", function(x, permutation) {
    stop(
      "No available application of `permuteVariables`. Check the arguments."
    )
  }
)

#' @name permuteVariables
#' @aliases permuteVariables,qspray,numeric-method 
#' @docType methods
#' @title Permute variables
#' @description Permute the variables of a \code{qspray} polynomial.
#'
#' @param x a \code{qspray} object
#' @param permutation a permutation
#'
#' @return A \code{qspray} object.
#' @export
#'
#' @examples
#' library(qspray)
#' f <- function(x, y, z) {
#'   x^2 + 5*y + z - 1
#' }
#' x <- qlone(1)
#' y <- qlone(2)
#' z <- qlone(3)
#' P <- f(x, y, z)
#' permutation <- c(3, 1, 2)
#' Q <- permuteVariables(P, permutation)
#' Q == f(z, x, y) # should be TRUE
setMethod(
  "permuteVariables", c("qspray", "numeric"), 
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
    powers <- apply(M, 1L, function(row) {
      removeTrailingZeros(row)
    }, simplify = FALSE)
    out <- new("qspray", powers = powers, coeffs = x@coeffs)
    passShowAttributes(x, out)
  }
)

setGeneric(
  "swapVariables", function(x, i, j) {
    stop(
      "No available application of `swapVariables`. Check the arguments."
    )
  }
)

#' @name swapVariables
#' @aliases swapVariables,qspray,numeric,numeric-method 
#' @docType methods
#' @title Swap variables
#' @description Swap two variables in a \code{qspray} polynomial.
#'
#' @param x a \code{qspray} object
#' @param i,j indices of the variables to be swapped
#'
#' @return A \code{qspray} object.
#' @export
#'
#' @examples
#' library(qspray)
#' f <- function(x, y, z) {
#'   x^2 + 5*y + z - 1
#' }
#' x <- qlone(1)
#' y <- qlone(2)
#' z <- qlone(3)
#' P <- f(x, y, z)
#' Q <- swapVariables(P, 2, 3)
#' Q == f(x, z, y) # should be TRUE
setMethod(
  "swapVariables", c("qspray", "numeric", "numeric"), 
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
    powers <- apply(M, 1L, function(row) {
      removeTrailingZeros(row)
    }, simplify = FALSE)
    out <- new("qspray", powers = powers, coeffs = x@coeffs)
    passShowAttributes(x, out)
  }
)

#' @title Compose 'qspray' polynomials
#' @description Substitutes the variables of a \code{qspray} polynomial with 
#'   some \code{qspray} polynomials. E.g. you have a polynomial \eqn{P(x, y)} 
#'   and you want the polynomial \eqn{P(x^2, x+y+1)} (see example).
#' 
#' @param qspray a \code{qspray} polynomial
#' @param listOfQsprays a list containing at least \code{n} \code{qspray} 
#'   polynomials where \code{n} is the number of variables of the polynomial 
#'   given in the \code{qspray} argument
#'
#' @return The \code{qspray} polynomial obtained by composing the polynomial 
#'   given in the \code{qspray} argument with the polynomials given in the 
#'   \code{listOfQsprays} argument.
#' @export
#'
#' @examples
#' library(qspray)
#' x <- qlone(1)
#' y <- qlone(2)
#' P <- x*y/2 + 4*y
#' X <- x^2
#' Y <- x + y + 1
#' composeQspray(P, list(X, Y)) # this is P(x^2, x+y+1)
composeQspray <- function(qspray, listOfQsprays) {
  if(isConstant(qspray)) {
    return(qspray)
  }
  n <- numberOfVariables(qspray)
  if(length(listOfQsprays) < n) {
    stop(
      sprintf(
        paste0(
          "The `listOfQsprays` argument must be a list containing ", 
          "at least %d objects coercible to `qspray` polynomials."
        ), n
      )
    )
  }
  coeffs <- qspray@coeffs
  powers <- qspray@powers
  result <- qzero()
  for(i in seq_along(powers)) {
    term <- qone()
    pwr <- powers[[i]]
    for(j in seq_along(pwr)) {
      p <- pwr[j]
      if(p != 0L) {
        term <- term * as.qspray(listOfQsprays[[j]])^p
      }
    }
    result <- result + coeffs[i] * term
  }
  if(isNamedList(listOfQsprays)) {
    showQsprayOption(result, "showQspray") <- 
      showQsprayXYZ(names(listOfQsprays))
  }
  result  
}

setGeneric(
  "changeVariables", function(x, listOfQsprays) {
    stop(
      "No available application of `changeVariables`. Check the arguments."
    )
  }
)

#' @name changeVariables
#' @aliases changeVariables,qspray,list-method 
#' @docType methods
#' @title Change of variables in a 'qspray' polynomial
#' @description Replaces the variables of a \code{qspray} polynomial with 
#'   some \code{qspray} polynomials. E.g. you have a polynomial \eqn{P(x, y)} 
#'   and you want the polynomial \eqn{P(x^2, x+y+1)}. This is an alias of 
#'   \code{\link{composeQspray}}.
#' 
#' @param x a \code{qspray} polynomial
#' @param listOfQsprays a list containing at least \code{n} \code{qspray} 
#'   objects, or objects coercible to \code{qspray} objects, where \code{n} 
#'   is the number of variables of the polynomial given in the \code{x} 
#'   argument; if this list is named, then its names will be used in the
#'   show options of the result
#'
#' @return The \code{qspray} polynomial obtained by replacing the variables of 
#'   the polynomial given in the \code{x} argument with the polynomials given 
#'   in the \code{listOfQsprays} argument.
#' @export
#'
#' @examples
#' library(qspray)
#' f <- function(x, y) x*y/2 + 4*y
#' x <- qlone(1)
#' y <- qlone(2)
#' P <- f(x, y)
#' X <- x^2
#' Y <- x + y + 1
#' changeVariables(P, list(X, Y)) == f(X, Y) # should be TRUE
setMethod(
  "changeVariables", c("qspray", "list"), 
  function(x, listOfQsprays) {
    composeQspray(x, listOfQsprays)
  }
)
