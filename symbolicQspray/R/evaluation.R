#' @title Assign values to the parameters of a 'symbolicQspray'
#' @description Substitutes some values to the parameters of a
#'   \code{symbolicQspray} polynomial.
#'
#' @param Qspray a \code{symbolicQspray} object
#' @param values vector of values to be substituted to the parameters; these
#'   values must be coercible to \code{bigq} numbers
#'
#' @return A \code{qspray} object.
#' @export
#'
#' @seealso Use \code{\link{changeParameters}} to apply a transformation of the
#'   parameters. Use \code{\link{substituteVariables}} to substitute some
#'   values to the variables.
#'
#' @examples
#' library(symbolicQspray)
#' f <- function(a1, a2, X, Y) {
#'   (a1 + 2)*X^2*Y + (a2/(a1^2+a2))*X*Y
#' }
#' Qspray <- f(qlone(1), qlone(2), Qlone(1), Qlone(2))
#' a <- c(2, "2/3")
#' ( qspray <- substituteParameters(Qspray, values = a) )
#' a <- gmp::as.bigq(a)
#' qspray == f(a[1], a[2], qlone(1), qlone(2)) ## should be TRUE
substituteParameters <- function(Qspray, values) {
  evalSymbolicQspray(Qspray, a = values, X = NULL)
}

#' @title Assign values to the variables of a 'symbolicQspray'
#' @description Substitutes some values to the variables of a
#'   \code{symbolicQspray} polynomial.
#'
#' @param Qspray a \code{symbolicQspray} object
#' @param values vector of values to be substituted to the variables; these
#'   values must be coercible to \code{bigq} numbers
#'
#' @return A \code{ratioOfQsprays} object.
#' @export
#'
#' @seealso Use \code{\link{changeVariables}} to apply a transformation of the
#'   variables. Use \code{\link{substituteParameters}} to substitute some
#'   values to the parameters.
#'
#' @examples
#' library(symbolicQspray)
#' f <- function(a1, a2, X, Y) {
#'   (a1 + 2)*X^2*Y + (a2/(a1^2+a2))*X*Y
#' }
#' a1 <- qlone(1); a2 <- qlone(2)
#' Qspray <- f(a1, a2, Qlone(1), Qlone(2))
#' values <- c(3, "2/3")
#' ( rOQ <- substituteVariables(Qspray, values) )
#' values <- gmp::as.bigq(values)
#' rOQ == f(a1, a2, values[1], values[2]) ## should be TRUE
substituteVariables <- function(Qspray, values) {
  evalSymbolicQspray(Qspray, a = NULL, X = values)
}

#' @title Evaluation of a 'symbolicQspray' polynomial
#' @description Evaluates a \code{symbolicQspray} polynomial by substituting
#'   some values to the parameters (same as \code{\link{substituteParameters}})
#'   or to the variables (same as \code{\link{substituteVariables}}) or both.
#'
#' @param Qspray a \code{symbolicQspray} object
#' @param a vector of values to be substituted to the parameters;
#'   these values must be coercible to \code{bigq} numbers
#' @param X vector of values to be substituted to the variables; these
#'   values must be coercible to \code{bigq} numbers
#'
#' @return If both \code{a} and \code{X} are \code{NULL}, this returns the
#'   input \code{symbolicQspray} object; otherwise, if \code{a} is not
#'   \code{NULL}, this returns a \code{qspray} object, and if
#'   \code{X} is not \code{NULL}, this returns a \code{ratioOfQsprays} object.
#' @export
#' @importFrom ratioOfQsprays evalRatioOfQsprays showRatioOfQspraysOption<-
#' @importFrom qspray showQsprayOption<- evalQspray
#' @importFrom gmp c_bigq as.bigq
#'
#' @examples
#' library(symbolicQspray)
#' a1 <- qlone(1); a2 <- qlone(2)
#' X1 <- Qlone(1); X2 <- Qlone(2); X3 <- Qlone(3)
#' ( Qspray <- (a1 + 2)*X1^2*X2 + (a2/(a1^2+a2))*X1*X2*X3 )
#' a <- c(2, 3)
#' X <- c(4, 3, 2)
#' ( qspray <- evalSymbolicQspray(Qspray, a = a) )
#' ( rOQ <- evalSymbolicQspray(Qspray, X = X) )
#' evalSymbolicQspray(Qspray, a = a, X = X)
#' evalQspray(qspray, X)
#' evalRatioOfQsprays(rOQ, a)
evalSymbolicQspray <- function(Qspray, a = NULL, X = NULL) {
  if(!is.null(a)) {
    coeffs <- c_bigq(lapply(Qspray@coeffs, evalRatioOfQsprays, values_re = a))
    toKeep <- which(coeffs != 0L)
    qspray <- new(
      "qspray",
      powers = Qspray@powers[toKeep],
      coeffs = as.character(coeffs)[toKeep]
    )
    if(is.null(X)) { # 'X' is NULL and 'a' is not NULL
      sSQ <- getShowSymbolicQspray(Qspray)
      showQsprayOption(qspray, "showMonomial") <- attr(sSQ, "showMonomial")
      qspray
    } else { # both 'a' and 'X' are not NULL
      evalQspray(qspray, values_re = X)
    }
  } else if(!is.null(X)){ # 'X' is not NULL and 'a' is NULL
    X <- as.bigq(X)
    if(anyNA(X)) {
      stop("Invalid values in `X`.")
    }
    monomials <- lapply(Qspray@powers, function(exponents) {
      if(length(exponents) != 0L) {
        powers <- lapply(which(exponents != 0L), function(i) {
          X[[i]]^exponents[i]
        })
        Reduce(`*`, powers)
      } else {
        as.bigq(1L)
      }
    })
    coeffs <- Qspray@coeffs
    roq <- as.ratioOfQsprays(0L)
    for(i in seq_along(coeffs)) {
      roq <- roq + monomials[[i]]*coeffs[[i]]
    }
    sSQ <- getShowSymbolicQspray(Qspray)
    showRatioOfQspraysOption(roq, "showRatioOfQsprays") <-
      attr(sSQ, "showRatioOfQsprays")
    roq
  } else { # both 'a' and 'X' are NULL
    Qspray
  }
}
