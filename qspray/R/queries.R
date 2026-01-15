setGeneric(
  "numberOfVariables", function(x) {
    stop(
      "No available application of `numberOfVariables` for this object."
    )
  }
)
setGeneric(
  "numberOfTerms", function(qspray) {
    stop(
      "No available application of `numberOfTerms` for this object."
    )
  }
)
setGeneric(
  "involvedVariables", function(x) {
    stop(
      "No available application of `involvedVariables` for this object."
    )
  }
)
setGeneric(
  "getCoefficient", function(qspray, exponents) {
    stop(
      "No available application of `getCoefficient`. Check the arguments."
    )
  }
)
setGeneric(
  "getConstantTerm", function(qspray) {
    stop(
      "No available application of `getConstantTerm` for this object."
    )
  }
)
setGeneric(
  "isConstant", function(x) {
    stop(
      "No available application of `isConstant` for this object."
    )
  }
)
setGeneric(
  "isUnivariate", function(x) {
    stop(
      "No available application of `isUnivariate` for this object."
    )
  }
)
setGeneric(
  "isQzero", function(qspray) {
    stop(
      "No available application of `isQzero` for this object."
    )
  }
)
setGeneric(
  "isQone", function(qspray) {
    stop(
      "No available application of `isQone` for this object."
    )
  }
)

#' @name numberOfVariables
#' @aliases numberOfVariables,qspray-method 
#' @docType methods
#' @title Number of variables in a 'qspray' polynomial
#' @description Number of variables involved in a \code{qspray} object (see 
#'   the note for the precise meaning).
#'
#' @param x a \code{qspray} object
#'
#' @return An integer.
#' @export
#' @seealso \code{\link{involvedVariables}}.
#' @note The number of variables in the \code{qspray} object \code{y^2+1} where
#'   \code{y=qlone(2)} is \code{2}, not \code{1}, although only one variable is 
#'   present. Strictly speaking, the function returns the maximal integer 
#'   \code{d} such that the variable \code{qlone(d)} occurs in the polynomial.
setMethod(
  "numberOfVariables", "qspray", 
  function(x) {
    as.integer(max(0L, arity(x)))
  }
)

#' @name involvedVariables
#' @aliases involvedVariables,qspray-method 
#' @docType methods
#' @title Variables involved in a 'qspray' polynomial
#' @description Variables involved in a \code{qspray} object.
#'
#' @param x a \code{qspray} object
#'
#' @return A vector of integers. Each integer represents the index of a 
#'   variable involved in \code{x}.
#' @export
#' @examples
#' x <- qlone(1); z <- qlone(3)
#' involvedVariables(x^2 + x*z + 1) # should be c(1L, 3L)
setMethod(
  "involvedVariables", "qspray", 
  function(x) {
    if(isConstant(x)) {
      integer(0L)
    } else {
      M <- powersMatrix(x)
      tests <- apply(M, 2L, function(col) {
        any(col != 0L)
      })
      which(tests)
    }
  }
)

#' @name numberOfTerms
#' @aliases numberOfTerms,qspray-method 
#' @docType methods
#' @title Number of terms in a 'qspray' polynomial
#' @description Number of terms of the polynomial defined by a 
#'   \code{qspray} object.
#'
#' @param qspray a \code{qspray} object
#'
#' @return An integer.
#' @export
setMethod(
  "numberOfTerms", "qspray", 
  function(qspray) {
    length(qspray@powers)
  }
)

#' @name getCoefficient
#' @aliases getCoefficient,qspray,numeric-method 
#' @docType methods
#' @title Get a coefficient in a 'qspray' polynomial
#' @description Get the coefficient of the term with the given monomial.
#' 
#' @param qspray a \code{qspray} object
#' @param exponents a vector of exponents, thereby defining a monomial;
#'   trailing zeros are ignored
#'
#' @return The coefficient as a \code{bigq} number.
#' @export
#' @importFrom gmp as.bigq
#'
#' @examples
#' library(qspray)
#' x <- qlone(1)
#' y <- qlone(2)
#' p <- 4*x^2 + 3*y - 5
#' getCoefficient(p, 2)          # coefficient of x^2
#' getCoefficient(p, c(2, 0))    # same as getCoefficient(p, 2)
#' getCoefficient(p, c(0, 1))    # coefficient of y because y=x^0*y^1
#' getCoefficient(p, 0)          # the constant term
#' getCoefficient(p, integer(0)) # the constant term 
#' getCoefficient(p, 3)          # there's no x^3
setMethod(
  "getCoefficient", c("qspray", "numeric"), 
  function(qspray, exponents) {
    stopifnot(isExponents(exponents))
    exponents <- removeTrailingZeros(exponents)
    n <- numberOfVariables(qspray)
    if(length(exponents) > n) {
      coeff <- 0L
    } else {
      powers <- vapply(qspray@powers, function(pows) {
        toString(grow(pows, n))
      }, character(1L))
      i <- match(toString(grow(exponents, n)), powers)
      if(is.na(i)) {
        coeff <- 0L
      } else {
        coeff <- qspray@coeffs[[i]]
      }
    }
    as.bigq(coeff)
  }
)

#' @name getConstantTerm
#' @aliases getConstantTerm,qspray-method 
#' @docType methods
#' @title Get the constant term of a 'qspray' polynomial
#' @description Get the constant term of a \code{qspray} polynomial.
#'
#' @param qspray a \code{qspray} object
#'
#' @return A \code{bigq} number.
#' @export
setMethod(
  "getConstantTerm", "qspray", 
  function(qspray) {
    getCoefficient(qspray, integer(0L))
  }
)

#' @name isConstant
#' @aliases isConstant,qspray-method 
#' @docType methods
#' @title Whether a 'qspray' polynomial is constant
#' @description Checks whether a \code{qspray} object defines a constant 
#'   polynomial.
#'
#' @param x a \code{qspray} object
#'
#' @return A Boolean value.
#' @export
setMethod(
  "isConstant", "qspray", 
  function(x) {
    numberOfVariables(x) == 0L
  }
)

#' @name isUnivariate
#' @aliases isUnivariate,qspray-method
#' @docType methods
#' @title Whether a 'qspray' is univariate
#' @description Checks whether a \code{qspray} object defines a
#'   univariate polynomial.
#'
#' @param x a \code{qspray} object
#'
#' @return A Boolean value.
#' @export
#' @note It is considered that a constant \code{qspray} is univariate, and 
#'   that the \code{qspray} object \code{y^2+1} where \code{y=qlone(2)} is 
#'   not univariate, although only one variable is present (see the note in 
#'   the documentation of \code{numberOfVariables}).
setMethod(
  "isUnivariate", "qspray",
  function(x) {
    numberOfVariables(x) %in% c(0L, 1L)
  }
)

#' @name isQzero
#' @aliases isQzero,qspray-method 
#' @docType methods
#' @title Whether a 'qspray' polynomial is null
#' @description Checks whether a \code{qspray} object defines the zero 
#'   polynomial.
#'
#' @param qspray a \code{qspray} object
#'
#' @return A Boolean value.
#' @export
setMethod(
  "isQzero", "qspray", 
  function(qspray) {
    length(qspray@coeffs) == 0L
  }
)

#' @name isQone
#' @aliases isQone,qspray-method 
#' @docType methods
#' @title Whether a 'qspray' polynomial is the unit polynomial
#' @description Checks whether a \code{qspray} object defines the unit 
#'   polynomial.
#'
#' @param qspray a \code{qspray} object
#'
#' @return A Boolean value.
#' @export
setMethod(
  "isQone", "qspray", 
  function(qspray) {
    length(qspray@coeffs) == 1L && getConstantTerm(qspray) == 1L
  }
)

#' @title Whether two 'qspray' polynomials are collinear
#' @description Checks whether the polynomials represented by two \code{qspray}
#'   objects are collinear, that is, whether they are equal up to a scalar 
#'   factor.
#'
#' @param qspray1,qspray2 two \code{qspray} objects 
#'
#' @return A Boolean value.
#' @export
#'
#' @examples
#' library(qspray)
#' qspray1 <- qsprayMaker(string = "1/2 x^(1, 1) + 4 x^(0, 2) + 5")
#' qspray2 <- "4/7" * qspray1
#' collinearQsprays(qspray1, qspray2)
collinearQsprays <- function(qspray1, qspray2) {
  if(isQzero(qspray1) && isQzero(qspray2)) {
    return(TRUE)
  }
  if(isQzero(qspray1) && !isQzero(qspray2)) {
    return(FALSE)
  }
  if(!isQzero(qspray1) && isQzero(qspray2)) {
    return(FALSE)
  }
  M1 <- powersMatrix(qspray1)
  M2 <- powersMatrix(qspray2)
  if(nrow(M1) != nrow(M2) || ncol(M1) != ncol(M2)) {
    return(FALSE)
  }
  ordr1 <- lexorder(M1)
  ordr2 <- lexorder(M2)
  powers1 <- M1[ordr1, , drop = FALSE]
  coeffs1 <- as.bigq(qspray1@coeffs[ordr1])
  powers2 <- M2[ordr2, , drop = FALSE]
  coeffs2 <- as.bigq(qspray2@coeffs[ordr2])
  if(any(powers1 != powers2)) {
    return(FALSE)
  }
  r <- coeffs2[1L] / coeffs1[1L]
  all(r * coeffs1 == coeffs2)
}

#' @title Whether a 'qspray' polynomial is homogeneous
#' @description Checks whether the polynomial defined by a \code{qspray} 
#'   object is homogeneous, and also returns the degree if this is true.
#'
#' @param qspray a \code{qspray} object
#'
#' @return A Boolean value indicating whether the polynomial defined by 
#'   \code{qspray} is homogeneous. Moreover, if it is homogeneous, the degree 
#'   is given in the attribute \code{"degree"} of the output.
#' @export
#'
#' @examples
#' lambda <- c(3, 2, 1)
#' p <- PSFpoly(4, lambda)
#' ( homogeneous <- isHomogeneousQspray(p) ) # should be TRUE
#' attr(homogeneous, "degree") == sum(lambda) # should be TRUE
isHomogeneousQspray <- function(qspray) {
  if(isConstant(qspray)) {
    out <- TRUE
    attr(out, "degree") <- 0L
  } else if(getConstantTerm(qspray) != 0L) {
    out <- FALSE
  } else {
    degrees <- vapply(qspray@powers, sum, integer(1L))
    if(all(degrees == degrees[1L])) {
      out <- TRUE
      attr(out, "degree") <- degrees[1L]
    } else {
      out <- FALSE
    }
  }
  out
}