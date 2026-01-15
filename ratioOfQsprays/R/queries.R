#' @include ratioOfQsprays.R
NULL

setGeneric("numberOfVariables")
setGeneric("involvedVariables")
setGeneric("isConstant")
setGeneric("isUnivariate")

#' @name numberOfVariables
#' @aliases numberOfVariables,ratioOfQsprays-method
#' @docType methods
#' @importFrom qspray numberOfVariables
#' @title Number of variables in a 'ratioOfQsprays'
#' @description Number of variables involved in a \code{ratioOfQsprays} object.
#'
#' @param x a \code{ratioOfQsprays} object
#'
#' @return An integer.
#' @seealso \code{\link{involvedVariables}}
#' @export
#' @note The number of variables in the \code{ratioOfQsprays} object
#'   \code{y / (1 + y)} where \code{y=qlone(2)} is \code{2}, not \code{1},
#'   although only one variable occurs. Rigorously speaking, the function
#'   returns the maximal integer \code{d} such that \code{qlone(d)} occurs in
#'   the 'ratioOfQsprays'.
setMethod(
  "numberOfVariables", "ratioOfQsprays",
  function(x) {
    max(numberOfVariables(x@numerator), numberOfVariables(x@denominator))
  }
)

#' @name involvedVariables
#' @aliases involvedVariables,ratioOfQsprays-method
#' @docType methods
#' @importFrom qspray involvedVariables
#' @title Variables involved in a 'ratioOfQsprays'
#' @description Variables involved in a \code{ratioOfQsprays} object.
#'
#' @param x a \code{ratioOfQsprays} object
#'
#' @return A vector of integers. Each integer represents the index of a
#'   variable involved in \code{x}.
#' @export
#' @seealso \code{\link{numberOfVariables}}.
#' @examples
#' x <- qlone(1); z <- qlone(3)
#' rOQ <- 2*x/z + x/(x+z) + z^2/x
#' involvedVariables(rOQ) # should be c(1L, 3L)
setMethod(
  "involvedVariables", "ratioOfQsprays",
  function(x) {
    union(
      involvedVariables(x@numerator), involvedVariables(x@denominator)
    )
  }
)

#' @name isConstant
#' @aliases isConstant,ratioOfQsprays-method
#' @docType methods
#' @importFrom qspray isConstant
#' @title Whether a 'ratioOfQsprays' is constant
#' @description Checks whether a \code{ratioOfQsprays} object defines a constant
#'   fraction of polynomials.
#'
#' @param x a \code{ratioOfQsprays} object
#'
#' @return A Boolean value.
#' @export
setMethod(
  "isConstant", "ratioOfQsprays",
  function(x) {
    numberOfVariables(x) == 0L
  }
)

#' @name isUnivariate
#' @aliases isUnivariate,ratioOfQsprays-method
#' @docType methods
#' @importFrom qspray isUnivariate
#' @title Whether a 'ratioOfQsprays' is univariate
#' @description Checks whether a \code{ratioOfQsprays} object defines a
#'   univariate fraction of polynomials.
#'
#' @param x a \code{ratioOfQsprays} object
#'
#' @note The \code{ratioOfQsprays} object \code{y / (1 + y)} where
#'   \code{y=qlone(2)} is not univariate, although it involves only one
#'   variable. The function returns \code{TRUE} when only \code{qlone(1)}
#'   is involved or when no variable is involved.
#'
#' @return A Boolean value.
#' @export
setMethod(
  "isUnivariate", "ratioOfQsprays",
  function(x) {
    numberOfVariables(x) %in% c(0L, 1L)
  }
)

#' @title Whether a 'ratioOfQsprays' is polynomial
#' @description Checks whether a \code{ratioOfQsprays} actually is polynomial,
#'   that is, whether its denominator is a constant \code{qspray} polynomial
#'   (and then it should be equal to one).
#'
#' @param roq a \code{ratioOfQsprays} object
#'
#' @return A Boolean value.
#' @export
#' @importFrom qspray isConstant
#'
#' @examples
#' x <- qlone(1)
#' y <- qlone(2)
#' roq <- (x^2 - y^2) / (x - y)
#' isPolynomial(roq)
#' roq == x + y
isPolynomial <- function(roq) {
  isConstant(roq@denominator)
}

#' @title Get the numerator of a 'ratioOfQsprays'
#' @description Get the numerator of a \code{ratioOfQsprays} object,
#'   preserving the show options.
#'
#' @param roq a \code{ratioOfQsprays} object
#'
#' @return A \code{qspray} object.
#' @export
getNumerator <- function(roq) {
  showOpts <- attr(roq, "showOpts")
  if(is.null(showOpts)) {
    roq <- setDefaultShowRatioOfQspraysOption(roq)
    showOpts <- attr(roq, "showOpts")
  }
  sROQ <- attr(showOpts, "showRatioOfQsprays")
  showQsprays <- attr(sROQ, "showQsprays")
  if(!is.null(showQsprays)) {
    showNumerator <- function(qspray) {
      showQsprays(qspray, roq@denominator)[1L]
    }
  } else {
    showNumerator <- attr(sROQ, "showQspray")
  }
  attr(showOpts, "showQspray") <- showNumerator
  attr(roq, "showOpts") <- showOpts
  passShowAttributes(roq, roq@numerator)
}

#' @title Get the denominator of a 'ratioOfQsprays'
#' @description Get the denominator of a \code{ratioOfQsprays} object,
#'   preserving the show options.
#'
#' @param roq a \code{ratioOfQsprays} object
#'
#' @return A \code{qspray} object.
#' @export
getDenominator <- function(roq) {
  showOpts <- attr(roq, "showOpts")
  if(is.null(showOpts)) {
    roq <- setDefaultShowRatioOfQspraysOption(roq)
    showOpts <- attr(roq, "showOpts")
  }
  sROQ <- attr(showOpts, "showRatioOfQsprays")
  showQsprays <- attr(sROQ, "showQsprays")
  if(!is.null(showQsprays)) {
    showNumerator <- function(qspray) {
      showQsprays(roq@numerator, qspray)[2L]
    }
  } else {
    showNumerator <- attr(sROQ, "showQspray")
  }
  attr(showOpts, "showQspray") <- showNumerator
  attr(roq, "showOpts") <- showOpts
  passShowAttributes(roq, roq@denominator)
}
