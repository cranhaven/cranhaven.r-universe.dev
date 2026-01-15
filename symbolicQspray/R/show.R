#' @title Print a 'symbolicQspray' object
#' @description Prints a \code{symbolicQspray} object given a function to print
#'   a \code{ratioOfQsprays} object.
#'
#' @param showRatioOfQsprays a function which prints a \code{ratioOfQsprays}
#'   object
#' @param showMonomial a function which prints a monomial, such as
#'  \code{\link[qspray:showMonomialXYZ]{showMonomialXYZ()}} (and not
#'  \code{showMonomialXYZ}!)
#' @param lbrace,rbrace used to enclose the coefficients
#' @param addition used to separate the terms
#' @param multiplication used to separate the coefficient and the monomial
#'   within a term
#'
#' @return A function which prints a \code{symbolicQspray} object.
#' @export
#'
#' @seealso \code{\link{showSymbolicQsprayX1X2X3}},
#'   \code{\link{showSymbolicQsprayXYZ}}.
#'
#' @note The function returned by this function is appropriate for usage in
#'   \code{\link{showSymbolicQsprayOption<-}} as the option
#'   \code{"showSymbolicQspray"} but in general we would rather
#'   use \code{\link{showSymbolicQsprayX1X2X3}} or
#'   \code{\link{showSymbolicQsprayXYZ}}, or rather set the options \code{"a"},
#'   \code{"X"} and \code{"quotientBar"}.
#'
#' @examples
#' set.seed(421)
#' ( Qspray <- rSymbolicQspray() )
#' showRatioOfQsprays <-
#'   showRatioOfQspraysXYZ(c("a", "b", "c"), quotientBar = " / ")
#' showMonomial <- showMonomialX1X2X3("X")
#' f <- showSymbolicQspray(showRatioOfQsprays, showMonomial, "{{{", "}}}")
#' f(Qspray)
#' # setting a show option:
#' showSymbolicQsprayOption(Qspray, "showSymbolicQspray") <- f
#' Qspray
#' # the show options are preserved by certain operations, e.g.:
#' 2*Qspray
showSymbolicQspray <- function(
  showRatioOfQsprays, showMonomial, lbrace = "{ ", rbrace = " }",
  addition = "  +  ", multiplication = " * "
) {
  showMonomials <- attr(showMonomial, "showMonomials") %||%
    function(powers) {
      vapply(powers, showMonomial, character(1L))
    }
  showMultipleRatiosOfQsprays <-
    attr(showRatioOfQsprays, "showMultipleRatiosOfQsprays") %||%
    function(rOQs) {
      vapply(rOQs, showRatioOfQsprays, character(1L))
    }
  f <- function(Qspray) {
    if(isQzero(Qspray)) {
      return("0")
    }
    Qspray <- orderedQspray(Qspray)
    monomials <- showMonomials(Qspray@powers)
    coeffs <- paste0(
      lbrace,
      showMultipleRatiosOfQsprays(Qspray@coeffs),
      rbrace
    )
    nterms <- numberOfTerms(Qspray)
    if(monomials[nterms] == "") {
      toPaste <- c(
        sprintf("%s%s%s", coeffs[-nterms], multiplication, monomials[-nterms]),
        coeffs[nterms]
      )
    } else {
      toPaste <- paste0(coeffs, multiplication, monomials)
    }
    paste0(toPaste, collapse = addition)
  }
  attr(f, "showRatioOfQsprays") <- showRatioOfQsprays
  attr(f, "showMonomial")       <- showMonomial
  inheritable <- NA
  attr(inheritable, "sROQ") <- attr(showRatioOfQsprays, "inheritable")
  attr(inheritable, "sM")   <- attr(showMonomial, "inheritable")
  attr(f, "inheritable") <- inheritable
  f
}

#' @title Print a 'symbolicQspray' object
#' @description Prints a \code{symbolicQspray} object.
#'
#' @param a a string, usually a letter, to denote the non-indexed variables
#'   of the \code{ratioOfQsprays} coefficients
#' @param X a string, usually a letter, to denote the non-indexed variables
#' @param quotientBar a string for the quotient bar between the numerator and
#'   the denominator of a \code{ratioOfQsprays} object, including surrounding
#'   spaces, e.g. \code{"/"}
#' @param ... arguments other than \code{showRatioOfQsprays} and
#'   \code{showMonomial} passed to \code{\link{showSymbolicQspray}}
#'
#' @return A function which prints \code{symbolicQspray} objects.
#' @export
#' @importFrom ratioOfQsprays showRatioOfQspraysX1X2X3
#' @importFrom qspray showMonomialX1X2X3
#'
#' @note This function is built by applying \code{\link{showSymbolicQspray}} to
#'   \code{\link[ratioOfQsprays]{showRatioOfQspraysX1X2X3}(a)} and
#'   \code{\link[qspray]{showMonomialX1X2X3}(X)}.
#'
#' @examples
#' set.seed(421)
#' Qspray <- rSymbolicQspray()
#' showSymbolicQsprayX1X2X3(quotientBar = " / ")(Qspray)
showSymbolicQsprayX1X2X3 <- function(
    a = "a", X = "X", quotientBar = " %//% ", ...
) {
  f <- showSymbolicQspray(
    showRatioOfQspraysX1X2X3(a, quotientBar = quotientBar),
    showMonomialX1X2X3(X), ...
  )
  attr(f, "showRatioOfQsprays") <-
    showRatioOfQspraysX1X2X3(a, quotientBar = quotientBar)
  attr(f, "showMonomial")       <- showMonomialX1X2X3(X)
  attr(f, "inheritable") <- TRUE
  f
}

#' @title Print a 'symbolicQspray' object
#' @description Prints a \code{symbolicQspray} object.
#'
#' @param a a string, usually a letter, to denote the non-indexed variables
#'   of the \code{ratioOfQsprays} coefficients
#' @param letters a vector of strings, usually some letters, to denote the
#'   variables of the polynomial
#' @param quotientBar a string for the quotient bar between the numerator and
#'   the denominator of a \code{ratioOfQsprays} object, including surrounding
#'   spaces, e.g. \code{" / "}
#' @param ... arguments other than \code{showRatioOfQsprays} and
#'   \code{showMonomial} passed to \code{\link{showSymbolicQspray}}
#'
#' @return A function which prints \code{symbolicQspray} objects.
#' @export
#' @importFrom ratioOfQsprays showRatioOfQspraysX1X2X3
#' @importFrom qspray showMonomialXYZ
#'
#' @note This function is built by applying \code{\link{showSymbolicQspray}} to
#'   \code{\link[ratioOfQsprays]{showRatioOfQspraysX1X2X3}(a)} and
#'   \code{\link[qspray]{showMonomialXYZ}(letters)}.
#'
#' @examples
#' set.seed(421)
#' Qspray <- rSymbolicQspray()
#' showSymbolicQsprayX1X2X3(quotientBar = " / ")(Qspray)
showSymbolicQsprayXYZ <- function(
    a = "a", letters = c("X", "Y", "Z"), quotientBar = " %//% ", ...
) {
  showSymbolicQspray(
    showRatioOfQspraysX1X2X3(a = a, quotientBar = quotientBar),
    showMonomialXYZ(letters), ...
  )
}

#' @title Print a 'symbolicQspray' object
#' @description Prints a \code{symbolicQspray} object.
#'
#' @param params vector of strings, usually some letters, to denote the
#'   parameters of the polynomial
#' @param vars a vector of strings, usually some letters, to denote the
#'   variables of the polynomial
#' @param quotientBar a string for the quotient bar between the numerator and
#'   the denominator of a \code{ratioOfQsprays} object, including surrounding
#'   spaces, e.g. \code{" / "}
#' @param ... arguments other than \code{showRatioOfQsprays} and
#'   \code{showMonomial} passed to \code{\link{showSymbolicQspray}}
#'
#' @return A function which prints \code{symbolicQspray} objects.
#' @export
#' @importFrom ratioOfQsprays showRatioOfQspraysXYZ
#' @importFrom qspray showMonomialXYZ
#'
#' @note This function is built by applying \code{\link{showSymbolicQspray}} to
#'   \code{\link[ratioOfQsprays]{showRatioOfQspraysXYZ}(params)} and
#'   \code{\link[qspray]{showMonomialXYZ}(vars)}.
#'
#' @examples
#' set.seed(421)
#' ( Qspray <- rSymbolicQspray() )
#' showSymbolicQsprayABCXYZ(c("a", "b", "c"), c("U", "V"))(Qspray)
showSymbolicQsprayABCXYZ <- function(
    params, vars = c("X", "Y", "Z"), quotientBar = " %//% ", ...
) {
  showSymbolicQspray(
    showRatioOfQspraysXYZ(letters = params, quotientBar = quotientBar),
    showMonomialXYZ(vars), ...
  )
}

#' @title Set a show option to a 'symbolicQspray' object
#' @description Set show option to a \code{symbolicQspray} object
#'
#' @param x a \code{symbolicQspray} object
#' @param which which option to set; this can be \code{"a"}, \code{"X"},
#'   \code{"quotientBar"}, \code{"showMonomial"}, \code{"showRatioOfQsprays"} or
#'   \code{"showSymbolicQspray"}
#' @param value the value for the option
#'
#' @return This returns the updated \code{symbolicQspray}.
#' @export
#' @importFrom ratioOfQsprays showRatioOfQspraysXYZ
#' @importFrom qspray showMonomialXYZ showMonomialX1X2X3
#'
#' @examples
#' set.seed(421)
#' Qspray <- rSymbolicQspray()
#' showSymbolicQsprayOption(Qspray, "a") <- "x"
#' showSymbolicQsprayOption(Qspray, "X") <- "A"
#' showSymbolicQsprayOption(Qspray, "quotientBar") <- " / "
#' Qspray
#' showSymbolicQsprayOption(Qspray, "showRatioOfQsprays") <-
#'   showRatioOfQspraysXYZ()
#' Qspray
`showSymbolicQsprayOption<-` <- function(x, which, value) {
  which <-
    match.arg(
      which,
      c(
        "a", "X", "quotientBar", "showMonomial",
        "showRatioOfQsprays", "showSymbolicQspray", "inheritable"
      )
    )
  showOpts <- attr(x, "showOpts") %||% TRUE
  if(!is.element(which, c("showMonomial", "showRatioOfQsprays"))) {
    attr(showOpts, which) <- value
    if(which == "inheritable") {
      attr(x, "showOpts") <- showOpts
      return(x)
    }
  }
  #
  sSQ  <- attr(showOpts, "showSymbolicQspray")
  #
  sMdefault <- showMonomialXYZ(c("X", "Y", "Z")) # showMonomialX1X2X3("X")
  attr(sMdefault, "showUnivariate") <-
    attr(sMdefault, "showTrivariate") <- showMonomialXYZ(c("X", "Y", "Z"))
  sM <- attr(sSQ, "showMonomial") %||% sMdefault
  #
  sROQdefault <- showRatioOfQspraysXYZ(
    attr(showOpts, "a") %||% "a",
    quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
  )
  attr(sROQdefault, "showUnivariate") <- showRatioOfQspraysXYZ(
    attr(showOpts, "a") %||% "a",
    quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
  )
  sROQ <- attr(sSQ, "showRatioOfQsprays") %||% sROQdefault
  #
  if(which == "a" || which == "quotientBar") {
    sROQ <- sROQdefault
  } else if(which == "showRatioOfQsprays") {
    sROQ <- value
  } else if(which == "X") {
    sM <- showMonomialX1X2X3(value)
    attr(sM, "showUnivariate") <- showMonomialXYZ(value)
  } else if(which == "showMonomial") {
    sM <- value
  }
  #
  if(which != "showSymbolicQspray") {
    sM0 <- sM
    sMU0 <- attr(sM0, "showUnivariate") %||% sM0
    attr(sM0, "showUnivariate") <- sMU0
    sMT0 <- attr(sM0, "showTrivariate") %||% sM0
    attr(sM0, "showTrivariate") <- sMT0
    inheritableM <- isTRUE(attr(sM0, "inheritable"))
    #
    sROQ0 <- sROQ
    sROQU0 <- attr(sROQ0, "showUnivariate") %||% sROQ0
    attr(sROQ0, "showUnivariate") <- sROQU0
    inheritableROQ <- isTRUE(attr(sROQ0, "inheritable"))
    #
    f <- function(Qspray) {
      univariate <- isUnivariate(Qspray)
      trivariate <- numberOfVariables(Qspray) <= 3L
      if(univariate) {
        sM <- sMU0
      } else if(trivariate) {
        sM <- sMT0
      } else {
        sM <- sM0
      }
      univariate <- numberOfParameters(Qspray) == 1L #all(vapply(Qspray@coeffs, isUnivariate, logical(1L)))
      if(univariate) {
        sROQ <- sROQU0
      } else {
        sROQ <- sROQ0
      }
      showSymbolicQspray(sROQ, sM)(Qspray)
    }
    attr(f, "showRatioOfQsprays") <- sROQ0
    attr(f, "showMonomial")       <- sM0
    #
    inheritable <- NA
    attr(inheritable, "sROQ") <- inheritableROQ
    attr(inheritable, "sM")   <- inheritableM
  } else { # which == "showSymbolicQspray"
    f <- value
    attr(f, "showRatioOfQsprays") <- sROQ
    attr(f, "showMonomial")       <- sM
    inheritable <- attr(f, "inheritable")
  }
  attr(showOpts, "showSymbolicQspray") <- f
  attr(showOpts, "inheritable") <- inheritable
  attr(x, "showOpts") <- showOpts
  x
}

setDefaultShowSymbolicQsprayOption <- function(Qspray) {
  showSymbolicQsprayOption(Qspray, "quotientBar") <- " %//% "
  invisible(Qspray)
}

getShowSymbolicQspray <- function(Qspray) {
  showOpts <- attr(Qspray, "showOpts")
  sSQ <- attr(showOpts, "showSymbolicQspray")
  if(is.null(sSQ)) {
    Qspray <- setDefaultShowSymbolicQsprayOption(Qspray)
    sSQ <- attr(attr(Qspray, "showOpts"), "showSymbolicQspray")
  }
  sSQ
}
