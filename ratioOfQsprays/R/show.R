#' @title Print a 'ratioOfQsprays' object
#' @description Prints a \code{ratioOfQsprays} object given a function to print
#'   a \code{qspray} object
#'
#' @param showQspray a function which prints a \code{qspray} object, which will
#'   be applied to the numerator and the denominator
#' @param quotientBar a string representing the quotient bar between the
#'   numerator and the denominator, including surrounding spaces,
#'   e.g \code{" / "}
#' @param lbracket,rbracket used to enclose the numerator and the denominator
#'
#' @return A function which takes as argument a \code{ratioOfQsprays} object
#'   and which prints it.
#' @export
#' @importFrom qspray isQone
#'
#' @seealso \code{\link{showRatioOfQspraysX1X2X3}},
#'   \code{\link{showRatioOfQspraysXYZ}},
#'   \code{\link{showRatioOfQspraysOption<-}},
#'   \code{\link[qspray]{showQspray}}.
#'
#' @note The function returned by this function can be used as the option
#'   \code{"showRatioOfQsprays"} of the setter function
#'   \code{\link{showRatioOfQspraysOption<-}}. That said, one would more often
#'   uses \code{\link{showRatioOfQspraysX1X2X3}} or
#'   \code{\link{showRatioOfQspraysXYZ}} for this option, which are both built
#'   with \code{showRatioOfQsprays}.
#'
#' @examples
#' set.seed(666)
#' ( roq <- rRatioOfQsprays() )
#' f <- showRatioOfQsprays(showQsprayX1X2X3("a"), " / ", "[[ ", " ]]")
#' f(roq)
#' # this is equivalent to
#' f <- showRatioOfQspraysX1X2X3("a", " / ", lbracket = "[[ ", rbracket = " ]]")
#' f(roq)
showRatioOfQsprays <- function(
    showQspray, quotientBar = "  %//%  ", lbracket = "[ ", rbracket = " ]"
) {
  showQsprays <- attr(showQspray, "showQsprays") %||%
    function(qspray1, qspray2) {
      c(showQspray(qspray1), showQspray(qspray2))
    }
  f <- function(roq) {
    enclose <- function(x) {
      sprintf(
        "%s%s%s", lbracket, x, rbracket
      )
    }
    if(isQone(roq@denominator)) {
      enclose(showQspray(roq@numerator))
    } else {
      strings <- showQsprays(roq@numerator, roq@denominator)
      sprintf(
        "%s%s%s",
        enclose(strings[1L]),
        quotientBar,
        enclose(strings[2L])
      )
    }
  }
  attr(f, "showQspray")  <- showQspray
  attr(f, "showQsprays") <- showQsprays
  attr(f, "inheritable") <- attr(showQspray, "inheritable")
  f
}

#' @title Print a 'ratioOfQsprays'
#' @description Print a \code{ratioOfQsprays} object given a string to denote
#'   the non-indexed variables.
#'
#' @param var a string, usually a letter, to denote the non-indexed variables
#' @param quotientBar a string representing the quotient bar between the
#'   numerator and the denominator, including surrounding spaces,
#'   e.g \code{" / "}
#' @param ... arguments other than \code{quotientBar} passed to
#'   \code{\link{showRatioOfQsprays}}
#'
#' @return A function which takes as argument a \code{ratioOfQsprays} object
#'   and which prints it.
#' @export
#' @importFrom qspray showQsprayX1X2X3
#'
#' @seealso \code{\link{showRatioOfQspraysXYZ}},
#'   \code{\link{showRatioOfQspraysOption<-}}.
#'
#' @note The function returned by this function can be used as the option
#'   \code{"showRatioOfQsprays"} of the setter function
#'   \code{\link{showRatioOfQspraysOption<-}}. If you do not use the
#'   ellipsis arguments, this is equivalent to set the \code{"x"}
#'   option and the \code{"quotientBar"} option (see example).
#'
#' @examples
#' set.seed(666)
#' ( roq <- rRatioOfQsprays() )
#' showRatioOfQspraysX1X2X3("X", " / ")(roq)
#' # setting a show option:
#' showRatioOfQspraysOption(roq, "showRatioOfQsprays") <-
#'   showRatioOfQspraysX1X2X3("X", " / ")
#' roq
#' # this is equivalent to set the "x" and "quotientBar" options:
#' showRatioOfQspraysOption(roq, "x") <- "X"
#' showRatioOfQspraysOption(roq, "quotientBar") <- " / "
showRatioOfQspraysX1X2X3 <- function(var, quotientBar = "  %//%  ", ...) {
  showRatioOfQsprays(showQsprayX1X2X3(var), quotientBar = quotientBar, ...)
}

#' @title Print a 'ratioOfQsprays'
#' @description Print a \code{ratioOfQsprays} object given some letters to
#'   denote the variables, by printing monomials in the style of
#'   \code{"x^2.yz"}.
#'
#' @param letters a vector of strings, usually some letters such as \code{"x"}
#'   and \code{"y"}, to denote the variables
#' @param quotientBar a string representing the quotient bar between the
#'   numerator and the denominator, including surrounding spaces,
#'   e.g \code{" / "}
#' @param ... arguments other than \code{quotientBar} passed to
#'   \code{\link{showRatioOfQsprays}}
#'
#' @return A function which takes as argument a \code{ratioOfQsprays} object
#'   and which prints it.
#' @export
#' @importFrom qspray showQsprayXYZ
#'
#' @note The function returned by this function can be used as the option
#'   \code{"showRatioOfQsprays"} of the setter function
#'   \code{\link{showRatioOfQspraysOption<-}}.
#'   As another note, let us describe the behavior of this function in a
#'   case when the number of variables of the \code{ratioOfQsprays} object to
#'   be printed is bigger than the number of provided letters. In such a case,
#'   the output will be the same as an application of the function
#'   \code{showRatioOfQspraysX1X2X3(x)} with \code{x} being the first letter
#'   provided. See the example.
#'
#' @seealso \code{\link{showRatioOfQspraysX1X2X3}},
#'   \code{\link{showRatioOfQspraysOption<-}}.
#'
#' @examples
#' set.seed(666)
#' ( roq <- rRatioOfQsprays() )
#' showRatioOfQspraysXYZ(c("X", "Y", "Z"), " / ")(roq)
#' # now take a ratioOfQsprays with four variables:
#' roq <- roq * qlone(4)
#' # then the symbols X1, X2, X3, X4 denote the variables now:
#' showRatioOfQspraysXYZ(c("X", "Y", "Z"), " / ")(roq)
#' # this is the method used by default to print the ratioOfQsprays objects,
#' # with the initial letters x, y, z which then become x1, x2, x3, x4:
#' roq
showRatioOfQspraysXYZ <- function(
  letters = c("x", "y", "z"), quotientBar = "  %//%  ", ...
) {
  f <-
    showRatioOfQsprays(showQsprayXYZ(letters), quotientBar = quotientBar, ...)
  attr(f, "showMultipleRatiosOfQsprays") <- function(rOQs) {
    n <- max(vapply(rOQs, numberOfVariables, integer(1L)))
    if(length(letters) >= n) {
      showQspray <- showQsprayXYZ(letters)
    } else {
      showQspray <- showQsprayX1X2X3(letters[1L])
    }
    sROQ <- showRatioOfQsprays(showQspray, quotientBar = quotientBar, ...)
    vapply(rOQs, sROQ, character(1L))
  }
  f
}

#' @title Set a show option to a 'ratioOfQsprays'
#' @description Set a show option to a \code{ratioOfQsprays} object.
#'
#' @param x a \code{ratioOfQsprays} object
#' @param which which option to set; this can be \code{"x"},
#'   \code{"quotientBar"}, \code{"showQspray"}, or \code{"showRatioOfQsprays"}
#' @param value the value of the option to be set
#'
#' @return This returns the updated \code{ratioOfQsprays}.
#' @export
#' @importFrom qspray showQspray showQsprayXYZ
#'
#' @seealso \code{\link{showRatioOfQsprays}}.
#'
#' @examples
#' set.seed(666)
#' ( roq <- rRatioOfQsprays() )
#' showRatioOfQspraysOption(roq, "quotientBar") <- " / "
#' roq
#' showRatioOfQspraysOption(roq, "x") <- "a"
#' roq
#' showRatioOfQspraysOption(roq, "showQspray") <- showQsprayXYZ()
#' roq
`showRatioOfQspraysOption<-` <- function(x, which, value) {
  which <-
    match.arg(which, c("x", "quotientBar", "showQspray", "showRatioOfQsprays"))
  showOpts <- attr(x, "showOpts") %||% TRUE
  attr(showOpts, which) <- value
  if(which != "showRatioOfQsprays") {
    if(which == "x") {
      sQ <- showQsprayXYZ(letters = value)
      sROQ <- showRatioOfQsprays(
        showQspray = sQ,
        quotientBar = attr(showOpts, "quotientBar") %||% "  %//%  "
      )
    } else if(which == "quotientBar") {
      sQ <- attr(showOpts, "showQspray") %||% showQsprayXYZ()
      sROQ <- showRatioOfQsprays(
        showQspray  = sQ,
        quotientBar = value
      )
    } else if(which == "showQspray") {
      sQ <- value
      sROQ <- showRatioOfQsprays(
        showQspray = sQ,
        quotientBar = attr(showOpts, "quotientBar") %||% "  %//%  "
      )
    }
  } else { # which == "showRatioOfQsprays"
    sQ   <- attr(value, "showQspray")
    sROQ <- value
  }
  attr(showOpts, "inheritable") <- attr(sROQ, "inheritable")
  attr(showOpts, "showQspray") <- sQ
  attr(showOpts, "showRatioOfQsprays") <- sROQ
  attr(x, "showOpts") <- showOpts
  x
}

setDefaultShowRatioOfQspraysOption <- function(roq) {
  showRatioOfQspraysOption(roq, "showRatioOfQsprays") <- showRatioOfQspraysXYZ()
  invisible(roq)
}

getShowRatioOfQsprays <- function(roq) {
  showOpts <- attr(roq, "showOpts")
  sROQ <- attr(showOpts, "showRatioOfQsprays")
  if(is.null(sROQ)) {
    # it's possible that showOpts has a "showQspray" attribute, from a call
    # to as.RatioOfQsprays
    sQ <- attr(showOpts, "showQspray")
    if(is.null(sQ)) {
      roq <- setDefaultShowRatioOfQspraysOption(roq)
    } else {
      showRatioOfQspraysOption(roq, "showQspray") <- sQ
    }
  }
  attr(attr(roq, "showOpts"), "showRatioOfQsprays")
}
