#' @title Print a 'qspray' object
#' @description Prints a \code{qspray} object given a function which prints 
#'   the monomials.
#'
#' @param showMonomial a function which takes as argument a sequence of 
#'   exponents and which returns a string representing the corresponding 
#'   monomial
#' @param compact a Boolean value; if \code{TRUE}, then the \code{+} sign 
#'   and the \code{-} sign will not be surrounded by spaces
#' @param multiplication used to separate the coefficient and the monomial 
#'   within a term
#'
#' @return A function which prints a \code{qspray} object.
#' @export
#' @importFrom gmp as.bigq
#' 
#' @seealso \code{\link{showQsprayX1X2X3}}, \code{\link{showQsprayXYZ}}, 
#'   \code{\link{showQsprayOption<-}}.
#'   
#' @note The function returned by this function can be used as the option 
#'   \code{"showQspray"} in the \code{\link{showQsprayOption<-}} function.
#'   But one generally prefers to use \code{\link{showQsprayX1X2X3}} or 
#'   \code{\link{showQsprayXYZ}} instead, which are both built with 
#'   \code{showQspray}.
#'   
#' @examples
#' set.seed(3141)
#' ( qspray <- rQspray() )
#' f <- showQspray(showMonomialX1X2X3("X"), compact = TRUE)
#' f(qspray)
#' # this is equivalent to:
#' f <- showQsprayX1X2X3("X", compact = TRUE)
#' f(qspray)
#' # if you want to adopt this way to show a qspray, use 
#' # the setter function \code{\link{showQsprayOption<-}}: 
#' showQsprayOption(qspray, "showQspray") <- 
#'   showQsprayX1X2X3("X", compact = TRUE)
#' qspray
#' # then this show option will be preserved by some operations on the qspray:
#' qspray^2
showQspray <- function(
    showMonomial, compact = FALSE, multiplication = "*"
) {
  showMonomials <- attr(showMonomial, "showMonomials") %||%
    function(powers) {
      vapply(powers, showMonomial, character(1L))
    }
  h <- function(qspray, monomials) {
    if(isQzero(qspray)) {
      return("0")
    }
    nterms <- numberOfTerms(qspray)
    constantTerm <- getConstantTerm(qspray)
    coeffs <- as.bigq(qspray@coeffs)
    plus <- vapply(coeffs, function(x) x >= 0L, FUN.VALUE = logical(1L))
    plusSign  <- ifelse(compact, "+", " + ")
    minusSign <- ifelse(compact, "-", " - ")
    signs <- c(ifelse(plus[-1L], plusSign, minusSign), "")
    abscoeffs <- as.character(abs(coeffs))
    terms <- paste0(
      ifelse(abscoeffs == "1", "", paste0(abscoeffs, multiplication)), monomials
    )
    if(constantTerm != 0L) {
      terms[nterms] <- as.character(abs(constantTerm))
    }
    leader <- if(plus[1L]) "" else "-"
    paste0(c(leader, c(rbind(terms, signs))), collapse = "")
  }
  g <- function(qspray1, qspray2) {
    qspray1 <- orderedQspray(qspray1)
    qspray2 <- orderedQspray(qspray2)
    powers1 <- qspray1@powers
    powers2 <- qspray2@powers
    monomials <- showMonomials(c(powers1, powers2))
    monomials1 <- monomials[seq_along(powers1)]
    monomials2 <- monomials[length(powers1) + seq_along(powers2)]
    c(h(qspray1, monomials1), h(qspray2, monomials2))
  }
  f <- function(qspray) {
    qspray <- orderedQspray(qspray)
    h(qspray, showMonomials(qspray@powers))
  }
  attr(f, "showQsprays") <- g
  attr(f, "inheritable") <- attr(showMonomial, "inheritable")
  f
}

#' @title Print a monomial
#' @description Prints a monomial in the style of \code{"x1.x3^2"}.
#'
#' @param x a string, usually a letter such as \code{"x"} or \code{"X"}, to 
#'   denote the non-indexed variables
#' @param collapse a string to denote the symbol representing the 
#'   multiplication, e.g. \code{"*"} or \code{""}
#'
#' @return A function which takes as argument a sequence of exponents and 
#'   which prints the corresponding monomial.
#' @export
#'
#' @seealso \code{\link{showQsprayX1X2X3}}, 
#'   \code{\link{showMonomialXYZ}}, \code{\link{showQsprayOption<-}}. 
#' 
#' @note The function returned by this function can be used as the option 
#'   \code{"showMonomial"} in the \code{\link{showQsprayOption<-}} function.
#'   But if you are happy with the default \code{collapse} argument, then you 
#'   can equivalently set the \code{"x"} option instead, thereby typing less 
#'   code. See the example. 
#' 
#' @examples
#' showMonomialX1X2X3("X")(c(1, 0, 2))
#' showMonomialX1X2X3("X", collapse = "*")(c(1, 0, 2))
#' showMonomialX1X2X3("X")(c(1, 0, 2)) == 
#'   showMonomialXYZ(c("X1", "X2", "X3"))(c(1, 0, 2))
#' showMonomialX1X2X3()(NULL)
#' # setting a show option:
#' set.seed(3141)
#' ( qspray <- rQspray() )
#' showQsprayOption(qspray, "showMonomial") <- showMonomialX1X2X3("X")
#' qspray
#' # this is equivalent to:
#' showQsprayOption(qspray, "showQspray") <- showQsprayX1X2X3("X")
#' # and also equivalent to:
#' showQsprayOption(qspray, "x") <- "X" 
showMonomialX1X2X3 <- function(x = "x", collapse = ".") {
  f <- function(exponents) {
    paste0(vapply(which(exponents != 0L), function(i) {
      e <- exponents[i]
      if(e == 1L) {
        sprintf("%s%s", x, i)
      } else {
        sprintf("%s%s^%d", x, i, e)
      }
    }, character(1L)), collapse = collapse)
  }
  attr(f, "inheritable") <- TRUE
  f
}

#' @title Print a monomial
#' @description Prints a monomial like \code{"x.z^2"} if possible (see details).
#'
#' @param letters a vector of strings, usually some letters such as \code{"x"} 
#'   and \code{"y"}, to denote the variables
#' @param collapse a string to denote the symbol representing the 
#'   multiplication, e.g. \code{"*"} or \code{"."}
#'
#' @return A function which takes as argument a sequence of exponents and 
#'   which prints the corresponding monomial.
#' @export
#' 
#' @details If the function returned by this function is applied to a vector 
#'   of exponents whose length is higher than the length of the \code{letters} 
#'   vector, then \code{\link{showMonomialX1X2X3}(x=letters[1])} is applied 
#'   (see the last example).
#' 
#' @seealso \code{\link{showQsprayXYZ}}, 
#'   \code{\link{showMonomialX1X2X3}}, \code{\link{showQsprayOption<-}}. 
#' 
#' @note The function returned by this function can be used as the option 
#'   \code{"showMonomial"} in the \code{\link{showQsprayOption<-}} function.
#' 
#' @examples
#' showMonomialXYZ()(c(1, 0, 2))
#' showMonomialXYZ(collapse = "*")(c(1, 0, 2))
#' showMonomialXYZ()(NULL)
#' # what happens if there are more exponents than letters:
#' showMonomialXYZ(c("a", "b"), collapse = "*")(c(1, 2, 3))
#' # same as:
#' showMonomialX1X2X3("a", collapse = "*")(c(1, 2, 3))
#' # setting a show option:
#' set.seed(3141)
#' ( qspray <- rQspray() )
#' showQsprayOption(qspray, "showMonomial") <- showMonomialXYZ(c("A", "B", "C"))
#' qspray
#' # this is equivalent to:
#' showQsprayOption(qspray, "showQspray") <- showQsprayXYZ(c("A", "B", "C"))
showMonomialXYZ <- function(letters = c("x", "y", "z"), collapse = ".") {
  primary <- function(exponents) {
    paste0(vapply(which(exponents != 0L), function(i) {
      e <- exponents[i]
      letter <- letters[i]
      if(e == 1L) {
        letter
      } else {
        sprintf("%s^%d", letter, e)
      }
    }, character(1L)), collapse = collapse)
  }
  secondary <- showMonomialX1X2X3(x = letters[1L], collapse = collapse)
  # condition <- function(exponents, n) {
  #   length(exponents) <= n
  # }
  # f <- function(exponents, n = length(letters)) {
  #   if(condition(exponents, n)) primary(exponents) else secondary(exponents)  
  # }
  # F <- function(powers, n = length(letters)) {
  #   check <- all(vapply(powers, condition, logical(1L), n = n))
  #   if(check) {
  #     vapply(powers, primary, character(1L))
  #   } else {
  #     vapply(powers, secondary, character(1L))
  #   }
  # }
  condition <- function(exponents) {
    length(exponents) <= length(letters)
  }
  f <- function(exponents) {
    if(condition(exponents)) primary(exponents) else secondary(exponents)  
  }
  showMonomials <- function(powers) {
    check <- all(vapply(powers, condition, logical(1L)))
    if(check) {
      vapply(powers, primary, character(1L))
    } else {
      vapply(powers, secondary, character(1L))
    }
  }
  attr(f, "showMonomials") <- showMonomials
  attr(f, "inheritable")   <- TRUE
  f
}

#' @title Print a polynomial
#' @description Prints a polynomial by printing monomials like \code{"x^2.yz"}.
#'
#' @param letters,collapse see \code{\link{showMonomialXYZ}}
#' @param ... arguments passed to \code{\link{showQspray}}, such as 
#'   \code{compact=TRUE}
#'
#' @return A function which prints a \code{qspray} object. It is constructed 
#'   with \code{\link{showQspray}} and \code{\link{showMonomialXYZ}}.
#' @export
#' 
#' 
#' @note The function returned by this function can be used as the option 
#'   \code{"showQspray"} in the \code{\link{showQsprayOption<-}} function.
#' 
#' @seealso \code{\link{showMonomialXYZ}}, \code{\link{showQspray}}, 
#'   \code{\link{showQsprayOption<-}}.
#' 
#' @examples
#' set.seed(3141)
#' ( qspray <- rQspray() )
#' showQsprayXYZ(c("X", "Y", "Z"))(qspray)
#' showQsprayXYZ(c("X", "Y", "Z"))(qlone(1) + qlone(2) + qlone(3) + qlone(4))
#' # setting a show option:
#' showQsprayOption(qspray, "showQspray") <- showQsprayXYZ(c("A", "B", "C"))
#' qspray
#' # this is equivalent to:
#' showQsprayOption(qspray, "showMonomial") <- showMonomialXYZ(c("A", "B", "C"))
showQsprayXYZ <- function(letters = c("x", "y", "z"), collapse = ".", ...) {
  showQspray(showMonomialXYZ(letters, collapse), ...)
}

#' @title Print a monomial
#' @description Prints a monomial like \code{"x^(1, 0, 2)"}. This way of 
#'   showing a monomial was used by default in previous versions of this 
#'   package.
#'
#' @param x a string, usually a letter such as \code{"x"} or \code{"X"}, to 
#'   denote the variable
#'
#' @return A function which takes as argument a sequence of exponents and 
#'   which prints the corresponding monomial.
#' @export
#'
#' @seealso \code{\link{showMonomialX1X2X3}}, \code{\link{showMonomialXYZ}}, 
#'   \code{\link{showQspray}}, \code{\link{showQsprayOption<-}}. 
#'
#' @examples
#' showMonomialOld("X")(c(1, 0, 2))
#' showMonomialOld("X")(NULL)
showMonomialOld <- function(x = "x") {
  f <- function(exponents) {
    paste0(sprintf("%s^(", x), toString(exponents), ")") 
  }
  attr(f, "inheritable") <- TRUE
  f
}

#' @title Print a 'qspray' object
#' @description Prints a \code{qspray} object given a string for the variable.
#'
#' @param x,collapse see \code{\link{showMonomialX1X2X3}}
#' @param ... arguments passed to \code{\link{showQspray}}, such as 
#'   \code{compact=TRUE}
#'
#' @return A function which prints a \code{qspray} object.
#' @export
#' 
#' @seealso \code{\link{showMonomialX1X2X3}}, \code{\link{showQspray}}, 
#'   \code{\link{showQsprayOption<-}}.
#'   
#' @note The way \code{qspray} objects are displayed can be controlled with the 
#'  help of the function \code{\link{showQsprayOption<-}}, and 
#'  \code{showQsprayX1X2X3()} is a possible option to pass in 
#'  \code{\link{showQsprayOption<-}}.
#'
#' @examples
#' set.seed(3141)
#' ( qspray <- rQspray() )
#' showQsprayX1X2X3("X")(qspray)
#' # setting a show option:
#' showQsprayOption(qspray, "showQspray") <- showQsprayX1X2X3("A")
#' qspray
#' # this is equivalent to:
#' showQsprayOption(qspray, "showMonomial") <- showMonomialX1X2X3("A")
#' # and also equivalent to:
#' showQsprayOption(qspray, "x") <- "A"
showQsprayX1X2X3 <- function(x = "x", collapse = ".", ...) {
  f <- showQspray(showMonomialX1X2X3(x, collapse), ...)
  attr(f, "inheritable") <- TRUE
  f
}

#' @title Set a show option to a 'qspray' object
#' @description Set a show option to a \code{qspray} object
#' 
#' @param x a \code{qspray} object
#' @param which which option to set; this can be \code{"x"}, 
#'   \code{"showMonomial"}, or \code{"showQspray"}
#' @param value the value of the option
#'
#' @return This returns the updated \code{qspray}.
#' @export
#' 
#' @note The interest of setting some show options to a 'qspray' is that these 
#'   options are preserved by some operations. See the examples and the README.
#'
#' @examples
#' set.seed(3141)
#' ( qspray <- rQspray() )
#' showQsprayOption(qspray, "x") <- "a"
#' qspray
#' # this is identical to:
#' showQsprayOption(qspray, "showMonomial") <- showMonomialX1X2X3("a")
#' # and also identical to:
#' showQsprayOption(qspray, "showQspray") <- showQsprayX1X2X3("a")
#' # old show method:
#' showQsprayOption(qspray, "showMonomial") <- showMonomialOld()
#' qspray
#' # show options are preserved by some operations:
#' qspray^2
#' 3*qspray
#' derivQspray(qspray, 1)
#' swapVariables(qspray, 1, 2)
#' substituteQspray(qspray, c(NA, NA, "3/2"))
#' # for the binary arithmetic operations, the show options of the first 
#' # operand are transferred to the result when possible:
#' ( qspray2 <- rQspray() )
#' qspray + qspray2 
`showQsprayOption<-` <- function(x, which, value) {
  which <- match.arg(which, c("x", "showMonomial", "showQspray", "inheritable"))
  showOpts <- attr(x, "showOpts") %||% TRUE
  attr(showOpts, which) <- value
  if(which == "inheritable") {
    attr(x, "showOpts") <- showOpts
    return(x)
  }
  if(which != "showQspray") {
    if(which == "x") {
      sM <- showMonomialXYZ(letters = value)
      attr(showOpts, "showMonomial") <- sM
      sQ <- showQspray(showMonomial = sM)
    } else if(which == "showMonomial") {
      sQ <- showQspray(showMonomial = value)
    } 
  } else {
    sQ <- value
  }
  attr(showOpts, "inheritable") <- isTRUE(attr(sQ, "inheritable"))
  attr(showOpts, "showQspray") <- sQ
  attr(x, "showOpts") <- showOpts
  x
}

setDefaultShowQsprayOption <- function(qspray) {
  showQsprayOption(qspray, "showMonomial") <- showMonomialXYZ()
  invisible(qspray)
}

getShowQspray <- function(qspray) {
  showOpts <- attr(qspray, "showOpts")
  sQ <- attr(showOpts, "showQspray") 
  if(is.null(sQ)) {
    sQ <- attr(attr(showOpts, "showSymbolicQspray"), "showQspray")
    if(is.null(sQ)) {
      qspray <- setDefaultShowQsprayOption(qspray)
      sQ <- attr(attr(qspray, "showOpts"), "showQspray")
    } else {
      showQsprayOption(qspray, "showQspray") <- sQ
    }
  } 
  sQ
}
