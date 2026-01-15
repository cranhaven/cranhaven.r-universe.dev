rationalMonomial <- function(variables, powers){
  if(length(powers) == 0L) return("1")
  factors <- gsub(
    "\\^1( ?)$", "\\1",
    paste0(variables, "^", powers, c(rep(" ", length(powers)-1L), ""))
  )
  paste0(factors, collapse = "")
}

qsprayMonomials <- function(powers, vars) {
  nterms <- length(powers)
  if(!is.null(vars) && length(vars) < max(lengths(powers))) {
    stop("Insufficient length of `vars`.")
  }
  variables <- vector(mode = "list", length = nterms)
  for(i in 1L:nterms){
    pwrs <- powers[[i]]
    zs <- which(pwrs != 0L)
    powers[[i]] <- pwrs[zs]
    if(is.null(vars)) {
      variables[[i]] <- paste0("x", zs)
    } else {
      variables[[i]] <- vars[zs]
    }
  }
  mapply(rationalMonomial, variables, powers, USE.NAMES = FALSE)
}

#' @title Pretty polynomial
#' @description Pretty form of a \code{qspray} polynomial.
#'
#' @param qspray a \code{qspray} object
#' @param vars variable names; \code{NULL} for \code{"x1"}, \code{"x2"}, ...
#'
#' @return A character string.
#' @export
#'
#' @importFrom Ryacas yac_str
#' @examples 
#' library(qspray)
#' P <- (qlone(1) + "1/2"*qlone(2))^2 + 5
#' prettyP <- prettyQspray(P, vars = c("x", "y"))
#' prettyP
#' cat(Ryacas::yac_str(sprintf("PrettyForm(%s)", prettyP)))
#' Ryacas::yac_str(sprintf("TeXForm(%s)", prettyP))
prettyQspray <- function(qspray, vars = NULL) {
  if(qspray == qzero()) {
    return("0")
  }
  monomials <- qsprayMonomials(qspray@powers, vars)
  letter <- setdiff(letters, vars)[1L]
  terms <- paste0(
    gsub(" ", "*", monomials, fixed = TRUE), 
    sprintf(" * %s Where %s==", letter, letter), 
    qspray@coeffs
  )
  x <- yac_str(
    paste0(vapply(terms, yac_str, character(1L)), collapse = " + ")
  )
  sub("^\\(\\( - ", "((-", sub("^\\( - ", "(-", gsub("([-\\+])", " \\1 ", x)))
}

rationalPolynomial <- function(powers, coeffs, stars = FALSE){
  monomials <- qsprayMonomials(powers, NULL)
  spaces <- rep(" ", length(coeffs))
  ones <- coeffs == "1"
  minusones <- coeffs == "-1"
  spaces[ones] <- ""
  coeffs[ones] <- ""
  spaces[minusones] <- ""
  coeffs[minusones] <- "-"
  if(stars){
    out <- gsub("(\\d) x", "\\1 * x",
                gsub("+  -", "-  ",
                     paste0(
                       paste0(coeffs, spaces, monomials), collapse = "  +  "
                     ),
                     fixed = TRUE
                )
    )
    out <- gsub("(\\d) 1", "\\1", out)
  }else{
    out <- gsub("+ -", "- ",
                paste0(
                  paste0(coeffs, spaces, monomials), collapse = " + "
                ),
                fixed = TRUE
    )
  }
  out
}

#' @title Multivariate polynomial as function
#' @description Coerces a \code{qspray} polynomial into a function.
#'
#' @param x object of class \code{qspray}
#' @param N Boolean, whether the function must numerically approximate 
#'   the result
#' @param ... ignored
#'
#' @return A function having the same variables as the polynomial. If 
#'   \code{N=FALSE}, it returns a string. If \code{N=TRUE}, it returns a number 
#'   if the result does not contain any variable, otherwise it returns a 
#'   R expression.
#' @export
#'
#' @importFrom Ryacas yac_str as_r
#'
#' @examples 
#' library(qspray)
#' P <- (qlone(1) + "1/2"*qlone(2))^2 + 5
#' f <- as.function(P)
#' g <- as.function(P, N = TRUE)
#' f(2, "3/7")
#' g(2, "3/7")
#' f("x", "y")
#' g("x", "y")
#' # the evaluation is performed by (R)yacas and complex numbers are
#' # allowed; the imaginary unit is denoted by `I`
#' f("2 + 2*I", "Sqrt(2)")
#' g("2 + 2*I", "Sqrt(2)")
as.function.qspray <- function(x, N = FALSE, ...) {
  string <- rationalPolynomial(x@powers, x@coeffs, stars = TRUE)
  expr <- sprintf("Simplify(%s)", string)
  nvars <- max(lengths(x@powers))
  vars <- paste0("x", seq_len(nvars))
  values <- paste0(paste0(vars, "==%s"), collapse = " And ")
  yacas <- paste0("Simplify(", expr, " Where ", values, ")")
  if(N) {
    yacas <- paste0("N(", yacas, ")")
    f <- function() {
      as_r(yac_str(
        do.call(function(...) sprintf(yacas, ...), lapply(vars, function(xi) {
          eval(parse(text = xi))
        }))
      ))
    }
  } else {
    f <- function() {
      yac_str(
        do.call(function(...) sprintf(yacas, ...), lapply(vars, function(xi) {
          eval(parse(text = xi))
        }))
      )
    }
  }
  formals(f) <- vapply(vars, function(xi) {
    `names<-`(alist(y=), xi)
  }, alist(NULL))
  f
}
