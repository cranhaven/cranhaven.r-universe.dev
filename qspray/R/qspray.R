#' @useDynLib qspray, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom methods setMethod setClass new canCoerce as setGeneric
#' @importFrom gmp as.bigq factorialZ asNumeric
#' @importFrom purrr transpose
#' @importFrom utils globalVariables
#' @include qspray.R
NULL

if(getRversion() >= "2.15.1") {
  globalVariables("showSymbolicQsprayOption<-")
}

setClass(
  "qspray",
  slots = c(powers = "list", coeffs = "character")
)

setMethod(
  "show", "qspray", 
  function(object) {
    f <- getShowQspray(object)
    cat(f(object), "\n")
  }
)


as.qspray.character <- function(x) {
  stopifnot(isFraction(x))
  if(as.bigq(x) == 0L) {
    new("qspray", powers = list(), coeffs = character(0L))
  } else {
    new("qspray", powers = list(integer(0L)), coeffs = x)
  }
}

as_qspray_gmp <- function(x) {
  if(x == 0L) {
    new("qspray", powers = list(), coeffs = character(0L))
  } else {
    new("qspray", powers = list(integer(0L)), coeffs = as.character(x))
  }
}

as.qspray.numeric <- function(x) {
  stopifnot(isInteger(x))
  if(x == 0L) {
    new("qspray", powers = list(), coeffs = character(0L))
  } else {
    new("qspray", powers = list(integer(0L)), coeffs = as.character(x))
  }
}

setGeneric(
  "as.qspray", function(x) {
    stop(
      "No available application of `as.qspray` for this object."
    )
  }
)

#' @name as.qspray
#' @aliases as.qspray,character-method as.qspray,qspray-method as.qspray,numeric-method as.qspray,bigz-method as.qspray,bigq-method
#' @exportMethod as.qspray
#' @docType methods
#' @title Coercion to a 'qspray' object
#'
#' @param x a \code{qspray} object or an object yielding a quoted integer or a 
#'   quoted fraction after an application of \code{as.character}, e.g. a 
#'   \code{bigq} number
#'
#' @return A \code{qspray} object.
#' @export
#'
#' @examples
#' as.qspray(2)
#' as.qspray("1/3")
setMethod(
  "as.qspray", "character",
  function(x) {
    as.qspray.character(x)
  }
)
#' @rdname as.qspray
setMethod(
  "as.qspray", "qspray",
  function(x) {
    x
  }
)
#' @rdname as.qspray
setMethod(
  "as.qspray", "numeric",
  function(x) {
    as.qspray.numeric(x)
  }
)
#' @rdname as.qspray
setMethod(
  "as.qspray", "bigz",
  function(x) {
    as_qspray_gmp(x)
  }
)
#' @rdname as.qspray
setMethod(
  "as.qspray", "bigq",
  function(x) {
    as_qspray_gmp(x)
  }
)

#' @name qspray-unary
#' @title Unary operators for qspray objects
#' @description Unary operators for qspray objects.
#' @aliases +,qspray,missing-method -,qspray,missing-method
#' @param e1 object of class \code{qspray}
#' @param e2 nothing
#' @return A \code{qspray} object.
setMethod(
  "+", 
  signature(e1 = "qspray", e2 = "missing"), 
  function(e1, e2) e1
)
#' @rdname qspray-unary
setMethod(
  "-", 
  signature(e1 = "qspray", e2 = "missing"), 
  function(e1, e2) {
    qspray <- new(
      "qspray", 
      powers = e1@powers, coeffs = as.character(-as.bigq(e1@coeffs))
    )
    passShowAttributes(e1, qspray)
  }
)


qspray_arith_character <- function(e1, e2) {
  qspray <- switch(
    .Generic,
    "+" = e1 + as.qspray.character(e2),
    "-" = e1 - as.qspray.character(e2),
    "*" = e1 * as.qspray.character(e2),
    "/" = e1 * as.qspray.character(paste0("1/", e2)),
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e1, qspray)
}

qspray_arith_qspray <- function(e1, e2) {
  x <- switch(
    .Generic,
    "+" = qspray_from_list(
      qspray_add(e1@powers, e1@coeffs, e2@powers, e2@coeffs)
    ),
    "-" = qspray_from_list(
      qspray_subtract(e1@powers, e1@coeffs, e2@powers, e2@coeffs)
    ),
    "*" = qspray_from_list(
      qspray_mult(e1@powers, e1@coeffs, e2@powers, e2@coeffs)
    ),
    "/" = {
      if(canCoerce(e1, "ratioOfQsprays")) {
        as(e1, "ratioOfQsprays") / as(e2, "ratioOfQsprays") 
      } else {
        stop(
          "Division of 'qspray' objects is possible only with the ",
          "'ratioOfQsprays' package, and this package is not loaded."
        )
      }
    },
    stop(gettextf(
      "Binary operator %s not defined for qspray objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e1, x)
}

qsprayPower <- function(e1, n) {
  stopifnot(isPositiveInteger(n))
  qspray_power(e1@powers, e1@coeffs, n)
}

qspray_arith_gmp <- function(e1, e2) {
  qspray <- switch(
    .Generic,
    "+" = e1 + as_qspray_gmp(e2),
    "-" = e1 - as_qspray_gmp(e2),
    "*" = e1 * as_qspray_gmp(e2),
    "/" = e1 * as_qspray_gmp(1L/e2),
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e1, qspray)
  qspray
}

qspray_arith_numeric <- function(e1, e2) {
  qspray <- switch(
    .Generic,
    "+" = e1 + as.qspray.numeric(e2),
    "-" = e1 - as.qspray.numeric(e2),
    "*" = e1 * as.qspray.numeric(e2),
    "/" = e1 / as.character(e2),
    "^" = qspray_from_list(qsprayPower(e1, e2)),
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e1, qspray)
}

character_arith_qspray <- function(e1, e2) {
  qspray <- switch(
    .Generic,
    "+" = as.qspray.character(e1) + e2,
    "-" = as.qspray.character(e1) - e2,
    "*" = as.qspray.character(e1) * e2,
    "/" = {
      if(canCoerce(e2, "ratioOfQsprays")) {
        e1 / as(e2, "ratioOfQsprays") 
      } else {
        stop(
          "Division by a 'qspray' object is possible only with the ",
          "'ratioOfQsprays' package, and this package is not loaded."
        )
      }
    },
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e2, qspray)
}

gmp_arith_qspray <- function(e1, e2) {
  qspray <- switch(
    .Generic,
    "+" = as_qspray_gmp(e1) + e2,
    "-" = as_qspray_gmp(e1) - e2,
    "*" = as_qspray_gmp(e1) * e2,
    "/" = {
      if(canCoerce(e2, "ratioOfQsprays")) {
        e1 / as(e2, "ratioOfQsprays") 
      } else {
        stop(
          "Division by a 'qspray' object is possible only with the ",
          "'ratioOfQsprays' package, and this package is not loaded."
        )
      }
    },
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e2, qspray)
}

numeric_arith_qspray <- function(e1, e2) {
  qspray <- switch(
    .Generic,
    "+" = as.qspray.numeric(e1) + e2,
    "-" = as.qspray.numeric(e1) - e2,
    "*" = as.qspray.numeric(e1) * e2,
    "/" = {
      if(canCoerce(e2, "ratioOfQsprays")) {
        e1 / as(e2, "ratioOfQsprays") 
      } else {
        stop(
          "Division by a 'qspray' object is possible only with the ",
          "'ratioOfQsprays' package, and this package is not loaded."
        )
      }
    },
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e2, qspray)
}

setMethod(
  "Arith", 
  signature(e1 = "qspray", e2 = "qspray"), 
  qspray_arith_qspray
)
setMethod(
  "Arith", 
  signature(e1 = "qspray", e2 = "character"), 
  qspray_arith_character
)
setMethod(
  "Arith", 
  signature(e1 = "qspray", e2 = "bigq"), 
  qspray_arith_gmp
)
setMethod(
  "Arith", 
  signature(e1 = "qspray", e2 = "bigz"), 
  qspray_arith_gmp
)
setMethod(
  "Arith", 
  signature(e1 = "character", e2 = "qspray"), 
  character_arith_qspray
)
setMethod(
  "Arith", 
  signature(e1 = "bigq", e2 = "qspray"), 
  gmp_arith_qspray
)
setMethod(
  "Arith", 
  signature(e1 = "bigz", e2 = "qspray"), 
  gmp_arith_qspray
)
setMethod(
  "Arith", 
  signature(e1 = "qspray", e2 = "numeric"), 
  qspray_arith_numeric
)
setMethod(
  "Arith", 
  signature(e1 = "numeric", e2 = "qspray"), 
  numeric_arith_qspray
)

setMethod(
  "Compare",
  signature(e1 = "qspray", e2 = "qspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = qspray_equality(e1@powers, e1@coeffs, e2@powers, e2@coeffs),
      "!=" = !qspray_equality(e1@powers, e1@coeffs, e2@powers, e2@coeffs),
      stop(gettextf(
        "Comparison operator %s not defined for qspray objects.", 
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "qspray", e2 = "character"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1 == as.qspray(e2),
      "!=" = e1 != as.qspray(e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.", 
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "qspray", e2 = "numeric"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1 == as.qspray(e2),
      "!=" = e1 != as.qspray(e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.", 
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "qspray", e2 = "bigz"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1 == as.qspray(e2),
      "!=" = e1 != as.qspray(e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.", 
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "qspray", e2 = "bigq"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1 == as.qspray(e2),
      "!=" = e1 != as.qspray(e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.", 
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "character", e2 = "qspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = as.qspray(e1) == e2,
      "!=" = as.qspray(e1) != e2,
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.", 
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "numeric", e2 = "qspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = as.qspray(e1) == e2,
      "!=" = as.qspray(e1) != e2,
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.", 
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "bigz", e2 = "qspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = as.qspray(e1) == e2,
      "!=" = as.qspray(e1) != e2,
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.", 
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "bigq", e2 = "qspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = as.qspray(e1) == e2,
      "!=" = as.qspray(e1) != e2,
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.", 
        dQuote(.Generic)
      ))
    )
  }
)
