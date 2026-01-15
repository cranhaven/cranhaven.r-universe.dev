#' @useDynLib symbolicQspray, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom qspray orderedQspray
#' @importFrom ratioOfQsprays as.ratioOfQsprays
#' @importFrom methods setMethod setClass new show setAs setGeneric
#' @importFrom gmp as.bigq
#' @include symbolicQspray.R
NULL

setClass(
  "symbolicQspray",
  slots = c(powers = "list", coeffs = "list")
)

setMethod(
  "show", "symbolicQspray",
  function(object) {
    f <- getShowSymbolicQspray(object)
    cat(f(object), "\n")
  }
)


setAs( # for usage in qspray::MSPcombination and unit tests of 'jack'
  "qspray", "symbolicQspray", function(from) {
    new(
      "symbolicQspray",
      powers = from@powers,
      coeffs = lapply(from@coeffs, as.ratioOfQsprays)
    )
  }
)

as_symbolicQspray_scalar <- function(x) {
  x <- as.ratioOfQsprays(x)
  if(x == 0L) {
    Qspray <- new("symbolicQspray", powers = list(), coeffs = list())
  } else {
    Qspray <-
      new("symbolicQspray", powers = list(integer(0L)), coeffs = list(x))
  }
  passShowAttributes(x, Qspray)
}

setGeneric(
  "as.symbolicQspray", function(x) {
    NULL
  }
)

#' @name as.symbolicQspray
#' @aliases as.symbolicQspray,character-method as.symbolicQspray,qspray-method as.symbolicQspray,ratioOfQsprays-method as.symbolicQspray,symbolicQspray-method as.symbolicQspray,numeric-method as.symbolicQspray,bigz-method as.symbolicQspray,bigq-method
#' @exportMethod as.symbolicQspray
#' @docType methods
#' @title Coercion to a 'symbolicQspray' object
#'
#' @param x a \code{symbolicQspray} object or an object for which
#'   \code{\link[ratioOfQsprays]{as.ratioOfQsprays}} is applicable
#'
#' @return A \code{symbolicQspray} object.
#' @export
#'
#' @examples
#' as.symbolicQspray(2)
#' as.symbolicQspray("1/3")
setMethod(
  "as.symbolicQspray", "character",
  function(x) {
    as_symbolicQspray_scalar(x)
  }
)

#' @rdname as.symbolicQspray
setMethod(
  "as.symbolicQspray", "qspray",
  function(x) {
    as_symbolicQspray_scalar(x)
  }
)

#' @rdname as.symbolicQspray
setMethod(
  "as.symbolicQspray", "ratioOfQsprays",
  function(x) {
    as_symbolicQspray_scalar(x)
  }
)

#' @rdname as.symbolicQspray
setMethod(
  "as.symbolicQspray", "symbolicQspray",
  function(x) {
    x
  }
)

#' @rdname as.symbolicQspray
setMethod(
  "as.symbolicQspray", "numeric",
  function(x) {
    as_symbolicQspray_scalar(x)
  }
)

#' @rdname as.symbolicQspray
setMethod(
  "as.symbolicQspray", "bigz",
  function(x) {
    as_symbolicQspray_scalar(x)
  }
)

#' @rdname as.symbolicQspray
setMethod(
  "as.symbolicQspray", "bigq",
  function(x) {
    as_symbolicQspray_scalar(x)
  }
)


#' @name symbolicQspray-unary
#' @title Unary operators for 'symbolicQspray objects
#' @description Unary operators for \code{symbolicQspray} objects.
#' @aliases +,symbolicQspray,missing-method -,symbolicQspray,missing-method
#' @param e1 object of class \code{symbolicQspray}
#' @param e2 nothing
#' @return A \code{symbolicQspray} object.
setMethod(
  "+",
  signature(e1 = "symbolicQspray", e2 = "missing"),
  function(e1, e2) e1
)
#' @rdname symbolicQspray-unary
setMethod(
  "-",
  signature(e1 = "symbolicQspray", e2 = "missing"),
  function(e1, e2) {
    Qspray <- new(
      "symbolicQspray",
      powers = e1@powers, coeffs = lapply(e1@coeffs, function(x) -x)
    )
    passShowAttributes(e1, Qspray)
  }
)


symbolicQsprayPower <- function(e1, n) {
  stopifnot(isPositiveInteger(n))
  Qspray <- symbolicQspray_from_list(SymbolicQspray_power(
    e1@powers, lapply(e1@coeffs, ratioOfQsprays_as_list), as.integer(n)
  ))
  passShowAttributes(e1, Qspray)
}
symbolicQspray_arith_scalar <- function(e1, e2) {
  Qspray <- switch(
    .Generic,
    "+" = e1 + as.symbolicQspray(e2),
    "-" = e1 - as.symbolicQspray(e2),
    "*" = e1 * as.symbolicQspray(e2),
    "/" = e1 * as.symbolicQspray(1L/as.ratioOfQsprays(e2)),
    "^" = symbolicQsprayPower(e1, e2),
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e1, Qspray)
}
scalar_arith_symbolicQspray <- function(e1, e2) {
  Qspray <- switch(
    .Generic,
    "+" = as.symbolicQspray(e1) + e2,
    "-" = -as.symbolicQspray(e1) + e2,
    "*" = as.symbolicQspray(e1) * e2,
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e2, Qspray)
}
ratioOfQsprays_arith_symbolicQspray <- function(e1, e2) {
  Qspray <- switch(
    .Generic,
    "+" = as.symbolicQspray(e1) + e2,
    "-" = -as.symbolicQspray(e1) + e2,
    "*" = as.symbolicQspray(e1) * e2,
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e2, Qspray)
}
symbolicQspray_arith_symbolicQspray <- function(e1, e2) {
  Qspray <- switch(
    .Generic,
    "+" = symbolicQspray_from_list(
      SymbolicQspray_add(
        e1@powers, lapply(e1@coeffs, ratioOfQsprays_as_list),
        e2@powers, lapply(e2@coeffs, ratioOfQsprays_as_list)
      )
    ),
    "-" = symbolicQspray_from_list(
      SymbolicQspray_subtract(
        e1@powers, lapply(e1@coeffs, ratioOfQsprays_as_list),
        e2@powers, lapply(e2@coeffs, ratioOfQsprays_as_list)
      )
    ),
    "*" = symbolicQspray_from_list(
      SymbolicQspray_multiply(
        e1@powers, lapply(e1@coeffs, ratioOfQsprays_as_list),
        e2@powers, lapply(e2@coeffs, ratioOfQsprays_as_list)
      )
    ),
    stop(gettextf(
      "Binary operator %s not defined for symbolicQspray objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e1, Qspray)
}
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "symbolicQspray"),
  symbolicQspray_arith_symbolicQspray
)
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "ratioOfQsprays"),
  symbolicQspray_arith_scalar
)
setMethod(
  "Arith",
  signature(e1 = "ratioOfQsprays", e2 = "symbolicQspray"),
  scalar_arith_symbolicQspray
)
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "qspray"),
  symbolicQspray_arith_scalar
)
setMethod(
  "Arith",
  signature(e1 = "qspray", e2 = "symbolicQspray"),
  scalar_arith_symbolicQspray
)
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "character"),
  symbolicQspray_arith_scalar
)
setMethod(
  "Arith",
  signature(e1 = "character", e2 = "symbolicQspray"),
  scalar_arith_symbolicQspray
)
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "bigz"),
  symbolicQspray_arith_scalar
)
setMethod(
  "Arith",
  signature(e1 = "bigz", e2 = "symbolicQspray"),
  scalar_arith_symbolicQspray
)
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "bigq"),
  symbolicQspray_arith_scalar
)
setMethod(
  "Arith",
  signature(e1 = "bigq", e2 = "symbolicQspray"),
  scalar_arith_symbolicQspray
)
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "numeric"),
  symbolicQspray_arith_scalar
)
setMethod(
  "Arith",
  signature(e1 = "numeric", e2 = "symbolicQspray"),
  scalar_arith_symbolicQspray
)


setMethod(
  "Compare",
  signature(e1 = "symbolicQspray", e2 = "symbolicQspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = SymbolicQspray_equality(
        e1@powers, lapply(e1@coeffs, ratioOfQsprays_as_list),
        e2@powers, lapply(e2@coeffs, ratioOfQsprays_as_list)
      ),
      "!=" = !(e1 == e2),
      stop(gettextf(
        "Comparison operator %s not defined for 'symbolicQspray' objects.", dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "symbolicQspray", e2 = "ratioOfQsprays"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1 == as.symbolicQspray(e2),
      "!=" = e1 != as.symbolicQspray(e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "symbolicQspray", e2 = "qspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1 == as.symbolicQspray(e2),
      "!=" = e1 != as.symbolicQspray(e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "symbolicQspray", e2 = "character"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1 == as.symbolicQspray(e2),
      "!=" = e1 != as.symbolicQspray(e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "symbolicQspray", e2 = "numeric"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1 == as.symbolicQspray(e2),
      "!=" = e1 != as.symbolicQspray(e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "symbolicQspray", e2 = "bigz"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1 == as.symbolicQspray(e2),
      "!=" = e1 != as.symbolicQspray(e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "symbolicQspray", e2 = "bigq"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1 == as.symbolicQspray(e2),
      "!=" = e1 != as.symbolicQspray(e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "ratioOfQsprays", e2 = "symbolicQspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = as.symbolicQspray(e1) == e2,
      "!=" = as.symbolicQspray(e1) != e2,
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "qspray", e2 = "symbolicQspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = as.symbolicQspray(e1) == e2,
      "!=" = as.symbolicQspray(e1) != e2,
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "character", e2 = "symbolicQspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = as.symbolicQspray(e1) == e2,
      "!=" = as.symbolicQspray(e1) != e2,
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "numeric", e2 = "symbolicQspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = as.symbolicQspray(e1) == e2,
      "!=" = as.symbolicQspray(e1) != e2,
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "bigz", e2 = "symbolicQspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = as.symbolicQspray(e1) == e2,
      "!=" = as.symbolicQspray(e1) != e2,
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "bigq", e2 = "symbolicQspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = as.symbolicQspray(e1) == e2,
      "!=" = as.symbolicQspray(e1) != e2,
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
