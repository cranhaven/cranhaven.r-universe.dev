#' @useDynLib ratioOfQsprays, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom qspray qone as.qspray isQone isQzero isConstant getConstantTerm
#' @importFrom methods setMethod setClass new show setAs setGeneric
#' @importFrom gmp as.bigq
#' @include ratioOfQsprays.R
NULL

setClass(
  "ratioOfQsprays",
  slots = c(numerator = "qspray", denominator = "qspray")
)

setMethod(
  "show", "ratioOfQsprays",
  function(object) {
    f <- getShowRatioOfQsprays(object)
    cat(f(object), "\n")
  }
)

setAs("integer", "ratioOfQsprays", function(from) {
  new("ratioOfQsprays", numerator = as.qspray(from), denominator = qone())
})
setAs("numeric", "ratioOfQsprays", function(from) {
  new("ratioOfQsprays", numerator = as.qspray(from), denominator = qone())
})
setAs("character", "ratioOfQsprays", function(from) {
  new("ratioOfQsprays", numerator = as.qspray(from), denominator = qone())
})
setAs("bigz", "ratioOfQsprays", function(from) {
  new("ratioOfQsprays", numerator = as.qspray(from), denominator = qone())
})
setAs("bigq", "ratioOfQsprays", function(from) {
  new("ratioOfQsprays", numerator = as.qspray(from), denominator = qone())
})
setAs("qspray", "ratioOfQsprays", function(from) {
  roq <- new("ratioOfQsprays", numerator = from, denominator = qone())
  passShowAttributes(from, roq)
})

as_ratioOfQsprays_scalar <- function(x) {
  new("ratioOfQsprays", numerator = as.qspray(x), denominator = qone())
}

as_ratioOfQsprays_qspray <- function(x) {
  roq <- new("ratioOfQsprays", numerator = as.qspray(x), denominator = qone())
  passShowAttributes(x, roq)
}

setGeneric(
  "as.ratioOfQsprays", function(x) {
    stop(
      "No available application of `as.ratioOfQsprays` for this object."
    )
  }
)

#' @name as.ratioOfQsprays
#' @aliases as.ratioOfQsprays,character-method as.ratioOfQsprays,ratioOfQsprays-method as.ratioOfQsprays,qspray-method as.ratioOfQsprays,numeric-method as.ratioOfQsprays,bigz-method as.ratioOfQsprays,bigq-method
#' @exportMethod as.ratioOfQsprays
#' @docType methods
#' @title Coercion to a 'ratioOfQsprays' object
#'
#' @param x a \code{ratioOfQsprays} object, a \code{qspray} object, or an
#'   object yielding a quoted integer or a quoted fraction after an application
#'   of \code{as.character}, e.g. a \code{bigq} number
#'
#' @return This returns \code{x} if \code{x} already is a \code{ratioOfQsprays}
#'   object, otherwise this returns the \code{ratioOfQsprays} object whose
#'   numerator is the coercion of \code{x} to a \code{qspray} object and whose
#'   denominator is the unit \code{qspray} object.
#' @export
#'
#' @examples
#' library(qspray)
#' as.ratioOfQsprays(2)
#' as.ratioOfQsprays("1/3")
#' ( qspray <- 5*qlone(1) + qlone(2)^2 )
#' as.ratioOfQsprays(qspray)
#' # show options are inherited:
#' showQsprayOption(qspray, "x") <- "A"
#' as.ratioOfQsprays(qspray)
setMethod(
  "as.ratioOfQsprays", "character",
  function(x) {
    as_ratioOfQsprays_scalar(x)
  }
)

#' @rdname as.ratioOfQsprays
setMethod(
  "as.ratioOfQsprays", "ratioOfQsprays",
  function(x) {
    x
  }
)

#' @rdname as.ratioOfQsprays
setMethod(
  "as.ratioOfQsprays", "qspray",
  function(x) {
    as_ratioOfQsprays_qspray(x)
  }
)

#' @rdname as.ratioOfQsprays
setMethod(
  "as.ratioOfQsprays", "numeric",
  function(x) {
    as_ratioOfQsprays_scalar(x)
  }
)

#' @rdname as.ratioOfQsprays
setMethod(
  "as.ratioOfQsprays", "bigz",
  function(x) {
    as_ratioOfQsprays_scalar(x)
  }
)

#' @rdname as.ratioOfQsprays
setMethod(
  "as.ratioOfQsprays", "bigq",
  function(x) {
    as_ratioOfQsprays_scalar(x)
  }
)

#' @name ratioOfQsprays-unary
#' @title Unary operators for 'ratioOfQsprays' objects
#' @description Unary operators for \code{ratioOfQsprays} objects.
#' @aliases +,ratioOfQsprays,missing-method -,ratioOfQsprays,missing-method
#' @param e1 object of class \code{ratioOfQsprays}
#' @param e2 nothing
#' @return A \code{ratioOfQsprays} object.
setMethod(
  "+",
  signature(e1 = "ratioOfQsprays", e2 = "missing"),
  function(e1, e2) e1
)
#' @rdname ratioOfQsprays-unary
setMethod(
  "-",
  signature(e1 = "ratioOfQsprays", e2 = "missing"),
  function(e1, e2) {
    passShowAttributes(e1, new(
      "ratioOfQsprays",
      numerator = -e1@numerator, denominator = e1@denominator
    ))
  }
)

#' make the denominator monic
#' @importFrom qspray leadingCoefficient
#' @noRd
monicRatioOfQsprays <- function(roq) {
  num <- roq@numerator
  den <- roq@denominator
  lc <- leadingCoefficient(den)
  new(
    "ratioOfQsprays",
    numerator   = num / lc,
    denominator = den / lc
  )
  # passShowAttributes(roq, new(
  #   "ratioOfQsprays",
  #   numerator   = num,
  #   denominator = den
  # ))
}

ratioOfQsprays_arith_ratioOfQsprays <- function(e1, e2) {
  num1 <- e1@numerator
  den1 <- e1@denominator
  num2 <- e2@numerator
  den2 <- e2@denominator
  roq <- switch(
    .Generic,
    "+" = {
      x <- ROQaddition(
        list("powers" = num1@powers, "coeffs" = num1@coeffs),
        list("powers" = den1@powers, "coeffs" = den1@coeffs),
        list("powers" = num2@powers, "coeffs" = num2@coeffs),
        list("powers" = den2@powers, "coeffs" = den2@coeffs)
      )
      ratioOfQsprays_from_list(x)
    },
    "-" = {
      x <- ROQsubtraction(
        list("powers" = num1@powers, "coeffs" = num1@coeffs),
        list("powers" = den1@powers, "coeffs" = den1@coeffs),
        list("powers" = num2@powers, "coeffs" = num2@coeffs),
        list("powers" = den2@powers, "coeffs" = den2@coeffs)
      )
      ratioOfQsprays_from_list(x)
    },
    "*" = {
      x <- ROQmultiplication(
        list("powers" = num1@powers, "coeffs" = num1@coeffs),
        list("powers" = den1@powers, "coeffs" = den1@coeffs),
        list("powers" = num2@powers, "coeffs" = num2@coeffs),
        list("powers" = den2@powers, "coeffs" = den2@coeffs)
      )
      ratioOfQsprays_from_list(x)
    },
    "/" = {
      x <- ROQdivision(
        list("powers" = num1@powers, "coeffs" = num1@coeffs),
        list("powers" = den1@powers, "coeffs" = den1@coeffs),
        list("powers" = num2@powers, "coeffs" = num2@coeffs),
        list("powers" = den2@powers, "coeffs" = den2@coeffs)
      )
      ratioOfQsprays_from_list(x)
    },
    stop(gettextf(
      "Binary operator %s not defined for ratioOfQsprays objects.",
      dQuote(.Generic)
    ))
  )
  passShowAttributes(e1, roq)
}
ratioOfQsprays_arith_qspray <- function(e1, e2) {
  roq <- switch(
    .Generic,
    "+" = e1 + as.ratioOfQsprays(e2),
    "-" = e1 - as.ratioOfQsprays(e2),
    "*" = e1 * as.ratioOfQsprays(e2),
    "/" = e1 / as.ratioOfQsprays(e2),
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e1, roq)
}
qspray_arith_ratioOfQsprays <- function(e1, e2) {
  roq <- switch(
    .Generic,
    "+" = as.ratioOfQsprays(e1) + e2,
    "-" = as.ratioOfQsprays(e1) - e2,
    "*" = as.ratioOfQsprays(e1) * e2,
    "/" = as.ratioOfQsprays(e1) / e2,
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e2, roq)
}
ratioOfQsprays_arith_character <- function(e1, e2) {
  roq <- switch(
    .Generic,
    "+" = e1 + as.ratioOfQsprays(e2),
    "-" = e1 - as.ratioOfQsprays(e2),
    "*" = new(
      "ratioOfQsprays",
      numerator   = e1@numerator * e2,
      denominator = e1@denominator
    ),
    "/" = new(
      "ratioOfQsprays",
      numerator   = e1@numerator / e2,
      denominator = e1@denominator
    ),
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e1, roq)
}
ratioOfQspraysPower <- function(ratioOfQsprays, n) {
  stopifnot(isInteger(n))
  numerator   <- ratioOfQsprays@numerator
  denominator <- ratioOfQsprays@denominator
  roqAsList <- ROQpower(
    list("powers" = numerator@powers,   "coeffs" = numerator@coeffs),
    list("powers" = denominator@powers, "coeffs" = denominator@coeffs),
    n
  )
  roq <- ratioOfQsprays_from_list(roqAsList)
  passShowAttributes(ratioOfQsprays, roq)
}
ratioOfQsprays_arith_gmp <- function(e1, e2) {
  roq <- switch(
    .Generic,
    "+" = e1 + as.ratioOfQsprays(e2),
    "-" = e1 - as.ratioOfQsprays(e2),
    "*" = new(
      "ratioOfQsprays",
      numerator = e1@numerator * e2,
      denominator = e1@denominator
    ),
    "/" = new(
      "ratioOfQsprays",
      numerator   = e1@numerator / e2,
      denominator = e1@denominator
    ),
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e1, roq)
}
ratioOfQsprays_arith_numeric <- function(e1, e2) {
  roq <- switch(
    .Generic,
    "+" = e1 + as.ratioOfQsprays(e2),
    "-" = e1 - as.ratioOfQsprays(e2),
    "*" = new(
      "ratioOfQsprays",
      numerator = e1@numerator * e2,
      denominator = e1@denominator
    ),
    "/" = new(
      "ratioOfQsprays",
      numerator   = e1@numerator / e2,
      denominator = e1@denominator
    ),
    "^" = ratioOfQspraysPower(e1, e2),
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e1, roq)
}
character_arith_ratioOfQsprays <- function(e1, e2) {
  roq <- switch(
    .Generic,
    "+" = as.ratioOfQsprays(e1) + e2,
    "-" = as.ratioOfQsprays(e1) - e2,
    "*" = new(
      "ratioOfQsprays",
      numerator = e1 * e2@numerator,
      denominator = e2@denominator
    ),
    "/" = monicRatioOfQsprays(
      new(
        "ratioOfQsprays",
        numerator   = e1 * e2@denominator,
        denominator = e2@numerator
      )
    ),
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e2, roq)
}
gmp_arith_ratioOfQsprays <- function(e1, e2) {
  roq <- switch(
    .Generic,
    "+" = as.ratioOfQsprays(e1) + e2,
    "-" = as.ratioOfQsprays(e1) - e2,
    "*" = new(
      "ratioOfQsprays",
      numerator = e1 * e2@numerator,
      denominator = e2@denominator
    ),
    "/" = monicRatioOfQsprays(
      new(
        "ratioOfQsprays",
        numerator   = e1 * e2@denominator,
        denominator = e2@numerator
      )
    ),
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e2, roq)
}
numeric_arith_ratioOfQsprays <- function(e1, e2) {
  roq <- switch(
    .Generic,
    "+" = as.ratioOfQsprays(e1) + e2,
    "-" = as.ratioOfQsprays(e1) - e2,
    "*" = new(
      "ratioOfQsprays",
      numerator = e1 * e2@numerator,
      denominator = e2@denominator
    ),
    "/" = monicRatioOfQsprays(
      new(
        "ratioOfQsprays",
        numerator   = e1 * e2@denominator,
        denominator = e2@numerator
      )
    ),
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
  passShowAttributes(e2, roq)
}

setMethod(
  "Arith",
  signature(e1 = "ratioOfQsprays", e2 = "ratioOfQsprays"),
  ratioOfQsprays_arith_ratioOfQsprays
)
setMethod(
  "Arith",
  signature(e1 = "ratioOfQsprays", e2 = "qspray"),
  ratioOfQsprays_arith_qspray
)
setMethod(
  "Arith",
  signature(e1 = "ratioOfQsprays", e2 = "character"),
  ratioOfQsprays_arith_character
)
setMethod(
  "Arith",
  signature(e1 = "ratioOfQsprays", e2 = "bigq"),
  ratioOfQsprays_arith_gmp
)
setMethod(
  "Arith",
  signature(e1 = "ratioOfQsprays", e2 = "bigz"),
  ratioOfQsprays_arith_gmp
)
setMethod(
  "Arith",
  signature(e1 = "qspray", e2 = "ratioOfQsprays"),
  qspray_arith_ratioOfQsprays
)
setMethod(
  "Arith",
  signature(e1 = "character", e2 = "ratioOfQsprays"),
  character_arith_ratioOfQsprays
)
setMethod(
  "Arith",
  signature(e1 = "bigq", e2 = "ratioOfQsprays"),
  gmp_arith_ratioOfQsprays
)
setMethod(
  "Arith",
  signature(e1 = "bigz", e2 = "ratioOfQsprays"),
  gmp_arith_ratioOfQsprays
)
setMethod(
  "Arith",
  signature(e1 = "ratioOfQsprays", e2 = "numeric"),
  ratioOfQsprays_arith_numeric
)
setMethod(
  "Arith",
  signature(e1 = "numeric", e2 = "ratioOfQsprays"),
  numeric_arith_ratioOfQsprays
)


setMethod(
  "Compare",
  signature(e1 = "ratioOfQsprays", e2 = "ratioOfQsprays"),
  function(e1, e2) {
    num <- e1@numerator*e2@denominator - e2@numerator*e1@denominator
    switch(
      .Generic,
      "==" = isQzero(num),
      "!=" = !(isQzero(num)),
      stop(gettextf(
        "Comparison operator %s not defined for ratioOfQsprays objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "ratioOfQsprays", e2 = "qspray"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1@numerator == e2 * e1@denominator,
      "!=" = !(e1 == e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "ratioOfQsprays", e2 = "character"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1@numerator == e2 * e1@denominator,
      "!=" = !(e1 == e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "ratioOfQsprays", e2 = "numeric"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1@numerator == e2 * e1@denominator,
      "!=" = !(e1 == e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "ratioOfQsprays", e2 = "bigz"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1@numerator == e2 * e1@denominator,
      "!=" = !(e1 == e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "ratioOfQsprays", e2 = "bigq"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e1@numerator == e2 * e1@denominator,
      "!=" = !(e1 == e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "qspray", e2 = "ratioOfQsprays"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e2@numerator == e1 * e2@denominator,
      "!=" = !(e1 == e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "character", e2 = "ratioOfQsprays"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e2@numerator == e1 * e2@denominator,
      "!=" = !(e1 == e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "numeric", e2 = "ratioOfQsprays"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e2@numerator == e1 * e2@denominator,
      "!=" = !(e1 == e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "bigz", e2 = "ratioOfQsprays"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e2@numerator == e1 * e2@denominator,
      "!=" = !(e1 == e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
setMethod(
  "Compare",
  signature(e1 = "bigq", e2 = "ratioOfQsprays"),
  function(e1, e2) {
    switch(
      .Generic,
      "==" = e2@numerator == e1 * e2@denominator,
      "!=" = !(e1 == e2),
      stop(gettextf(
        "Comparison operator %s not defined for these two objects.",
        dQuote(.Generic)
      ))
    )
  }
)
