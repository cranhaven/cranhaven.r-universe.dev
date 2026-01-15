#' @importFrom ratioOfQsprays numberOfVariables
#' @noRd
passShowAttributes <- function(source, target) {
  if( # if source has been created with as.symbolicQspray
    inherits(source, "ratioOfQsprays")
  ) {
    sROQ <- attr(attr(source, "showOpts"), "showRatioOfQsprays")
    if(!is.null(sROQ)) {
      showSymbolicQsprayOption(target, "showRatioOfQsprays") <- sROQ
    }
    return(target)
  }
  sSQ <- getShowSymbolicQspray(source)
  showOpts <- attr(source, "showOpts")
  inheritable <- attr(showOpts, "inheritable")
  inheritableSROQ <- isTRUE(attr(inheritable, "sROQ"))
  inheritableSM   <- isTRUE(attr(inheritable, "sM"))
  if(inheritableSROQ && inheritableSM) {
    attr(target, "showOpts") <- showOpts
  } else {
    if(!inheritableSROQ) {
      n1 <- suppressWarnings(
        max(vapply(source@coeffs, numberOfVariables, integer(1L)))
      )
      n2 <- suppressWarnings(
        max(vapply(target@coeffs, numberOfVariables, integer(1L)))
      )
      inheritableSROQ <- n1 >= n2
    }
    if(inheritableSROQ) {
      showSymbolicQsprayOption(target, "showRatioOfQsprays") <-
        attr(sSQ, "showRatioOfQsprays")
    }
    inheritableSM <- inheritableSM ||
      numberOfVariables(source) >= numberOfVariables(target)
    if(inheritableSM) {
      showSymbolicQsprayOption(target, "showMonomial") <-
        attr(sSQ, "showMonomial")
    }
  }
  target
}

arity <- function(qspray) {
  suppressWarnings(max(lengths(qspray@powers)))
}

#' @title (internal) Make a 'symbolicQspray' object from a list
#' @description This function is for internal usage. It is exported because
#'   it is also used for internal usage in others packages.
#'
#' @param x list returned by the Rcpp function
#'   \code{returnSymbolicQspray}
#'
#' @return A \code{symbolicQspray} object.
#' @export
#' @importFrom ratioOfQsprays ratioOfQsprays_from_list
symbolicQspray_from_list <- function(x) {
  powers <- x[["powers"]]
  if(is.null(powers)) {
    new("symbolicQspray", powers = list(), coeffs = list())
  }
  else {
    new(
      "symbolicQspray",
      powers = powers,
      coeffs = lapply(x[["coeffs"]], ratioOfQsprays_from_list)
    )
  }
}

qspray_as_list <- function(x) {
  list("powers" = x@powers, "coeffs" = x@coeffs)
}

ratioOfQsprays_as_list <- function(x) {
  list(
    "numerator"   = qspray_as_list(x@numerator),
    "denominator" = qspray_as_list(x@denominator)
  )
}

`%||%` <- function(x, y) {
  if(is.null(x)) y else x
}

isPositiveInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x
}

isNonnegativeInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x && x != 0
}

isExponents <- function(x) {
  is.numeric(x) && !anyNA(x) && all(floor(x) == x)
}

#' @importFrom utils head
#' @noRd
removeTrailingZeros <- function(x) {
  n <- length(x)
  while(x[n] == 0L && n > 0L) {
    n <- n - 1L
  }
  head(x, n)
}

grow <- function(powers, n) {
  c(powers, integer(n - length(powers)))
}

arity <- function(qspray) {
  suppressWarnings(max(lengths(qspray@powers)))
}

powersMatrix <- function(qspray) {
  n <- numberOfVariables(qspray)
  if(n == 0L) {
    matrix(NA_integer_, 0L, 0L)
  } else {
    do.call(rbind, lapply(qspray@powers, grow, n = n))
  }
}

isPermutation <- function(x) {
  setequal(x, seq_along(x))
}

isNamedList <- function(x) {
  is.list(x) && length(names(x)) == length(x)
}
