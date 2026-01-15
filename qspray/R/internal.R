passShowAttributes <- function(source, target) {
  showOpts <- attr(source, "showOpts")
  inheritable <- isTRUE(attr(showOpts, "inheritable")) || 
    numberOfVariables(source) >= numberOfVariables(target)
  if(inheritable) {
    attr(target, "showOpts") <- showOpts
  } 
  target
}

#' @title (internal) Make a 'qspray' object from a list
#' @description This function is for internal usage. It is exported because 
#'   it is also used for internal usage in others packages.
#'
#' @param qspray_as_list list returned by the Rcpp function 
#'   \code{returnQspray}
#'
#' @return A \code{qspray} object.
#' @export
qspray_from_list <- function(qspray_as_list) {
  powers <- qspray_as_list[["powers"]]
  if(is.null(powers)) {
    new(
      "qspray", 
      powers = list(), coeffs = character(0L)
    )
  } else {
    new(
      "qspray", 
      powers = powers, coeffs = qspray_as_list[["coeffs"]]
    )
  }
}

`%||%` <- function(x, y) {
  if(is.null(x)) y else x
}

isString <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

isStringVector <- function(x) {
  is.character(x) && !anyNA(x)
}

isInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && as.integer(x) == x
}

isPositiveInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x
}

isNonnegativeInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x && x != 0
}

isFraction <- function(x) {
  if(!is.character(x) || length(x) != 1L) {
    return(FALSE)
  }
  x <- trimws(x)
  if(grepl("^\\-*\\d+$", x)) {
    return(TRUE)
  }
  nd <- trimws(strsplit(x, "/")[[1L]])
  if(length(nd) != 2L) {
    FALSE
  } else {
    n <- nd[1L]
    if(!grepl("^\\-*\\d+$", n)) {
      FALSE
    } else {
      d <- nd[2L]
      if(!grepl("^\\d+$", d) || grepl("^0+$", d)) {
        FALSE
      } else {
        TRUE
      }
    }
  }
}

isFractionOrNA <- function(x) {
  is.na(x) || isFraction(x)
}

isExponents <- function(x) {
  is.numeric(x) && !anyNA(x) && all(floor(x) == x)
}

isCoeffs <- function(x) {
  all(vapply(as.character(x), isFraction, FUN.VALUE = logical(1L)))
}

isDecreasing <- function(x) {
  all(diff(x) <= 0)
}

isPartition <- function(lambda){
  length(lambda) == 0L || 
    all(vapply(lambda, isPositiveInteger, FUN.VALUE = logical(1L))) && 
    isDecreasing(lambda)
}

arity <- function(qspray) {
  suppressWarnings(max(lengths(qspray@powers)))
}

grow <- function(powers, n) {
  c(powers, integer(n - length(powers)))
}

powersMatrix <- function(qspray) {
  n <- numberOfVariables(qspray)
  if(n == 0L) {
    matrix(NA_integer_, 0L, 0L)
  } else {
    do.call(rbind, lapply(qspray@powers, grow, n = n))
  }
}

#' @importFrom utils head
#' @noRd
removeTrailingZeros <- function(x) {
  n <- length(x)
  while(x[n] == 0 && n > 0L) {
    n <- n - 1L
  }
  head(x, n)
}

isPermutation <- function(x) {
  setequal(x, seq_along(x))
}

fromString <- function(string) {
  as.integer(strsplit(string, ",", fixed = TRUE)[[1L]])
}

Columns <- function(M) {
  lapply(seq_len(ncol(M)), function(j) {
    M[, j]
  })
}

Rows <- function(M) {
  lapply(seq_len(nrow(M)), function(i) {
    M[i, ]
  })
}

lexorder <- function(M){
  do.call(
    function(...) order(..., decreasing = TRUE), 
    Columns(M)
  )
}

# drop the first n variables of a qspray
# this assumes these variables are not involved in the qspray!
#' @importFrom utils tail
#' @noRd
dropVariables <- function(n, qspray) {
  powers <- lapply(qspray@powers, function(expnts) {
    tail(expnts, -n)
  })
  new("qspray", powers = powers, coeffs = qspray@coeffs)
}

isNamedList <- function(x) {
  is.list(x) && length(names(x)) == length(x)
}

partitionAsString <- function(lambda) {
  sprintf("[%s]", toString(lambda))
}
