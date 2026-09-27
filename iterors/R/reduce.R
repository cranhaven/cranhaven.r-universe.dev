#' Compute the sum, product, or general reduction of an iterator.
#'
#' `reduce(obj, fun)` applies a 2-argument function `fun` between
#' successive elements of obj. For example if `fun` is `+`,
#' `reduce(it, `+`, init=0)` computes `0 + nextElem(it) +
#' nextElem(it) + nextElem(it) + ...` until the iterator finishes,
#' and returns the final value.
#'
#' @export
#' @param obj an iterable object
#' @param fun A function of as least two arguments.
#' @param ... Extra parameters will be passed to each call to `fun`.
#' @param init A starting value.
#' @return The result of accumulation.
#' @author Peter Meilstrup
#'
#' @examples
#' it <- icount(5)
#' total <- reduce(it, `+`) # 15
#'
#' it <- icount(5)
#' reduce(it, paste0, "") # "12345"
#'
#' it <- icount(5)
#' reduce(it, `*`, init=1) # 120
#'
reduce <- function(obj, fun=`+`, init=0, ...) UseMethod("reduce")

#' @rdname reduce
#' @export
#' @exportS3Method
reduce.iteror <- function(obj, fun=`+`, init=0, ...) {
  obj <- iteror(obj)
  rgs <- formals(args(fun))
  if (!is.function(fun)
      || length(rgs) < 2
      && !("..." %in% names(rgs))) {
    stop("`fun` must be a function of two arguments")
  } else {
    repeat {
      val <- obj(or = break)
      init <- fun(init, val, ...)
    }
  }
  init
}

#' @description
#' `i_accum(obj)` returns the iterator containing
#' each intermediate result. The default settings
#' produce a cumulative sum.
#' @rdname reduce
#' @examples
#' # triangular numbers: 1, 1+2, 1+2+3, ...
#' take(i_accum(icount()), 10, 'numeric')
#' @export
i_accum <- function(obj, fun=`+`, init=0, ...) {
  obj <- iteror(obj)

  rgs <- formals(args(fun))
  if (!is.function(fun)
      || length(rgs) < 2
      && !("..." %in% names(rgs))) {
    stop("`fun` must be a function of two arguments")
  }

  nextOr_ <- function(or) {
    val <- nextOr(obj, return(or))
    init <<- fun(init, val, ...)
  }

  iteror_internal(nextOr_)
}

#' @export
#' @exportS3Method
#' @rdname reduce
#' @description `sum.iteror(it)` is equivalent to \code{reduce(it, `+`)}
sum.iteror <- function(..., na.rm=FALSE) {
  it <- i_chain(...)
  if (na.rm) {
    it <- i_drop(it, is.na)
  }
  reduce(it)
}

#' @exportS3Method
#' @export
#' @rdname reduce
#' @param na.rm Whether to drop NA values when computing sum or prod.
#' @description `prod.iteror(it)` is equivalent to \code{reduce(it, `*`)}.
prod.iteror <- function(..., na.rm=FALSE) {
  it <- i_chain(...)
  if (na.rm) {
    it <- i_drop(it, is.na)
  }
  reduce(it, `*`, 1)
}
