#' Helpers for converting model specifications in R to Julia equivalents
#'
#' @name jl-helpers-utils
#' @keywords internal
#' @return A Julia object of type `<JuliaProxy>`
#'
#' @examplesIf check_julia_ok()
#' \donttest{
#' # (general) Use `jl()` to evaluate arbitrary Julia expressions from string
#' jl("1 .+ [1,3]")
#'
#' # `jl()` takes elements in `...` that you can reference in the expression
#' jl("1 .+ a", a = c(1L, 3L)) # Named arguments are introduced as variables
#' jl("1 .+ %s", "[1,2]") # Unnamed arguments are interpolated via `sprintf()`
#'
#' # Use `is_jl()` to test if object is a Julia (`<JuliaProxy>`) object
#' is_jl(jl("1"))
#'
#' # Use `jl_put()` and `jl_get()` to transfer data between R and Julia
#' jl_put(1L)
#' identical(jl_get(jl_put(1L)), 1L)
#'
#' # `jl_dict()` opinionatedly constructs a Dictionary data type
#' ## Basic `list()`-like usage
#' jl_dict(age = 20:25, sex = c("M", "F"))
#' ## Splats when a single list is supplied
#' jl_dict(list(a = 1, b = 2))
#' ## Wrap scalars in `I()` to prevent vector conversion
#' jl_dict(a = 1:2, b = 3:4, c = I(5))
#'
#' stop_julia()
#' }
NULL

#' @rdname jl-helpers-utils
#' @param x An object
#' @param type Type of Julia object to additional test for
#' @export
is_jl <- function(x, type) {
  inherits(x, "JuliaProxy") &&
    if (!missing(type)) {
      stopifnot(is.character(type) && (length(type) == 1L))
      jl_string <- sprintf("typeof(x) <: %s", type)
      isTRUE(jl(jl_string, x = x, .R = TRUE))
    } else {
      TRUE
    }
}

#' @rdname jl-helpers-utils
#' @export
jl_put <- function(x) {
  stopifnot(!is_jl(x))
  JuliaConnectoR::juliaPut(x)
}

#' @rdname jl-helpers-utils
#' @export
jl_get <- function(x) {
  stopifnot(is_jl(x))
  x <- JuliaConnectoR::juliaGet(x)
  JL_attr <- grep(x = names(attributes(x)), "^JL[A-Z]+$", value = TRUE)
  attributes(x)[JL_attr] <- NULL
  x
}

#' @rdname jl-helpers-utils
#' @param expr A string of Julia code
#' @param ... Interpolated elements. In the case of `jl()`:.
#'    - If all named, elements are introduced as Julia variables in the `expr`
#'    - If all unnamed, elements are interpolated into `expr` via [sprintf()]
#' @param .R Whether to simplify and return as R object, if possible.
#' @param .passthrough Whether to return `expr` as-is if it's already a Julia
#'   object. Mostly for internal use.
#' @export
jl <- function(expr, ..., .R = FALSE, .passthrough = FALSE) {
  if (is_jl(expr) && .passthrough) return(expr)
  dots <- list(...)
  stopifnot(is.character(expr) && length(expr) == 1L)
  if (length(dots) == 0) {
    # eval if no dots
    out <- JuliaConnectoR::juliaEval(expr)
  } else {
    dots_names <- names(dots)
    stopifnot(all(nzchar(dots_names)))
    if (is.null(dots_names)) {
      # sprintf for unnamed dots
      s_interpolated <- do.call(sprintf, c(fmt = expr, dots))
      out <- JuliaConnectoR::juliaEval(s_interpolated)
    } else {
      # let block for named dots
      out <- do.call(JuliaConnectoR::juliaLet, c(expr = expr, dots))
    }
  }
  # resolve `.R` and return as R or Julia
  if (!.R && !is_jl(out)) {
    out <- jl_put(out)
  }
  if (.R && is_jl(out)) {
    out <- jl_get(out)
  }
  out
}

#' @rdname jl-helpers-utils
#' @export
jl_dict <- function(...) {
  dots <- list(...)
  if (length(dots) == 1L && is.null(names(dots)) && is.list(dots)) {
    # Splats when only arg is unnamed and is a list
    dots <- dots[[1L]]
  }
  nms <- names(dots)
  # Turns values into vectors unless `I()`
  dots <- lapply(dots, function(x) {
    if (!inherits(x, "AsIs")) as.list(x) else x
  })
  stopifnot(!is.null(nms) && all(nzchar(nms)))
  jl("x.namedelements", x = dots)
}
