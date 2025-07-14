#' Generate NAs of the right type for counts
#'
#' Counts can be of type integer or double. When setting up NAs to counts, this
#' needs to be reflected by using the right type of NAs. This function addresses
#' this need.
#'
#' @author Thibaut
#'
#' @param x A count vector.
#'
#' @return A `NA` of the type matching the input.
#'
#' @keywords internal
#' @noRd
NA_counts_ <- function(x) {
  if (is.integer(x)) {
    return(NA_integer_)
  } else if (is.double(x)) {
    return(NA_real_)
  } else {
    msg <- sprintf(
        "Cannot set NA values for counts of type `%s`",
        typeof(x))
    stop(msg)
  }
}

stopf <- function(fmt, ..., .use_call = TRUE, .call = sys.call(-1L)) {
    .call <- if (isTRUE(.use_call)) .call[1L] else NULL
    msg <- sprintf(fmt, ...)
    err <- simpleError(msg, .call)
    stop(err)
}

.assert_not_missing <- function(x, arg, call) {
    if (missing(x))
        stopf("argument `%s` is missing, with no default.", arg, .call = call)
}

.assert_scalar_character <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!(is.character(x) && length(x) == 1))
        stopf("`%s` must be a character vector of length 1.", arg, .call = call)

}

.assert_bool <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!(is.logical(x) && length(x) == 1) || is.na(x))
        stopf("`%s` must be boolean (TRUE/FALSE).", arg, .call = call)
}

not_implemented <- function(x, call. = FALSE) {
    stop(
        sprintf("Not implemented for class %s", paste(class(x), collapse = ", ")),
        call. = call.
    )
}
