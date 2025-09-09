
#' A python / javascript-like "truthy" notion
#'
#' Values are truthy that are not null, NA, empty, 0, or FALSE.
#'
#' Note that this is per se not vectorised, because a non-empty list or vector is "truthy" as such.
#' @param x Any object, value or NULL
#'
#' @return logical
#' @export
truthy <- function(x)
{
  if (invalid(x))
    return(F)
  if (is.atomic(x))
  {
    if (is.character(x))
      return(b(as.logical(max(str_length(x)))))
    if (length(x) == 1)
      return(b(as.logical(x)))
    return(all(b(as.logical(x))))
  }
  else if (is.list(x))
  {
    return(length(x) != 0)
  }
  return(b(as.logical(x)))
}

#' @describeIn truthy x is not truthy
#' @export
falsy <- function(x)
{
  return (!truthy(x))
}

#' A notion of valid and invalid
#'
#' An object is valid if it is not null, not missing (NA), and is not an empty vector.
#' Note that this is per se not vectorised, because a non-empty list or vector is valid as such.
#' @param x Any object, value or NULL
#'
#' @return logical
#' @examples
#' invalid(NULL) # TRUE
#' invalid(NA) # TRUE
#' invalid(list()) # TRUE
#' invalid("a") # FALSE
#' invalid(c(1,2,3)) # FALSE
#' @export
invalid <- function(x)
{
  if (is_null(x))
  {
    return(TRUE)
  }
  if (is_vector(x))
  {
    return(length(x) == 0)
  }
  is_na(x)
}

#' @describeIn invalid x is not invalid
#' @export
valid <- function(x)
{
  if (is_null(x))
  {
    return(FALSE)
  }
  if (is_vector(x))
  {
    return(length(x) > 0)
  }
  !is_na(x)
}

