#' Linearly interpolate missing values in a numeric vector
#'
#' This value fills in missing values (\code{NA}s) in a numeric vector by
#' linear interpolation
#'
#' @param x The numeric vector to interpolate. The first and last element must
#' not be \code{NA}, otherwise an error is generated.
#' @param max_allow Maximum number of consecutive missing values to allow. If
#' there is any number of consecutive \code{NA} values in \code{x} longer than
#' \code{max_allow}, the function will fail with an error. Set to \code{NULL} to
#' fully disable this check.
#'
#' @details This function can be handy when running \code{\link{fitFluMoDL}}, for
#' example to fill in small gaps in the temperatures vector.
#' But it can be more generally useful as well.
#'
#' @return The numeric vector \code{x}, with any missing values replaced by
#' linear interpolants.
#'
#' @export
linterp <- function(x, max_allow = 3) {
  if (!is.numeric(x)) stop("`x` must be numeric")
  if (!is.null(max_allow) && (!is.numeric(max_allow) || length(max_allow)>1))
    stop("Argument `max_allow` must be NULL or a numeric vector of length 1.")
  if (is.na(x[1]) || is.na(rev(x)[1]))
    stop("The first and the last elements of `x` cannot be NA.")

  if (sum(is.na(x))==0) return(x) # Return immediately if no missing values

  posNA <- which(is.na(x)) # Find all NAs
  startNA <- posNA[c(2,diff(posNA))>1] # Find the start of any consecutive NA segments
  endNA <- rev(rev(posNA)[c(-2, diff(rev(posNA)))<(-1)]) # ...and the end
  startNA <- startNA-1 # For convenience, get the previous (non-missing) value in each segment
  endNA <- endNA+1 # For convenience, get the next (non-missing) value in each segment
  dist <- endNA - startNA + 1
  if (sum(dist-2>max_allow)>0)
    stop(sprintf("`x` cannot have segments with consecutive NA values longer than max_allow. \n  Increase max_allow or set it to NULL to turn off this check."))

  for (i in 1:length(startNA)) {
    x[startNA[i]:endNA[i]] <- seq(x[startNA[i]], x[endNA[i]],
                                  length.out = endNA[i] - startNA[i] + 1)
  }
  return(x)
}

