#' An Enhanced Version of as.numeric
#'
#' This function coerces objects into a numeric vector. There are several differences between 
#' this function and \code{as.numeric}. First, if \code{as.character2} fails to coerce (this is 
#' usually because there are characters in the input object), it will raise an error and stop 
#' rather than to give a warning. Second, it can handle data frame object, list, and recursive 
#' list. Third, it can coerce number-like factors exactly into what users see on the screen.
#'
#' @param ... one or more objects to be coerced.
#'
#' @return a numeric vector, or, if fails, an error will be raised.
#'
#' @export
#' @examples
#' # Try to coerce data frame
#' a <- c(55, 66, 77, 88, 66, 77, 88)
#' b <- factor(a)
#' df <- data.frame(a, b)
#' as.numeric2(df, a*2)
#' # Try a list
#' l <- list(a, a*2)
#' as.numeric2(l)
#' # Try a list of lists
#' l2 <- list(l, l)
#' as.numeric2(l2)
as.numeric2 <-
function(...) {
  y <- as.character2(...)
  len_of_na1 <- length(which(is.na(y)))
  yy <- as.numeric(y)
  rm(y)
  len_of_na2 <- length(which(is.na(yy)))
  if (len_of_na2 > len_of_na1) {
    stop("Some elements cannot be coerced to numeric.")
  }
  else {
    return(yy)
  }
}
