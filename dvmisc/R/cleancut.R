#' Convert Numeric to Factor with Convenient Interface
#'
#' So you can stop guess-and-checking with \code{\link{cut}}.
#'
#' @param x Numeric vector.
#' @param breaks Character string, e.g. \code{"[-Inf, 0), [0, 10], (10, Inf)"}.
#' @param labels Character vector.
#'
#' @return Factor or integer vector.
#'
#' @examples
#' x <- rnorm(100)
#' y <- cleancut(x, "(-Inf, -1), [-1, 1], (1, Inf)")
#' tapply(x, y, range)
#'
#' y <- cleancut(x, "(-Inf, -1), [-1, 1], (1, Inf)", c("<-1", "-1 to 1", ">1"))
#' tapply(x, y, range)
#'
#' @export
cleancut <- function(x, breaks, labels = NULL) {
  
  # Split breaks into individual endpoints
  split_x <- strsplit(breaks, ",")[[1]]
  
  # Remove all spaces
  split_x <- gsub(" ", "", split_x)
  
  # Split into lower and upper bounds
  lowers <- split_x[seq(1, length(split_x), 2)]
  uppers <- split_x[seq(2, length(split_x), 2)]
  if (is.null(labels)) {
    labels <- paste(lowers, uppers, sep = ", ")
  } else if (is.logical(labels) && ! labels) {
    labels <- 1: length(lowers)
  }
  
  # Extract numeric lower and upper bound
  lbounds <- sapply(lowers, function(x) as.numeric(substring(x, 2)))
  ubounds <- sapply(uppers, function(x) as.numeric(substring(x, 1, nchar(x) - 1)))
  
  # Extract lower and upper "symbols"
  lsymbols <- ifelse(grepl("[", lowers, fixed = TRUE), ">=", ">")
  usymbols <- ifelse(grepl("]", uppers, fixed = TRUE), "<=", "<")
  
  # Get locations of each category
  locs <- mapply(
    FUN = function(ls, lb, us, ub) eval(parse(text = paste("which(x", ls, lb, "&", "x", us, ub, ")"))),
    ls = lsymbols, lb = lbounds, us = usymbols, ub = ubounds
  )
  
  # Return error if any index shows up multiple times
  if (any(table(unlist(locs)) > 1)) {
    stop("Some values in 'x' map to multiple categories. Please check 'breaks'")
  }
  
  y <- c()
  for (ii in 1: length(labels)) {
    y[locs[[ii]]] <- labels[ii]
  }
  if (is.integer(labels)) return(y)
  factor(y, levels = labels, labels = labels)
  
}
