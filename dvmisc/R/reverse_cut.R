#' Reverse Cut
#' 
#' Convenience function to get reversed factor levels from \code{\link{cut}}. 
#' Currently requires specifying \code{breaks} as vector of cutpoints rather 
#' than number of desired intervals.
#' 
#' @param x,breaks,include.lowest,right See \code{\link{cut}}.
#' specifying number of intervals is not currently supported).
#' @param ... Arguments to pass to \code{\link{cut}}.
#' 
#' @return Factor variable.
#' 
#' @examples 
#' # In mtcars dataset, create 3 mpg groups
#' table(cut(mtcars$mpg, breaks = c(-Inf, 15, 20, Inf)))
#' 
#' # Repeat with reverse_cut to get factor levels ordered from high to low
#' table(reverse_cut(mtcars$mpg, breaks = c(Inf, 20, 15, -Inf)))
#' 
#' # You can specify breaks from low to high, but then include.lowest and right 
#' # arguments get confusing
#' table(reverse_cut(mtcars$mpg, breaks = c(-Inf, 15, 20, Inf), right = TRUE))
#' 
#' @export
reverse_cut <- function(x, breaks, include.lowest = FALSE, right = TRUE, ...) {
  
  breaks <- sort(breaks, decreasing = TRUE)
  y <- cut(-x, -breaks, include.lowest = ! include.lowest, right = ! right, ...)
  
  # Use gsub to reverse polarity of factor levels
  levels_y <- levels(y)
  for (ii in 1: length(levels_y)) {
    levels_y.ii <- levels_y[ii]
    count <- 0
    for (jj in 1: length(breaks)) {
      count <- count + grepl(-breaks[jj], levels_y.ii)
      levels_y[ii] <- gsub(-breaks[jj], breaks[jj], levels_y[ii])
      if (count == 2) break
    }
  }
  levels(y) <- levels_y
  return(y)
  
}
