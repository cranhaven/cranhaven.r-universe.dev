# Internal function to derive the estimated computation time for a test. 
# The function transforms the estimated seconds into day/hour/minute/second 
# format. Called by gofCheckTime
.get.time <- function(x) {
  x.day <- floor(x / 86400)
  x.remainder <- x %% 86400
  x.hour <- floor(x.remainder / 3600)
  x.remainder <- x.remainder %% 3600
  x.min <- floor(x.remainder / 60)
  x.remainder <- x.remainder %% 60
  out <- list(x.day, x.hour, x.min, x.remainder)
  class(out) <- "goftime"
  if (any(!is.na(unlist(out)))) {
    return(out)
  }
}

#' Printing function for an object of class goftime. goftime objects are created
#' by the function gofCheckTime.
#' @param x An object of class goftime
#' @param ... Additional arguments passed to print
#' @export
print.goftime <- function(x, ...) {
cat(sprintf(
"The computation will take approximately %d d, %d h, %d min and %d sec.", 
x[[1]], x[[2]], x[[3]], x[[4]]), 
fill = TRUE)
}
