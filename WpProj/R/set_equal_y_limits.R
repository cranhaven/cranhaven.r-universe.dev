set_equal_y_limits <- function(x) {
  stopifnot(is.list(x))
  classes <- sapply(x, class)
  if(!all(apply(classes,1,function(X) length(unique(X))==1))) {
    stop("All members of the list must have the same class")
  }
  class(x) <- classes[1]
  UseMethod("set_equal_y_limits", x)
}
