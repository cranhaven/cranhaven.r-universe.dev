scale_to_01 <- function(x, one_value = "omit"){
  stopifnot(is.numeric(x), is.matrix(x), one_value %in% c("omit", "fail"))
  X <- as.matrix(x)
  unique_counts <- apply(X, MARGIN = 2, FUN = function(x) length(unique(x)))
  # message(paste(unique_counts))
  stopifnot(is.numeric(unique_counts))
  if(any(unique_counts == 1)){
    # message("1")
    if(identical(one_value, "omit")){
      # message("2")
      X <- X[,which(unique_counts != 1), drop = FALSE]
    } else {
      stop("At least one column of sorting matrix has only one value")
    }
  }
  # message("dimensions of X: ", dim(X))
  col_mins <- apply(X, 2, min)
  X <- sweep(X, 2, col_mins)
  col_maxs <- apply(X, 2, max)
  X <- sweep(X, 2, col_maxs, FUN = "/")
  return(X)
}
