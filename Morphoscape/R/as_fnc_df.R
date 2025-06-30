as_fnc_df <- function(x, func.names = NULL, scale = TRUE){
  
  if (length(dim(x)) != 2) {
    stop("'x' must be a matrix or data frame.", call. = FALSE)
  }
  
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }
  
  if (is.null(func.names)) {
    func.names <- names(x)[-(1:2)]
  }
  else if (!is.character(func.names)) {
    stop("'func.names' must be a character vector of functional traits in 'x'.", call. = FALSE)
  }
  else if (!all(func.names %in% names(x)[-(1:2)])) {
    stop("All entries in 'func.names' must be names of non-coordinate columns in 'x'", call. = FALSE)
  }
  if (any(tolower(func.names) %in% c("x", "y"))) {
    stop("No 'func.names' can be named \"x\" or \"y\".", call. = FALSE)
  }
  x <- x[c(names(x)[1:2], func.names)]
  
  if (!all(vapply(x, is.numeric, logical(1L)))) {
    stop("All columns in 'x' containing coordinates and functional traits must be numeric.", call. = FALSE)
  }

  if (scale){
    for (i in func.names) {
      x[[i]] <- scale.z(x[[i]])
    }
  }
  names(x)[1:2] <- c("x", "y")
  attr(x, "func.names") <- func.names
  
  class(x) <- c("fnc_df", class(x))
  return(x)
}

#Scales variable to [0,1]
scale.z <- function(Z) {
  Z <- Z - min(Z, na.rm = TRUE)
  Z <- Z/max(Z, na.rm = TRUE)
  return(Z)
}