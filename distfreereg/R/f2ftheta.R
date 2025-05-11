f2ftheta <- function(f, X, n){
  stopifnot(is.function(f))
  if("x" %in% names(formals(f))){
    validate_numeric(x = X)
    if(!is.matrix(X)){
      message("Coercing X to matrix...")
      X <- as.matrix(X)
    }
    output <- function(theta) apply(X, MARGIN = 1, FUN = f, theta = theta)
  } else {
    if("X" %in% names(formals(f))){
      output <- function(theta) f(X = X, theta = theta)
    } else {
      stopifnot(is.numeric(n), n > 0)
      # if(!is.null(X)) message("Supplied X ignored")
      output <- function(theta) rep(unname(theta), n)
    }
  }
  return(output)
}