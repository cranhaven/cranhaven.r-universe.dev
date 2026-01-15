isFraction <- function(x) {
  if(!is.character(x) || length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  x <- trimws(x)
  if(grepl("^\\-*\\d+$", x)) {
    return(TRUE)
  }
  nd <- trimws(strsplit(x, "/")[[1L]])
  if(length(nd) != 2L) {
    FALSE
  } else {
    n <- nd[1L]
    if(!grepl("^\\-*\\d+$", n)) {
      FALSE
    } else {
      d <- nd[2L]
      if(!grepl("^\\d+$", d) || grepl("^0+$", d)) {
        FALSE
      } else {
        TRUE
      }
    }
  }
}

checkM <- function(M, square = FALSE) {
  if(is.matrixZQ(M)) {
    M <- as.character(M)
    if(anyNA(M)) {
      stop("Found missing values in `M`.")
    }
  } else {
    stopifnot(is.matrix(M))
    storage.mode(M) <- "character"
    check <- all(vapply(M, isFraction, logical(1L)))
    if(!check) {
      stop("Invalid matrix `M`.")
    }
  }
  if(square) {
    stopifnot(nrow(M) == ncol(M))
  }
  M
}
