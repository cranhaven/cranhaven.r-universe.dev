# Utility functions used in other files

#' @importFrom Rcpp evalCpp
#' @useDynLib cppcontainers, .registration = TRUE

check_length <- function(i) {
  if(length(i) != 1L) {
    stop("The arguments must each be of length one.")
  }
}

get_type <- function(x) {
  if(methods::.hasSlot(x, "type")) {
    return(x@type)
  } else {
    return(x@key_type)
  }
}

check_type <- function(x_type, i, index) {
  if(index) {
    if(!is.numeric(i)) {
      stop("from and to must be numeric indices.")
    }
  } else {
    type_match <- switch(x_type,
      integer = is.numeric(i),
      double = is.numeric(i),
      string = is.character(i),
      boolean = is.logical(i),
      FALSE
    )
    if(!type_match) {
      stop("from and to are not of the same data type as x.")
    }
  }
}

assign_na <- function(x_type) {
  return(switch(x_type,
    integer = NA_integer_,
    double = NA_real_,
    string = NA_character_,
    boolean = NA
  ))
}

check_insert_value <- function(value) {
  if(is.numeric(value)) {
    if(!is.finite(value[1L])) {
      stop("Only finite numbers allowed.")
    }
  } else if(is.logical(value)) {
    if(is.na(value[1L])) {
      stop("NA is not allowed.")
    }
  }
}

check_insert_values <- function(values) {
  if(is.numeric(values)) {
    if(any(!is.finite(values))) {
      stop("Only finite numbers allowed.")
    }
  } else if(is.logical(values)) {
    if(anyNA(values)) {
      stop("NA is not allowed.")
    }
  }
}
