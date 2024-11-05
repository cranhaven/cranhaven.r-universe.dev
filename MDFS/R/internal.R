prepare_decision <- function(decision) {
  if (!is.vector(decision) && !is.factor(decision)) {
    stop("Decision must be a vector, a list or a factor.")
  }

  # NOTE(yoctozepto): ``unlist`` undoes the list effects which otherwise break
  # ``order`` below. And yes, a list is a vector but does not behave like one.
  # Have I already mentioned R is quirky as duck?
  classes <- unlist(unique(decision))

  if (length(classes) != 2) {
    stop("Decision must be binary.")
  }

  # Reconstruct the decision the way we need for C code, i.e., (0, 1) only.
  result <- vector(mode = "integer", length = length(decision))
  # NOTE(yoctozepto): Getting max on unordered factors with ``max`` fails
  # but ordering works. Oh well! ;-)
  result[decision == classes[order(classes)][2]] <- as.integer(1)

  return(result)
}

prepare_integer_in_bounds <- function(x, x_desc, lower_bound, upper_bound = as.integer(2^31 - 1)) {
  if (!is.numeric(x)) {
    stop(paste(x_desc, "has to be an integer."))
  }

  result <- as.integer(x)

  if (result != x) {
    stop(paste(x_desc, "has to be an integer."))
  }

  if (result < lower_bound) {
    stop(paste(x_desc, "must be at least", lower_bound))
  }

  if (result > upper_bound) {
    stop(paste(x_desc, "must be at most", upper_bound))
  }

  return(result)
}

prepare_double_in_bounds <- function(x, x_desc, lower_bound, upper_bound = NULL) {
  if (!is.numeric(x)) {
    stop(paste(x_desc, "has to be a numeric value."))
  }

  result <- as.double(x)

  if (result != x) {
    stop(paste(x_desc, "has to be a numeric value."))
  }

  if (result < lower_bound) {
    stop(paste(x_desc, "must be at least", lower_bound))
  }

  if (!is.null(upper_bound) && result > upper_bound) {
    stop(paste(x_desc, "must be at most", upper_bound))
  }

  return(result)
}
