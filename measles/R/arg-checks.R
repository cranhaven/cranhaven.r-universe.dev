# This file contains functions for checking the arguments
# of the exported package functions.


# Checks if argument contains NA values
stopifany_na <- function(x) {
  if (any(is.na(x))) {
    if (length(x) > 1) {
      stop(paste(match.call()$x, "must not contain NA values."))
    } else {
      stop(paste(match.call()$x, "must not be NA."))
    }
  }
}

# Checks if argument is an integer
# - Also optionally checks argument is within range [lb, ub]
stopifnot_int <- function(x, lb = NULL, ub = NULL) {
  stopifany_na(x)

  if (!is.numeric(x) || !all.equal(x, as.integer(x))) {
    stop(
      paste(match.call()$x, "must be an integer, but is of class(es): "),
      paste(class(x), collapse = ", ")
    )
  }

  if (!is.null(lb) && any(x < lb)) {
    stop(
      paste(match.call()$x, "must be greater than or equal to", lb, ", but is:"),
      paste(x[x < lb], collapse = ", ")
    )
  }

  if (!is.null(ub) && any(x > ub)) {
    stop(
      paste(match.call()$x, "must be less than or equal to", ub, ", but is:"),
      paste(x[x > ub], collapse = ", ")
    )
  }
}

# Checks if argument is a double
# - Also optionally checks argument is within range [lb, ub]
stopifnot_double <- function(x, lb = NULL, ub = NULL) {
  stopifany_na(x)

  if (!is.numeric(x)) {
    stop(
      paste(match.call()$x, "must be a double, but is of class(es): "),
      paste(class(x), collapse = ", ")
    )
  }

  if (!is.null(lb) && any(x < lb)) {
    stop(
      paste(match.call()$x, "must be greater than or equal to", lb, ", but is:"),
      paste(x[x < lb], collapse = ", ")
    )
  }

  if (!is.null(ub) && any(x > ub)) {
    stop(
      paste(match.call()$x, "must be less than or equal to", ub, ", but is:"),
      paste(x[x > ub], collapse = ", ")
    )
  }
}

# Checks if model object is of class "epiworld_model"
stopifnot_model <- function(model) {
  if (!inherits(model, "epiworld_model")) {
    stop(
      "The -model- object must be of class 'epiworld_model'. ",
      "The object passed to the function is of class(es): ",
      paste(class(model), collapse = ", ")
    )
  }
}
