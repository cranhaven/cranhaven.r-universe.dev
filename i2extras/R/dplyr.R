# The following functions are needed to make [incidence2_fit] objects work
# nicely with dplyr.  It is based on the
# (guide)[(https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat)]
# by Davis Vaughan.  The idea is to think to an object in terms of it's
# invariants (structural information that must be true for an object to be of
# class incidence). Where an operation breaks these invariants a tibble is
# returned instead of an incidence object.


#' Check whether incidence2_fit object invariants hold
#'
#' @param x data.frame to have it's invariants checked
#' @param to [incidence2_fit] object we want
#'
#' @return TRUE or FALSE
#'
#' @noRd
incidence2_fit_can_reconstruct <- function(x, to) {

  x_names <- names(x)

  # check groups are present
  groups <- attr(to, "groups")
  if (!is.null(groups)) {
    if (!(all(groups %in% x_names))) {
      return(FALSE)
    }
  }

  # check count_variable is present
  count_variable <- attr(to, "count_variable")
  if (!(count_variable %in% x_names)) {
    return(FALSE)
  }

  # check data is present
  data_variable <- attr(to, "data")
  if (!(data_variable %in% x_names)) {
    return(FALSE)
  }

  # check model is present
  model <- attr(to, "model")
  if (!(model %in% x_names)) {
    return(FALSE)
  }

  # check fitted is present
  fitted_var <- attr(to, "fitted")
  if (!all(fitted_var %in% x_names)) {
    return(FALSE)
  }

  # check error_vars are present
  error_vars <- attr(to, "error_vars")
  if (!is.null(error_vars)) {
    if (!(all(error_vars %in% x_names))) {
      return(FALSE)
    }
  }

  # check error_vars are present
  warning_vars <- attr(to, "warning_vars")
  if (!is.null(warning_vars)) {
    if (!(all(warning_vars %in% x_names))) {
      return(FALSE)
    }
  }

  # ensure no rows are duplicated within x
  if (anyDuplicated((x))) {
    return(FALSE)
  }

  TRUE
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' Function to reconstruct object of incidence2_fit class
#'
#' Once you have encoded the invariant logic into incidence2_fit_can_reconstruct,
#' we need a second function that applies that check and either performs the
#' actual reconstruction, or falls back to a bare tibble.
#'
#' @param x x data.frame to have it's invariants checked
#' @param to object we want
#'
#' @noRd
incidence2_fit_reconstruct <- function(x, to) {
  if (incidence2_fit_can_reconstruct(x, to)) {
    df_reconstruct(x, to)
  } else {
    new_bare_tibble(x)
  }
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# This function is a data frame specific helper.  Currently we are recommended
# to copy in to our own package but it may evenutally find it's way in to one of
# the tidy packages. See:
# https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat
df_reconstruct <- function(x, to) {
  attrs <- attributes(to)

  # Keep column and row names of `x`
  attrs$names <- names(x)
  attrs$row.names <- .row_names_info(x, type = 0L)

  # Otherwise copy over attributes of `to`
  attributes(x) <- attrs
  x
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# new_bare_tibble() is a small wrapper around tibble::new_tibble() that also
# forces extra attributes to be dropped through the use of
# vctrs::new_data_frame(). In the future, new_tibble() might have an option
# to do this directly. See:
# https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat
new_bare_tibble <- function(x) {
  # Strips all attributes off `x` since `new_tibble()` currently doesn't
  x <- vctrs::new_data_frame(x)
  new_tibble(x, nrow = nrow(x))
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Need to define a few base R methods to ensure things work as expected

#' @export
`[.incidence2_fit` <- function(x, i, j, ...) {
  out <- NextMethod()
  incidence2_fit_reconstruct(out, x)
}

#' @export
`[<-.incidence2_fit` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  incidence2_fit_reconstruct(out, x)
}

#' @export
`names<-.incidence2_fit` <- function(x, value) {
  current_names <- names(x)

  model_var <- attr(x, "model")
  model_index <- which(current_names %in% model_var)
  attr(x, "model") <- value[model_index]

  fitted_var <- attr(x, "fitted")
  fitted_index <- which(current_names %in% fitted_var)
  attr(x, "fitted") <- value[fitted_index]

  group_vars <- attr(x, "groups")
  if (!is.null(group_vars)) {
    group_index <- which(current_names %in% group_vars)
    attr(x, "groups") <- value[group_index]
  }

  count_variable_var <- attr(x, "count_variable")
  count_index <- which(current_names %in% count_variable_var)
  attr(x, "count_variable") <- value[count_variable_var]

  data_var <- attr(x, "data")
  data_index <- which(current_names %in% data_var)
  attr(x, "data") <- value[data_index]

  error_vars <- attr(x, "error_vars")
  if (!is.null(error_vars)) {
    error_index <- which(current_names %in% error_vars)
    attr(x, "error_vars") <- value[error_index]
  }

  warning_vars <- attr(x, "warning_vars")
  if (!is.null(warning_vars)) {
    warning_index <- which(current_names %in% warning_vars)
    attr(x, "warning_vars") <- value[warning_index]
  }

  out <- NextMethod()
  incidence2_fit_reconstruct(out, x)
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Registered in `.onLoad()` in zzz.R
dplyr_reconstruct_incidence2_fit <- function(data, template) {
  incidence2_fit_reconstruct(data, template)
}
# -------------------------------------------------------------------------
