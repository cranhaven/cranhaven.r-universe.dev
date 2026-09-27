# R/methods-compat.R
#
# S7 methods that enable IDE compatibility features.
#
# These methods make survey design objects behave like data frames for the
# purposes of column name autocomplete in RStudio and Positron. When a
# survey object is the left-hand side of a pipe, IDEs call names() to
# discover column names and offer them as completions in tidy-select
# arguments of analysis functions like get_means(), get_freqs(), etc.
#
# Class defined in R/core-classes.R

# names() for survey design objects
#
# Returns the column names of the underlying data frame, enabling IDE
# column name completion in analysis functions. IDEs inspect the first
# argument of a pipe chain via names() to build the completion list for
# tidy-select arguments in downstream calls like get_means() or get_freqs().
#
# @param x A survey design object.
# @return A character vector of column names from x@data.
# Class defined in R/core-classes.R
S7::method(names, survey_base) <- function(x) names(x@data)


# ── broom tidy / glance for survey_variance ──────────────────────────────────
#
# These S3 shims are registered in zzz.R under the broom generics. They
# provide minimal compatibility so `broom::tidy()` and `broom::glance()`
# work on `get_variance()` results without invoking broom's default
# data.frame fallbacks. No new semantics — tidy() returns the result as a
# plain tibble; glance() returns a 1-row summary.

# @keywords internal
# @noRd
tidy.survey_variance <- function(x, ...) {
  tibble::as_tibble(x)
}

# @keywords internal
# @noRd
glance.survey_variance <- function(x, ...) {
  tibble::tibble(
    n_rows = nrow(x),
    n_vars = length(unique(x$name))
  )
}


# ── broom tidy / glance for survey_covariance ────────────────────────────────
#
# Same minimal shim pattern as `survey_variance`. Registered in zzz.R under
# the broom generics. tidy() returns the result as a plain tibble; glance()
# returns a 1-row summary with row count and pair count (n_pairs = nrow(x)).

# @keywords internal
# @noRd
tidy.survey_covariance <- function(x, ...) {
  tibble::as_tibble(x)
}

# @keywords internal
# @noRd
glance.survey_covariance <- function(x, ...) {
  tibble::tibble(
    n_rows = nrow(x),
    n_pairs = nrow(x)
  )
}
