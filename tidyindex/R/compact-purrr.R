# nocov start - compat-purrr.R
# Latest version: https://github.com/r-lib/rlang/blob/main/R/compat-purrr.R

# This file provides a minimal shim to provide a purrr-like API on top of
# base R functions. They are not drop-in replacements but allow a similar style
# of programming.
#
# Changelog:
#
# 2022-06-07:
# * `transpose()` is now more consistent with purrr when inner names
#   are not congruent (#1346).
#
# 2021-12-15:
# * `transpose()` now supports empty lists.
#
# 2021-05-21:
# * Fixed "object `x` not found" error in `imap()` (@mgirlich)
#
# 2020-04-14:
# * Removed `pluck*()` functions
# * Removed `*_cpl()` functions
# * Used `as_function()` to allow use of `~`
# * Used `.` prefix for helpers

map <- function(.x, .f, ...) {
  .f <- as_function(.f, env = global_env())
  lapply(.x, .f, ...)
}

globalVariables("global_env")
# nocov end
