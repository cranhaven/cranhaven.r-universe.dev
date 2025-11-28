# The function is aimed to test whether several vectors being arguments of a
# function commensurable just to strictly prevent wrong values on the
# function input instead of default warnings. This function should be used in
# conjunction with `checkmate::assert_true`

#' @noRd
commensurable <- function(lengths){
  checkmate::assert_integer(
    lengths, lower = 1L, any.missing = FALSE, min.len = 2
  )
  x <- lengths[lengths > 1L]
  length(x) < 2L || !(stats::var(x) > 0)
}
