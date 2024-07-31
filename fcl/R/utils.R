#' @importFrom ymd ymd
NULL

prepare_args <- function(..., .len = NULL) {
  args <- list(...)
  if (is.null(.len)) {
    lens <- vapply(args, length, integer(1))
    .len <- max(lens)
  }
  rep_n <- function(x) {
    if (length(x) == 1L) {
      rep(x, .len)
    } else if (length(x) == .len) {
      x
    } else {
      msg <- "all arguments must be length 1"
      if (.len != 1) {
        msg <- sprintf("%s or %d", msg, .len)
      }
      stop(msg, call. = FALSE)
    }
  }
  args <- lapply(args, rep_n)
  args
}
