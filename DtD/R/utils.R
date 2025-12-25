#' @importFrom checkmate assert_integer
.get_eq_length_args <- function(lens, ...){
  args <- list(...)
  assert_integer(lens, lower = 1L, len = length(args))

  max_len <- max(lens)
  if(max_len == 0 || any(!lens %in% c(1L, max_len)))
    stop("Argument lengths are not valid")

  not_max <- which(lens != max_len)
  args[not_max] <- lapply(args[not_max], rep, times = max_len)
  args
}

#' @importFrom utils globalVariables
globalVariables(c("V", "S", "D", "T.", "r", "vol", "time", "tol", "eps"))

#' @importFrom checkmate assert_numeric assert_number
.check_args <- function(...){
  a <- list(...)

  with.default(a, {
    if(!is.null(a$V))
      assert_numeric(V   , lower = 1e-16, finite = TRUE)
    if(!is.null(a$S))
      assert_numeric(S   , lower = 1e-16, finite = TRUE)
    if(!is.null(a$D))
      assert_numeric(D   , lower = 1e-16, finite = TRUE)
    if(!is.null(a$T.))
      assert_numeric(T.  , lower = 1e-16, finite = TRUE)
    if(!is.null(a$r))
      assert_numeric(r                  , finite = TRUE)
    if(!is.null(a$vol))
      assert_numeric(vol , lower = 1e-16, finite = TRUE)
    if(!is.null(a$time)){
      assert_numeric(time               , finite = TRUE)
      stopifnot(all(diff(time) > 1e-16))
    }
    if(!is.null(a$tol))
      assert_number(tol      , lower = 1e-16, finite = TRUE)
    if(!is.null(a$eps))
      assert_number(eps      , lower = 1e-16, finite = TRUE)
  })

  invisible()
}
