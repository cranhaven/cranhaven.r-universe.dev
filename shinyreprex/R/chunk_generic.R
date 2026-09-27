#' @description
#' For standard R objects, such as static numbers, strings, or lists, we don't need to
#' dive deeper to reproduce them so can just attach the object to the code so that
#' it can be extracted in other methods.
#'
#' @include repro_chunk.R
#' @noRd
S7::method(repro_chunk, S7::class_any) <- function(x, repro_code = Repro(), env = rlang::caller_env()) {
  repro_code@code <- x
  repro_code
}
