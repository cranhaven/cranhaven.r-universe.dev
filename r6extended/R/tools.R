
#' function to get hash for R objects
#' @param x the thing to hash
#' @keywords internal
r6e_hash <- function(x){
  digest::digest(x, algo="xxhash64")
}