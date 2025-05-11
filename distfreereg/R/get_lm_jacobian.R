get_lm_jacobian <- function(m){
  stopifnot(is(m, "lm"), !is.null(m[["x"]]))
  return(m[["x"]])
}
