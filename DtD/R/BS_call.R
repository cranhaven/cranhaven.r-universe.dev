#' @rdname get_underlying
#' @export
BS_call <- function(V, D, T., r, vol){
  lens <- c(length(V), length(D), length(T.), length(r), length(vol))
  if(all(lens == 1)) # return quickly
    return(drop(BS_call_cpp(V = V, D = D, T = T., r = r, vol = vol)))

  # make sure all args have same length and call cpp code
  .check_args(V = V, D = D, T. = T., r = r, vol = vol)
  args <- .get_eq_length_args(
    lens = lens, V = V, D = D, T = T., r = r, vol = vol)
  with.default(
    args, drop(mapply(BS_call_cpp, V = V, D = D, T = T, r = r, vol = vol)))
}


