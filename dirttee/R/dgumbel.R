#' @export
dgumbel <-
function(x, location = 0, scale = 1){
  if(any(scale <= 0)) stop("standard deviation must be strictly > 0.")
  (1/scale)*exp((x - location)/scale - exp((x - location)/scale))
}
