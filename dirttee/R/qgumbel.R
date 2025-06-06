#' @export
qgumbel <-
function(p, location = 0, scale = 1){
  if(any(scale <= 0)) stop("standard deviation must be strictly > 0.")
  location + scale * log(- log(1 - p))
}
