#' @export
rgumbel <- function(n, location = 0, scale = 1){
  if(any(scale <= 0)) stop("standard deviation must be strictly > 0.")
  scale*log(-log(1 - runif(n))) + location
}