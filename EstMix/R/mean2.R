#' trancate data and take mean
#' @keywords internal
#' @export
mean2 = function(x){
  x = x[!is.na(x)]
  x_new = sort(x)
  l = length(x)
  if(l<5) warning("The vector has less than 5 non-na values!")
  low = ceiling(l*.05)
  high = l-low
  x[1:low] = x[low+1]
  x[high:l] = x[high-1]
  return(mean(x))
}
