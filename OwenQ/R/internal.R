isNotPositiveInteger <- function(x){
  !is.numeric(x) || x<1 || (floor(x) != x)
}
