isInteger <- function(x){
  is.numeric(x) && isTRUE(length(x) == 1) && isTRUE(all.equal(x, as.integer(x)))
}
