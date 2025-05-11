isPermutation <- function(x){
  isTRUE(is.numeric(x) && length(unique(x)) == length(x) && all(sort(x) == 1:length(x)))
}
