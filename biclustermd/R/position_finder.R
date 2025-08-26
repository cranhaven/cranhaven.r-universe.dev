#' Find the index of the first nonzero value in a vector
#' 
#' @param vec A binary vector
#' @return Position of the first nonzero value in a vector.

position_finder <- function(vec) {
  if(length(which(vec == 1)) != 0) {
    return(min(which(vec == 1)))
  } else {
    return(999999)
  }
}