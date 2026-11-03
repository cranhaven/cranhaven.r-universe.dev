#' Compute the difference between mean of subvectors of two vectors
#'
#' @param i starting index
#' @param i_t final index
#' @param y first vector of elements
#' @param z second vector of elements
#'
#' @return a real, the difference between means of subvectors of two vectors
#' @export
#'
#' @examples
c_fun <- function(i,i_t, y ,z){
  res <- mean(y[(i_t+1):i]) -  mean(z[(i_t+1):i])
  return(res)
}
