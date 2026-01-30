#' Mapping numeric vector to a matrix
#' 
#' Maps a vector (pixel of a satellite time series) to a matrix.
#' 
#' @param         x a numeric vector whose length must be a multiple of \code{lenPeriod}
#' @param lenPeriod a numeric, number of observations per period
#' 
#' @export
#' 
#' @return A matrix with \code{nrow} equal to \code{length(x)/lenPeriod} and
#' \code{ncol} equal to \code{lenPeriod}.
#' 
#' @seealso \code{\link[sephora]{fill_initialgap_MOD13Q1}}, \code{\link[sephora]{phenopar}}, 
#' \code{\link[sephora]{vecFromData}}.
#' 
vecToMatrix <- function(x, lenPeriod=23){
  
  if(length(x) %% lenPeriod !=0){
    stop("Length of 'x' must be a multiple of 'lenPeriod'")
  }
  
  output <- matrix(nrow=length(x)/lenPeriod, ncol=lenPeriod)
  
  for(i in seq_len(nrow(output))){
    output[i,] <- x[((i-1) * lenPeriod + 1):(i * lenPeriod)]
  }

output
}
