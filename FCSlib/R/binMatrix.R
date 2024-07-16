#' @title Binned representation of a matrix
#' 
#' @description Groups each column in a matrix into several bins of a given length for better (and faster) data plotting
#' @aliases binMatrix
#' @usage binMatrix(f, lineTime, nIntervals, columns, mode = "mean", plot = TRUE)
#' @param f A vector or a matrix
#' @param lineTime Line (row) acquisition rate (in seconds)
#' @param nIntervals Number of intervals into which the all columns will be grouped
#' @param columns Number of columns of the resulting binned matrix
#' @param mode Set to "mean" (default) or "sum" to average or sum all the points in every interval, respectively
#' @param plot Boolean, set to TRUE (default) to plot the result
#' @details This function groups all the points in each column of the matrix 'f' into 'nIntervals' bins of length = length(f)/nIntervals.
#' Then, averages or sums all of the points in each bin and plots the result.
#' If 'f' is a vector, 'columns' is used to build the resulting matrix.
#' If 'f' is a matrix, then 'columns' takes the value of the number of columns in 'f'.
#'
#' @export
#' @import fields
#' @return A matrix of 'nIntervals' rows
#' @author Alejandro Linares
#' 
#' @seealso \code{\link{binTimeSeries}}
#' 
#' @examples
#' \donttest{
#' ### Please navigate to
#' ### (https://github.com/FCSlib/FCSlib/tree/master/Sample%20Data)
#' ### to find this sample data
#' 
#' # Automatic plot
#' x <- read.table("Pax.dat")
#' x <- binMatrix(x[,1], lineTime =  1e-3, nIntervals =  500,
#'                columns = 64, mode = "mean", plot = T)
#' 
#' # Manual plot (useful for adding custom labels)
#' x <- read.table("Pax.dat")
#' x <- binMatrix(x[,1], lineTime =  1e-3, nIntervals =  500,
#'                columns = 64, mode = "mean", plot = F)
#' image.plot(x)
#' }

binMatrix <- function(f, lineTime, nIntervals, columns, mode = "mean", plot = TRUE){
  if (is.vector(f)){
    rdm <- matrix(f, nrow = columns)
  } else if (is.matrix(f)){
    rdm <- f
    columns <- min(dim(f))
  } else stop("'f' must be a vector or a matrix")
  
  if (nIntervals < 2){
    stop("'nIntervals' must be a positive integer and greater than 1")
  }
  
  rawrows <- floor(length(f)/columns)
  stepSize <- floor(rawrows/nIntervals)
  sbdm <- abdm <- matrix(0, nrow = columns, ncol = nIntervals)
  
  vtau <- 0
  for (i in 2:nIntervals){
    vtau[i] <- (vtau[i-1]) + lineTime*stepSize
  }
  
  for (i in 1:columns){
    b <- c <- NULL
    x <- 1
    a <- rdm[i,]
    for (j in 1:nIntervals){
      b[j] <- sum(a[x:(j*stepSize)])
      c[j] <- mean(a[x:(j*stepSize)])
      x <- x + stepSize
    }
    sbdm[i,] <- b
    abdm[i,] <- c
  }
  
  if (mode == "mean" | mode == "MEAN"){
    result <- abdm
  } else result <- sbdm
  
  if (plot){
    if (mode == "mean" | mode == "MEAN"){
      image.plot(x = (1:columns), y = vtau, z = result, xlab = "Pixel", ylab = "Time (s)",
                 main = paste0("Binned matrix (averaged), ", nIntervals, " intervals of ", stepSize, " lines each"), cex.main = 0.9)
    } else image.plot(x = (1:columns), y = vtau, z = result, xlab = "Pixel", ylab = "Time (s)",
                      main = paste0("Binned matrix (added), ", nIntervals, " intervals of ", stepSize, " lines each"), cex.main = 0.9)
  }
  
  return(result)
}
