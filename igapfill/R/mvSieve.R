#' Sieve of amount of missing values in a time series of satellite images
#' 
#' This function computes the number of pixels with missing values (no data) in each element
#' of a time series of satellite images. For practical purposes, this function assumes that
#' the images have been stored in a set of different sub-directories; each sub-directory can
#' represent a period/season/year.
#' 
#' @param        dirs character vector given sub-directory names from which images will be read.
#' @param filesPerDir numeric indicating how many images are stored in each directory. 
#' @param startPeriod numeric indicating in which period the time series first image was recorded.
#' @param   endPeriod numeric indicating in which period the time series last image was recorded.
#' @param    colNames character vector. Default \code{month.name} which assumes that \code{filesPerDir=12}.
#' 
#' @export
#' 
#' @importFrom gtools mixedsort
#' @importFrom terra rast
#' @importFrom terra global
#' 
#' @seealso \code{\link[igapfill]{minmaxBlock}}
#' 
#' @return An \eqn{n \times m} matrix where \eqn{n} is equal to \code{length(dirs)} and \eqn{m} is equal to
#' \code{filesPerDir}. By default, the row names of this matrix are equal to \code{startPeriod:endPeriod}
#' and the column names are equal to \code{colNames}.
#' 
mvSieve <- function(dirs, filesPerDir, startPeriod, endPeriod, colNames = month.name){
  
  percentMat <- matrix(nrow=length(dirs), ncol=filesPerDir)
  
  for(i in seq_len(nrow(percentMat))){
    TIFs <- mixedsort(list.files(path = dirs[i], full.names = TRUE))
    for(j in seq_len(ncol(percentMat))){
      r <- rast(TIFs[j])
      # temp <- spRast_valuesCoords(r, na_rm = TRUE)
      # percentMat[i,j] <- ifelse(nrow(temp$values) == 1, 0, nrow(temp$values))  # as.numeric(global(r, fun="isNA")) / totalPixels
      percentMat[i,j] <- as.numeric(global(r, fun="isNA")) # / totalPixels
    }
  }
  
  row.names(percentMat) <- startPeriod:endPeriod
  colnames(percentMat) <- colNames
  
  percentMat
  
}
