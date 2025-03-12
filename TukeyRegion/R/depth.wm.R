depth.wm <- function(data, depth.level = 1 / nrow(data), weighted = TRUE, 
                     break.ties = "atRandom", ...){
  # Input consistency check
  if (!is.matrix(data)){
    stop("'Argument 'data' should be a matrix")
  }
  if (ncol(data) < 2 || nrow(data) <= ncol(data)){
    stop(paste("Argument 'data' should be a matrix with at least d = 2 columns", 
               "and at least d + 1 rows"))
  }
  if (depth.level < 1 / nrow(data)){
    stop(paste("Argument 'depth.level' should be greater than", 
               "1/(number of rows in 'data')"))
  }
  if (depth.level > nrow(data)){
    stop(paste("Argument 'depth.level' should not be greater than", 
               "number of rows in 'data'"))
  }
  # Calculate depth
  depths <- depth.halfspace(data, data, ...)
  if (depth.level < 1){ # If we select points by their depth values
    num <- sum(depths >= depth.level)
    # Check whether points with such depth level exist
    if (num > 0){
      # Calculate the (weighted) mean
      if (weighted){
        ctr <- t(depths[depths >= depth.level]) %*% 
          data[depths >= depth.level,] / sum(depths[depths >= depth.level])
      }else{
        ctr <- colMeans(data[depths >= depth.level,,drop = FALSE])
      }
      return(as.vector(ctr))
    }else{
      stop("No points possessing required depth level.")
    }
  }else{ # If we select 'depth.level' deepest points
    n <- nrow (data)
    lowestDepth <- sort(depths, decreasing = TRUE)[depth.level]
    iAbove <- which(depths > lowestDepth)
    # Determine tied points having equal depth level and treat them specially
    iEqual <- which(depths == lowestDepth)
    if (break.ties == "atRandom"){
      indices <- c(iAbove, sample(iEqual, depth.level - length(iAbove)))
    }else{
      indices <- c(iAbove, iEqual[seq(depth.level - length(iAbove))])
    }
    # Calculate the (weighted) mean
    if (weighted){
      ctr <- t(depths[indices]) %*% data[indices,] / 
        sum(depths[indices])
    }else{
      ctr <- colMeans(data[indices,,drop = FALSE])
    }
    return(as.vector(ctr))
  }
}
