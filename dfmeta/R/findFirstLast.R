#' @export 
findFirstLast <-
function(myDF, findFirst=TRUE) {
  # myDF should be a data frame or matrix 
  
  # By default, this function finds the first occurence of each unique value in a column
  # If instead we want to find last, set findFirst to FALSE.  
  # This below, will give `maxOrMin` a value of -1 finding the min of the negative indecies is the same as finding the max of the positive indecies. 
  maxOrMin <- ifelse(findFirst, 1, -1) 
  
  # For each column in myDF, make a list of all unique values (`levs`) and iterate over that list, 
  #   finding the min (or max) of all the indicies of where that given value appears within the column  
  apply(myDF, 2, function(colm) {
    levs <- unique(colm)
    sapply(levs, function(lev) {
      inds <- which(colm==lev)
      ifelse(length(inds)==0, NA, maxOrMin*min(inds*maxOrMin) ) 
    })   
  })
}
