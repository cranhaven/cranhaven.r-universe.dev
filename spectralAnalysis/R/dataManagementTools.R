#' @importFrom  plyr   aaply  llply  laply  alply  
NULL


# Some internal functions are defined first, which will be used in the main functions of this R file

strsplitVectorElement <- function(x, splitter, element){
  unname(aaply(x, 1, 
    function(xx) unlist(strsplit(xx, splitter, fixed = T))[element]
  ))
}	

substrLeft <- function(x, n){
	unname(aaply(x, 1, 
					function(xx) substr(xx, 1, n)
			))
}

substrMiddle <- function(x, nRemoveLeft, nRemoveRight){
  xWithoutRightPart <- unname(aaply(as.matrix(x), 1, 
    function(xx) substr(xx, 1, nchar(xx)-nRemoveRight)
  ))
  unname(aaply(as.matrix(xWithoutRightPart), 1, 
    function(xx) substr(xx, 1+nRemoveLeft, nchar(xx))
  ))
}

substrRight <- function(x, n){
  unname(aaply(as.matrix(x), 1, 
    function(xx) substr(xx, (nchar(xx)-n+1), nchar(xx))  
  ))
}

positionMatchingIDs <- function(x, uniqueIDs){
  unname(aaply(as.matrix(x), 1, 
    function(xx){
      result <- which(uniqueIDs == xx)
      if(!length(result)) result <- NA
      result
    }
  ))
}

strsplitVectorLength <- function(x, splitter){
  unname(aaply(x, 1, 
          function(xx) length(unlist(strsplit(xx, splitter, fixed = T)))
      ))
}

strsplitVectorLength <- function(x, splitter){
  unname(aaply(x, 1, 
          function(xx) length(unlist(strsplit(xx, splitter, fixed = T)))
      ))
}

selectPartsStrings <- function(allFiles, splitter, selectedElements){
  result <- rep(NA, length(allFiles))
  for(iSplit in 1:length(allFiles)){
    result[iSplit] <- paste(unlist(strsplit(allFiles[iSplit], splitter, fixed = T))[selectedElements[[iSplit]]], collapse = splitter)
  }
  result
}

# The standard method in R is way to slow such that a method adapted to our specific needs was developed, hereby increasing the calculation speed by a factor of 7.25

timeInSeconds <- function(x, element){
  unname(aaply(x, 1, function(xx)
    sum(as.numeric(unlist(strsplit(xx, ":", fixed = T)))*c(3600, 60, 1))
  ))
}	



