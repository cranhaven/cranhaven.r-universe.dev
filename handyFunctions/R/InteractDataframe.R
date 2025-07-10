
#' merge two data.frame based on xcol and ycol
#'
#' @param x the first data.frame
#' @param y the second data.frame
#' @param xcol colnames which you want to merged in first data.frame
#' @param ycol colnames which you want to merged in second data.frame
#'
#' @return return the new data.frame merged
#' @export
#'
#' @importFrom stringr str_match
#'
#' @examples
#' library(handyFunctions)
#' data(people)
#' data(grade)
#' mergeCustom(people, grade, "..name", "name")
#'
mergeCustom <- function(x, y, xcol, ycol) {
  tempx <- x
  tempy <- y
  colnames(tempy)[which(colnames(tempy) == ycol)] <- xcol
  result <- merge(tempx, tempy, by = xcol)
  return(result)
}
#
#' return index of x data.frame with the given vector/list or ycol in data.frame
#' (if set the accurate match or not)
#'
#' @param SourceData the source data.frame which you want to query
#' @param sourceCol the col names or index of query field in source data.frame
#' @param queryCol the col names or index of return field in source data.frame
#' @param queryInfo vector/list the query info
#' @param queryType logical if set it to accurate match (default: TRUE)
#'
#' @return a vector in query field matched with query info in source data
#' @export
#'
#' @examples
#' library(handyFunctions)
#' data(grade)
#' queryingInfo(grade, "name", "chinese", c("Ming Li", "Bang Wei"))
#'
queryingInfo <- function(SourceData, sourceCol, queryCol, queryInfo, queryType = TRUE) {
  SourceData <- SourceData[(!duplicated(SourceData[, c(sourceCol, queryCol)])), ]
  queryInfo <- as.character(queryInfo)
  index <- matchIndex(SourceData[, sourceCol], queryInfo, queryType)

  return(SourceData[, queryCol][index])
}

#' Return the index of source vector matched with query vector
#'
#' @param SourceInfo the source vector
#' @param queryInfo the query vector
#' @param queryType logical If set it to accurate match (default: TRUE)
#'
#' @return the index of source vector matched with query vector
#' @export
#'
#' @examples
#' library(handyFunctions)
#' data(grade)
#' matchIndex(grade[, "name"], c("Ming Li", "Bang Wei"))
#'
matchIndex <- function(SourceInfo, queryInfo, queryType = TRUE) {

  # SourceData = SourceData[(!duplicated(SourceData[,c(sourceCol,queryCol)])),]
  queryInfo <- as.character(queryInfo)
  SourceInfo <- as.character(SourceInfo)

  if (queryType == TRUE) {
    index <- unlist(lapply(queryInfo, function(x) {
      which(SourceInfo == x)
    }))
    # index
    return(index)
  } else {
    index <- unlist(lapply(queryInfo, function(x) {
      !is.na(str_match(SourceInfo, x))
    }))

    return(index)
  }
}
