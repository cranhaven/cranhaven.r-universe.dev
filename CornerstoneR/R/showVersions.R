#' @title Show Versions of R and CornerstoneR
#' @description Write the versions of R and CornerstoneR in a Cornerstone dataset.
#' @template returnResults
#' @return
#'   Logical [\code{TRUE}] invisibly and outputs to Cornerstone or, 
#'   if \code{return.results = TRUE}, \code{\link{list}} of 
#'   resulting \code{\link{data.frame}} objects:
#'   \item{versions}{
#'     Dataset with versions of R and CornerstoneR.
#'   }
#' @export
#' @examples
#'   res = showVersions(return.results = TRUE)
#'   res$versions
showVersions = function(return.results = FALSE) {
  assertFlag(return.results)
  
  nRows = 3
  dtVersions = data.table(Name = character(nRows), Value = character(nRows))
  
  dtVersions[1, `:=` (Name = "R Version", Value = paste0(version$major, ".", version$minor, " ("
                                                         , version$year, "-", version$month, "-", version$day, ")"
                                                         ))]
  dtVersions[2, `:=` (Name = "R Platform", Value = version$platform)]
  dtVersions[3, `:=` (Name = "CornerstoneR Version", Value = as.character(utils::packageVersion("CornerstoneR")))]
  
  # Export to Cornerstone
  cs.out.dataset(dtVersions, "Versions")
  # return results
  if (return.results) {
    return(list(versions = dtVersions))
  } else {
    invisible(TRUE)
  }
}
