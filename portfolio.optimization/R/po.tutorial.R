#' @title Open a specific portfolio.optimization package tutorial 
#'
#' @description
#' \code{po.tutorial} returns the filename of a specific portfolio.optimization 
#' package tutorial. If no tutorial is given or the tutorial is missspelled, a 
#' list of available tutorials is printed.
#'
#' @param tutorial name of the tutorial to open 
#' 
#' @return Nothing if no tutorial specified, otherwise the path to the tutorial.
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' file.edit(po.tutorial("101"))
#' file.edit(po.tutorial("compare"))
#' }
#' 
po.tutorial <- function(tutorial="") {
  tutorials <- c("101", "compare", "13030", "scenario")
  tutorial_files <- c("1-po101", "2-compare", "3-13030", "4-scenario")
  
  if(!tutorial %in% tutorials) {
    print(paste0("Available tutorials: ", paste(tutorials, collapse=", ")))
  }
  else {
    tutorial_pos <- which(tutorials == tutorial)
    return(paste0(path.package("portfolio.optimization"), "/", tutorial_files[tutorial_pos], ".R"))
  }
}
