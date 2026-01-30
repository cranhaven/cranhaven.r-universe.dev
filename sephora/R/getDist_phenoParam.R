#' Utility function
#' 
#' Extracts an estimated phenological parameter from a \code{list}. Useful when 
#' \code{phenopar_polygon} was applied to estimate phenological dates over a polygon.
#' 
#' @param       LIST list, containing 6 estimated phenological parameters
#' @param phenoParam character. What phenological parameter should be extracted?
#' 
#' @returns A numeric vector 
#' 
#' @seealso \code{\link[sephora]{getSpiralPlot}}, \code{\link[sephora]{phenopar_polygon}}
#' 
getDist_phenoParam <- function(LIST, 
                               phenoParam=c("GU","SoS","Mat","Sen","EoS","Dor")){
  phenoParam <- match.arg(phenoParam)
  unlist(sapply(LIST, 
                function(x) as.numeric(as.character(x[[1]][which(x[[1]][[2]] == phenoParam),1]))))
  
}
