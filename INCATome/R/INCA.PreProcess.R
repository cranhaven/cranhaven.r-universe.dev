#' Background Correction of two-color microarray data
#'
#' Performs a background correction by substraction method of two-color microarray data.
#' @param x an RGList object
#' @param method a character specifying the method to employ for background correction. Choices are: "subtract" or "normexp".
#' @param offset a numerical value to add to intensities
#' @return A new RGList object containing the background corrected array data. Of note, negative values generated from the correction are transformed to NA values.
#' @examples
#' #Load the INCATome Dataset
#' data(INCATomeData)
#' attach(INCATomeData)
#' db=INCA.PreProcess(RGdata,method="subtract")
#' @import limma
#' @export

INCA.PreProcess <- function(x, method, offset = 0){
  
  if (is.null(x$R) || is.null(x$G)) 
    stop("Input doesn't contain R and G components")
  if (missing(method)){
    stop("The preprocessing method is missing")
  }
  
  RG <- backgroundCorrect(x, method, offset = offset)
  R <- RG$R
  G <- RG$G
  R[R <= 0] <- NA
  G[G <= 0] <- NA
  RG$R <- R
  RG$G <- G
  return(RG)
}