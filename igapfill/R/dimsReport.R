#' Dimensions and splitting characteristics of images to process
#' 
#' This function returns a few messages at the console. These messages
#' report on \emph{(i)} the dimensions of the images located at \code{path} and 
#' \emph{(ii)} the ways in which these dimensions can be split.
#' 
#' @param    path character, full path indicating the directory containing the images
#'                to process with \code{\link[gapfill]{Gapfill}}.
#' @param     mes character, when not provided the default message is 
#'                \emph{"The images located at 'path' have:"}.
#'                
#' @export
#' 
#' @importFrom raster stack
#' @importFrom raster nrow
#' @importFrom raster ncol
#' @importFrom numbers divisors
#' 
#' @seealso \code{\link[numbers]{divisors}}, \code{\link[igapfill]{waysToSplit}}
#' 
#' @return At the console there will be a series of messages, no further actions will be
#' taken.
#' 
dimsReport <- function(path, mes){
  
  pathFILES <- list.files(path = path, pattern = ".tif", full.names = TRUE)
  STACK <- stack(pathFILES)
  
  div_nrow <- divisors(nrow(STACK))
  div_ncol <- divisors(ncol(STACK))
  
  if(missing(mes)){
    mes <- cat(colorText("The images located at ", 80),
               colorText(path, 159), 
               colorText(" have:", 80))
  } else {
    mes <- colorText(mes, 80)
  }
  
  message(mes)
  
  message(colorText("nROW", 200), colorText(paste0(" = ", nrow(STACK)), 80))
  message(colorText("nROW", 118), colorText(paste0(" = ", ncol(STACK)), 80))
  
  message(colorText("---FYI---", foreground = 220))
  
  message(colorText("nROW", 200), 
          colorText(" can be split in ", 80), 
          colorText(div_nrow, 159), 
          colorText(" equal parts", 80))
  message(colorText("nCOL", 118), 
          colorText(" can be split in ", 80),
          colorText(div_ncol, 159), 
          colorText(" equal parts", 80))
  
}
