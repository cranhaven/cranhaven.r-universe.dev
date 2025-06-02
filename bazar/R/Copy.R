#' @title 
#' Copy data to the clipboard
#' 
#' @description 
#' The function \code{Copy} can typically be used 
#' to copy data from a data frame, in order to paste 
#' it somewhere else (in Excel for instance). 
#' 
#' @param x
#' An object. 
#' 
#' @param size
#' integer. Number of kilobytes. 
#' Increase this value if the object \code{x} 
#' is too big. 
#' 
#' @param quote
#' See the eponymous argument in \code{\link[utils]{write.table}}. 
#' 
#' @param sep 
#' character. The field separator string. 
#' 
#' @param na
#' character. The string to use for missing values. 
#' 
#' @param dec 
#' character. The string to use for decimal points 
#' in numeric or complex columns. 
#' 
#' @param ...
#' Additional arguments to be passed to 
#' \code{\link[utils]{write.table}}. 
#' 
# #' @seealso \code{\link[utils]{writeClipboard}} 
# #' in package \pkg{utils}. 
# #' 
#' @importFrom utils write.table
#' @export
#' 
Copy <-
function(x,
         size = 128L, # number of kbytes
         quote = TRUE, 
         sep = "\t", 
         na = "",
         dec = ".", 
         ...)
{
  utils::write.table(x,
                     file = paste0("clipboard-", size),
                     quote = quote, 
                     sep = sep,
                     na = na,
                     dec = dec, 
                     row.names = FALSE,
                     col.names = !is.atomic(x), 
                     ...)
}
