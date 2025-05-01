#' Copy and Paste from Excel-Like Files
#'
#' See \code{\link[chinese.misc]{V}}.
#'
#' @param tofactor if this is equal to numeric 1 or \code{TRUE}, characters will be converted to factors. Otherwise no 
#' conversion will be done. The default is not to convert.
#' @param keepblank if characters are not to be converted to factors, this argument decides how to deal with 
#' blank cells in character columns. If it is numeric 1 or \code{TRUE}, a blank cell will be converted
#' to "" (size 0 string). Otherwise it is viewed as \code{NA} (default).
#' @param sep a single character to differentiate cells of a table. The default value should be used when 
#' your data is from Excel.
#'
#' @export
VC <- function(tofactor = 0, keepblank = 0, sep = "\t") {
    asfa <- ifelse(identical(tofactor, TRUE) | identical(tofactor, 1), TRUE, FALSE)
    nast <- c("NA", "")
    if((identical(keepblank, 1) | identical(keepblank, TRUE)) & asfa == FALSE)
        nast <- "NA"
    utils::read.table("clipboard", header = TRUE, blank.lines.skip = FALSE, sep = sep, na.strings =nast, stringsAsFactors = asfa, quote = "")
}
