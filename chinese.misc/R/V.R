#' Copy and Paste from Excel-Like Files
#'
#' These functions make it easy for copy and paste data from Excel-like files, especially when there are 
#' blank cells or when different columns have different lengths. All of them have the same arguments.
#' \itemize{
#'   \item \code{V}, when you do not copy rownames or colnames
#'   \item \code{VR}, when the 1st column is for rownames and there are no colnames in what you copy
#'   \item \code{VC}, when there are colnames but no rownames
#'   \item \code{VRC} and the same: \code{VCR}, when there are both rownames and colnames
#' }
#' If you copy something from a text document (e.g., Windows Notepad), the function may warn 
#' "incomplete final line found by readTableHeader...". This is because your content does not end with an end of
#' line sign. You can simply ignore this warning!
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
V <- function(tofactor = 0, keepblank = 0, sep = "\t") {
    asfa <- ifelse(identical(tofactor, TRUE) | identical(tofactor, 1), TRUE, FALSE)
    nast <- c("NA", "")
    if((identical(keepblank, 1) | identical(keepblank, TRUE)) & asfa == FALSE)
        nast <- "NA"
    utils::read.table("clipboard", header = FALSE, blank.lines.skip = FALSE, sep = sep, na.strings = nast, stringsAsFactors = asfa, quote = "")
}
