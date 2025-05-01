#' A Default Cutter
#'
#' This is simply a jiebar object created when the package is loaded. 
#' \code{write} is set to \code{FALSE}, so as to prevent segmented text from 
#' being automatically written into disk.
#'
#' @export
#' @examples
#' require(jiebaR)
#' x <- c("drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'  "drink some water")
#' seg_file(x, from = "v")
#' seg_file(x, from = "v", mycutter = DEFAULT_cutter)
DEFAULT_cutter <- jiebaR::worker(write = FALSE)
