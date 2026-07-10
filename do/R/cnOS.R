#' Chinese operating system
#' Whether the computer is Chinese operating system
#' @return logical
#' @export
#'
#' @examples
#' cnOS()
cnOS <- function(){
    grepl('chinese',tolower(Sys.getlocale()))
}