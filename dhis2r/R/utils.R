#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
#' @import httr2
#' @import R6
check_internet <- function(){
  stop_if_not(.x = has_internet(), msg = "Please check your internet connetion")
}

