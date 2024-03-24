#' Row Highlight
#'
#' Creates a data structure for highlighting rows of the table based on indices provided by the user
#'
#' @param dx index rows of data.frame to highlight
#' @param col color
#' @export
row.hl <-
function(dx,
                   col="yellow")
  {
   if(is.logical(dx)) {dx<-which(dx)}
   return(list(dx=dx, col=col))
  }

