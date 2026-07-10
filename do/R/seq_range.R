#' sequence range of one vector
#'
#' @param x one vector
#' @param by default is 1
#' @return number sequence
#' @export
#'
#' @examples
#' seq_range(letters)
#' seq_range(letters,2)
seq_range <- function(x,by=1){
    if (length(x)==0) return(0)
    seq(1,length(x),by)
}