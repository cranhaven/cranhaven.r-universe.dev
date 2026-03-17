

#' @title `S3` method dispatch of \link[base]{length} on \link[spatstat.geom]{hyperframe}
#' 
#' @param x a \link[spatstat.geom]{hyperframe}
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @seealso \link[spatstat.geom]{dim.hyperframe}
#' 
#' @keywords internal
#' @export length.hyperframe
#' @export
length.hyperframe <- function(x) {
  unclass(x)$nvars
}