#' Create colours for a response scale for an item
#'
#' @param start Color to start with
#' @param mid Color in the middle, for bidimensional scales
#' @param end Color to end with
#' @param length The number of response options
#' @param show Whether to show the colours
#'
#' @return The colours as hex codes.
#' @export
#'
#' @rdname itemScaleColours
#' @examples uniDimColors("#000000", "#00BB00", length=5, show=FALSE);
biDimColors <- function(start,
                        mid,
                        end,
                        length,
                        show=TRUE) {
  startToMid <-
    grDevices::colorRampPalette(c(start, mid))(ceiling(length/2));
  midToEnd <-
    grDevices::colorRampPalette(c(mid, end))(ceiling(length/2));
  res <- c(startToMid, utils::tail(midToEnd, -1));
  if (show) {
    graphics::plot(rep(1,length),
                   col=res,
                   pch=15,
                   cex=3,
                   axes=FALSE,
                   xlab="",
                   ylab="");
  }
  return(res);
}
