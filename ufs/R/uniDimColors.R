#' @export
#' @rdname itemScaleColours
uniDimColors <- function(start,
                         end,
                         length,
                         show=TRUE) {
  res <- grDevices::colorRampPalette(c(start, end))(length);
  if (show) {
    graphics::plot(
      rep(1,length),
      col=res,
      pch=15,
      cex=3,
      axes=FALSE,
      xlab="",
      ylab="");
  }
  return(res);
}
