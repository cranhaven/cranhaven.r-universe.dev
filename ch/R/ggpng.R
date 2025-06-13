#' @title Create a picture with a transparent background.
#' @description Use the ImageMagick command line to convert the PDF saved
#' by ggplot2 to PNG format with a transparent background and to
#'  set the resolution.
#' @param  x A file name that does not have a suffix.
#' @param  dpi The default dpi is 600.
#' You can enhance the dpi value to produce a higher resolution PNG file.
#' @param  ...  see :\link{ggsave}
#' @details  You need to install ImageMagick!
#' Please check if the ImageMagick is added to the environment variable.
#' @importFrom ggplot2  theme_set theme element_rect ggsave
#' @return You will get a PNG file with the result drawn by ggplot2.
#' @author  Chai
#' @export
ggpng <- function(x, dpi = 600, ...) {
  x1 <- paste0(x, ".pdf")
  x2 <- paste0(x, ".png")
  ggsave(x1, ...)
  system(paste("magick -density ", dpi, x1, "-quality 90", x2))
}

#' @rdname ggpng
#' @param p  ggplot2 object
#' @details  this ggplot2 object will automatically add a theme with a
#' transparent background.
#' @export
ggPNG <- function(x, p, dpi = 600, ...) {
  x1 <- paste0(x, ".pdf")
  x2 <- paste0(x, ".png")
  p <- p +
    theme(
      plot.background = element_rect(
        fill = NA, color = NA
      ),
      panel.background = element_rect(
        fill = NA, color = NA
      )
    )
  ggsave(x1, ...)
  system(paste("magick -density ", dpi, x1, "-quality 90", x2))
}
