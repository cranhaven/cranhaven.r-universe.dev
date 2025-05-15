#' gg_color_hue
#' 
#' @description create colors.
#'
#' @param n an integer specifying the number of colors to create.
#'
#' @return color-coded vector
#' @noRd
#'
#' @examples
#' col <- gg_color_hue(3)
#' plot(iris$Species, col = col)
#' 
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
