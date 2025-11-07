#' Duke Color Palette (Discrete) and Scales
#'
#' An eight-color colorblind friendly qualitative discrete palette that is
#' based on colors on the Duke branding guidelines.
#'
#' @references https://brand.duke.edu/colors/
#' @return Character vector of Duke palette HEX codes.
#' @export
#'
#' @examples
#' duke_pal()
duke_pal <- function() {
  values <- c(
    "#012169", "#C84E00", "#00539B", "#339898", "#A1B70D", "#E89923",
    "#FFD960", "#262626"
  )
  f <- scales::manual_pal(values)
  attr(f, "max_n") <- length(values)
  f
}
