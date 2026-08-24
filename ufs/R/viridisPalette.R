#' Convenience function to get 2-7 color viridis palettes
#'
#' This function only exists to avoid importing the viridis package.
#'
#' @param x The number of colors you want (seven at most).
#'
#' @return A vector of colours.
#' @export
viridisPalette <- function(x) {
  if (x > 7) {
    stop("Can only automatically select up to seven colors: ",
         "if you want ", x, " colors, specify them yourself. ",
         "For example, you could use:\n\n  viridis::viridis(",
         x, ")\n\nIf you don't have the viridis package installed, ",
         "you can install it with:\n\n  install.packages('viridis')\n\n");
  }
  res <-
    list(
      c("#000000FF"),
      c("#440154FF", "#FDE725FF"),
      c("#440154FF", "#21908CFF", "#FDE725FF"),
      c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"),
      c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF"),
      c("#440154FF", "#443A83FF", "#31688EFF", "#21908CFF", "#35B779FF",
        "#8FD744FF", "#FDE725FF")
    );
  return(res[[x]]);
}
