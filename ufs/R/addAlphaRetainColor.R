# addAlphaRetainColor <- function(color,
#                                 transparency,
#                                 background = "#ffffff") {
#   col_rgb <- grDevices::col2rgb(color);
#   bg_rgb <- grDevices::col2rgb(background);
#   toLighter = mean(col_rgb) < mean(bg_rgb);
#   fullInvisibility
#   if (toLighter) {
#     lowest = which(col_rgb == min(col_rgb));
#     lowestNew <- bg_rgb[lowest] - col_rgb[lowest]
#   }
# }
