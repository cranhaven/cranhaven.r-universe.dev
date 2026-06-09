# visualize — aboveR
# Color ramps for terrain and change maps

#' Diverging Color Ramp for Change Maps
#'
#' Returns a blue-white-red color ramp centered at zero, suitable for
#' terrain change visualisation. Cut (negative) is blue, fill (positive)
#' is red.
#'
#' @param n Integer. Number of colors. Default `100`.
#'
#' @returns Character vector of hex color values.
#'
#' @export
#'
#' @examples
#' cols <- change_colors(10)
#' image(matrix(1:10), col = cols)
change_colors <- function(n = 100L) {
  grDevices::colorRampPalette(c("#2166AC", "#67A9CF", "#D1E5F0",
                                 "#F7F7F7",
                                 "#FDDBC7", "#EF8A62", "#B2182B"))(n)
}

#' Terrain Color Ramp
#'
#' Green-brown-white hypsometric color ramp for elevation maps.
#'
#' @param n Integer. Number of colors. Default `100`.
#'
#' @returns Character vector of hex color values.
#'
#' @export
#'
#' @examples
#' cols <- terrain_colors(10)
#' image(matrix(1:10), col = cols)
terrain_colors <- function(n = 100L) {
  grDevices::colorRampPalette(c("#1B7837", "#7FBC41", "#C2A541",
                                 "#A67026", "#8C510A", "#D9D9D9",
                                 "#F5F5F5"))(n)
}

#' Flood Depth Color Ramp
#'
#' Light-to-dark blue color ramp for flood depth visualisation.
#'
#' @param n Integer. Number of colors. Default `100`.
#'
#' @returns Character vector of hex color values.
#'
#' @export
#'
#' @examples
#' cols <- flood_colors(10)
#' image(matrix(1:10), col = cols)
flood_colors <- function(n = 100L) {
  grDevices::colorRampPalette(c("#DEEBF7", "#9ECAE1", "#4292C6",
                                 "#2171B5", "#08519C", "#08306B"))(n)
}
