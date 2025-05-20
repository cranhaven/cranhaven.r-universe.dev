#' Add perimeters around the relationship zones
#'
#' Pass it the original ggplot, and this will return it, with the perimeters added.
#' @param g the original ggplot
#' @param L the number of rows (or columns) in the ancestor-match matrices
#' @param perisize the size of the line to color in the perimeters
#' @keywords internal
gg_add_zone_perimeters <- function(
  g,
  L,
  perisize = 0.5
) {

  # determine number of generations
  GenP1 <- ceiling(log(L + 1, base = 2))

  # now get the relationships to include
  num_relat <- c(1, 3, 6, 10, 15)[GenP1]

  # get a tibble or perimeter endpoints
  rzp <- relationship_zone_perimeters() %>%
    filter(zone %in% relationship_zone_names[1:num_relat])


  g2 <- g + annotate(
    "rect",
    xmin = rzp$xmin,
    xmax = rzp$xmax,
    ymin = rzp$ymin,
    ymax = rzp$ymax,
    fill = NA,
    colour = "black",
    size = perisize
  )

  g2
}

