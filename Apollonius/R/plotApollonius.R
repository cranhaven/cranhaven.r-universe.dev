#' @title Plot Apollonius graph
#' @description Plot an Apollonius graph.
#'
#' @param apo an output of \code{\link{Apollonius}}
#' @param limits either \code{NULL} or a vector of length two passed to the
#'   arguments \code{xlim} and \code{ylim} of \code{\link[graphics]{plot}};
#'   if \code{NULL}, automatic limits are calculated
#' @param circles Boolean, whether to plot the original sites as circles with
#'   the given radii
#' @param fill Boolean, whether to fill the circles if \code{circles=TRUE}
#'   or to plot only their border
#' @param centers when \code{circles=TRUE} and \code{fill=FALSE}, whether to
#'   plot the centers of the circles
#' @param colors a character string controlling the colors of the sites;
#'   \code{"random"} to get multiple colors with
#'   \code{\link[colorsGen]{randomColor}}, \code{"distinct"} to get multiple
#'   colors with \code{\link[Polychrome]{createPalette}}, or a color name or
#'   a hexadecimal color code
#' @param distinctArgs if \code{colors = "distinct"}, a list of arguments
#'   passed to \code{\link[Polychrome]{createPalette}}
#' @param randomArgs if \code{colors = "random"}, a list of arguments passed
#'   to \code{\link[colorsGen]{randomColor}}
#' @param ... arguments passed to \code{\link[graphics]{plot}}, such as
#'   \code{xlab} and \code{ylab}
#'
#' @return No returned value, called for plotting.
#' @export
#'
#' @importFrom grDevices extendrange
#' @importFrom graphics plot points lines
#' @importFrom plotrix draw.circle
#'
#' @examples
#' library(Apollonius)
#' sites <- rbind(
#'   c(0, 0),
#'   c(4, 1),
#'   c(2, 4),
#'   c(7, 4),
#'   c(8, 0),
#'   c(5, -2),
#'   c(-4, 4),
#'   c(-2, -1),
#'   c(11, 4),
#'   c(11, 0)
#' )
#' radii <- c(1, 1.5, 1.25, 2, 1.75, 0.5, 0.4, 0.6, 0.7, 0.3)
#' apo <- Apollonius(sites, radii)
#' opar <- par(mar = c(3, 3, 1, 1))
#' plotApolloniusGraph(
#'   apo, fill = FALSE, colors = "random", xlab = NA, ylab = NA
#' )
#' par(opar)
plotApolloniusGraph <- function(
    apo, limits = NULL, circles = TRUE, fill = TRUE, centers = TRUE,
    colors = "distinct",
    distinctArgs = list(seedcolors = c("#ff0000", "#00ff00", "#0000ff")),
    randomArgs = list(hue = "random", luminosity = "dark"), ...
) {
  stopifnot(isBoolean(circles))
  stopifnot(isBoolean(fill))
  stopifnot(isBoolean(centers))
  stopifnot(isString(colors))
  sites  <- apo[["diagram"]][["sites"]]
  nsites <- nrow(sites)
  radii  <- sites[, "weight"]
  dsites <- apo[["graph"]][["sites"]]
  edges  <- apo[["graph"]][["edges"]]
  hsegments <- edges[["segments"]]
  hrays     <- edges[["rays"]]
  #
  if(colors == "distinct") {
    clrs <- distinctColors(nsites, distinctArgs)
  } else if(colors == "random") {
    clrs <- rcolors(nsites, randomArgs)
  } else {
    clrs <- rep(colors, nsites)
  }
  #
  if(is.null(limits)) {
    x <- extendrange(sites[, "x"])
    y <- extendrange(sites[, "y"])
    limits <- c(min(x[1L], y[1L]), max(x[2L], y[2L]))
  }
  #
  plot(NULL, xlim = limits, ylim = limits, asp = 1, ...)
  if(circles) {
    borders <- if(fill) NA else clrs
    cols    <- if(fill) clrs else NA
    for(i in 1L:nsites) {
      draw.circle(
        sites[i, "x"], sites[i, "y"], radius = radii[i],
        border = borders[i], col = cols[i], lwd = 2
      )
    }
    if(!fill && centers) {
      for(i in 1L:nsites) {
        points(
          sites[i, "x"], sites[i, "y"], pch = 19L, col = clrs[i]
        )
      }
    }
  } else {
    for(i in 1L:nsites) {
      points(
        sites[i, "x"], sites[i, "y"], pch = 19L, col = clrs[i]
      )
    }
  }
  points(dsites, pch = 19)
  for(i in seq_along(hsegments)) {
    lines(hsegments[[i]], col="black", lwd = 2)
  }
  for(i in seq_along(hrays)) {
    lines(hrays[[i]], col="black", lwd = 2)
  }
  invisible()
}
