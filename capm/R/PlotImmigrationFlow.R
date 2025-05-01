#' Plot immigration flows
#' @description Plot rimmigration flows from many sources to one destination. 
#' @param data \code{\link{data.frame}} with sources and destination.
#' @param source \code{data}'s column name or index with places' names. Sources' names and destination's name must be in this column.
#' @param destination destination's name.
#' @param n.sources number of sources to plot. If smaller than the total number of sources \code{source}, the less frequent sources are aggregated.
#' @param agg.sources.prefix string. If n.sources is smaller than the total number of sources, \code{agg.sources.prefix} is used to label the aggregated sources.
#' @param agg.sources.suffix \code{\link{character}}. If n.sources is smaller than the total number of sources, \code{agg.sources.prefix} is used to label the aggregated sources.
#' @param cls Optional \code{\link{character}} \code{\link{vector}} with \code{n.sources} + 1 colors.
#' @param start.degree The starting degree from which the circle begins to draw. It is passed to the \code{start.degree} argumento of \code{circlize::circos.par} function.
#' @param sources.label.dist Data point on y-axis to separate the sources' labels from the circle. It is passed to the \code{y} argument of \code{circlize::circos.text} function.
#' @param sources.label.size Font size for sources' labels. It is passed to the \code{cex} argument of \code{circlize::circos.text} function.
#' @param ticks.label.size Font size for sources' labels. It is passed to the \code{labels.cex} argument of \code{circlize::circos.axis} function.
#' @details The numbers arround the circle indicate the number of animals.
#' @references Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics. DOI: 10.1093/bioinformatics/btu393
#' 
#' Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @export
#' @examples
#' data(dogs)
#' cls <- c("blue3", "orange", "skyblue", "darkgreen", "yellow3", "black")
#' PlotImmigrationFlow(dogs, "acquisition_city", "Pinhais",
#'                     cls = cls, agg.sources.suffix = " cities")
#' 
PlotImmigrationFlow <- function(data = NULL, source = NULL, destination = NULL, n.sources = 5, agg.sources.prefix = "Other ", agg.sources.suffix = " sources", cls = NULL, start.degree = 0, sources.label.dist = 0.15, sources.label.size = 0.75, ticks.label.size = 0.7) {
  
  # Workaround to the "no visible binding for global variable" note.
  Count <- colors <- NULL
  
  sources <- FreqTab(data, source)
  names(sources)[which(names(sources) == source)] <- "source"
  sources[sources$source != destination, 1][-(1:(n.sources - 1)), 1] <-
    paste0(agg.sources.prefix, nrow(sources) - n.sources, agg.sources.suffix)
  sources <- sources %>%
    group_by(source) %>%
    summarise(Count = sum(Count)) %>%
    arrange(desc(Count)) %>%
    transmute(from = source,
              to = destination,
              value = Count)
  
  if (is.null(cls)) {
    col <- colors()[sample(length(colors()), n.sources + 1)]
  } else {
    col <- cls
  }
  circos.clear()
  circos.par(start.degree = start.degree)
  chordDiagram(x = sources[sources$from != destination, ],
               directional = 1,
               direction.type = "arrows",
               link.arr.length = 0.5,
               annotationTrack = "grid",
               preAllocateTracks = 1,
               grid.col = col,
               col = col)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim),
                ylim[1] + sources.label.dist,
                cex = sources.label.size,
                sector.name,
                facing = "clockwise",
                niceFacing = TRUE,
                adj = c(0, .5))
    circos.axis(h = .6,
                labels.cex = ticks.label.size,
                major.tick = FALSE,
                minor.ticks = 0,
                lwd = 0,
                sector.index = sector.name,
                track.index = 2)
  }, bg.border = NA)
}
