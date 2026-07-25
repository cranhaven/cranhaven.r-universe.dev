stopPlot <- function(msg) {
    grid.newpage()
    pushViewport(viewport())
    grid.text(msg)
}
