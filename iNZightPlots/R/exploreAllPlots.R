#' Explore all Univariate Plots
#'
#' Allows easy viewing of every variable in the data set.
#' The user will be prompted to see the next variable.
#'
#' @param data a data frame
#' @return NULL
#' @author Tom Elliott
#' @export
#' @examples
#' if (interactive())
#'     exploreAllPlots(iris)
#'
exploreAllPlots <- function(data) {
    ## Runs iNZightPlot on all of the variables, with a click-for-next thing.

    grid.newpage()
    pushViewport(viewport())

    # Title window:
    grid.text(paste("Click to see next plot."))

    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
    for (i in 1:ncol(data)) {
        iNZightPlot(data[, i], varnames = list(x = colnames(data)[i]))
        dev.flush()
    }
    devAskNewPage(oask)
    grid.newpage()
}
