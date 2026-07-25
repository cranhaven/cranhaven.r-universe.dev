#' @title Export iNZightPlots as an SVG
#'
#' @description \code{exportSVG} is designed to export the iNZight plot as a temporary SVG that is opened in a web browser.
#' The iNZightPlot must be drawn to a graphics device before exporting can occur.
#'
#' @param x iNZight plot object or function that captures iNZight environment
#' @param file Name of temporary svg file generated (by default: 'inzightplot.svg')
#' @param ... additional arguments
#' @return Opens up an SVG file of \code{x} with filename \code{file} in a web browser
#' @author Yu Han Soh
#' @export
exportSVG <- function(x, file = tempfile(fileext = ".svg"), ...) {
    UseMethod("exportSVG")
}

#' @describeIn exportSVG method for functions
#' @param width the width of the plot device
#' @param height the height of the plot device
#' @export
exportSVG.function <- function(x, file = tempfile(fileext = ".svg"),
                               width = dev.size()[1],
                               height = dev.size()[2], ...) {
    # get current directory
    # curdir <- getwd()

    # set directory to temp directory
    # tdir <- tempdir()
    # setwd(tdir)
    # on.exit(setwd(curdir))

    # create pdf graphics device into here:
    pf <- tempfile(fileext = ".pdf")
    on.exit(file.remove(pf))
    pdf(pf, width = width, height = height, onefile = TRUE)
    # on.exit(file.remove('tempfile.pdf'), add = TRUE, after = FALSE)

    # do exporting:
    obj <- x()
    exportSVG(obj, file)

    # turn off device:
    dev.off()
}

#' @describeIn exportSVG method for an existing plot object
#' @export
exportSVG.inzplotoutput <- function(x, file = tempfile(fileext = ".svg"), ...) {
    # suggest gridSVG:
    if (!requireNamespace("gridSVG", quietly = TRUE)) {
        stop(
            paste("Required packages aren't installed",
                "Use 'install.packages('iNZightPlots', depends = TRUE)' to install them.",
                sep = "\n"
            )
        )
    }

    # curdir <- getwd()
    # work in a temp directory
    # setwd(tempdir())
    # on.exit(setwd(curdir))

    gridSVG::grid.export(file)

    # open in browser?
    browseURL(file.path(file))
}
