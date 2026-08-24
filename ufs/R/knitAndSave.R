#' knitAndSave
#'
#' @param plotToDraw The plot to knit using [knitFig()] and save using [ggSave()].
#' @param figCaption The caption of the plot (used as filename if no filename is specified).
#' @param file,path The filename to use when saving the plot, or the path where to save the
#' file if no filename is provided (if `path` is also omitted, `getWd()` is used).
#' @param figWidth,figHeight The plot dimensions, by default specified in inches (but 'units' can
#' be set which is then passed on to [ggSave()].
#' @param units,dpi The units and DPI of the image which are then passed on to [ggSave()].
#' @param catPlot Whether to use [cat()] to print the knitr fragment.
#' @param ... Additional arguments are passed on to [ggSave()]. Note that file (and ...) are
#' vectorized (see the [ggSave()] manual page).
#'
#' @return The [knitFig()] result, visibly.
#' @export
#'
#' @examples \dontrun{plot <- ggBoxplot(mtcars, 'mpg');
#' knitAndSave(plot, figCaption="a boxplot", file=tempfile(fileext=".png"));}
knitAndSave <- function(plotToDraw,
                        figCaption,
                        file = NULL,
                        path=NULL,
                        figWidth=ufs::opts$get("ggSaveFigWidth"),
                        figHeight=ufs::opts$get("ggSaveFigHeight"),
                        units=ufs::opts$get("ggSaveUnits"),
                        dpi=ufs::opts$get("ggSaveDPI"),
                        catPlot = ufs::opts$get("knitAndSave.catPlot"),
                        ...) {
  if (substr(figCaption,
             start=nchar(figCaption),
             stop=nchar(figCaption)) != ".") {
    figCaption <-
      paste0(figCaption, ".");
  }

  if (is.null(path)) {
    path <- getwd();
  }

  if (is.null(file)) {
    ### Save PNG, SVG, and PDF
    file <-
      c(file.path(path,
                  strToFilename(figCaption, "png")),
        file.path(path,
                  strToFilename(figCaption, "svg")),
        file.path(path,
                  strToFilename(figCaption, "pdf")));
  }

  ### Save file(s)
  ggSave(plotToDraw,
         file=file,
         width=figWidth,
         height=figHeight,
         units=units,
         dpi=dpi,
         ...);

  ### Knit (and return) figure
  knitFig(plotToDraw,
          figWidth=figWidth,
          figHeight=figHeight,
          figCaption=figCaption,
          catPlot=catPlot);

}
