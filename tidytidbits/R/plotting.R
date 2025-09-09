
#' Directory creation
#'
#' Creates directory if it does not yet exist
#'
#' @param folder Folder path
#'
#' @return Folder path
#' @export
prepare_directory <- function(folder)
{
  folder <- path.expand(folder)
  if (!dir.exists(folder))
  {
    dir.create(folder, recursive = T, showWarnings = F)
  }
  return(folder)
}

prepareDirectory <- prepare_directory

#' Directory creation and file path concatenation
#'
#' Given a folder, file base name and suffix, ensures the
#' directory exists, and returns the ready file path.
#'
#' @param folder Folder path, without trailing slash
#' @param fileBaseName File base name, excluding trailing dot
#' @param fileSuffix File suffix without leading dot (e.g., "png", "pdf")
#'
#' @return Complete file path
#' @export
prepare_path <- function(folder, fileBaseName, fileSuffix)
{
  prepare_directory(folder)
  stringr::str_c(folder, "/", fileBaseName, ".", fileSuffix)
}

pathToSave <- prepare_path

# opens a cairo PDF device for old-style plot printing
openCairoPDF <- function(folder, fileBaseName, width, height, family = "DejaVu Serif", ...)
{
  filename <- prepare_path(folder, fileBaseName, "pdf")
  cairo_pdf(filename = filename, width = width, height = height, family = family, ...)
  return(filename)
}

closeCairoPDF <- function(filename)
{
  dev.off()
  extrafont::embed_fonts(filename)
}

.check_papersize <- function(plot, width, height)
{
  if (missing(width) || missing(height))
  {
    papersize <- attr(plot, "papersize", exact = T)
    if (!is_null(papersize))
    {
      width <- papersize[[1]]
      height <- papersize[[2]]
    }
    else
      stop("Saving plot: Must specify width and height")
  }
  if (grid::is.unit(width))
  {
    width <- grid::convertUnit(width, "in", valueOnly = TRUE)
  }
  if (grid::is.unit(height))
  {
    height <- grid::convertUnit(height, "in", valueOnly = TRUE)
  }
  c(width, height)
}

#' Save plot as PDF
#'
#' @param plot A plot object that can be printed, e.g. result of ggplot2, plot_grid
#' @param folder Destination folder (will be created if it does not exist)
#' @param fileBaseName File base name (suffix ".pdf" will be added)
#' @param width,height PDF width and height in inches or as \code{grid::\link{unit}}.
#'        If missing and the plot object has a "papersize" attribute c(width, height), this will be used.
#' @param ... Further arguments which will be passed to \code{\link{cairo_pdf}}, e.g. family
#'
#' @export
save_pdf <- function(plot, folder, fileBaseName, width, height, ...)
{
  g(width, height) %=% .check_papersize(plot, width, height)
  filename <- openCairoPDF(folder, fileBaseName, width, height, ...)
  print(plot)
  closeCairoPDF(filename)
  invisible()
}

#' Save plot as PNG
#'
#' @param plot A plot object that can be printed, e.g. result of ggplot2, plot_grid
#' @param folder Destination folder (will be created if it does not exist)
#' @param fileBaseName File base name (suffix ".png" will be added)
#' @param width,height PNG width and height in inches or as \code{grid::\link{unit}}.
#'        If missing and the plot object has a "papersize" attribute c(width, height), this will be used.
#' @param dpi Resolution (determines file size in pixels, as size is given in inches)
#' @param background Initial background color, "white" or "transparent"
#' @param ... Further arguments which will be passed to \code{\link{png}}, e.g. family
#'
#' @return invisible NULL
#' @export
save_png <- function(plot,
                     folder,
                     fileBaseName,
                     width,
                     height,
                     dpi = 300,
                     background = c("white", "transparent"),
                     ...)
{
  background <- match.arg(background)
  g(width, height) %=% .check_papersize(plot, width, height)

  filename <- pathToSave(folder, fileBaseName, "png")

  png(filename = filename,
      width = width,
      height = height,
      units = "in",
      bg = background,
      type = "cairo",
      antialias = "subpixel",
      res = dpi,
      ...)
  print(plot)
  dev.off()
  invisible()
}


#' Named color palette
#'
#' Returns the palette named by names.
#' This is useful to pick only a few specific colors from a larger palette.
#'
#' @param palette Colors
#' @param names Names
#' @param color_order If specified, will reorder palette by this ordering vector
#'
#' @return A named palette.
#'         If the palette is longer than names, will only use the first n entries.
#'         If names is longer than palette, will recycle colors.
#' @export
named_palette <- function(palette, names, color_order = NULL)
{
  if (!is_null(color_order))
  {
    palette <- palette[color_order]
  }

  if (length(names) < length(palette)) # cut
    palette <- palette[1:length(names)]
  else if (length(names) > length(palette)) # recycle
    palette <- rep(palette, length.out = length(names))
  base::names(palette) <- names

  palette
}

