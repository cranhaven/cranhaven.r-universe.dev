# Richard Wen
# rrwen.dev@gmail.com

# (settings) ----
.pkgenv <- new.env(parent = emptyenv())
.pkgenv$units <- "inches"
.pkgenv$exportUnits <- .pkgenv$units
.pkgenv$exportPPI <- 150

# (settings_arrow) ----
.pkgenv$arrowAngle <- 30
.pkgenv$arrowLength <- 0
.pkgenv$arrowUnits <- .pkgenv$units
.pkgenv$arrowEnds <- "last"
.pkgenv$arrowType <- "open"

# (settings_box) ----
.pkgenv$boxWidth <- 1
.pkgenv$boxHeight <- 1
.pkgenv$boxRadius <- 0
.pkgenv$boxFillColor <- "transparent"
.pkgenv$boxOpacity <- 1
.pkgenv$boxLineWidth <- 1
.pkgenv$boxLineType <- "solid"
.pkgenv$boxLineColor <- "black"

# (settings_circle) ----
.pkgenv$circleRadius <- 0.5
.pkgenv$circleFillColor <- "transparent"
.pkgenv$circleOpacity <- 1
.pkgenv$circleLineWidth <- 1
.pkgenv$circleLineType <- "solid"
.pkgenv$circleLineColor <- "black"

# (settings_curve) ----
.pkgenv$curveCurvature <- 1
.pkgenv$curveAngle <- 90
.pkgenv$curvePoints <- 1
.pkgenv$curveShape <- 0.5
.pkgenv$curveSquare <- TRUE
.pkgenv$curveSquareShape <- 1
.pkgenv$curveOpacity <- 1
.pkgenv$curveLineColor <- "black"
.pkgenv$curveLineWidth <- 1
.pkgenv$curveLineType <- "solid"
.pkgenv$curveInflect <- FALSE
.pkgenv$curveOpen <- TRUE

# (settings_line) ----
.pkgenv$lineOpacity <- 1
.pkgenv$lineWidth <- 1
.pkgenv$lineType <- "solid"
.pkgenv$lineColor <- "black"

# (settings_page) ----
.pkgenv$pageWidth <- 8.5
.pkgenv$pageHeight <- 11

# (settings_point) ----
.pkgenv$pointPCH <- 20
.pkgenv$pointSize <- 1
.pkgenv$pointFillColor <- "transparent"
.pkgenv$pointOpacity <- 1
.pkgenv$pointLineColor <- "black"
.pkgenv$pointLineType <- "solid"
.pkgenv$pointLineWidth <- 1

# (settings_text) ----
.pkgenv$text <- "text"
.pkgenv$textJust <- "centre"
.pkgenv$textHjust <- NULL
.pkgenv$textVjust <- NULL
.pkgenv$textAngle <- 0
.pkgenv$textOverlap <- FALSE
.pkgenv$textOpacity <- 1
.pkgenv$textColor <- "black"
.pkgenv$textSize <- 12
.pkgenv$textFace <- "plain"
.pkgenv$textFamily <- "sans"
.pkgenv$textLineHeight <- 1.2

# (functions) ----


#' Draw a Box on the Page
#'
#' Draws a box on the page given positioning, dimensions and styling.
#' \cr\cr
#' \if{html}{\figure{drawbox.png}{options: width=250 alt="Figure: drawBox Example"}}
#' \if{latex}{\figure{drawbox.png}{options: width=3in alt="Figure: drawBox Example"}}
#'
#' @param x Numeric value for the x-axis position of the center.
#' @param y Numeric value for the y-axis position of the center.
#' @param width Numeric value for the width.
#' @param height Numeric value for the height.
#' @param radius Numeric value for the radius to create rounded box corners.
#' @param fillColor Character value for the fill color.
#' @param opacity Numeric value for the transparency with values ranging from 0 (transparent) to 1 (non-transparent).
#' @param lineColor Character value for the color of the lines.
#' @param lineWidth Numeric value for the width of the lines.
#' @param lineType Character value for the line type. One of "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash" (see "lty" in \link[graphics]{par}).
#' @param units Character value for the \link[grid]{unit} to use when specifying measurements.
#' @param ... Additional arguments passed to \link[grid]{grid.rect}.
#'
#' @return A \link[grid]{grid.rect} \link[grid]{grob} object.
#'
#' @importFrom grid gpar grid.roundrect unit
#' @export
#' @seealso \link{drawSettings}
#'
#' @examples
#' library(draw)
#'
#' # Set drawing settings
#' drawSettings(pageWidth = 5, pageHeight = 5, units = "inches")
#'
#' # Create a new drawing page
#' drawPage()
#'
#' # Draw a square
#' drawBox(x = 1, y = 4, width = 1, height = 1)
#'
#' # Draw a square with rounded corners
#' drawBox(x = 4, y = 4, width = 1, height = 1, radius = 0.25)
#'
#' # Draw a rectangle
#' drawBox(x = 1, y = 1, width = 1, height = 0.5)
#'
#' # Draw a rectangle with rounded corners
#' drawBox(x = 4, y = 1, width = 1, height = 0.5, radius = 0.25)
#'
#' # Export the drawing page to a PDF
#' drawExport("drawBox.pdf")
#'
drawBox <- function(x, y,
                    width = .pkgenv$boxWidth,
                    height = .pkgenv$boxHeight,
                    radius = .pkgenv$boxRadius,
                    fillColor = .pkgenv$boxFillColor,
                    opacity = .pkgenv$boxOpacity,
                    lineColor = .pkgenv$boxLineColor,
                    lineWidth = .pkgenv$boxLineWidth,
                    lineType = .pkgenv$boxLineType,
                    units = .pkgenv$units, ...) {
  out <- grid::grid.roundrect(x = x,
                              y = y,
                              width = width,
                              height = height,
                              r = grid::unit(radius, units),
                              gp = grid::gpar(
                                fill = fillColor,
                                alpha = opacity,
                                col = lineColor,
                                lwd = lineWidth,
                                lty = lineType
                              ),
                              default.units = units, ...)
  invisible(out)
}

#' Draw a Circle on the Page
#'
#' Draws a circle on the page given positioning, dimensions and styling.
#' \cr\cr
#' \if{html}{\figure{drawcircle.png}{options: width=250 alt="Figure: drawCircle Example"}}
#' \if{latex}{\figure{drawcircle.png}{options: width=3in alt="Figure: drawCircle Example"}}
#'
#' @inheritParams drawBox
#' @param radius Numeric value for radius of the circle.
#' @param ... Additional arguments passed to \link[grid]{grid.circle}
#'
#' @return A \link[grid]{grid.circle} \link[grid]{grob} object.
#'
#' @importFrom grid gpar grid.circle unit
#' @export
#' @seealso \link{drawSettings}
#'
#' @examples
#' library(draw)
#'
#' # Set drawing settings
#' drawSettings(pageWidth = 5, pageHeight = 5, units = "inches")
#'
#' # Create a new drawing page
#' drawPage()
#'
#' # Draw a small circle
#' drawCircle(x = 2.5, y = 2.5, radius = 0.5)
#'
#' # Draw a mid sized circle
#' drawCircle(x = 2.5, y = 2.5, radius = 1)
#'
#' # Draw a large circle
#' drawCircle(x = 2.5, y = 2.5, radius = 2)
#'
#' # Export the drawing page to a PDF
#' drawExport("drawCircle.pdf")
#'
drawCircle <- function(x, y,
                       radius = .pkgenv$circleRadius,
                       fillColor = .pkgenv$circleFillColor,
                       opacity = .pkgenv$circleOpacity,
                       lineColor = .pkgenv$circleLineColor,
                       lineWidth = .pkgenv$circleLineWidth,
                       lineType = .pkgenv$circleLineType,
                       units = .pkgenv$units, ...) {
  out <- grid::grid.circle(x = x,
                           y = y,
                           r = radius,
                           gp = grid::gpar(
                             fill = fillColor,
                             alpha = opacity,
                             col = lineColor,
                             lwd = lineWidth,
                             lty = lineType
                           ),
                           default.units = units, ...)
  invisible(out)
}

#' Draw a Curve on the Page
#'
#' Draws a curve on the page given positioning, dimensions and styling.
#' \cr\cr
#' \if{html}{\figure{drawcurve.png}{options: width=250 alt="Figure: drawCurve Example"}}
#' \if{latex}{\figure{drawcurve.png}{options: width=3in alt="Figure: drawCurve Example"}}
#'
#' @inheritParams drawBox
#' @param x Numeric vector of length 2 for x-axis position of starting and ending points.
#' @param y Numeric vector of length 2 for y-axis position of starting and ending points.
#' @param curvature Numeric value for the curvature of the curve. Values of 0 create a straight line, negative values create left-hand curves, and positive values create right-hand curves.
#' @param angle Numeric value of the curve control point skewness ranging from 0 to 180. Values less than 90 skew towards the start point, and values more than 90 skew towards the end point.
#' @param points Numeric value for the number of curve control points with higher numbers creating a smoother curve.
#' @param shape Numeric value for the shape of the curve ranging from -1 to 1 (See \link[grid]{grid.xspline}).
#' @param square Logical value indicating whether curve control points are created in a city-block or oblique way. It is recommended to set this to TRUE if \emph{points} is 1 and \emph{angle} is 90, and FALSE otherwise (see \link[grid]{grid.curve}).
#' @param squareShape Numeric value controlling curve behaviours relative to control points. Applies if \emph{square} is TRUE.
#' @param inflect Logical value indicating if the curve should be cut in half and inverted (TRUE) or not (FALSE).
#' @param open Logical value indicating whether to open the curve (TRUE) or close the curve (FALSE).
#' @param arrowAngle Numeric value of the angle for arrows. Smaller numbers create narrower arrows, and larger numbers produce wider arrows.
#' @param arrowLength Numeric value of the length for arrows.
#' @param arrowUnits Character value of the measurement unit for arrows.
#' @param arrowEnds Character value indicating which end to draw arrows on lines. Must be one of "last", "first" or "both".
#' @param arrowType Character value indicating if the arrow heads should be closed or open. Must be one of "open" or "closed".
#' @param ... Additional arguments passed to \link[grid]{grid.curve}.
#'
#' @return A \link[grid]{grid.curve} \link[grid]{grob} object.
#'
#' @importFrom grid arrow gpar grid.curve unit
#' @export
#' @seealso \link{drawSettings}
#'
#' @examples
#' library(draw)
#'
#' # Set drawing settings
#' drawSettings(pageWidth = 5, pageHeight = 5, units = "inches")
#'
#' # Create a new drawing page
#' drawPage()
#'
#' # Draw a curved angle
#' drawCurve(x = c(1, 4),
#'           y = c(1, 2),
#'           angle = 90)
#'
#' # Draw a curved angle with arrows
#' drawCurve(x = c(1, 4),
#'           y = c(2, 3.75),
#'           angle = 90, arrowLength = 0.1, arrowEnd = "both")
#'
#' # Draw an inflected curve
#' drawCurve(x = c(1, 4),
#'           y = c(3, 4),
#'           angle = 90, inflect = TRUE)
#'
#' # Export the drawing page to a PDF
#' drawExport("drawCurve.pdf")
#'
drawCurve <- function(x, y,
                      curvature = .pkgenv$curveCurvature,
                      angle = .pkgenv$curveAngle,
                      points = .pkgenv$curvePoints,
                      shape = .pkgenv$curveShape,
                      square = .pkgenv$curveSquare,
                      squareShape = .pkgenv$curveSquareShape,
                      opacity = .pkgenv$curveOpacity,
                      lineColor = .pkgenv$curveLineColor,
                      lineWidth = .pkgenv$curveLineWidth,
                      lineType = .pkgenv$curveLineType,
                      inflect = .pkgenv$curveInflect,
                      open = .pkgenv$curveOpen,
                      arrowAngle = .pkgenv$arrowAngle,
                      arrowLength = .pkgenv$arrowLength,
                      arrowUnits = .pkgenv$arrowUnits,
                      arrowEnds = .pkgenv$arrowEnds,
                      arrowType = .pkgenv$arrowType,
                      units = .pkgenv$units, ...) {
  out <- grid::grid.curve(x1 = x[[1]],
                          y1 = y[[1]],
                          x2 = x[[2]],
                          y2 = y[[2]],
                          curvature = curvature,
                          angle = angle,
                          ncp = points,
                          shape = shape,
                          square = square,
                          squareShape = squareShape,
                          inflect = inflect,
                          open = open,
                          gp = grid::gpar(
                            alpha = opacity,
                            col = lineColor,
                            lwd = lineWidth,
                            lty = lineType
                          ),
                          arrow = grid::arrow(
                            angle = arrowAngle,
                            length = grid::unit(arrowLength, arrowUnits),
                            ends = arrowEnds,
                            type = arrowType
                          ),
                          default.units = units, ...)
  invisible(out)
}

#' Export Current Drawing Page to a File
#'
#' @param f Character value of the file path to save to. Must include file name and extension.
#' @param width Numeric value of the image width.
#' @param height Numeric value of the image height.
#' @param ppi Numeric value of the image resolution quality in Pixels Per Inch (PPI).
#' @param format Character value of the extension for file without a period ".".
#' @param textSize Size of text (pt) in image.
#' @param units Character value for the unit to use when specifying measurements. Can be one of the following:
#' \itemize{
#'   \item "px", "pixels", "pixel", "pix"
#'   \item "in", "inches", "inch"
#'   \item "cm", "centimeters", "centimeter", "centimetre", "centimetres"
#'   \item "mm", "millimeters", "millimeter", "millimetre", "millimetres"
#' }
#' @param ... Additional arguments passed to \link[grDevices]{dev.copy}.
#'
#' @return The name and number of the device, according to \link[grDevices]{dev.copy}, which has been copied to.
#'
#' @importFrom tools file_ext
#' @importFrom grDevices bmp dev.copy dev.off jpeg postscript pdf png setPS setEPS svg tiff
#' @export
#' @seealso \link{drawSettings}
#'
#' @examples
#' library(draw)
#'
#' # Set drawing settings
#' drawSettings(pageWidth = 5, pageHeight = 5, units = "inches")
#'
#' # Create a new drawing page
#' drawPage()
#'
#' # Draw a square
#' drawBox(x = 1, y = 4, width = 2, height = 2)
#'
#' # Export the drawing page to a PDF
#' drawExport("export.pdf")
#'
#' # Export the drawing page to a PNG
#' drawExport("export.png", ppi=300)
#'
drawExport <- function(f,
                       width = .pkgenv$pageWidth,
                       height = .pkgenv$pageHeight,
                       ppi = .pkgenv$exportPPI,
                       format = tools::file_ext(f),
                       textSize = .pkgenv$textSize,
                       units = .pkgenv$exportUnits, ...) {

  # (export_support) Supported vector and raster files
  format <- tolower(format)
  supportedVectors <- c("pdf", "svg", "eps", "ps")
  supportedRasters <- c("bmp", "jpeg", "jpg", "png", "tiff")

  # (export_units) Convert units for dev.copy
  units = tolower(units)
  supportedUnits <- c("in", "inches", "inch",
                      "cm","centimeter", "centimeters", "centimetres", "centimetre",
                      "px", "pixel", "pixels", "pix",
                      "mm", "millimeter", "millimeters", "millimetre", "millimetres")
  if (units %in% c("inches", "inch")) {
    units = "in"
  } else if (units %in% c("centimeter", "centimeters", "centimetres", "centimetre")) {
    units = "cm"
  } else if (units %in% c("pixel", "pixels", "pix")) {
    units = "px"
  } else if (units %in% c("millimeter", "millimeters", "millimetre", "millimetres")) {
    units = "mm"
  }

  # (export_units_error) Error on unsupported units
  if (!units %in% supportedUnits) {
    msg <- paste0("Unknown unit ", "\".", format,  "\"" , " for export. Supported units are:")
    for (supportedFormat in supportedUnits) {
      msg <- paste0(msg, "\n *.", supportedFormat)
    }
    stop(msg)
  }

  # (export_format) Export to vector or raster formats
  if (format %in% supportedVectors) {

    # (export_format_ps) Set postscript types
    if (format == "ps") {
      grDevices::setPS()
      ps <- grDevices::postscript
    } else if (format == "eps") {
      grDevices::setEPS()
      eps <- grDevices::postscript
    }

    # (export_format_vector) Export vector file
    out <- grDevices::dev.copy(pdf,
                               f,
                               width = width,
                               height = height,
                               pointsize = textSize, ...)

  } else if (format %in% supportedRasters) {

    # (export_format_raster) Export raster file
    jpg <- grDevices::jpeg
    out <- grDevices::dev.copy(get(format),
                               f,
                               width = width,
                               height = height,
                               units = units,
                               pointsize = textSize,
                               res = ppi, ...)
  } else {

    # (export_format_error) Unsupported format
    msg <- paste0("Unknown file format ", "\".", format,  "\"" , " for export. Supported file formats are:")
    for (supportedFormat in c(supportedVectors, supportedRasters)) {
      msg <- paste0(msg, "\n *.", supportedFormat)
    }
    stop(msg)
  }
  grDevices::dev.off()
  invisible(out)
}

#' Draw a Line on the Page
#'
#' Draws a line on the page given positioning, dimensions and styling.
#' \cr\cr
#' \if{html}{\figure{drawline.png}{options: width=250 alt="Figure: drawLine Example"}}
#' \if{latex}{\figure{drawline.png}{options: width=3in alt="Figure: drawLine Example"}}
#'
#' @inheritParams drawBox
#' @inheritParams drawCurve
#' @param x Numeric vector for the x-axis positions of the control points.
#' @param y Numeric vector for the y-axis positions of the control points.
#' @param ... Additional arguments passed to \link[grid]{grid.lines}.
#'
#' @return A \link[grid]{grid.lines} \link[grid]{grob} object.
#'
#' @importFrom grid arrow gpar grid.lines unit
#' @export
#' @seealso \link{drawSettings}
#'
#' @examples
#' library(draw)
#'
#' # Set drawing settings
#' drawSettings(pageWidth = 5, pageHeight = 5, units = "inches")
#'
#' # Create a new drawing page
#' drawPage()
#'
#' # Draw a solid line
#' drawLine(x = c(1, 4),
#'          y = c(1 ,1))
#'
#' # Draw a dashed line
#' drawLine(x = c(1, 4),
#'          y = c(2 ,2),
#'          lineType = "dashed")
#'
#' # Draw a dotted line with ending arrow
#' drawLine(x = c(1, 4),
#'          y = c(3 ,3),
#'          lineType = "dotted", arrowEnds = "last")
#'
#' # Draw thick two dash line with starting arrow
#' drawLine(x = c(1, 4),
#'          y = c(4, 4),
#'          lineWidth = 3, lineType = "twodash", arrowEnds = "first")
#'
#' # Export the drawing page to a PDF
#' drawExport("drawLine.pdf")
#'
drawLine <- function(x, y,
                     opacity = .pkgenv$lineOpacity,
                     lineColor = .pkgenv$lineColor,
                     lineWidth = .pkgenv$lineWidth,
                     lineType = .pkgenv$lineType,
                     arrowAngle = .pkgenv$arrowAngle,
                     arrowLength = .pkgenv$arrowLength,
                     arrowUnits = .pkgenv$arrowUnits,
                     arrowEnds = .pkgenv$arrowEnds,
                     arrowType = .pkgenv$arrowType,
                     units = .pkgenv$units, ...){
  out <- grid::grid.lines(x = x,
                          y = y,
                          gp = grid::gpar(
                            alpha = opacity,
                            col = lineColor,
                            lwd = lineWidth,
                            lty = lineType
                          ),
                          arrow = grid::arrow(
                            angle = arrowAngle,
                            length = grid::unit(arrowLength, arrowUnits),
                            ends = arrowEnds,
                            type = arrowType
                          ),
                          default.units = units, ...)
  invisible(out)
}


#' Create a New Drawing Page
#'
#' @param width Numeric value for the page width.
#' @param height Numeric value for the page height.
#' @param units Character value for the \link[grid]{unit} to use when specifying measurements.
#' @param ... Additional arguments to \link[grid]{viewport}.
#'
#' @return A \link[grid]{viewport} object.
#' @export
#' @seealso \link{drawSettings}
#'
#' @examples
#' library(draw)
#'
#' # Set drawing settings
#' drawSettings(pageWidth = 5, pageHeight = 5, units = "inches")
#'
#' # Create a new drawing page
#' drawPage()
#'
drawPage <- function(width = .pkgenv$pageWidth,
                     height = .pkgenv$pageHeight,
                     units = .pkgenv$units, ...) {

  # (page_new) Create blank page and set drawing unit of measure
  grid::grid.newpage()
  .pkgenv$units <- units

  # (page_dimensions) Set page dimensions with viewport
  out <- grid::viewport(width = width,
                        height = height,
                        default.units = units, ...)
  grid::pushViewport(out)
  invisible(out)
}

#' Draw a Point on the Page
#'
#' Draws a point on the page given positioning, dimensions and styling.
#' \cr\cr
#' \if{html}{\figure{drawpoint.png}{options: width=250 alt="Figure: drawPoint Example"}}
#' \if{latex}{\figure{drawpoint.png}{options: width=3in alt="Figure: drawPoint Example"}}
#'
#' @inheritParams drawBox
#' @param pch Numeric value indicating which plotting symbol to use (see \link[graphics]{points}). Some examples include 0 for square, 1 for circle, 2 for triangle, and 4 for X.
#' @param size Numeric value for the size of the point.
#' @param ... Additional arguments passed to \link[grid]{grid.points}.
#'
#' @return A \link[grid]{grid.points} \link[grid]{grob} object.
#'
#' @importFrom grid gpar grid.points unit
#' @export
#' @seealso \link{drawSettings}
#'
#' @examples
#' library(draw)
#'
#' # Set drawing settings
#' drawSettings(pageWidth = 5, pageHeight = 5, units = "inches")
#'
#' # Create a new drawing page
#' drawPage()
#'
#' # Draw a square point
#' drawPoint(x = 1, y = 4, pch = 0)
#'
#' # Draw a larger circle point
#' drawPoint(x = 4, y = 4, pch = 1, size = 1.5)
#'
#' # Draw a triangle point
#' drawPoint(x = 1, y = 1, pch = 2, lineWidth = 2)
#'
#' # Draw a red X point
#' drawPoint(x = 4, y = 1, pch = 4, lineColor = "red")
#'
#' # Export the drawing page to a PDF
#' drawExport("drawPoint.pdf")
#'
drawPoint <- function(x, y,
                      pch = .pkgenv$pointPCH,
                      size = .pkgenv$pointSize,
                      fillColor = .pkgenv$pointFillColor,
                      opacity = .pkgenv$pointOpacity,
                      lineColor = .pkgenv$pointLineColor,
                      lineWidth = .pkgenv$pointLineWidth,
                      lineType = .pkgenv$pointLineType,
                      units = .pkgenv$units, ...) {
  out <- grid::grid.points(x = x,
                           y = y,
                           pch = pch,
                           size = grid::unit(size, units),
                           gp = grid::gpar(
                             fill = fillColor,
                             alpha = opacity,
                             col = lineColor,
                             lwd = lineWidth,
                             lty = lineType
                           ),
                           default.units = units)

  invisible(out)
}

#' Modify Default Drawing Settings
#'
#' @param ... Default settings to modify draw package settings stored in an environment.
#'
#' @details The following parameters are available to change: \cr
#'
#' \strong{*}
#' \itemize{
#'   \item \strong{units} [default = "inches"]: Character value of default measurement unit for applicable draw function parameters (such as width, height, radius, x, y, etc).
#' }
#'
#' \link[grid]{arrow}
#' \itemize{
#'   \item \strong{arrowAngle} [default = 30]: Numeric value of the angle for arrows. Smaller numbers create narrower arrows, and larger numbers produce wider arrows.
#'   \item \strong{arrowLength} [default = 0]: Numeric value of the length for arrows.
#'   \item \strong{arrowUnits} [default = \emph{units}]: Character value of the measurement unit for arrows. Default is the same as \emph{units} parameter under \strong{*}.
#'   \item \strong{arrowEnds} [default = "last"]: Character value indicating which end to draw arrows on lines. Must be one of "last", "first" or "both".
#'   \item \strong{arrowType} [default = "open"]: Character value indicating if the arrow heads should be closed or open. Must be one of "open" or "closed".
#' }
#'
#' \link{drawBox}
#' \itemize{
#'   \item \strong{boxWidth} [default = 1]: Numeric value of the width for boxes.
#'   \item \strong{boxHeight} [default = 1]: Numeric value of the height for boxes.
#'   \item \strong{boxRadius} [default = 0]: Numeric value of the radius for boxes to create rounded corners.
#'   \item \strong{boxFillColor} [default = "transparent"]: Character value of the color to fill boxes with.
#'   \item \strong{boxOpacity} [default = 1]: Numeric value of transparency for boxes ranging from 0 (transparent) to 1 (non-transparent).
#'   \item \strong{boxLineWidth} [default = 1]: Numeric value of the width for the box lines.
#'   \item \strong{boxLineType} [default = "solid"]: Character value of the type of line for the boxes. One of "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash" (see "lty" in \link[graphics]{par}).
#'   \item \strong{boxLineColor} [default = "black"]: Character value of the color for the box lines.
#' }
#'
#' \link{drawCircle}
#' \itemize{
#'   \item \strong{circleRadius} [default = 0.5]: Numeric value of the radius for circles.
#'   \item \strong{circleFillColor} [default = "transparent"]: Character value of the color to fill circles with.
#'   \item \strong{circleOpacity} [default = 1]: Numeric value of the transparency for circles ranging from 0 (transparent) to 1 (non-transparent).
#'   \item \strong{circleLineWidth} [default = 1]: Numeric value of the width for the circle lines.
#'   \item \strong{circleLineType} [default = "solid"]: Character value of the type of line for the circles. One of "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash" (see "lty" in \link[graphics]{par}).
#'   \item \strong{circleLineColor} [default = "black"]: Character value of the color for the circle lines.
#' }
#'
#' \link{drawCurve}
#' \itemize{
#'   \item \strong{curveCurvature} [default = 1]: Numeric value of the curvature for the curves. Values of 0 create a straight line, negative values create left-hand curves, and positive values create right-hand curves.
#'   \item \strong{curveAngle} [default = 90]: Numeric value of the curve control point skewness ranging from 0 to 180. Values less than 90 skew towards the start point, and values more than 90 skew towards the end point.
#'   \item \strong{curvePoints} [default = 1]: Numeric value of the number of control points for the curves, where higher values create smoother curves.
#'   \item \strong{curveShape} [default = 0.5]: Numeric value of the shape for the curves ranging from -1 to 1 (See \link[grid]{grid.xspline}).
#'   \item \strong{curveSquare} [default = TRUE]: Logical value indicating whether curve control points are created in a city-block or oblique way. It is recommended to set this to TRUE if \emph{curvePoints} is 1 and \emph{curveAngle} is 90, and FALSE otherwise (see \link[grid]{grid.curve}).
#'   \item \strong{curveSquareShape} [default = 1]: Numeric value controlling curve behaviours relative to control points. Applies if \emph{curveSquare} is TRUE.
#'   \item \strong{curveOpacity} [default = 1]: Numeric value of the transparency for the curves ranging from 0 (transparent) to 1 (non-transparent).
#'   \item \strong{curveLineColor} [default = "black"]: Character value of the color for the curve lines.
#'   \item \strong{curveLineWidth} [default = 1]: Character value of the width for the curve lines.
#'   \item \strong{curveLineType} [default = "solid"]: Character value of the type of line for the curves. One of "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash" (see "lty" in \link[graphics]{par}).
#'   \item \strong{curveInflect} [default = FALSE]: Logical value indicating if curve should be cut in half and inverted (TRUE) or not (FALSE).
#'   \item \strong{curveOpen} [default = TRUE]: Logical value indicating whether to open the curves (TRUE) or close the curves (FALSE).
#' }
#'
#' \link{drawExport}
#' \itemize{
#'   \item \strong{exportUnits} [default = \emph{units}]: Character value of measurement unit for exporting. Default is the same as \emph{units} parameter under \strong{*}.
#'   \item \strong{exportPPI} [default = 150]: Numeric value of the image quality measured in Pixels Per Inch (PPI).
#' }
#'
#' \link{drawLine}
#' \itemize{
#'   \item \strong{lineOpacity} [default = 1]: Numeric value of the transparency of the lines ranging from 0 (transparent) to 1 (non-transparent).
#'   \item \strong{lineWidth} [default = 1]: Numeric value of the width of the lines.
#'   \item \strong{lineType} [default = "solid"]: Character value of the type for the lines. One of "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash" (see "lty" in \link[graphics]{par}).
#'   \item \strong{lineColor} [default = "black"]: Character value of the color of the lines.
#' }
#'
#' \link{drawPage}
#' \itemize{
#'   \item \strong{pageWidth} [default = 8.5]: Numeric value of the width of the drawing page.
#'   \item \strong{pageHeight} [default = 11]: Numeric value of the height of the drawing page.
#' }
#'
#' \link{drawPoint}
#' \itemize{
#'   \item \strong{pointPCH} [default = 20]: Numeric value indicating which plotting symbol to use (see \link[graphics]{points}). Some examples include 0 for square, 1 for circle, 2 for triangle, and 4 for X.
#'   \item \strong{pointSize} [default = 1]: Numeric value of the point size.
#'   \item \strong{pointFillColor} [default = "transparent"]: Character value of the color to fill each point.
#'   \item \strong{pointOpacity} [default = 1]: Numeric value of the transparency for the points ranging from 0 (transparent) to 1 (non-transparent).
#'   \item \strong{pointLineColor} [default = "black"]: Character value of the color of the points.
#'   \item \strong{pointLineType} [default = "solid"]: Character value of the type for the lines. One of "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash" (see "lty" in \link[graphics]{par}).
#'   \item \strong{pointLineWidth} [default = 1]: Numeric value of the width of the point lines.
#' }
#'
#' \link{drawText}
#' \itemize{
#'   \item \strong{text} [default = "text"]: Character value of the text to display at the defined position.
#'   \item \strong{textJust} [default = "centre"]: Character value of the text justification. One of "left", "right", "centre", "center", "bottom", or "top".
#'   \item \strong{textHjust} [default = NULL]: Numeric value of the horizontal justification.
#'   \item \strong{textVjust} [default = NULL]: Numeric value of the vertical justification.
#'   \item \strong{textAngle} [default = 0]: Numeric value of the angle to rotate text.
#'   \item \strong{textOverlap} [default = FALSE]: Logical value indicating if overlapping text should be removed (TRUE) or not (FALSE).
#'   \item \strong{textOpacity} [default = 1]: Numeric value of the transparency for text ranging from 0 (transparent) to 1 (non-transparent).
#'   \item \strong{textColor} [default = "black"]: Character value of the color for text.
#'   \item \strong{textSize} [default = 12]: Numeric value of the text font size in pt.
#'   \item \strong{textFace} [default = "plain"]: Character value of the text font face. One of "plain", "bold", "italic", "oblique", and "bold.italic" (see \emph{fontface} in \link[grid]{gpar}).
#'   \item \strong{textFamily} [default = "sans"]: Character value of text font family to use (see \emph{family} in \link[graphics]{par}). Common values are "serif", "sans" and "mono".
#'   \item \strong{textLineHeight} [default = 1.2]: Numeric value of text line height as a multiple of the size of the text.
#' }
#'
#' @return A list of the current draw settings with changes.
#' @export
#'
#' @examples
#' library(draw)
#'
#' # Set page dimensions and units to inches
#' drawSettings(pageWidth = 5, pageHeight = 5, units = "inches")
#'
#' # Set export resolution in Pixels Per Inch (PPI)
#' drawSettings(exportPPI = 300)
#'
#' # Set default width and height for all boxes in inches
#' drawSettings(boxWidth = 1, boxHeight = 1)
#'
#' # Create a new drawing page
#' drawPage()
#'
#' # Draw default 1 by 1 inch boxes near the center
#' drawBox(x = 2, y = 2.5)
#' drawBox(x = 3, y = 2.5)
#'
#' # Draw a non-default 2 by 2 inch box in the center
#' drawBox(x = 2.5, y = 2.5, width = 2, height = 2)
#'
#' # Export the current drawing page
#' drawExport("drawSettingsExample.pdf")
#'
drawSettings <- function(...) {
  settings <- as.list(match.call())
  keys <- names(settings)
  for (s in keys[2:length(keys)]) {
    .pkgenv[[s]] <- settings[[s]]
  }
  invisible(as.list(.pkgenv))
}


#' Draw Text on the Page
#'
#' Draws text on the page given positioning, dimensions and styling.
#' \cr\cr
#' \if{html}{\figure{drawtext.png}{options: width=250 alt="Figure: drawText Example"}}
#' \if{latex}{\figure{drawtext.png}{options: width=3in alt="Figure: drawText Example"}}
#'
#' @inheritParams drawBox
#' @param text Character value of the text to display at \emph{xy} position.
#' @param just Character value of the text justification. One of "left", "right", "centre", "center", "bottom", or "top".
#' @param hjust Numeric value of the horizontal justification.
#' @param vjust Numeric value of the vertical justification.
#' @param angle Numeric value of the angle to rotate text.
#' @param overlap Logical value indicating if overlapping text should be removed (TRUE) or not (FALSE).
#' @param opacity Numeric value of the transparency for text ranging from 0 (transparent) to 1 (non-transparent).
#' @param color Character value of the color for text.
#' @param size Numeric value of the text font size in pt.
#' @param face Character value of the text font face. One of "plain", "bold", "italic", "oblique", and "bold.italic" (see \emph{fontface} in \link[grid]{gpar}).
#' @param family Character value of text font family to use (see \emph{family} in \link[graphics]{par}). Common values are "serif", "sans" and "mono".
#' @param lineHeight Numeric value of text line height as a multiple of the size of the text.
#' @param ... Additional arguments passed to \link[grid]{grid.text}.
#'
#' @return A \link[grid]{grid.text} \link[grid]{grob} object.
#'
#' @importFrom grid gpar grid.text unit
#' @export
#' @seealso \link{drawSettings}
#'
#' @examples
#' library(draw)
#'
#' # Set drawing settings
#' drawSettings(pageWidth = 5, pageHeight = 5, units = "inches")
#'
#' # Create a new drawing page
#' drawPage()
#'
#' # Draw text on top left corner
#' drawText(x = 1, y = 4, text = "Top Left")
#'
#' # Draw bold text on top right corner
#' drawText(x = 4, y = 4, text = "Top Right", face = "bold")
#'
#' # Draw serif text on bottom left corner
#' drawText(x = 1, y = 1, text = "Bottom Left", family = "serif")
#'
#' # Draw larger text on bottom right corner
#' drawText(x = 4, y = 1, text = "Bottom Right", size = 14)
#'
#' # Export the drawing page to a PDF
#' drawExport("drawText.pdf")
#'
drawText <- function(x, y,
                     text = .pkgenv$text,
                     just = .pkgenv$textJust,
                     hjust = .pkgenv$textHjust,
                     vjust = .pkgenv$textVjust,
                     angle = .pkgenv$textAngle,
                     overlap = .pkgenv$textOverlap,
                     opacity = .pkgenv$textOpacity,
                     color = .pkgenv$textColor,
                     size = .pkgenv$textSize,
                     face = .pkgenv$textFace,
                     family = .pkgenv$textFamily,
                     lineHeight = .pkgenv$textLineHeight,
                     units = .pkgenv$units, ...) {
  out <- grid::grid.text(x = x,
                         y = y,
                         label = text,
                         just = just,
                         hjust = hjust,
                         vjust = vjust,
                         rot = angle,
                         check.overlap = overlap,
                         gp = grid::gpar(
                           alpha = opacity,
                           col = color,
                           fontsize = size,
                           fontface = face,
                           fontfamily = family,
                           lineheight = lineHeight
                         ),
                         default.units = units)
  invisible(out)
}
