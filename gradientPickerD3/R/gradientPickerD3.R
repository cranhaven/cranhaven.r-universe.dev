#' gradientPickerD3
#'
#' Creates a widget for an interactive selection and modification of a color gradient.
#' 'gradientPickerD3' allows the addition, removement and replacement of color ticks.
#' List of numeric values will automatically translate in its corresponding tick position within the numeric range.
#' App returns a data.frame containing tick values, colors and the positions in percent (0.0 to 1.0) for each color tick in the gradient.
#' The original JS 'jquery-gradient-picker' was implemented by Matt Crinklaw-Vogt.
#' Widget and JS modifications were done by CD. Peikert.
#'
#' @param payload list containing 'ticks' and 'colors' to initialize the gradient. Ticks have to been numerical and in a logical order. Colors can be provided as R colors or HEX format.
#' @param width,height must be a valid CSS unit (like \code{'100\%'})
#' @param elementId \code{string} id as a valid \code{CSS} element id
#' @param border_extensions add to the min and max data range to cover the whole color spectrum
#' @param decimal_places number of decimal places
#' @importFrom htmlwidgets createWidget
#' @importFrom jsonlite toJSON
#' @examples
#' ticks <- c(-1.8740103,  -0.0040747,  1.4022244,  2.2177949,  3.2116766)
#' payload <- list(
#'   colors=c("purple","blue", "green", "yellow", "red"),
#'   ticks=ticks
#' )
#' gradientPickerD3(payload)
#' @seealso \link{gradientPickerD3_example}
#' @source The interface was designed based on jquery-gradient-picker \url{https://github.com/tantaman/jquery-gradient-picker}, \link{htmlwidgets} and \link{shiny}
#' @export
#'
gradientPickerD3 <-
  function(payload,
           width = NULL,
           height = NULL,
           elementId = NULL,
           border_extensions = 0.001,
           decimal_places = 8) {
    if (length(payload) == 2) {
      payload$ticks <- sapply(payload$ticks, round, decimal_places)
      payload$ticks[1] <-  payload$ticks[1] - border_extensions
      n <- length(payload$ticks)
      payload$ticks[n] <-  payload$ticks[n] + border_extensions
      shift_ticks <- payload$ticks - min(payload$ticks)
      payload[["procent"]] <-
        round(shift_ticks / diff(range(shift_ticks)), decimal_places)
      payload[["colorstring"]] <-
        paste0(payload$colors, ' ', payload[["procent"]] * 100, '%')
      payload$ticks <- sapply(payload$ticks, round, decimal_places)
    }
    
    x <- jsonlite::toJSON(payload)
    
    # create widget
    htmlwidgets::createWidget(
      name = 'gradientPickerD3',
      x,
      width = width,
      height = height,
      package = 'gradientPickerD3',
      elementId = elementId
    )
  }

#' Shiny bindings for gradientPickerD3
#'
#' Output and render functions for using gradientPickerD3 within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a gradientPickerD3
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @importFrom htmlwidgets shinyWidgetOutput
#' @name gradientPickerD3-shiny
#'
#' @export
gradientPickerD3Output <-
  function(outputId,
           width = '100%',
           height = '100%') {
    htmlwidgets::shinyWidgetOutput(outputId, 'gradientPickerD3', width, height, package = 'gradientPickerD3')
  }

#' @importFrom htmlwidgets shinyRenderWidget
#' @rdname gradientPickerD3-shiny
#' @export
renderGradientPickerD3 <-
  function(expr,
           env = parent.frame(),
           quoted = FALSE) {
    if (!quoted) {
      expr <- substitute(expr)
    } # force quoted
    htmlwidgets::shinyRenderWidget(expr, gradientPickerD3Output, env, quoted = TRUE)
  }
