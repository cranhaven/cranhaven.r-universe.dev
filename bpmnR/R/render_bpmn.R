# ============================== MAIN FUNCTION =================================

#' Render BPMN diagram.
#'
#' This renders a BPMN diagram based on a BPMN object.
#'
#' @param bpmn A BPMN object as a list of data.frames for the BPMN elements and
#'   an XML document for the XML-based interchange format for the BPMN process.
#' @param viewer.suppress Never display the widget within the RStudio Viewer
#'   (useful for widgets that require a large amount of space for rendering).
#'   Defaults to FALSE.
#' @param width Fixed width for widget (in css units). The default is NULL,
#'   which results in intelligent automatic sizing based on the widget's
#'   container.
#' @param height Fixed height for widget (in css units). The default is NULL,
#'   which results in intelligent automatic sizing based on the widget's
#'   container.
#' @param elementId Use an explicit element ID for the widget (rather than an
#'   automatically generated one). Useful if you have other JavaScript that
#'   needs to explicitly discover and interact with a specific widget instance.
#' @param xml_version_number The version of the XML standard used.
#' @param xml_encoding_declaration The character encoding used in the XML
#'   declaration. \sQuote{UTF-8} is the default encoding used.
#' @param ... Additional arguments passed to methods.
#'
#' @author Alessio Nigro
#'
#' @import htmlwidgets
#' @importFrom xml2 read_xml
#' @return Rendered BPMN model in htmlwidget.
#'
#' @examples
#'
#' library(dplyr)
#' nodes <- tibble(id = "task", name = "Task name",
#' objectType = "task", gatewayDirection = NA)
#' events <- tibble(id = c("start","end"), name = c("Start event","End event"),
#' objectType = c("startEvent","endEvent"))
#' flows <- tibble(id = c("flow1","flow2"), name = c("flow1","flow2"),
#' sourceRef = c("start","task"), targetRef = c("task","end"),
#' objectType = c("sequenceFlow","sequenceFlow"))
#' model <- create_bpmn(nodes, flows, events)
#' render_bpmn(model)
#'
#' @export
render_bpmn <- function(bpmn,
                        viewer.suppress = FALSE,
                        width = NULL,
                        height = NULL,
                        elementId = NULL,
                        xml_version_number = "1.0",
                        xml_encoding_declaration = "UTF-8",
                        ...) {
  UseMethod("render_bpmn")
}

#' @rdname render_bpmn
#' @export
render_bpmn.bpmn <-
  function(bpmn,
           viewer.suppress = FALSE,
           width = NULL,
           height = NULL,
           elementId = NULL,
           xml_version_number = "1.0",
           xml_encoding_declaration = "UTF-8",
           ...) {
    # Defines XML declaration to check it if "bpmn" is not of class "bpmn"
    xml_declaration <-
      paste0(
        '<?xml version="',
        xml_version_number,
        '" encoding="',
        xml_encoding_declaration,
        '"?>'
      )

    # Converts XML to character type
    if (inherits(bpmn, "bpmn")) {
      # Converts XML part of BPMN object to character type
      bpmn_model <- as.character(bpmn[["xml"]])
    } else if (inherits(bpmn, "character") &&
               substring(bpmn, 1, 38) != xml_declaration) {
      # Reads XML from file and converts it to character type
      # (currently not possible to execute this statement)
      bpmn_model <- as.character(read_xml(bpmn))
    } else {
      # Converts XML to character type
      # (currently not possible to execute this statement)
      bpmn_model <- as.character(bpmn)
    }

    # Forwards options using "x"
    x <- list(bpmn_model = bpmn_model)

    # Creates widget
    htmlwidgets::createWidget(
      name = "render_bpmn",
      x,
      width = width,
      height = height,
      package = "bpmnR",
      elementId = elementId,
      sizingPolicy(
        viewer.fill = TRUE,
        viewer.suppress = viewer.suppress,
        browser.fill = TRUE
      )
    )
  }

#' Shiny bindings for render_bpmn
#'
#' Output and render functions for using render_bpmn within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a render_bpmn
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @author Alessio Nigro
#' @return Rendered BPMN model in Shiny widget.
#' @name render_bpmn-shiny
#'
#' @export
render_bpmnOutput <-
  function(outputId,
           width = "100%",
           height = "400px") {
    htmlwidgets::shinyWidgetOutput(outputId, "render_bpmn", width, height, package = "bpmnR")
  }

#' @author Alessio Nigro
#'
#' @return Rendered BPMN model in Shiny widget.
#' @rdname render_bpmn-shiny
#' @export
renderRender_bpmn <-
  function(expr,
           env = parent.frame(),
           quoted = FALSE) {
    # Forces "quoted"
    if (!quoted) {
      expr <- substitute(expr)
    }
    htmlwidgets::shinyRenderWidget(expr, render_bpmnOutput, env, quoted = TRUE)
  }
