#' @name output_gadget
#' @title 'RAVE' dashboard output gadgets
#' @param outputId output ID in the root scope of shiny session
#' @param inputId input ID, automatically assigned internally
#' @param icon gadget icon
#' @param type,gadgets gadget type(s), currently supported: \code{'standalone'},
#' \code{'download'}, \code{'actionbutton'}
#' @param class additional class to the gadget or its container
#' @param expr shiny output call expression, for example,
#' \code{shiny::plotOutput({...})}
#' @param quoted whether \code{expr} is quoted; default is false
#' @param env environment where \code{expr} should be evaluated
#' @param container optional container for the gadgets and outputs; will be ignored
#' if \code{wrapper} is false
#' @param wrapper whether to wrap the gadgets and the output within
#' a 'HTML' container
#' @param ... ignored
#' @export
output_gadget <- function(
  outputId, icon = NULL,
  type = c("standalone", "download", 'actionbutton', 'custom'),
  class = NULL, inputId = NULL, ...
) {
  type <- match.arg(type)

  if(is.null(icon)) {
    icon <- switch (
      type,
      standalone = shiny_icons$external_link,
      download = shiny_icons$download,
      {
        shiny_icons$puzzle
      }
    )
  }

  title <- switch (
    type,
    standalone = "Open in a new tab",
    download = "Download",
    {
      ""
    }
  )

  if(type %in% c('actionbutton', 'download')) {
    if(length(inputId) != 1) {
      inputId <- sprintf("%s__%s", outputId, type)
    }
  }

  class <- dipsaus::combine_html_class(
    "ravedash-output-widget",
    switch (
      type,
      actionbutton = "action-button shiny-bound-input",
      download = "shiny-download-link",
      {NULL}
    ),
    class
  )

  args <- list(
    href = "#",
    class = class,
    `data-type` = type,
    `data-target` = outputId,
    title = title,
    icon,
    ...
  )
  args$id <- inputId
  if(type %in% c("download")) {
    args$target <- "_blank"
    args$download <- ""
    args$href <- ""
  }

  do.call(shiny::tags$a, args)
}

#' @rdname output_gadget
#' @export
output_gadget_container <- function(
    expr, gadgets = c("standalone", "download"),
    quoted = FALSE, env = parent.frame(), outputId = NULL,
    class = NULL, container = NULL, wrapper = TRUE) {

  if(is.null(container)) {
    if(wrapper) {
      container <- function(...) {
        shiny::div(
          class = "ravedash-output-widget-wrapper",
          ...
        )
      }
    } else {
      container <- function(...) {
        shiny::tagList(...)
      }
    }
  }

  if(!quoted) {
    expr <- substitute(expr)
  }
  if(is.null(outputId)) {
    expr <- match.call(definition = eval(expr[[1]], envir = env), call = expr, expand.dots = TRUE, envir = env)
    outputId <- eval(expr$outputId, envir = env)
  }

  gs <- lapply(gadgets, function(type) {
    output_gadget(
      outputId = outputId,
      type = type
    )
  })
  if(length(gs)) {
    gs <- shiny::div(class = dipsaus::combine_html_class(
      "ravedash-output-widget-container", class), gs)
  } else {
    gs <- NULL
  }
  container(
    gs,
    eval(expr, envir = env)
  )

}

