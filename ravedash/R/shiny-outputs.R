#' @name register_output
#' @title Register output and output options
#' @description Enable advanced output gadgets such as expanding the output
#' in another browser window, or downloading the rendered data.
#' @param outputId output ID in the scope of current shiny session
#' @param session shiny session instance
#' @param ...,output_opts,.opt output options
#' @param extras extra information to store
#' @param render_function shiny render function
#' @param export_type type of export file formats supported, options are
#' \code{'none'} (do not export), \code{'custom'}, \code{'pdf'} (for figures),
#' \code{'csv'} (for tables), \code{'3dviewer'} (for 'RAVE' 3D viewers),
#' \code{'htmlwidget'} (for 'HTML' widgets).
#' @param export_settings a list of settings, depending on export type; see
#' 'Details'.
#' @param quoted whether \code{render_function} is quoted; default is false
#' @returns Registered output or output options.
#' @details Default shiny output does not provide handlers for downloading the
#' figures or data, and is often limited to the 'HTML' layouts. 'RAVE' dashboard
#' provides such mechanisms automatically with few extra configurations.
#' @examples
#'
#'
#' if(interactive()) {
#'
#' library(shiny)
#' library(ravedash)
#'
#' rave_id <- paste(sample(c(letters, LETTERS, 0:9), 20, replace = TRUE),
#'                  collapse = "")
#'
#' ui <- function(req) {
#'   query_string <- req$QUERY_STRING
#'   if(length(query_string) != 1) {
#'     query_string <- "/"
#'   }
#'   query_result <- httr::parse_url(query_string)
#'
#'   if(!identical(toupper(query_result$query$standalone), "TRUE")) {
#'     # normal page
#'     basicPage(
#'       output_gadget_container(
#'         plotOutput("plot", brush = shiny::brushOpts("plot__brush")),
#'       )
#'     )
#'   } else {
#'     # standalone viewer
#'     uiOutput("viewer")
#'   }
#' }
#'
#' server <- function(input, output, session) {
#'
#'   bindEvent(
#'     safe_observe({
#'       query_string <- session$clientData$url_search
#'       query_result <- httr::parse_url(query_string)
#'
#'       if(!identical(toupper(query_result$query$module), "standalone_viewer")) {
#'         # normal page
#'         register_rave_session(session = session, .rave_id = rave_id)
#'         register_output(
#'           renderPlot({
#'             plot(1:100, pch = 16)
#'           }),
#'           outputId = "plot", export_type = "pdf",
#'           output_opts = list(brush = shiny::brushOpts("plot__brush"))
#'         )
#'         output$plot <- renderPlot({
#'           input$btn
#'           plot(rnorm(100), pch = 16)
#'         })
#'       } else {
#'         # standalone viewer
#'         standalone_viewer(outputId = "plot", rave_id = rave_id)
#'       }
#'     }),
#'     session$clientData$url_search
#'   )
#'
#'
#' }
#'
#' shinyApp(ui, server, options = list(port = 8989))
#' }
#'
NULL

#' @rdname register_output
#' @export
register_output_options <- function(
    outputId, ..., .opt = list(), extras = list(),
    session = shiny::getDefaultReactiveDomain()) {
  reactive_handlers <- get_default_handlers(session)
  if(!"output_options" %in% names(reactive_handlers)) {
    reactive_handlers$output_options <- dipsaus::fastmap2()
  }
  output_options <- reactive_handlers$output_options
  re <- as.list(output_options[[session$ns(outputId)]])
  re$args <- c(list(...), .opt)
  if(!inherits(re$extras, "fastmap2")) {
    re$extras <- dipsaus::fastmap2()
  }
  dipsaus::list_to_fastmap2(as.list(extras), re$extras)
  output_options[[session$ns(outputId)]] <- re
  invisible(re)
}

#' @rdname register_output
#' @export
get_output_options <- function(outputId, session = shiny::getDefaultReactiveDomain()) {
  reactive_handlers <- get_default_handlers(session = session)
  output_options <- reactive_handlers$output_options
  if(!is.list(output_options)) {
    return(list())
  }
  as.list(output_options[[session$ns(outputId)]])
}


#' @rdname register_output
#' @export
register_output <- function(
    render_function, outputId,
    export_type = c("none", "custom", "pdf", "csv", "3dviewer", "htmlwidget"),
    export_settings = list(), quoted = FALSE,
    output_opts = list(),
    session = shiny::getDefaultReactiveDomain()) {

  export_type <- match.arg(export_type)
  fenv <- new.env(parent = parent.frame())

  # this is root session
  opt <- get_output_options(outputId, session = session)
  extras <- opt$extras
  if(!is.list(extras) || !inherits(extras, "fastmap2")) {
    extras <- list()
  }

  for(nm in names(output_opts)) {
    if(nchar(nm)) {
      opt$args[[nm]] <- output_opts[[nm]]
    } else {
      stop("register_output: `output_opts` must be a named list")
    }
  }

  # no file downloader is considered
  # obtain the expression
  find_expr <- function(call) {
    if(!is.call(call)) { return() }
    call <- match.call(
      definition = eval(call[[1]], envir = fenv),
      call = call, expand.dots = TRUE)
    call_list <- as.list(call)
    argnames <- names(call_list)
    if("expr" %in% argnames) {
      expr <- call[['expr']]
      env <- eval(call[['env']], envir = fenv)
      if(!is.environment(env)) {
        env <- eval(call[['envir']], envir = fenv)
      }
      if(!is.environment(env)) {
        env <- fenv
      }
      return(list(
        expr = expr,
        env = env
      ))
    }
    if("x" %in% argnames) {
      Recall(call[['x']])
    }
  }

  if(!quoted) {
    render_expr0 <- substitute(render_function)
  } else {
    render_expr0 <- render_function
    render_function <- eval(render_function, envir = fenv)
  }

  render_details <- find_expr(render_expr0)
  if(is.list(render_details)) {
    extras$render_expr <- render_details$expr
    extras$render_env <- render_details$env
  }

  extras$export_type <- export_type
  if( export_type != "none" ) {
    logger("Registering output {outputId} with {export_type} download type.", level = "trace", use_glue = TRUE)

    export_settings <- as.list(export_settings)
    export_settings$outputId <- outputId
    export_settings <- do.call(sprintf("format_export_settings.%s", export_type), list(export_settings))
    extras$export_settings <- export_settings

    session$output[[sprintf("%s__download", outputId)]] <- shiny::downloadHandler(
      filename = function() {
        fname <- export_settings$filename
        if(is.null(fname)) {
          fname <- sprintf("[rave-export]%s.%s", session$ns(outputId),
                           export_settings$extension)
        } else {
          fname <- paste0(fname, ".", export_settings$extension)
        }
        fname
      },
      content = function(con) {

        data <- NULL
        opt <- get_output_options(outputId, session = session)
        render_expr <- opt$extras$render_expr
        render_env <- opt$extras$render_env

        if(is.environment(render_env) && is.language(render_expr)) {
          export_settings$pre(con)
          on.exit({ export_settings$post(con, data) }, add = TRUE, after = FALSE)
          shiny::isolate({
            data <- eval(render_expr, envir = new.env(parent = render_env))
          })
        } else {
          stop("Cannot find renderer's details from the following renderer's expression: \n", deparse1(render_expr0))
        }

      }
    )
  }

  session$output[[outputId]] <- render_function

  register_output_options(
    outputId = outputId, .opt = opt$args,
    session = session, extras = extras
  )

}

#' @rdname register_output
get_output <- function(outputId, session = shiny::getDefaultReactiveDomain()) {

  module_id <- session$ns(NULL)
  # make sure we are working on the root scope
  session <- session$rootScope()

  # get module information
  if(!is.null(module_info)) {
    module_info <- get_active_module_info(session = session)
    module_id <- module_info$id
  }

  # get renderer's information
  reactive_handlers <- get_default_handlers(session = session)
  output_options <- reactive_handlers$output_options
  ns <- shiny::NS(module_id)
  outputId_full <- ns(outputId)

  output_opt <- get_output_options(outputId = outputId_full, session = session)

  render_function <- session$getOutput(outputId_full)

  if(!is.function(render_function)) {
    stop("Cannot find render function for output: ", outputId)
  }

  # get output function
  ui_function <- attr(render_function, "outputFunc")

  # get output options
  output_opt <- as.list(output_options[[outputId_full]])

  list(
    namespace = ns(NULL),
    outputId = outputId,
    outputId_full = outputId_full,
    render_function = render_function,
    output_function = ui_function,
    output_args = output_opt$args,
    extras = output_opt$extras
  )

}
