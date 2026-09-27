#' Reproduce Code
#'
#' @description
#' Construct the code within a given `shiny::reactive` object
#' to be able to re-create the output outside of a Shiny session.
#'
#' @param x `shiny::reactive` object to make reproducible
#'
#' @returns
#' A character string, that when printed (using `base::cat`),
#' displays the script that reproduces the contents of `x`.
#'
#' @examples
#' library(shiny)
#'
#' ui <- fluidPage(
#'   h1("Reproducible Code Example"),
#'   inputPanel(
#'     sliderInput(
#'       "min_width",
#'       "Minimum Petal Width",
#'       min(iris$Petal.Width),
#'       max(iris$Petal.Width),
#'       min(iris$Petal.Width),
#'       step = 0.1
#'     ),
#'     selectInput(
#'       "summary_fn",
#'       "Summary Function",
#'       c("Mean" = "mean", "Median" = "median", "SD" = "sd"),
#'       selected = "mean"
#'     )
#'   ),
#'   fluidRow(
#'     column(
#'       width = 5,
#'       h2("Table"),
#'       tableOutput("table")
#'     ),
#'     column(
#'       width = 7,
#'       h2("Code"),
#'       verbatimTextOutput("code")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   iris_filt <- reactive({
#'     iris[with(iris, Petal.Width > input$min_width), ]
#'   })
#'
#'   summary_tbl <- reactive({
#'     aggregate(
#'       Sepal.Width ~ Species,
#'       data = iris_filt(),
#'       FUN = get(input$summary_fn)
#'     )
#'   })
#'
#'   output$table <- renderTable(summary_tbl())
#'   output$code <- renderText(reprex_reactive(summary_tbl))
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' @export
reprex_reactive <- function(x) {
  if (!inherits(x, "reactiveExpr")) {
    x_name <- rlang::caller_arg(x)

    if (is.character(x_name) && grepl("^[A-Za-z0-9_\\.]+\\(\\)$", x_name)) {
      stop(
        sub("()", "", x_name, fixed = TRUE),
        " has already been evaluated, please remove brackets to pass through reactive object"
      )
    } else {
      stop("Unable to generate reproducible code for ", x_name, ", must be an unevaluated reactive object")
    }
  }

  repro_chunk(x)@script
}
