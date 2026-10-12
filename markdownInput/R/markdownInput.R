## Contains the module's functions


# UI function ------------------------------------------------------------------
#' Create a markdown input control with a result preview
#'
#' @param inputId The code{input} slot that will be used to access the value.
#' @param label Label of the input.
#' @param value The initial text to be contained in the editor.
#' @param theme The Ace theme to be used by the editor. The theme in Ace
#' determines the styling and coloring of the editor.
#' Use \code{\link[shinyAce]{getAceModes}} to enumerate all the themes
#' available.
#' @param height A number (which will be interpreted as a number of pixels) or
#' any valid CSS dimension (such as "50\%", "200px", or "auto").
#' @param class The CSS class name of the input. (optional)
#'
#' @return A tabset containing two tabs: \enumerate{ \item{"Write" tab:
#'   }{Containing a code editor (\code{\link[shinyAce]{aceEditor}}).}
#'   \item{"Preview" tab: }{Containing the preview of the markdown render.} }
#'
#' @import shiny
#' @import shinyAce
#'
#' @author Julien Diot \email{juliendiot42@@gmail.com}
#'
#' @references
#' \enumerate{
#'  \item{\href{https://CRAN.R-project.org/package=shinyAce}{shinyAce} package:
#' }{Vincent Nijs, Forest Fang, Trestle Technology, LLC and Jeff Allen (2019).
#' shinyAce: Ace Editor Bindings for Shiny.}
#'
#' \item{\href{https://CRAN.R-project.org/package=shiny}{shiny} package: }{
#' Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson
#' (2018). shiny: Web Application Framework for R.}
#'
#' \item{\href{https://CRAN.R-project.org/package=markdown}{markdown} package:
#' }{ JJ Allaire, Jeffrey Horner, Yihui Xie, Vicent Marti and Natacha Porte
#' (2018). markdown: 'Markdown' Rendering for R.} }
#'
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'  library(shiny)
#'  library(markdownInput)
#'  ui <- fluidPage(titlePanel("Markdown input"),
#'                  sidebarLayout(
#'                    # inputs:
#'                    sidebarPanel(
#'                      markdownInput(
#'                        "mdInputID",
#'                        label = "Write your text",
#'                        value = "Write some _markdown_ **here:**"
#'                        )
#'                    ),
#'
#'                    # outputs:
#'                    mainPanel(
#'                    h3("Raw value of the input:"),
#'                    verbatimTextOutput("rawResult"))
#'                  ))
#'
#'  server <- function(input, output, session) {
#'    # myText is a reactive variable containing the raw markdown text
#'    myText <- callModule(moduleMarkdownInput, "mdInputID")
#'
#'    # show "myText"
#'    output$rawResult <- renderPrint({
#'      print(myText())
#'    })
#'
#'  }
#'
#'  shinyApp(ui, server)
#'
#'}
#' @export
markdownInput <- function(inputId,
                          label,
                          value = "Some **markdown** _text_",
                          theme = "github",
                          height = "400px",
                          class = "") {
  ns <- shiny::NS(inputId)

  shiny::div(class = class, style = "margin-bottom: 15px;",


             shiny::tags$label(label),

             shiny::tabsetPanel(selected = 1,
                                type = "tabs",

                                # text input:
                                shiny::tabPanel(
                                  title = "Write",
                                  value = 1,

                                  shinyAce::aceEditor(
                                    ns("mdInput"),
                                    value = value,
                                    mode = "markdown",
                                    theme = theme,
                                    height = height
                                  )
                                ),


                                # MD preview:
                                shiny::tabPanel(
                                  title = "Preview",
                                  value = 2,

                                  shiny::uiOutput(ns("mdPreview"))
                                )
             )
  )
}




# Server function --------------------------------------------------------------
#' Server function of the \code{markdownInput} module
#'
#' @param input The session's input object.
#' @param output The session's output object.
#' @param session The shiny's session.
#' @param class (optional) The CSS class name of the priview tab.
#'
#' @return The reactive value of the input.
#'
#' @import shiny
#' @import markdown
#'
#' @author Julien Diot \email{juliendiot42@@gmail.com}
#'
#' @references \enumerate{
#' \item{\href{https://CRAN.R-project.org/package=shinyAce}{shinyAce} package:
#' }{Vincent Nijs, Forest Fang, Trestle Technology, LLC and Jeff Allen (2019).
#' shinyAce: Ace Editor Bindings for Shiny.}
#'
#' \item{\href{https://CRAN.R-project.org/package=shiny}{shiny} package: }{
#' Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson
#' (2018). shiny: Web Application Framework for R.}
#'
#' \item{\href{https://CRAN.R-project.org/package=markdown}{markdown} package:
#' }{ JJ Allaire, Jeffrey Horner, Yihui Xie, Vicent Marti and Natacha Porte
#' (2018). markdown: 'Markdown' Rendering for R.} }
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'  library(shiny)
#'  library(markdownInput)
#'  ui <- fluidPage(titlePanel("Markdown input"),
#'                  sidebarLayout(
#'                    # inputs:
#'                    sidebarPanel(
#'                      markdownInput(
#'                        "mdInputID",
#'                        label = "Write your text",
#'                        value = "Write some _markdown_ **here:**"
#'                        )
#'                    ),
#'
#'                    # outputs:
#'                    mainPanel(
#'                    h3("Raw value of the input:"),
#'                    verbatimTextOutput("rawResult"))
#'                  ))
#'
#'  server <- function(input, output, session) {
#'    # myText is a reactive variable containing the raw markdown text
#'    myText <- callModule(moduleMarkdownInput, "mdInputID")
#'
#'    # show "myText"
#'    output$rawResult <- renderPrint({
#'      print(myText())
#'    })
#'
#'  }
#'
#'  shinyApp(ui, server)
#'
#'}
#' @export
moduleMarkdownInput <- function(input, output, session, class = "") {

  output$mdPreview <- shiny::renderUI({

    shiny::div(class = class,
               shiny::withMathJax(
                 shiny::HTML(
                   markdown::markdownToHTML(text = input$mdInput,
                                            fragment.only = TRUE)
                 )
               )
    )

  })

  shiny::reactive(input$mdInput)

}
