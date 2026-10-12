#
# This is a Shiny web application presenting the "markdownInput" package.

library(shiny)
library(markdownInput)
ui <- fluidPage(titlePanel("Markdown input"),
                sidebarLayout(
                    # inputs:
                    sidebarPanel(
                        markdownInput(
                            "mdInputID",
                            label = "Write your text",
                            value = "Write some _markdown_ **here:**"
                        )
                    ),

                    # outputs:
                    mainPanel(
                        h3("Raw value of the input:"),
                        verbatimTextOutput("rawResult"))
                ))

server <- function(input, output, session) {
    # myText is a reactive variable containing the raw markdown text
    myText <- callModule(moduleMarkdownInput, "mdInputID")

    # show "myText"
    output$rawResult <- renderPrint({
        print(myText())
    })

}

shinyApp(ui, server)
