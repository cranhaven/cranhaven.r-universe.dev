
### Creates a table from a range of rows taken in the current table
###   Dependencies on specific packages: none.
###   Dependencies in generated code: dplyr.

page_slice <- list(

  ui = function() ..ui(page="slice",
    fluidRow(
      column(width=6,
        box(width='100%',
          fluidRow(
            column(width=6, numericInput("slice.top",..s2(.IGoR$Z$slice$top),1)),
            column(width=6, uiOutput("slice.end"))
          ),
          checkboxInput("slice.drop",..s4(.IGoR$Z$any$drop),FALSE)
      )),
      column(width=6, ..load.ui("slice"))
  ) ),


  server = function(input, output, session) {

    ..aaServer(input,output,"slice")

    output$slice.end <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        numericInput("slice.end", ..s2(.IGoR$Z$slice$end), nrow(..data(input)))
    })

    output$slice.command2 <- renderUI(
      ..textarea("slice", "slice(range)", 3,
        if (!is.null(input$slice.end)) {
          n <- nrow(..data(input))
          ..command2(
            if (is.na(input$slice.end)||is.na(input$slice.top)) ""
            else
              glue(
                if (input$slice.top>1)
                  if (input$slice.end<n)
                    if (input$slice.top!=input$slice.end)
                      if (input$slice.drop)
                           "slice(-({input$slice.top}:{input$slice.end}))"
                      else "slice({input$slice.top}:{input$slice.end})"
                    else if (input$slice.drop)
                           "slice(-{input$slice.top})"
                      else "slice({input$slice.top})"
                  else
                    if (input$slice.drop)
                         "head({input$slice.top-1})"
                    else "tail({n-input$slice.top+1})"
                else
                  if (input$slice.drop)
                       "tail({n-input$slice.end})"
                  else "head({input$slice.end})"
          )  )
        }
    ) )

  }
)
