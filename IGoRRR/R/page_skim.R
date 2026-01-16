
### Produces summary on columns of the current table
### Specific dependencies on packages: none.

page_skim <- list(

  ui = function() ..ui(page="skim",
    fluidRow(
      column(width=6, uiOutput("skim.control")),
      column(width=6, ..load.ui("skim"))
  ) ),


  server = function(input, output, session) {

    ..aaServer(input,output,"skim")

    output$skim.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        ..select.ui("skim", buttons.title=..s2(.IGoR$Z$skim$skim),
                    buttons.all=FALSE, buttons.class=FALSE,
                    drop=FALSE)
    })

    ..output.select.what(input,output,"skim", columns.all=TRUE)
    ..output.select.drop(input,output,"skim")

    output$skim.command2 <- renderUI(
      ..textarea("skim", "skim(columns)", 2,
       if (!is.null(input$skim.type)
         &&((input$skim.type<4)||..isNotEmpty(input$skim.pattern))
          )
         ..command2(
           "skim(",
           ..select(input,"skim"),
           ")"
    ) )  )

  }
)
