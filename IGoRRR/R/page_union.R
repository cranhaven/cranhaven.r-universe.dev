
### Make a table with several one using an operation on sets
###   Dependencies on specific packages: none.
###   Dependencies in generated code: none.

page_union <- list(

  ui = function() ..ui(page="union",
    fluidRow(
      column(width=6,
        box(width='100%',
          uiOutput("union.data")
      ) ),
      column(width=6,
        ..load.ui("union"),
        uiOutput("union.type")
  ) ) ),


  server = function(input, output, session) {

    ..aaServer(input,output,"union")

    output$union.data<- renderUI({
      .IGoR$state$list
      selectizeInput("union.data", ..s1(.IGoR$Z$all$join.data), choices=c(.IGoR$TABLE,..tables()))
    })

    output$union.type <- renderUI(
      if (..isNotEmpty(input$union.data))
        box(width='100%',
          radioButtons("union.type", ..s2(.IGoR$Z$union$type), ..Zitems("union","types"))
    )   )

    output$union.command2 <- renderUI(
      ..textarea("union", "...(table,table2)", 4,
        if (..isNotEmpty(input$union.data)
          &&!is.null(input$union.type))
          ..command2(
            glue("{input$union.type}({input$union.data})")
    ) )   )

  }
)
