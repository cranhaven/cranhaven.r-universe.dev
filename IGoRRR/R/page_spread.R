
### Make a large format transposed copy of the current table
###   Dependencies on specific packages: none.
###   Dependencies in generated code: tidyr.

page_spread <- list(

  ui = function() ..ui(page="spread",
    fluidRow(
      column(width=6,
        imageOutput("spread.long",height='200px'),
        uiOutput("spread.columns")
      ),
      column(width=6,
        imageOutput("spread.wide",height='200px'),
        box(width='100%', checkboxInput("spread.pivot",..s2(.IGoR$Z$spread$pivot),FALSE)),
        ..load.ui("spread")
  ) ) ),

  server = function(input, output, session) {

    ..aServer(input,output,"spread")

    output$spread.wide <- renderImage(list(src=..image("wide.png")),deleteFile = FALSE)

    output$spread.long <- renderImage(list(src=..image("long.png")),deleteFile = FALSE)

    output$spread.columns <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        box(width='100%',
          fluidRow(
            column(width=6, selectizeInput("spread.K",..s1(.IGoR$Z$spread$var.k),
                                           choices = c(.IGoR$CHRCOLV,..columns(input$main.data,"character")))),
            column(width=6, uiOutput("spread.sep"))
          ),
          fluidRow(
            column(width=6, selectizeInput("spread.V", ..s1(.IGoR$Z$spread$var.v),
                                           choices = c(.IGoR$COLV,..columns(input$main.data))))
        ) )
    })

    output$spread.sep <- renderUI(
      if (..isNotEmpty(input$spread.K)&&..isFALSE(input$spread.pivot))
        checkboxInput("spread.sep",..s4(paste0(.IGoR$Z$spread$prefix,input$spread.K,"'")), FALSE)
    )

    output$spread.command2 <- renderUI(
      ..textarea("spread", "spread(k,v)", 2,
        if (..isNotEmpty(input$spread.K)&&..isNotEmpty(input$spread.V))
          if (input$spread.K==input$spread.V) {
            output$spread.comment <-  renderText(.IGoR$Z$gather$msg.error)
            ""
          }
          else
          if (..isTRUE(input$spread.pivot))
            ..command2(
              "pivot_wider(",
              "names_from=", shQuote(input$spread.K),
              ", values_from=", shQuote(input$spread.V),
              ")"
            )
          else
            ..command2(
              "spread(",
              ..name(input$spread.K), ",", ..name(input$spread.V),
              if (..isTRUE(input$spread.sep)) ", sep=\"\"",
              ")"
    ) )     )
    
    # We must try the command on the whole rows as duplicated keys would crash
    # and this cannot detected by the default behaviour of testing only the 1st row
    observeEvent({input$spread.command2; .IGoR$state$meta},
      ..try(input,output,"spread", 
            .subset=glue("select({..name(input$spread.K)},{..name(input$spread.V)})")
    ) )

  }
)
