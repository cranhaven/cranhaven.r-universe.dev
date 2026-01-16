
### Create a copy of current table with contents of one or veral others appended to it
###   Dependencies on specific packages : none.
###   Dependencies in generated code: dplyr.

page_rbind <- list(

  ui = function() ..ui(page="rbind",
    fluidRow(
      column(width=6,
        box(width='100%', uiOutput("rbind.data")
      ) ),
      column(width=6, ..load.ui("rbind"))
  ) ),


  server = function(input, output, session) {

    ..aServer(input,output,"rbind")

    output$rbind.data<- renderUI({
      .IGoR$state$list
      fluidRow(
        column(width=6,
          selectizeInput("rbind.data", ..s1(.IGoR$Z$rbind$data),
                         multiple=TRUE, choices=c(.IGoR$TABLE,..tables())
      ) ) )
    })

    output$rbind.command2 <- renderUI(
      ..textarea("rbind", "bind_rows(tables)", 2,
        if (..isNotEmpty(input$rbind.data))
          ..command2(
            paste0("bind_rows(",..collapse0(input$rbind.data),")")
    ) )   )

    observeEvent({.IGoR$state$meta; input$rbind.command2},
      ..try(input,output,"rbind",
        function (x)
          sprintf(.IGoR$Z$rbind$msg.result,
            ncol(..data(input)),
            do.call(partial(paste, sep='+'),
                    Map(function(x) ncol(get(x, envir=.IGoR$env)),
                        input$rbind.data)),
            ncol(x)
          )
    ) )

  } # server
) # page
