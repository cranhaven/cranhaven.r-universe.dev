
### Make a long format transposed copy of the current table
###   Dependencies on specific packages: none.
###   Dependencies in generated code: tidyr.


page_gather <- list(

  ui = function() ..ui(page="gather",
    fluidRow(
      column(width=6,
        imageOutput("gather.wide",height='120px'),
        box(width='100%', checkboxInput("gather.pivot", ..s2(.IGoR$Z$gather$pivot),FALSE)),
        uiOutput("gather.control")
      ),
      column(width=6,
        imageOutput("gather.long",height='200px'),
        ..load.ui("gather"),
        box(width='100%',
          fluidRow(
            column(width=6, textInput("gather.out.K", ..s1(.IGoR$Z$gather$out.k), "")),
            column(width=6, textInput("gather.out.V", ..s1(.IGoR$Z$gather$out.v), ""))
          )
  ) ) ) ),


  server = function(input, output, session) {

    ..aServer(input,output,"gather")

    output$gather.wide <- renderImage(list(src=..image("wide.png")),deleteFile = FALSE)

    output$gather.long <- renderImage(list(src=..image("long.png")),deleteFile = FALSE)

    output$gather.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        ..select.ui("gather", NULL,
                    buttons.title=..s2(.IGoR$Z$gather$gather), buttons.all=FALSE,
                    buttons.class=..isTRUE(input$gather.pivot),
                    drop=FALSE)
    })

    ..output.select.what(input,output,"gather", columns.all=TRUE)
    ..output.select.drop(input,output,"gather")

    output$gather.command2 <- renderUI(
      ..textarea("gather", "gather(k,v,columns)", 2,
        if (!is.null(input$gather.type)
          &&..isNotEmpty(input$gather.out.K)&&..isNotEmpty(input$gather.out.V)
          &&((input$gather.type<4)||..isNotEmpty(input$gather.pattern))
           )
          if (..isTRUE(input$gather.pivot))
            ..command2(
              "pivot_longer(",
              if (input$gather.type==2) {
                a <- paste0("is.",input$gather.class)
                if (..isTRUE(input$gather.drop)) a <- glue("Negate({a})")
                glue("where({a})")
              }
              else {
                a <- ..select(input,"gather")
                if (..isNotEmpty(a)) glue("c({..collapse0(a)})") else "everything()"
              },
              ", names_to=", shQuote(input$gather.out.K),
              ", values_to=", shQuote(input$gather.out.V),
              ")"
            )
          else
            ..command2(
              "gather(",
              ..name(input$gather.out.K), ", ", ..name(input$gather.out.V),
              ..select(input,"gather") %>% {if (..isNotEmpty(.)) paste0(", ", .)},
              ")"
            )
    ) )

    observeEvent({.IGoR$state$meta; input$gather.command2},
      ..try(input,output,"gather",
        function(x) sprintf(.IGoR$Z$gather$msg.result, ncol(x))
    ) )

  }
)

