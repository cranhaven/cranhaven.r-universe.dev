
### Make a copy of current table with a colum converted using a correspondence table
###   Dependencies on specific packages: none.
###   Dependencies in generated code: dplyr.

page_labels <- list(

  ui = function() ..ui(page="labels", control=TRUE),


  server = function(input, output, session) {

    ..aServer(input,output,"labels")

    output$labels.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        tagList(
          fluidRow(
            column(width=3,
              box(width='100%',
                selectizeInput("labels.old", ..s1(.IGoR$Z$any$old.var),
                               choices=c(.IGoR$COLV,..columns(input$main.data,c("character","integer","logical"))))
            ) ),
            column(width=9, uiOutput("labels.new"))
          ),
          fluidRow(
            column(width=6,
              box(width='100%',
                selectizeInput("labels.data", ..s1(.IGoR$Z$labels$data), choices=c(.IGoR$TABLE,..tables())),
                uiOutput("labels.data.columns")
        ) ) ) )
    })

    output$labels.new <- renderUI(
      if (..isNotEmpty(input$main.data)&&..isNotEmpty(input$labels.old))
        box(width='100%',
          column(width=3, textInput("labels.new", ..s2(.IGoR$Z$any$new.col), input$labels.old)),
          column(width=6, textInput("labels.out", ..s2(.IGoR$Z$any$out), "labels.out")),
          column(width=3, uiOutput("labels.load"))
    )   )

    output$labels.data.columns <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$labels.data))
        fluidRow(
          column(width=6, selectizeInput("labels.data.levels", ..s1(.IGoR$Z$labels$data.levels),
                                        choices=c(.IGoR$COLV,
                                                  ..columns(input$labels.data,c("character","integer","logical"))
          )                             )         ),
          column(width=6, selectizeInput("labels.data.labels", ..s1(.IGoR$Z$labels$data.labels),
                                         choices=c(.IGoR$CHRCOLV,..columns(input$labels.data,"character"))
          )                             )
        )
    })

    output$labels.command2 <- renderUI(
      ..textarea("labels", "mutate(new=factor(old,levels=...,labels=...))", 5,
        if (..isNotEmpty(input$labels.old)
          &&..isNotEmpty(input$labels.data)
          &&..isNotEmpty(input$labels.data.levels)&&..isNotEmpty(input$labels.data.labels)
           )  {
          d <- input$labels.data
          n <- ..name(input$labels.new)
          o <- ..name(input$labels.old)
          ..command2(
            if (d %not in% ..columns(input$main.data))             # when label table name is also a name of a column
                 glue("mutate({n}=factor({o},")                    # of the source table, dplyr is fooled and use the later
            else glue("`$<-`({n},factor(.${o},"),                  # so in that case we use base R instead
            '\n      ', glue("levels={d}${..name(input$labels.data.levels)},"),
            '\n      ', glue("labels={d}${..name(input$labels.data.labels)}))")
          )
        }
    ) )

    observeEvent(input$labels.command2, ..try(input,output,"labels"))
  }
)
