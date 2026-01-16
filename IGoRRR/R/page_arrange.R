
### Reorder the rows of the current table
###   Dependencies on specific packages: none.
###   Dependencies in generated code: dplyr.

page_arrange <- list(

  ui = function() ..ui(page="arrange", control=TRUE),


  server = function(input, output, session) {

    ..aaServer(input,output,"arrange", meta=FALSE)

    output$arrange.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6,
            box(width='100%',
              column(width=6, selectizeInput("arrange.columns", ..s1(.IGoR$Z$any$vars),
                                             multiple = TRUE, options = list(placeholder = .IGoR$Z$any$cols),
                                             choices = ..columns(input$main.data))
              ),
              column(width=6, uiOutput("arrange.desc"))
          ) ),
          column(width=6, ..load.ui("arrange",input$main.data))
       )
    })

    output$arrange.desc <- renderUI(
      if (!is.null(input$arrange.columns))
        selectizeInput("arrange.desc", ..s3(.IGoR$Z$arrange$desc),
                       multiple = TRUE, options = list(placeholder = .IGoR$Z$any$cols),
                       choices = iconv(input$arrange.columns,from="UTF-8"))
    )

    output$arrange.command2 <- renderUI(
      ..textarea("arrange", "arrange(columns)", 3, {
        columns <- input$arrange.columns
        if (!is.null(columns)) {
          l <- ifelse(columns %in% input$arrange.desc, glue("desc({..name(columns)})"), ..name(columns))
          ..command2(glue("arrange({..collapse0(l)})"))
        }
      })
    )

  }
)
