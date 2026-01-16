
### View the list of known tables
###   Dependencies on specific packages: htmltools.
###   No generated code.

page_tables <- list(

  ui = function() ..ui(page="tables", command=FALSE,
    uiOutput("tables.output"),
    fluidRow(
      column(width=6, actionButton("tables.delete",.IGoR$Z$tables$delete)),
      column(width=6,
        box(width='100%',
          column(width=6, actionButton("tables.log", .IGoR$Z$tables$log)),
          column(width=6, textInput("tables.log.out", ..s2(.IGoR$Z$tables$log.out), "log"))
  ) ) ) ),


  server = function(input, output, session) {

    output$tables.output <- renderUI({
      .IGoR$state$list
      .IGoR$state$meta
      .IGoR$state$data
      do.call(tags$table,
        list(style = "border: 1px solid black; padding: 10px; width: 100%",
          tags$tr(
           tags$th(.IGoR$Z$tables$table),
           tags$th(.IGoR$Z$any$vars),
           tags$th(.IGoR$Z$any$rows),
           tags$th(.IGoR$Z$tables$created),
           tags$th(.IGoR$Z$tables$source)
          ),
          purrr::imap(..tables(),
            function (x,i) {
              t <- get(x, envir=.IGoR$env)
              tags$tr(
                tags$td(checkboxInput(paste0("tables.",i),x,FALSE)),
                tags$td(ncol(t)),
                tags$td(nrow(t)),
                tags$td(toString(attr(t,'created'))),
                tags$td(attr(t,'source'))
              )
            }
      ) ) )
    })

    observeEvent(input$tables.delete,
      isolate({
        t <- ..tables()
        l <- purrr::map_if(1:length(t),
               function(i) input[[paste0("tables.",i)]],
               function(i) {rm(list=t[i], envir=.IGoR$env); i}
             )
        if (length(l)>0) {
          .IGoR$state$list<- Sys.time()
          if (input$main.data %in% l) {
            .IGoR$state$meta <- Sys.time()
            .IGoR$state$data <- Sys.time()
          }
          ..renderTables(input,output)
        }
      })
    )

    observeEvent(input$tables.log,
      isolate({
        t <- make.names(input$tables.log.out)
        assign(t, .IGoR$log, envir=.IGoR$env)
        ..newTable(input,output, t)
      })
    )
  }
)
