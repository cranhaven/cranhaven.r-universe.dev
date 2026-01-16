
### Manually edit the contents of the current table
###   Dependencies on specific packages: rhandontable, lubridate.
###   No generated code.

page_edit <- list(

  ui = function() ..ui(page="edit", command=FALSE,
           uiOutput("edit.control")
  ),


  server = function(input, output, session) {

    err <- function () 
      if (..isNotEmpty(input$main.data)) {
        df <- ..data(input)
        no <- Filter(function(x) !(is.logical(x)|is.numeric(x)|is.character(x)|is.factor(x)|is.Date(x)),df)
        if (nrow(df)>200) 'size'
        else if (length(no)>0) 'class'
        else 'none'
      } else 'empty'

    output$edit.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        switch(err(),
          'size'  = ..s1(.IGoR$Z$edit$error.size),
          'class' = ..s1(.IGoR$Z$edit$error.class),
          'none'  =
            list(
              fluidRow(
                column(width=3, sliderInput("edit.width",.IGoR$Z$edit$width, 0, 200, 100)),
                column(width=3),
                column(width=6,
                  box(width='100%',
                    column(width=7, textInput("edit.out", ..s2(.IGoR$Z$any$out), input$main.data)),
                    column(width=5, actionButton("edit.save", .IGoR$Z$edit$save,
                                                 icon=icon("sync"), 
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
              ) ) ),
              rHandsontableOutput("hot")
        )   )
    })

    values <- reactiveValues()

    observe({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        values$df <- if (err()=='none') ..data(input)
    })

    observe(
      if (!is.null(input$hot)) values$df <- hot_to_r(input$hot)
    )


    output$hot <- renderRHandsontable({
      df <- values$df
      if (!is.null(df))
        rhandsontable(df, useTypes=TRUE, height=400, rowHeaderWidth=input$edit.width) %>%
        hot_cols(manualColumnResize=TRUE) %>%
        hot_table(highlightCol=TRUE, highlightRow=TRUE)
    })

    observeEvent(input$edit.save, {
      df <- isolate(hot_to_r(input$hot))
      t  <- make.names(isolate(input$edit.out))
      assign(t, df, envir=.IGoR$env)
      ..newTable(input,output,t,.select=TRUE)
      if (t==input$main.data)
        .IGoR$state$data <- Sys.time()
    })

  }
)

