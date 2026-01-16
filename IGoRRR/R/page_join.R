
### Merges the current table with another on some key
###   Dependencies on specific packages: dplyr.
###   Dependencies in generated code: dplyr.

page_join <- list(

  ui = function() ..ui(page="join",
    fluidRow(
      column(width=6,
        box(width='100%',
          uiOutput("join.columns"),
          uiOutput("join.data")
      ) ),
      column(width=6,
        ..load.ui("join"),
        uiOutput("join.type")
  ) ) ),


  server = function(input, output, session) {

    ..jServer(input,output,"join")

    output$join.type <- renderUI(
      if (..isNotEmpty(input$join.data)&&
          (length(input$join.columns2)==length(input$join.columns)))
        box(width='100%',
          radioButtons("join.type", ..s2(.IGoR$Z$join$type),
            ..Znames("join","type",
              if (length(input$join.columns)==0) "crossing" else c("inner","left","right","full","anti","semi")
    )   ) ) )

    output$join.command2 <- renderUI(
      ..textarea("join", "...join(table,columns)", 4,
        if (..isNotEmpty(input$join.data)
          &&!is.null(input$join.type)
          &&(length(input$join.columns2)==length(input$join.columns))
           )
          ..command2(
            if (length(input$join.columns)>0) {
              by <- ..collapse0(
                      ifelse(input$join.columns==input$join.columns2,
                             glue("\"{input$join.columns}\""),
                             glue("\"{input$join.columns}\" = \"{input$join.columns2}\"")
                    ) )
              if ((length(input$join.columns)>1)||(input$join.columns!=input$join.columns2)) by <- glue("c({by})")
              join <- if (input$join.type=="crossing") "" else "_join"
              glue("{input$join.type}{join}({input$join.data}, by={by})")
            }
            else glue("crossing({input$join.data})")
    ) )   )

    observeEvent(input$join.command2,
      ..try(input,output,"join",
        function (x) {
          t1 <- select_at(..data(input),input$join.columns)
          t2 <- select_at(get(input$join.data,envir=.IGoR$env),input$join.columns2)
          sprintf(.IGoR$Z$join$msg.result,nrow(t1),nrow(t2),
            if (length(input$join.columns)==0) nrow(t1)*nrow(t2)
            else if (length(input$join.columns)==length(input$join.columns2)) {
              l <- input$join.columns2
              names(l) <- input$join.columns
              nrow(do.call(paste0(input$join.type,if (input$join.type!="crossing") "_join"),list(t1,t2,l)))
            }
          )
        }
    ) )

  }
)
