
### Creates a copy of a current table with only selected columns
###   Dependencies on specific packages: none.
###   dependencies in generated code: dplyr.

page_select <- list(

  ui = function() ..ui(page="select", control=TRUE),


  server = function(input, output, session) {

    ..vServer(input,output,"select")

    output$select.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
      fluidRow(
        column(width=6, ..select.ui("select", buttons.title=..s2(.IGoR$Z$select$select), buttons.all=FALSE)),
        column(width=6, ..load.ui("select"))
      )
    })

    ..output.select.drop(input,output,"select")

    output$select.columns.more <- renderUI(
      if (!is.null(input$select.type)
        &&(((input$select.type==1)&&(length(input$select.columns)>0))
         ||(input$select.type>3))
         )
        checkboxInput("select.everything",..s4(.IGoR$Z$select$everything),FALSE)
    )

    output$select.command2 <- renderUI(
      ..textarea("select", "select(columns)", 3,
        if (!is.null(input$select.type))
          if ((input$select.type==1)&&..isTRUE(input$select.drop)&&(length(input$select.columns)==0))
            ..command2("identity()")
          else
            ..command2(
              "select",
              if ((input$select.type==2)&&..isNotEmpty(input$select.class))
                if (..isTRUE(input$select.drop))
                     glue("_if(Negate(is.{input$select.class})")
                else glue("_if(is.{input$select.class}")
              else paste0("(",..select(input,"select")),
              if ((((input$select.type==1)&&(length(input$select.columns)>0))
                 ||(input$select.type>3))
                &&..isTRUE(input$select.everything)) ", everything()",
              ")"
    ) )   )

    observeEvent({.IGoR$state$meta; input$select.command2},
      ..try(input,output,"select",
        function(x) sprintf(.IGoR$Z$select$msg.result,ncol(x),..collapse(colnames(x)))
    ) )

  }
)
