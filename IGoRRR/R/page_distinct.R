
### 'distinct' and 'count' inteerface
###   Dependencies on specific packages: none.
###   Dependencies in generated code: dplyr.

page_distinct <- list(

  ui = function() ..ui(page="distinct",
    fluidRow(
      column(width=6,
        box(width='100%',
          uiOutput("distinct.group")
      ) ),
      column(width=6,
        ..load.ui("distinct"),
        box(width='100%',
          fluidRow(
            column(width=6, radioButtons("distinct.type", "", ..Znames("distinct","type",c("list","count")))),
            column(width=6, uiOutput("distinct.more"))
  ) ) ) ) ),


  server = function(input, output, session) {

    ..aServer(input,output,"distinct")

    output$distinct.more <- renderUI(
      if (..isEQ(input$distinct.type,"count"))
        textInput("distinct.name", ..s2(.IGoR$Z$distinct$count.var), "n")
      else
      if (length(input$distinct.group)==1)
        checkboxInput("distinct.sort", ..s5(.IGoR$Z$distinct$sort), TRUE)
    )

    output$distinct.group <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        selectizeInput("distinct.group", ..s1(.IGoR$Z$any$vars),
                       multiple = TRUE, options = list(placeholder = .IGoR$any$all),
                       choices = ..columns(input$main.data)
        )
    })

    output$distinct.command2 <- renderUI(
     ..textarea("distinct", "distinct(columns)", 4,
      if (!is.null(input$distinct.type))
        ..command2(
          if (input$distinct.type=="count")
            if (..isNE(input$distinct.name,"n"))
              paste0(
                ..group_by(input,"distinct"),
                glue("summarise({input$distinct.name}=n())"),
                ..ungroup(input,"distinct",1)
              )
            else glue("count({..collapse(input$distinct.group)})")
          else   glue("distinct({..collapse(input$distinct.group)})"),
          if ((input$distinct.type=="list")&&(length(input$distinct.group)==1)&&..isTRUE(input$distinct.sort))
            paste0(NL,glue("arrange({..collapse(input$distinct.group)})"))
    ) ) )

    observeEvent(input$distinct.command2,
      ..try(input,output,"distinct",
        function(x)
          if (input$distinct.type=="count")
            sprintf(.IGoR$Z$distinct$msg.result, nrow(x))
          else
            if (length(input$distinct.group)==0)
              sprintf(.IGoR$Z$distinct$msg.rows, nrow(x))
            else
            if (ncol(x)==1)
                 sprintf(.IGoR$Z$distinct$msg.values, nrow(x),
                         (if (is.character(x[[1]])||is.factor(x[[1]])) ..collapse1 else ..collapse0)(x[[1]]))
            else sprintf(.IGoR$Z$distinct$msg.count, nrow(x)),
        .subset=if (length(input$distinct.group)==0) "identity()"        # None selected means keep all columns
                else glue("select({..collapse(input$distinct.group)})")
    ) )

  }
)
