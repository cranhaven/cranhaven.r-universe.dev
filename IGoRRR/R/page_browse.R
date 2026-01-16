
### View the current table by rows
###   Dependencies on specific packages: htmltools.
###   No generated code.

page_browse <- list(

  ui = function() ..ui(page="browse", command=FALSE,
    fluidRow(
      column(width=6,
        box(width='100%',
          column(width=4, uiOutput("browse.row.type")),
          column(width=8,
             uiOutput("browse.row"),
             textOutput("browse.row.comment")
    ) ) ) ),
    fluidRow(
      column(width=1,
        ..dropdownButton(page="browse_cols", title=.IGoR$Z$any$vars,
          uiOutput("browse.ids"),
          ..select.ui("browse", box=FALSE, buttons.title=..s2(.IGoR$Z$browse$vars), selected=3)
        ),
        ..dropdownButton(page="browse_rows", title=.IGoR$Z$any$rows,
          uiOutput("browse.where")
        ),
        ..dropdownButton(page="browse_page", title=.IGoR$Z$any$layout,
          fluidRow(
            column(width=6, sliderInput("browse.ncols",..s2(.IGoR$Z$browse$ncols),1,4,3)),
            column(width=6,
              checkboxInput("browse.label",..s4(.IGoR$Z$browse$label),FALSE),
              checkboxInput("browse.sort", ..s4(.IGoR$Z$browse$sort), FALSE)
      ) ) ) ),
      column(width=11, htmlOutput("browse.html"))
  ) ),

  server = function(input, output, session) {

    build_table <- function(.data,.ids,.row){

      f <- function(.data,.row,.n,.header=FALSE,.class=TRUE) {

      type <- function(.i)
        switch(class(.data[[.i]])[1],
               "logical"="<lgl>",
               "numeric"="<dbl>",
               "integer"="<int>",
             "character"="<chr>",
                "factor"="<fct>",
               "POSIXct"=,
                  "Date"="<date>"
              )

      name <- function(.i)
        if (..isTRUE(input$browse.label)) {
          a <- attr(.data[[.i]],'label')
          if (is.null(a)) colnames(.data)[.i] else a
        }
      else colnames(.data)[.i]

      if ((ncol(.data)>0)&&(nrow(.data)>0))
        map(1:((ncol(.data)-1)/.n+1),
          function(i)
            do.call(if (.header) tags$th else tags$tr,
              map(1:.n,
                function(j) {
                  k <- (i-1)*.n+j
                  if (ncol(.data)<k) tagList(tags$td(" "),tags$td(" "),tags$td(" "))
                  else tagList(
                         tags$td(tags$span(style="color:blue",name(k))),
                         if (.class&&!..isTRUE(input$browse.label))
                           tags$td(tags$span(style="color:blue",type(k))),
                          tags$td(
                            (if (class(.data[[k]])[1] %in% c("factor","character","Date")) identity else toString)
                              (.data[.row,k])
                       )  )
                }
        )   ) )
    }

    if (length(.row)>0)
      tagList(
        do.call(tags$table,
                list(style = "border: 1px solid black; padding: 1%; width: 100%; background: rgb(224,224,255)",
                     f(.ids,.row,1,TRUE,FALSE))),
        br(),
        do.call(tags$table,
                list(style = "border: 1px solid black; padding: 1%; width: 100%",
                     f(.data,.row,if (length(input$browse.ncols)==0) 3 else input$browse.ncols)))
      )
    }

    df <-   reactive({
      .IGoR$state$data
      df    <- as.data.frame(..data(input))
      ids   <- ..if.sync(input,"browse.ids")
      where <- ..if.sync(input,"browse.where")
      l <- names(df)
      df <- df %>% mutate(row.names(.))
      if (..isNotEmpty(where)) {
        r <- tryCatch(eval(parse(text=glue("df %>% filter({where})"))),
                      error=identity)
        df <- if (is(r,"condition")) {
                output$browse.comment <- renderText(r$message)
                df %>% filter(FALSE)
              }
              else {
                output$browse.comment <- renderText(str_sub(..look(input$browse.where),4))
                r
      }       }
      else output$browse.comment <- renderText("")
      lc <- ..select.columns(input,output,"browse")
      lc <- setdiff(lc,ids)
      if (..isTRUE(input$browse.sort)) lc <- sort(lc)
      list(data=df[,lc,drop=FALSE],ids=df[,c("row.names(.)",ids),drop=FALSE])
    })

    no <- reactive(
      if (nrow(df()$data)>0) {
        i <-
          if (length(input$browse.row.type)==0) 1
          else
          if ((input$browse.row.type=='no')&&(length(input$browse.row)>0)) input$browse.row
          else
          if ((input$browse.row.type=='id')&&(length(input$browse.ids)>0)&&(length(input$browse.row.value)>0))
            which(df()$ids[input$browse.ids[1]]==input$browse.row.value)
          else
          if ((input$browse.row.type=='name')&&(length(input$browse.row.name)>0))
            which(df()$ids["row.names(.)"]==input$browse.row.name)
          else 1
        if (length(i)>1) {
          output$browse.row.comment <- renderText(sprintf(.IGoR$Z$browse$msg.nobs,length(i)))
          i[1]
        } else {
          output$browse.row.comment <- renderText("")
          i
      } }
    )

   ..output.select.what(input,output,"browse")

    output$browse.where <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        tagList(
          textInput(..do.sync(input,"browse.where"),..s3(.IGoR$Z$any$where), width='100%'),
          verbatimTextOutput("browse.comment")
       )
    })

    output$browse.row.type <- renderUI(
      radioButtons("browse.row.type",..s2(.IGoR$Z$any$row),
                   ..Znames("browse","row.type",c("no","name",if (..isNotEmpty(input$browse.ids)) "id"))
    ) )

    output$browse.row <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)&&!is.null(input$browse.row.type)) {
        df <- df()
        if (input$browse.row.type=='no')
          sliderInput("browse.row","",1,nrow(df$data),1,step=1,round=TRUE)
        else
        if ((input$browse.row.type=='id')&&(length(input$browse.ids)>0))
          textInput("browse.row.value",..s2(input$browse.ids[1]),iconv(df$ids[1,input$browse.ids[1]],from="UTF-8"))
        else
        if (input$browse.row.type=='name')
          if (nrow(df$ids)<=.IGoR$MAXROWNAMES)
              selectizeInput("browse.row.name",..s2("row.names(.)"),choices=df$ids$`row.names(.)`)
          else textInput("browse.row.name",..s2("row.names(.)"),df$ids[1,"row.names(.)"])
      }
    })

    output$browse.ids <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        selectizeInput(..do.sync(input,"browse.ids"),..s3(.IGoR$Z$browse$ids),
                       multiple=TRUE,  options = list(placeholder = .IGoR$Z$any$cols.chr),
                       choices=..columns(input$main.data,"character"))
    })

    output$browse.html <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)) build_table(df()$data,df()$ids,no())
    })

  }
)
