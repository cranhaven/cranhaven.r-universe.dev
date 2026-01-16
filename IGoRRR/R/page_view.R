
### View the data of the current table by columns
###   Dependencies on specific packages: none.
###   No generated code.

page_view <- list(

  ui = function() ..ui(page="view", command=FALSE,
    fluidRow(
      column(width=6, box(width='100%', uiOutput("view.columns"))),
      column(width=6, box(width='100%', uiOutput("view.page.no")))
     ),
     fluidRow(
       column(width=1,
         ..dropdownButton(page="view_rows", title=.IGoR$Z$any$rows,
           uiOutput("view.rownames"),
           uiOutput("view.where"),
           fluidRow(
             column(width=6, uiOutput("view.group")),
             column(width=6, uiOutput("view.group.no"))
         ) ),
        ..dropdownButton(page="view_page", title=.IGoR$Z$any$layout,
          fluidRow(
            column(width=6, sliderInput("view.page.size",..s2(.IGoR$Z$view$page.size),1,100,10))
        ) ) ),
        column(width=11, htmlOutput("view.html.table"))
    ) ),

  server = function(input, output, session) {

    build_table <- function(.data){
      # Updating the column menu is not instantaneous : input field is refreshed later
      cols <- input$view.columns
      if (length(intersect(cols,names(.data)))!=length(cols)) cols <- character(0)
      rows <- if (length(input$view.page.size)>0) input$view.page.size else 10
      page <- ..if.sync(input,"view.page.no")
      if (is.null(page)) page <- 1

      l1 <- do.call(tags$tr,
                    map(c(sprintf("%dx%d",nrow(.data),length(cols)),cols),tags$th)
      )
      top <- min((page-1)*rows+1,nrow(.data))
      end <- min(page*rows,nrow(.data))
      l2 <- if (end>=top)
        map(seq(top,end),
            function (i) {
              l <- if (length(cols)>0) as.list(.data[i,cols,drop=FALSE]) else list()
              names(l) <- NULL
              do.call(tags$tr,
                      append(
                        list(tags$td(tags$span(style="color:blue",rownames(.data)[i]))),
                        map(l,function(x) tags$td(toString(x)))
                      ) )
            }
        )

      do.call(tags$table,
        append(
          list(style = "border: 1px solid black; padding: 1%; width: 100%;",l1),
          l2
      ) )
    }

    df <- reactive({
      .IGoR$state$data
      .IGoR$state$meta
      df <- as.data.frame(..data(input))
      where <- ..if.sync(input,"view.where")
      group <- ..if.sync(input,"view.group")
      df1 <- df %>%
        mutate(..row.names=row.names(.)) %>%
        group_by_at(group)          # drops the row.names
      if ((length(input$view.rownames)==0)&&..isNotEmpty(where)) {
        df2 <- tryCatch(eval(parse(text=glue("df1 %>% filter({where})"))),
                        error=identity)
        if (is(df2,"condition")) {
          output$view.comment <- renderText(df2$message)
          df <- df %>% filter(FALSE)
        }
        else {
          output$view.comment <- renderText(str_sub(..look(input$view.where),4))
          df1 <- df2
          df <- as.data.frame(df1)
          row.names(df) <- df$..row.names
          df$..row.names <- NULL
        }
      }
      else
        output$view.comment <- renderText('')
      if ((length(input$view.rownames)==0)&&(length(group)>0)) {
        list(data=df,groups=length(attr(df1,"groups")$.rows),   #  dplyr >1.0.0
             groups_size=Map(length, attr(df1,"groups")$.rows), #
             groups_rows=attr(df1,"groups")$.rows               #
        )
      }
      else
        list(data=df,groups=0)
    })

    output$view.group <- renderUI({
      .IGoR$state$meta
      if ((length(input$view.rownames)==0))
        selectizeInput(..do.sync(input,"view.group"), label=..s3(.IGoR$Z$any$group),
                       multiple = TRUE, options = list(placeholder = .IGoR$Z$any$cols.discrete),
                       choices = ..columns(input$main.data,"discrete"))
    })

    output$view.group.no <- renderUI(
      if ((length(input$view.rownames)==0)&&(length(input$view.group)>0))
        sliderInput(..do.sync(input,"view.group.no"),..s2(.IGoR$Z$view$group.no),1,df()$groups,1, step=1, round=TRUE)
    )

    output$view.page.no <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)) {
        df <- df()
        n <- if (!is.null(input$view.rownames)) length(input$view.rownames)
        else if((df$groups==0)||is.null(input$view.group.no)) nrow(df$data)
        else df$groups_size[[input$view.group.no]]
        m <- trunc((n-1)/input$view.page.size)+1
        sliderInput(..do.sync(input,"view.page.no"),..s2(.IGoR$Z$view$page.no),1,m,1,step=1, round=TRUE, sep="")
      }
    })

    output$view.rownames <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        if (nrow(..data(input))<=.IGoR$MAXROWNAMES)
          selectizeInput("view.rownames",..s3(.IGoR$Z$any$rows),
                         multiple = TRUE, options = list(placeholder = sprintf(.IGoR$Z$view$nrows,nrow(..data(input)))),
                         choices = rownames(..data(input))
          )
          else h4(sprintf("%d observations",nrow(..data(input))))
    })

    output$view.columns <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        selectizeInput("view.columns",..s1(.IGoR$Z$any$vars),
                       multiple = TRUE, options = list(placeholder=sprintf(.IGoR$Z$view$ncols, ncol(..data(input)))),
                       choices = ..columns(input$main.data)
        )
    })

    output$view.where <- renderUI(
      if (..isNotEmpty(input$main.data)&&(length(input$view.rownames)==0))
        tagList(
          textInput(..do.sync(input,"view.where"),..s3(.IGoR$Z$any$where),width='100%'),
          verbatimTextOutput("view.comment")
    )   )

    output$view.html.table <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)) {
        df <- df()
        if (length(input$view.rownames)>0)
          build_table(subset(df$data,rownames(df$data) %in% input$view.rownames))
        else
          if (df$groups>0){
            group.no <- ..if.sync(input,"view.group.no")
            if (length(group.no)>0) build_table(df$data[df$groups_rows[[group.no]],])
          }
        else
          build_table(df$data)
    } } )

  }
)
