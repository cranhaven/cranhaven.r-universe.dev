
### Display a table of summary statistics, cross-classified by various variables 
###   Dependencies on specific packages: tables.
###   Dependencies in generated code: tables, dplyr.

page_tabular <- list(

  ui = function() ..ui(page="tabular", control=TRUE, command=FALSE,
    ..commandBox("tabular"),
    tableOutput("tabular.output")
  ),


  server = function(input, output, session) {

    ..bServer(input,output,"tabular")

    statsv <- c("n","all","row","col","mean","median","q1","q3","p10","p90","min","max") %>%
      {names(.) <- .; .} %>% ..Zrename()

    statNames <- function(l) names(statsv)[Vectorize(function (x) which(x==statsv))(l)]

    stat <- function(i,w)
      if (nchar(w)==0)
        c(n="",
        all="Percent('all')",
        row="Percent('row')",
        col="Percent('col')",
       mean="mean",
     median="median",
         q1="quantile*Arguments(p=.25)",
         q3="quantile*Arguments(p=.75)",
        p10="quantile*Arguments(p=.1)",
        p90="quantile*Arguments(p=.9)",
        min="min",
        max="max")[i]
      else {
        w <- ..name(w)
        glue(
          c(n="sum*{w}",
          all="Percent('all',fn=wtd.percent)*{w}",
          row="Percent('row',fn=wtd.percent)*{w}",
          col="Percent('col',fn=wtd.percent)*{w}",
         mean="wtd.mean*Arguments(w={w})",
       median="wtd.quantile*Arguments(p=.5,w={w})",
           q1="wtd.quantile*Arguments(p=.25,w={w})",
           q3="wtd.quantile*Arguments(p=.75,w={w})",
          p10="wtd.quantile*Arguments(p=.1,w={w})",
          p90="wtd.quantile*Arguments(p=.9,w={w})",
          min="min",
          max="max")[i]
      )}

    output$tabular.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6,
            box(width='100%',
              fluidRow(
                column(width=6, ..s1(.IGoR$Z$tabular$vars.qual)),
                column(width=6, checkboxInput("tabular.factor",..s4(.IGoR$Z$tabular$factor), FALSE))
              ),
              fluidRow(
                column(width=6, uiOutput("tabular.X")),
                column(width=6, uiOutput("tabular.X.nest"),
                                uiOutput("tabular.X.all"),
                                uiOutput("tabular.X.drop"))
              ),
              fluidRow(
                column(width=6, uiOutput("tabular.Y")),
                column(width=6, uiOutput("tabular.Y.nest"),
                                uiOutput("tabular.Y.all"),
                                uiOutput('tabular.Y.drop'))
              ),
              hr(),
              fluidRow(
                column(width=6, selectizeInput("tabular.W", ..s3(.IGoR$Z$any$weight), ..numeric(input)))
          ) ) ),
          column(width=6,
            box(width='100%',
              column(width=6,radioButtons("tabular.type",..s2(.IGoR$Z$tabular$type),..Znames("tabular","type",c("n","all","row","col","var")))),
              column(width=6,uiOutput("tabular.args"))
            ),
            box(width='100%',
              checkboxInput("tabular.heading",..s4(.IGoR$Z$tabular$heading), FALSE)
            ),
            ..save.ui("tabular",.title=.IGoR$Z$tabular$save)
        ) )
    })

    output$tabular.X <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        selectizeInput("tabular.X", .IGoR$Z$tabular$col,
                       multiple = TRUE,
                       options = list(placeholder = .IGoR$any[[if (..isTRUE(input$tabular.factor)) "cols.discrete" else "cols.fct"]]),
                       choices = ..columns(input$main.data,    if (..isTRUE(input$tabular.factor)) "discrete"      else "factor")
        )
    })

    output$tabular.X.all <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)&&(length(input$tabular.X)>0))
        checkboxInput("tabular.X.all", ..s4(.IGoR$Z$tabular$all), TRUE)
    })

    output$tabular.X.drop <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)&&(length(input$tabular.X)>0))
        checkboxInput("tabular.X.drop", ..s4(.IGoR$Z$tabular$drop.col), FALSE)
    })

    output$tabular.X.nest <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)&&(length(input$tabular.X)>1))
        checkboxInput("tabular.X.nest", ..s5(.IGoR$Z$tabular$nest), TRUE)
    })

    output$tabular.Y <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        selectizeInput("tabular.Y", .IGoR$Z$tabular$row,
                       multiple = TRUE,
                       options = list(placeholder = .IGoR$any[[if (..isTRUE(input$tabular.factor)) "cols.discrete" else "cols.fct"]]),
                       choices = ..columns(input$main.data,    if (..isTRUE(input$tabular.factor)) "discrete"      else "factor")
        )
    })

    output$tabular.Y.all <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)&&(length(input$tabular.Y)>0))
        checkboxInput("tabular.Y.all", ..s4(.IGoR$Z$tabular$all), TRUE)
    })

    output$tabular.Y.drop <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)&&(length(input$tabular.Y)>0))
        checkboxInput("tabular.Y.drop", ..s4(.IGoR$Z$tabular$drop.row), FALSE)
    })

    output$tabular.Y.nest <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)&&(length(input$tabular.Y)>1))
        checkboxInput("tabular.Y.nest", ..s5(.IGoR$Z$tabular$nest), TRUE)
    })

    output$tabular.args <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        if (..isEQ(input$tabular.type,'var')&&(length(input$tabular.W)>0))
          tagList( # Statistic over another column
            selectizeInput("tabular.Z", ..s1(.IGoR$Z$tabular$Z), ..numeric(input)),
            selectizeInput("tabular.Z.funs", ..s1(.IGoR$Z$tabular$Z.funs),
              multiple=TRUE, options = list(placeholder = .IGoR$Z$any$funs),
              choices=..Zrename(c(mean="mean",
                                median="median",
                                    q1="q1",
                                    q3="q3",
                                   p10="p10",
                                   p90="p90",
                                   min="min",
                                   max="max"))
          ) )
        else
        if (..isIn(input$tabular.type,c("col","row","all")))
          sliderInput("tabular.digits", ..s2(.IGoR$Z$tabular$digits), min=0, max=2, value=2)
        else
        if (..isEQ(input$tabular.type,"n"))
          checkboxInput("tabular.sep",..s4(.IGoR$Z$tabular$sep),FALSE)
    })

    ..saveButton(input,output,"tabular")

    labels <- function (x)
      Vectorize(function (x)
        attr(..data(input)[[x]],'label') %>%
        {if (is.null(.)) x else paste0('Heading(',shQuote(.),')*',x)}
      )(x)

    output$tabular.command2 <- renderUI(
      ..textarea("tabular", "tabular(...)", 4,
        if (!is.null(input$tabular.type)) {
          X <- ..name(input$tabular.X) %>%
               {if ((length(.)>0)&&..isTRUE(input$tabular.heading)) labels(.) else .}
          Y <- ..name(input$tabular.Y) %>%
               {if ((length(.)>0)&&..isTRUE(input$tabular.heading)) labels(.) else .}
          Z <- ..name(input$tabular.Z) %>%
               {if ((length(.)>0)&&..isTRUE(input$tabular.heading)) labels(.) else .}
          all <- if (..isTRUE(input$tabular.heading))
                      glue('Heading("{.IGoR$Z$tabular$all.heading}")*1')
                 else "1"
          z <- if (input$tabular.type=='var')
                 if (..isNotEmpty(input$tabular.Z)&&(length(input$tabular.Z.funs)>0)) {
                   h <- paste0(
                          'Heading("',
                          if (..isTRUE(input$tabular.heading)) statNames(input$tabular.Z.funs)
                          else input$tabular.Z.funs,
                          '")*'
                        )
                   g <- map_chr(input$tabular.Z.funs,function(x) stat(x,input$tabular.W))
                   f <- paste0(
                          if (..isTRUE(input$tabular.heading)) h
                          else ifelse(str_detect(g,'\\*'),h,''),
                          g
                        )
                   paste0(Z,'*',
                        if (length(input$tabular.Z.funs)==1) f
                        else paste0('(',paste(f, collapse='+'),')')
                   )
                 }
                 else ""
               else
                paste0(
                  if ((input$tabular.type!='n')&&..isTRUE(input$tabular.heading))
                    glue('Heading("{statNames(input$tabular.type)}")*'),
                  stat(input$tabular.type,input$tabular.W),
                  if ((input$tabular.type!='n')&&!is.null(input$tabular.digits))
                    glue('*Format(partial(sprintf,"%.{input$tabular.digits}f")())*Justify(r)')
                )
          y <- if (length(Y)==0) ""
               else paste0(
                      if (length(Y)>1)
                        paste(Y,collapse=if (!..isTRUE(input$tabular.Y.nest)) '+' else '*')
                      else Y,
                      if (..isTRUE(input$tabular.Y.all)) paste0('+',all)
                    )
          x <- if (length(X)==0) ""
               else paste0(
                      if (length(X)>1)
                         paste(X,collapse=if (!..isTRUE(input$tabular.X.nest)) '+' else '*')
                      else X,
                      if (..isTRUE(input$tabular.X.all)) paste0('+',all)
                    )
          if ((nchar(z)==0)&&(nchar(x)==0)) z <- all
          else if ((nchar(z)>0)&&(nchar(x)>0)) x <- glue("*({x})")
          x <- paste0(z,x)
          f <- (input$tabular.type=='n')&&..isTRUE(input$tabular.sep)
          d <- c(
            if (..isTRUE(input$tabular.X.drop)) "col",
            if (..isTRUE(input$tabular.Y.drop)) "row"
          )
          if (f||(length(d)>0))
            x <- paste0(
                   if (((length(input$tabular.X)>0)&&.isTRUE(input$tabular.X.all))
                     ||((length(input$tabular.Y)>0)&&.isTRUE(input$tabular.Y.all))
                     ||((length(input$tabular.X)>1)&&.isFALSE(input$tabular.X.nest))
                    ||((length(input$tabular.Y)>1)&&.isFALSE(input$tabular.Y.nest))
                      ) paste0('(',x,')') else x,
                   if (f) "\n     *Format(partial(format,big.mark=' ')())*Justify(r)",
                   if (length(d)>0) paste0("\n     *DropEmpty(which=",..collapse2(d),")")
                 )
          ..command2(
            if (..isTRUE(input$tabular.factor)) {
              d <- ..data(input)
              l <- c(input$tabular.X,input$tabular.Y)
              l <- l[map_lgl(l,function(x) !is.factor(d[[x]]))]
              if (length(l)>0) paste0(glue("mutate_at({..collapse2(l)}, as.factor)"), NL)
            },
            paste0(
              "tabular(",y," ~ ",x,"\n   , . )"),
               if (..isTRUE(input$tabular.save)) {
                 f  <- parseSavePath(.IGoR$config$volumes,input$tabularSaveButton)$datapath
                 if (..isNotEmpty(f)) paste0(NL,"{",glue("Hmisc::html(.,\"{f}\")"),";.}")
               }
            )
          }
    ) )

    toHtml <- tables::html.tabular

    observeEvent({.IGoR$state$data; input$tabular.command2},
      isolate(
        output$tabular.output <- renderText(
          if (..isNotEmpty(input$main.data)&&..isNotEmpty(input$tabular.command2)) {
            command <- paste0(input$main.data,' %>% ',input$tabular.command2)
            x <- tryCatch(eval(parse(text=command)),
                          error=function(e) {
                            if (e$message %in% c("node stack overflow", .IGoR$Z$tabular$overflow))
                              e$message <- paste0(e$message,"\n",.IGoR$Z$tabular$msg.error1)
                            else
                            if (e$message=="No levels in 'x'!")
                              e$message <- paste0(e$message,"\n",.IGoR$Z$tabular$msg.error0)
                            else
                            if (e$message=="attempt to use zero-length variable name")
                              e$message <- paste0(e$message,"\n",.IGoR$Z$tabular$msg.error2)
                            output$tabular.comment <- renderText(e$message)
                            NULL
                          }
                  )
            if (!is.null(x)) {
              ..writeLog("tabular",command)
              output$tabular.comment <- renderText("")
              toHtml(x,options=htmloptions(pad=TRUE, HTMLleftpad=TRUE))
          } }

   ) ) )

  }
)
