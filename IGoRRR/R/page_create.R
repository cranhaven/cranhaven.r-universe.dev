
### Creates a new table from scratch
###   Dependencies on specific packages : none.
###   Dependencies in gerenated code: none.

page_create <- list(

  ui = function() ..ui(page="create",
    fluidRow(
      column(width=6,
        box(width='100%',
          textInput("create.columns",    ..s1(.IGoR$Z$create$names)),
          checkboxInput("create.factors",..s4(.IGoR$Z$any$stringsAsFactors),FALSE),
          textInput("create.na.strings", ..s2(.IGoR$Z$create$na.strings),"NA")
      ) ),
      column(width=6,..load.ui("create"))
  ) ),


  server = function(input, output, session) {

    ..rLogo(input,output,"create")

    na.strings <- function () if (input$create.na.strings=="NA") "" else glue(", na.strings=\"{input$create.na.strings}\"")

    command1 <- "read.table(header=TRUE, text=\"{input$create.columns}"

    output$create.command1 <- renderText(
      .IGoR$create.command1 <- paste0(make.names(input$create.out),' <- ',glue(command1)))

    output$create.command2 <- renderUI(
      ..textarea("create", .IGoR$Z$create$data, 5, ''))

    output$create.command3 <- renderText(
      .IGoR$create.command3 <- glue("\", stringsAsFactors={input$create.factors}{na.strings()})"))

    observeEvent({input$create.command2
                  input$create.out
                  input$create.columns
                  input$create.na.strings
                  input$create.factors},
      output$create.comment <- renderText({
        output$create.preview <- NULL
        t <- make.names(input$create.out)
        s <- paste0(glue(command1),'\n',input$create.command2,glue(.IGoR$create.command3))
        if (..isNotEmpty(s)) {
          x <- tryCatch(eval(parse(text=s), envir=.IGoR$env),
                        error=identity)
          if (!..isCondition(x)) {
            ..enableLoad(input,output,"create")
            sprintf(.IGoR$Z$create$msg.result,t,nrow(x),ncol(x))
          }
          else {
            ..disableLoad(output,"create")
            x$message
        } }
        else {
          ..disableLoad(output,"create")
          ""
      } } )
    )

    observeEvent(input$create.load,
      ..do(input,output,"create",
           paste0(.IGoR$create.command1,'\n',input$create.command2,.IGoR$create.command3)
    ) )

  }
)

