
### 'gf_bar' interface
###   Dependencies on specific packages: none.
###   Dependencies in generated code: ggformula.

page_bar <- list(

  ui = function() ..ui(page="bar", graphics=TRUE),


  server = function(input, output, session) {

    ..gServer(input,output,"bar")

    output$bar.save.control <- renderUI(if (..isNotEmpty(input$bar.X)) ..save.ui("bar"))

    output$bar.control<- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6,
            box(width='100%',
              fluidRow(
                column(width=6, selectizeInput("bar.X", label=..s1(.IGoR$Z$any$var.qual.x), ..discrete(input))),
                column(width=6, uiOutput("bar.X.label"))
          ) ) ),
          column(width=6, uiOutput("bar.save.control"))
       )
    })

    output$bar.dropdown <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        ..dropdownButton(page="bar",
          fluidRow(
            column(width=4, radioButtons("bar.fill.type", ..s2(.IGoR$Z$bar$fill.type),..Znames("bar","type",c("var","all")))),
            column(width=3, uiOutput("bar.fill")),
            column(width=5,
              uiOutput("bar.fill.column.label"),
              uiOutput("bar.fill.position")
          ) ),
          fluidRow(
            column(width=6, checkboxInput("bar.coordflip",..s4(.IGoR$Z$bar$coordflip),FALSE)),
            column(width=6, checkboxInput("bar.reorder",  ..s4(.IGoR$Z$bar$reorder),  FALSE))
          ),
          ..hr(),
          tags$b(.IGoR$Z$any$y),
          fluidRow(
            column(width=6, checkboxInput("bar.breaks",..s4(.IGoR$Z$bar$breaks),FALSE)),
            column(width=6, ..label.ui("bar","Y","count"))
        ) )
    })

    ..output.gVarLabel(input,output,"bar","X")

    output$bar.fill <- renderUI({
      .IGoR$state$meta
      if (!is.null(input$bar.fill.type))
        if (input$bar.fill.type=="var")
             selectizeInput("bar.fill.column", ..s3(.IGoR$Z$any$var.qual), choices=..discrete(input,none=.IGoR$NONE))
        else selectizeInput("bar.fill.value", "", choices=.IGoR$COLORS)
    })

    output$bar.fill.position <- renderUI(
      if (..isEQ(input$bar.fill.type,"var")&&..isNotEmpty(input$bar.fill.column))
        radioButtons("bar.fill.position","",..Znames("bar","position",c("stack","dodge")))
    )

    output$bar.fill.column.label <- renderUI(
      if (..isEQ(input$bar.fill.type,"var")&&..isNotEmpty(input$bar.fill.column))
        ..gLabel.ui(input,"bar","fill.column")
    )

    output$bar.command2 <- renderUI(
      ..textarea("bar", "gf_bar(~x)", 3, 
        if (..isNotEmpty(input$main.data)&&..isNotEmpty(input$bar.X)) {
          x <- if (..isTRUE(input$bar.reorder)) glue("reorder({..name(input$bar.X)},`n()`)") else ..nameg(input$bar.X)
          fill <- if (length(input$bar.fill.type)==0) ""
                  else
                    if (input$bar.fill.type=="var")
                      if (!..isNotEmpty(input$bar.fill.column)) ""
                      else {
                        p <- if (..inOrNULL(input$bar.fill.position,"stack")) ""
                             else glue(", position='{input$bar.fill.position}'")
                        glue(", fill=~{..nameg(input$bar.fill.column)}{p}")
                      }
                    else
                      if (!..isNE(input$bar.fill.value,'black')) ""
                      else glue(", fill='{input$bar.fill.value}'")
          r <- c(
            if (..isTRUE(input$bar.breaks))    "scale_y_continuous(breaks=function (x) pretty(x, 5))",
            if (..isTRUE(input$bar.coordflip)) "coord_flip()"
          )
          ..command2(
            if (..isTRUE(input$bar.reorder)) paste0(glue("group_by({..name(input$bar.X)})"),NL,"mutate(n())",NL),
            glue("gf_bar( ~ {x}{fill})"),
            if (length(r)>0) paste0(NL,glue("gf_refine({paste(r,collapse=', ')})")),
 		        ..gTitleCmd(input,"bar", X=TRUE,
 		          c(if (..isNE(input$bar.Y.label,"count")) glue("y={shQuote(input$bar.Y.label)}"),
 		            ..gLabel.arg(input,"bar","fill.column","fill")
 		        ) ),
            ..gSaveCmd(input,"bar")
          )
        }
    ) )
    

  }
)

