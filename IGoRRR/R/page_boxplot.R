
### 'gf_boxplot' interface
###   Dependencies on specific packages: none.
###   Dependencies in generated code: ggformula.

page_boxplot <- list(

  ui = function() ..ui(page="boxplot", graphics=TRUE),


  server = function(input, output, session) {

    ..gServer(input,output,"boxplot")

    output$boxplot.save.control <- renderUI(if (..isNotEmpty(input$boxplot.Y)) ..save.ui("boxplot"))

    output$boxplot.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6,
            box(width='100%',
              fluidRow(
                column(width=6, selectizeInput("boxplot.Y", ..s1(.IGoR$Z$any$var.quan.y), ..numeric(input))),
                column(width=6, uiOutput("boxplot.Y.label"))
              ),
              fluidRow(
                column(width=6, selectizeInput("boxplot.X", ..s3(.IGoR$Z$any$var.qual.x), ..discrete(input))),
                column(width=6, uiOutput("boxplot.X.label"))
          ) ) ),
          column(width=6, uiOutput("boxplot.save.control"))
        )
    })

    output$boxplot.dropdown <- renderUI(
      ..dropdownButton(page="boxplot",
        checkboxInput("boxplot.coordflip",..s4(.IGoR$Z$boxplot$coordflip), FALSE)
    ) )

    ..output.gVarLabel(input,output,"boxplot","Y")

    ..output.gVarLabel(input,output,"boxplot","X")

    output$boxplot.command2 <- renderUI(
      ..textarea("boxplot", "gf_boxplot(y~x)", 3,
        if (..isNotEmpty(input$main.data)&&..isNotEmpty(input$boxplot.Y))
          ..command2(
            "gf_boxplot(",
            glue(if (..isNotEmpty(input$boxplot.X))
                      "{..nameg(input$boxplot.Y)} ~ {..nameg(input$boxplot.X)}"
                 else " ~ {..nameg(input$boxplot.Y)}"),
            ")",
            if (..isTRUE(input$boxplot.coordflip)) paste0(NL,"gf_refine(coord_flip())"),
 		        ..gTitleCmd(input,"boxplot", Y=TRUE, X=..isNotEmpty(input$boxplot.X)),
		        ..gSaveCmd(input,"boxplot")
    ) )   )

  }
)
