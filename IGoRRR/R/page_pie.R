
### Displays a pie chart for a qualitative column of current table
###   Dependencies on specific packages: none.
###   Dependencies in generated code: ggformula.

page_pie <- list(

  ui = function() ..ui(page="pie", graphics=TRUE),


  server = function(input, output, session) {

    ..gServer(input,output,"pie")

     output$pie.save.control <- renderUI(if (..isNotEmpty(input$pie.Y)) ..save.ui("pie"))

    output$pie.control<- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6,
            box(width='100%',
              fluidRow(
                column(width=6, selectizeInput("pie.Y", ..s1(.IGoR$Z$any$var.qual), ..discrete(input))),
                column(width=6, uiOutput("pie.Y.label"))
              ),
              fluidRow(
                column(width=6, selectizeInput("pie.W", ..s3(.IGoR$Z$any$weight), ..numeric(input))),
                column(width=6, selectizeInput("pie.X", ..s3(.IGoR$Z$pie$facet),  ..discrete(input)))
          ) ) ),
          column(width=6, uiOutput("pie.save.control"))
        )
    })

    ..output.gVarLabel(input,output,"pie","Y")

    output$pie.command2 <- renderUI(
      ..textarea("pie", "gf_bar(~1, fill=~x) + coord_polar('y')", 4,
        if (..isNotEmpty(input$main.data)&&..isNotEmpty(input$pie.Y)) {
          Y <- ..nameg(input$pie.Y)
          x <- if (..isNotEmpty(input$pie.X)) glue("| {..nameg(input$pie.X)}, position=position_fill()") else ""
          w <- if (..isNotEmpty(input$pie.W)) input$pie.W else ""
          f <- if (..isNotEmpty(input$pie.W)) "col" else "bar"
          ..command2(
            glue("gf_{f}({w} ~ 1 {x}, fill=~{Y})"),NL,
            "gf_refine(coord_polar('y'))",NL,
            "gf_theme(axis.ticks=element_blank(), axis.text=element_blank(), axis.title.y=element_blank())",
            ..gTitleCmd(input,"pie",
              c("y=''",
                if (..isNE(input$pie.Y.label,input$pie.Y)) glue("fill={shQuote(input$pie.Y.label)}"))
            ),
            ..gSaveCmd(input,"pie")
          )
        }
    ) )

  }
)
