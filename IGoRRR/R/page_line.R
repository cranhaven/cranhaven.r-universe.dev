

### 'gf_line' interface
###   Dependencies on specific packages: lubridate.
###   Dependencies in generated code: ggformula.


page_line <- list(

  ui = function() ..ui(page="line", graphics=TRUE),


  server = function(input, output, session) {

    SPAN = .75

    ..gServer(input,output,"line")

    output$line.save.control <- renderUI(if (..isNotEmpty(input$line.Y)&&..isNotEmpty(input$line.X)) ..save.ui("line"))

    output$line.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6,
            box(width='100%',
              fluidRow(
                column(width=6, selectizeInput("line.X", ..s1(.IGoR$Z$any$var.quan.x), ..numeric(input))),
                column(width=6, uiOutput("line.X.label"))
              ),
              fluidRow(
                column(width=6, selectizeInput("line.Y", ..s1(.IGoR$Z$any$var.quan.y), ..numeric(input))),
                column(width=6, uiOutput("line.Y.label"))
              ),
              fluidRow(
                column(width=6, selectizeInput("line.group", ..s1(.IGoR$Z$line$group), ..discrete(input))),
                column(width=6, uiOutput("line.group.label"))
          ) ) ),
          column(width=6,
            box(width='100%',
              column(width=6, checkboxInput("line.loess", ..s2(.IGoR$Z$line$loess), FALSE)),
              column(width=6, uiOutput("line.loess"))
            ),
            uiOutput("line.save.control")
        ) )
    })

    ..output.gVarLabel(input,output,"line","X")

    ..output.gVarLabel(input,output,"line","Y")

    ..output.gVarLabel(input,output,"line","group")


    output$line.loess <- renderUI(
      if (..isTRUE(input$line.loess)) sliderInput("line.loess.span", "",.1, 2, SPAN, step=.05)
    )

    output$line.command2 <- renderUI(
      ..textarea("line", "gf_line(y ~ x)", 6,
        if (..isNotEmpty(input$main.data)&&..isNotEmpty(input$line.Y)&&..isNotEmpty(input$line.X)) {
          color <- if (..isNotEmpty(input$line.group)) glue(", color=~{..nameg(input$line.group)}") else ""
          pronoun <- if (..isNotEmpty(input$line.group)) "data" else ""
          if (..isTRUE(input$line.loess)) {
            span <- if (..inOrNULL(input$line.loess.span, SPAN)) ""
                    else glue(", span={input$line.loess.span}")
            x <- glue(".{pronoun}${..name(input$line.X)}")
            y <- glue(".{pronoun}${..name(input$line.Y)}")
            df <- ..data(input)
            if (is.Date(df[[input$line.X]])) x <- glue("as.numeric({x})")
          }
          X <- ..nameg(input$line.X)
          Y <- ..nameg(input$line.Y)
          ..command2(
            if (..isFALSE(input$line.loess))
                 glue ("gf_line({Y} ~ {X}{color})")
            else paste0(
                   glue("filter(!is.na({..name(input$line.Y)}))"),NL,
                   ..group_by(input,"line"),
                   glue("mutate(..y=loess({y} ~ {x}{span})$fitted)"),
                   ..ungroup(input,"line"),NL,
                   glue("gf_point({Y} ~ {X}{color})"),NL,
                   glue("gf_line(..y ~ {X}{color}, linetype = 'dashed')")
                 ),
            ..gTitleCmd(input,"line", X=TRUE, Y=TRUE,
              c(if (..isNotEmpty(input$line.group)) ..gLabel.arg(input,"line","group","color")
            ) ),
            ..gSaveCmd(input,"line")
          )
        }
    ) )

  }
)



