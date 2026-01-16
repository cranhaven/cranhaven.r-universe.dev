
### 'gf_point' interface
###   Dependencies on specific packages: none.
###   Dependencies in generated code: ggformula.

page_points <- list(

  ui = function() ..ui(page="points", graphics=TRUE),


  server = function(input, output, session) {

    ..gServer(input,output,"points")

    output$points.save.control <- renderUI(if (..isNotEmpty(input$points.Y)&&..isNotEmpty(input$points.X)) ..save.ui("points"))

    output$points.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6,
            box(width='100%',
              fluidRow(
                column(width=6, selectizeInput("points.X", ..s1(.IGoR$Z$any$var.quan.x), ..numeric(input))),
                column(width=6, uiOutput("points.X.label"))
              ),
              fluidRow(
                column(width=6, selectizeInput("points.Y", ..s1(.IGoR$Z$any$var.quan.y), ..numeric(input))),
                column(width=6, uiOutput("points.Y.label"))
          ) ) ),
          column(width=6,
            box(width='100%',
              radioButtons("points.type",..s2(.IGoR$Z$points$type),..Znames("points","type",c("points","line","lm")))
            ),
            uiOutput("points.save.control")
        ) )
    })

    output$points.dropdown <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)&&..isNotEmpty(input$main.data))
        ..dropdownButton(page="points",
          fluidRow(
            column(width=4, radioButtons("points.size.type",..s2(.IGoR$Z$points$size.type),..Znames("points","type",c("var","all")))),
            column(width=3, uiOutput("points.size")),
            column(width=5, uiOutput("points.size.column.label"))
          ),
          fluidRow(
            column(width=4, radioButtons("points.color.type",..s2(.IGoR$Z$points$color.type),..Znames("points","type",c("var","all")))),
            column(width=3, uiOutput("points.color")),
            column(width=5, uiOutput("points.color.column.label"))
          ),
          fluidRow(
            column(width=7, uiOutput("points.shape")),
            column(width=5, uiOutput("points.shape.column.label"))
          ),
          ..hr(),
          strong(.IGoR$Z$any$x),
          fluidRow(
            column(width=6, uiOutput("points.scale")),
            column(width=6, checkboxInput("points.fixed",..s3(.IGoR$Z$points$fixed), FALSE))
        ) )
    })

    ..output.gVarLabel(input,output,"points","X")

    ..output.gVarLabel(input,output,"points","Y")

    output$points.scale <- renderUI({
      .IGoR$state$meta
      numericInput("points.scale",..s3(.IGoR$Z$points$scale), NA)
    })

    output$points.size <- renderUI({
      .IGoR$state$meta
      if (!is.null(input$points.size.type))
        if (input$points.size.type=="var")
             selectizeInput("points.size.column", ..s3(.IGoR$Z$any$var.quan), choices=..numeric(input,none=.IGoR$NONE))
        else sliderInput("points.size.value", "", min=1, max=10, value=1)
    })

    output$points.size.column.label <- renderUI(
      if (..isEQ(input$points.size.type,"var")&&..isNotEmpty(input$points.size.column))
        ..gLabel.ui(input,"points","size.column")
    )

    output$points.color <- renderUI({
      .IGoR$state$meta
      if (!is.null(input$points.color.type))
        if (input$points.color.type=="var")
             selectizeInput("points.color.column", ..s3(.IGoR$Z$any$var.qual), choices=..discrete(input,none=.IGoR$NONE))
        else selectizeInput("points.color.value", "", choices=.IGoR$COLORS)
    })

    output$points.color.column.label <- renderUI(
      if (..isEQ(input$points.color.type,"var")&&..isNotEmpty(input$points.column.column))
        ..gLabel.ui(input,"points","color.column")
    )

    output$points.shape <- renderUI({
      .IGoR$state$meta
      if (..isNE(input$points.type,"line"))
        selectizeInput("points.shape.column", ..s3(.IGoR$Z$points$shape), choices=..discrete(input,none=.IGoR$NONE))
    })

    output$points.shape.column.label <- renderUI(
      if (..isNotEmpty(input$points.shape.column))
        ..gLabel.ui(input,"points","shape.column")
    )

    output$points.command2 <- renderUI(
      ..textarea("points", "gf_point(y~x)", 4,
        if (!is.null(input$points.type)
            &&(..isNotEmpty(input$points.Y)&&..isNotEmpty(input$points.X))
            ) {
          X <- ..nameg(input$points.X)
          Y <- ..nameg(input$points.Y)
          color <- if (length(input$points.color.type)==0) ""
                   else
                     if (input$points.color.type=="var")
                       if (..isNotEmpty(input$points.color.column)) glue(", color=~{input$points.color.column}") else ''
                    else
                       if (..isNE(input$points.color.value,'black')) glue(", color='{input$points.color.value}'") else ''
          size <- if (length(input$points.size.type)==0) ""
                  else
                    if (input$points.size.type=="var")
                      if (..isNotEmpty(input$points.size.column)) glue(", size=~{input$points.size.column}") else ''
                    else
                      if (..isNE(input$points.size.value,1)) glue(", size={input$points.size.value}") else ''
          shape <- if (!..isNotEmpty(input$points.shape.column)||(input$points.type=="line")) ""
                   else glue(", shape=~{input$points.shape.column}")
          type <- if (input$points.type=="line") "line" else "point"
          ..command2(
            if (input$points.type=="lm")
              paste0(glue("gf_lm({Y} ~ {X}, interval='confidence')"),NL),
            glue ("gf_{type}({Y} ~ {X}{color}{shape}{size})"),
            if (..isNotNA(input$points.scale)) {
              scale <- round(input$points.scale)
              x <- ..data(input)[[input$points.X]]
              x1 <- (min(x,na.rm=TRUE) %/% scale)*scale
              x2 <- max(x,na.rm=TRUE)
              paste0(NL,glue("gf_refine(scale_x_continuous(breaks=seq({x1},{x2},{scale})))"))
            },
            if (..isTRUE(input$points.fixed)) paste0(NL,"gf_refine(coord_fixed())"),
 		        ..gTitleCmd(input,"points", X=TRUE, Y=TRUE,
 		          c(..gLabel.arg(input,"points","size.column", "size"),
 		            ..gLabel.arg(input,"points","color.column","color"),
 		            ..gLabel.arg(input,"points","shape.column","shape")
 		        ) ),
		        ..gSaveCmd(input,"points")
          )
        }
    ) )

  }
)
