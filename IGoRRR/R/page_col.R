
### 'gf_col' interface
###   Dependencies on specific packages: none.
###   Dependencies in generated code: ggformula.


page_col <- list(

  ui = function() ..ui(page="col", graphics=TRUE),


  server = function(input, output, session) {

    ..gServer(input,output,"col")

    output$col.save.control <- renderUI(
      if (..isNotEmpty(input$col.X)&&..isNotEmpty(input$col.N))
        ..save.ui("col")
    )

    output$col.control<- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6,
            box(width='100%',
              fluidRow(
		            column(width=6, selectizeInput("col.X", ..s1(.IGoR$Z$any$var.qual.x), choices=..discrete(input))),
			          column(width=6, selectizeInput("col.reorder", ..s3(.IGoR$Z$col$reorder), choices=..numeric(input)))
              ),
              ..s1(.IGoR$Z$any$y),
              fluidRow(
                column(width=6, selectizeInput("col.N", ..s1(.IGoR$Z$col$var.N), choices=..numeric(input))),
                column(width=6, selectizeInput("col.N.color", ..s2(.IGoR$Z$any$color), choices=.IGoR$COLORS))
              ),
              fluidRow(
                column(width=6, selectizeInput("col.M", ..s3(.IGoR$Z$col$var.M), choices=..numeric(input))),
                column(width=6, uiOutput("col.M.color"))
          ) ) ),
          column(width=6, uiOutput("col.save.control"))
        )
    })

    output$col.dropdown <- renderUI(
      ..dropdownButton(page="col",
        checkboxInput("col.coordflip",..s4(.IGoR$Z$col$coordflip), FALSE)
    ) )

    output$col.M.color <- renderUI(
      if (..isNotEmpty(input$col.M))
        selectizeInput("col.M.color",..s2(.IGoR$Z$any$color), choices=.IGoR$COLORS)
    )

    output$col.command2 <- renderUI(
      ..textarea("col", "gf_col(y~x)", 6,
        if (..isNotEmpty(input$main.data)&&..isNotEmpty(input$col.X)&&..isNotEmpty(input$col.N)) {
          s <- c(if (..isNotEmpty(input$col.M))
                 paste0(
                   "\n     ",
                   "scale_y_continuous(\n       labels=abs,\n       ",
                   glue("limits=with({input$main.data},partial(max,na.rm=TRUE)(c({..name(input$col.N)},{..name(input$col.M)}))) %>% max() %>% c(-.,.)"),
                   ")"
                 ),
                if (..isTRUE(input$col.coordflip)) "\n     coord_flip()"
          )
          cN <- if (..isEQ(input$col.N.color,"black")) "" else glue(", fill=\"{input$col.N.color}\"")
		      x <- if (..isNotEmpty(input$col.reorder))
		                glue("reorder({..name(input$col.X)},{..name(input$col.reorder)})")
		           else ..nameg(input$col.X)
		      ..command2(
            glue("gf_col({..nameg(input$col.N)} ~ {x}{cN})"),
            if (..isNotEmpty(input$col.M)) {
              cM <- if (..isEQ(input$col.M.color,"black")) "" else glue(", fill=\"{input$col.M.color}\"")
              paste0(NL,glue("gf_col(-{..nameg(input$col.M)} ~ {..nameg(input$col.X)}{cM})"))
            },
            if (..isNotEmpty(input$col.M)) paste0(NL,glue("gf_labs(y=\"{input$col.M}     |     {input$col.N}\")")),
            if (length(s)>0) paste0(NL,glue("gf_refine({..collapse0(s)})")),
			      ..gTitleCmd(input,"col"),
            ..gSaveCmd(input,"col")
		      )
		    }
    ) )

  }
)
