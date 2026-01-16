
### 'gf_histogram' interface
###   Dependencies on specific packages: none.
###   Dependencies in generated code: ggformula.


page_histogram <- list(

  ui = function() ..ui(page="histogram", graphics=TRUE),


  server = function(input, output, session) {

    ..gServer(input,output,"histogram")

    output$histogram.save.control <- renderUI(if (..isNotEmpty(input$histogram.X)) ..save.ui("histogram"))

    output$histogram.control<- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6,
            box(width='100%',
              column(width=6, selectizeInput("histogram.X", ..s1(.IGoR$Z$any$var.quan), choices=..numeric(input))),
              column(width=6, uiOutput("histogram.X.label"))
          ) ),
          column(width=6, uiOutput("histogram.save.control"))
       )
    })

    output$histogram.dropdown <- renderUI(
      ..dropdownButton(page="histogram",
        fluidRow(
			    column(width=6, radioButtons("histogram.bins.type",..s2(.IGoR$Z$histogram$bins.type),..Znames("histogram","bins.type",c("bins","binwidth")))),
          column(width=6, uiOutput("histogram.bins"))
			  ),
			  fluidRow(
			    column(width=6, checkboxInput("histogram.kde",..s4(.IGoR$Z$histogram$kde), FALSE)),
			    column(width=6, uiOutput("histogram.kde.bwm"))
			  ),
			  ..hr(),
			  tags$b(.IGoR$Z$any$y),
			  fluidRow(
			    column(width=6), ..label.ui("histogram","Y","density"))
    )   )

    ..output.gVarLabel(input,output,"histogram","X")

    output$histogram.bins <- renderUI(
      if (!is.null(input$histogram.bins.type))
        numericInput("histogram.bins","",if (input$histogram.bins.type=="bins") 25))

    output$histogram.kde.bwm <- renderUI(
      if (..isTRUE(input$histogram.kde))
	      numericInput("histogram.kde.bwm",..s2(.IGoR$Z$histogram$kde.bwm), 1)
    )

    output$histogram.command2 <- renderUI(
      ..textarea("histogram", "gf_dhistogram(~x)", 3,
        if (..isNotEmpty(input$main.data)&&..isNotEmpty(input$histogram.X)) {
          bins <- if ((length(input$histogram.bins)==0)
                    ||(length(input$histogram.bins.type)==0)
                    ||((input$histogram.bins.type=="bins")&&(input$histogram.bins==25))
                    ||((input$histogram.bins.type=="binwidth")&&is.na(input$histogram.bins))) ""
                  else glue(", {input$histogram.bins.type}={input$histogram.bins}")
          X <- ..nameg(input$histogram.X)
          ..command2(
            glue("gf_dhistogram( ~ {X}{bins})"),
            if (..isTRUE(input$histogram.kde)) {
			        bwm <- if (..isEQ(input$histogram.kde.bwm,1)) "" else glue(", adjust={input$histogram.kde.bwm}")
			        paste0(" %>%\n   ",glue("gf_dens( ~ {X}{bwm})"))
			       },
 		         ..gTitleCmd(input,"histogram",X=TRUE,
 		         if (..isNE(input$histogram.Y.label,"density")) glue("y={shQuote(input$histogram.Y.label)}")),
		         ..gSaveCmd(input,"histogram")
		      )
        }
    ) )

  }
)
