# sac<-reactive({
#   a = try(withProgress(message = 'Spatial Autorange',
#                        spatialAutoRange(rasterLayer = data$Env,
#                                         doParallel = T,
#                                         plotVariograms = TRUE,
#                                         showPlots = FALSE)))
#   a
# })
#
# range<-reactive({
#   sac<-sac()
#   round(sac$range,0)
#
# })
output$ui_spatial_auto_range<-renderUI({
  observeEvent(input$sp_auto,{
    validate(
      need(length(input$var_auto) > 0, 'Choose specie predictors first !')
    )

    data$var_auto<-raster::subset(data$Env,input$var_auto)

        output$tableRange <- DT::renderDataTable({
              datatable(tableRange(),
              rownames = FALSE,
              selection="none",
              options = list(scrollX=TRUE, scrollY=250, lengthMenu=list(c(20, 50, 100, -1), c('20', '50', '100', 'All')), pageLength=20)

              )})
       observeEvent(input$vario_var,{
              output$variogram<-renderPlot({
                  sac<-sac()
                  vect<-names(data$var_auto)
                  data$variogram<-plot(sac$variograms[[which(vect==input$vario_var)]])
                  plot(sac$variograms[[which(vect==input$vario_var)]])
                                        })
                  })

                output$barchart <- renderPlot({
                  sac<-sac()
                  data$barchar<-sac$plots$barchart
                  sac$plots$barchart
                })

                output$mapplot <- renderPlot({
                  sac<-sac()
                  data$mapplot<-sac$plots$mapplot
                  sac$plots$mapplot
                })
        })
  fluidRow(column(12, h4("Spatial autocorrelation "), align="center"),
           mainPanel(width = 8, tabsetPanel(type = "tabs",
                                            tabPanel("Apply",
                                                     p('Spatial autocorrelation ranges in input environnemental variables '),
                                                     selectInput('var_auto', 'Please select the specie predictors', names(data$Env), multiple = TRUE, selectize = TRUE),
                                                     p('The spatial blocking procedure can take a long time depending on the number of input variables'),
                                                     myActionButton("sp_auto",label=("Apply"), "primary")),
                                            tabPanel("Barchart",
                                                     p('Spatial autocorrelation ranges in input environnemental variables '),
                                                     downloadButton('download_barchart','Download'),
                                                     plotOutput("barchart")),
                                            tabPanel("Map plot",
                                                     p('Corresponding spatial blocks (the selected block size is based on median spatial autocorrelation range across all input environnemental variables)'),
                                                     downloadButton('download_mapplot','Download'),
                                                     plotOutput("mapplot")),
                                            tabPanel("Autocorrelation range table",
                                                     p('Spatial autocorrelation ranges table in input covariates'),
                                                     downloadButton('download_tableRange','Download'),
                                                     DT::dataTableOutput("tableRange")),
                                            tabPanel("Variogram",
                                                     selectInput('vario_var', 'Please select the predictor to see variogram corresponding', names(data$var_auto), multiple = FALSE, selectize = TRUE),
                                                     downloadButton('download_variogram','Download'),
                                                     plotOutput("variogram"))

           ),
           id = "tabs")

  )
})
