# visBeta

library(shiny)
source("helper_visBeta.R")

shinyServer(function(input, output, session) {

  observe({
    val <- input$maxValue
    # Control the maximum value of the data and Concentration sliders.
    updateSliderInput(session, "Conc", max = val)
    updateSliderInput(session, "data", max = val)
  })


  shapes <- reactive({
    if(input$param == "useMode") {
      sh <- c(input$Mode * (input$Conc-2) + 1, (1-input$Mode) * (input$Conc-2) + 1)
    } else {
      sh <- c(input$Shape1, input$Shape2)
    }
    if(length(sh) < 2)
      sh <- c(0, 0)
    sh
  })

  output$distPlot <- renderPlot({
    # draw a beta curve
    visBeta(shapes = shapes(),
            data=input$data,
            showPost = input$showPost,
            param = input$param)
  })

  results <- reactive({
    resultsBeta(shapes = shapes(),
            data = input$data,
            showPost = input$showPost)
  })

  # Show the values using an HTML table
  output$results <- renderTable({
    results()
  })

})
