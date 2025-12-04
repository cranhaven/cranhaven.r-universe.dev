# code for shiny app "visGamma"

library(shiny)
source("helper_visGamma.R")

shinyServer(function(input, output, session) {

  observe({
    val <- input$maxValue
    # Control the maximum value of units and counts sliders.
    updateSliderInput(session, "units", max = val) # , value=min(val, input$units))
    updateSliderInput(session, "count", max = val) # , value=min(val, input$count))
  })

  observe({
    max <- 10
    if(input$units > 0)
      max <- max(10, round(2 * input$count / input$units))
    # Control the maximum value of the other sliders.
    updateSliderInput(session, "Mode", max = max, value=min(max, input$Mode))
    updateSliderInput(session, "Shape", max = max, value=min(max, input$Shape))
  })

  shape <- reactive({
    if(input$param == "useMode") {
      input$Mode * input$Rate + 1
    } else {
      input$Shape
    }
  })
  
  output$distPlot <- renderPlot({
    # draw a gamma curve
    visGamma(shape = shape(),
            rate = input$Rate,
            count = input$count,
            units = input$units,
            showPost = input$showPost,
            param = input$param)
  })

  results <- reactive({
    resultsGamma(shape = shape(),
            rate = input$Rate,
            count = input$count,
            units = input$units,
            showPost = input$showPost)
  })

  # Show the values using an HTML table
  output$results <- renderTable({
    results()
  })

})
