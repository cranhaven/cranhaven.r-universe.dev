# visBeta

library(shiny)
source("helper_visQuad.R")

shinyServer(function(input, output, session) {



  output$distPlot <- renderPlot({
    # draw a beta curve
    visQuad(beta0 = input$beta0,
            beta1 = input$beta1,
            beta2 = input$beta2)
  })


})
