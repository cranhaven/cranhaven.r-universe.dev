# code for shiny app "visQuad" 


shinyUI(fluidPage(
  titlePanel("Quadratic functions"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("beta0", 
        label = "Intercept:",
        min = -2, max = 2, step = 0.1, value = 0),

      sliderInput("beta1", 
        label = "Linear coefficient:",
        min = -10, max = 10, step = 0.1, value = 1),
        
      sliderInput("beta2", 
        label = "Quadratic coefficient:",
        min = -10, max = 10, step = 0.1, value = 0)
    ),
    
    mainPanel(

    
      plotOutput("distPlot")
     )
  )
))
