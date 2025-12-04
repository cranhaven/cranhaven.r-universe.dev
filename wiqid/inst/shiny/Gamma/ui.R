# code for shiny app "visGamma" this version with radio buttons


shinyUI(fluidPage(
  titlePanel("Gamma priors"),
  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("param", "Parameters to use:",
             c("Mode+Rate"="useMode", "Shape+Rate"="useShape"), inline=TRUE),

      sliderInput("Mode", 
        label = "Prior mode:",
        min = 0, max = 10, step = 0.1, value = 2),

      sliderInput("Rate", 
        label = "Prior rate:",
        min = 0, max = 10, step = 0.1, value = 1),
        
      sliderInput("Shape", 
        label = "Prior shape:",
        min = 0, max = 10, step = 0.1, value = 1),
      hr(),  
      sliderInput("units", 
        label = "Data: number of sample units",
        min = 0, max = 10, step = 1, value = 0),
        
     sliderInput("count", 
        label = "Data: total count",
        min = 0, max = 10, step = 1, value = 0),
        
      checkboxInput("showPost",
        label = "Display the posterior",
        value = FALSE),

     sliderInput("maxValue", 
        label = "Change upper limits for data",
        min = 10, max = 500, step = 10, value = 10),
        
        p(em("Code by Mike Meredith"), align="center")
    ),
    
    mainPanel(

      h3("Display gamma distributions, Poisson likelihoods, and gamma posteriors"),
      p("Choose which parameters to use, then control the curve with the rate slider and either the mode or shape sliders."),
    
      plotOutput("distPlot"),
      tableOutput("results"),
      
      h3("Notes"),
      p("The rate and shape must be > 0; when the slider is at 0, the value used is 0.0001"),
      p("The data consist of the number of units in the sample and the total count, eg, 10 goals in 6 football matches, or 10 ticks on 6 rats examined."),
      p("The likelihood curve is scaled so that the area under the curve = 1. This is done by multiplying the output from dpois by the number of units."),
      p("The mode of the likelihood curve is the Maximum Likelihood Estimate of the rate, lambda."),
      br()
    )
  )
))
