# code for shiny app "visBeta" this version with radio buttons


shinyUI(fluidPage(
  titlePanel("Beta priors"),
  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("param", "Parameters to use:",
             c("Mode+Concentration"="useMode", "Shapes"="useShape"), inline=TRUE),

      sliderInput("Mode", 
        label = "Prior mode:",
        min = 0, max = 1, step = 0.01, value = 0.2),

      sliderInput("Conc", 
        label = "Prior concentration:",
        min = 2, max = 20, step = 0.1, value = 5),
        
      sliderInput("Shape1", 
        label = "Prior shape1:",
        min = 0, max = 10, step = 0.1, value = 0.8),

      sliderInput("Shape2", 
        label = "Prior shape2:",
        min = 0, max = 10, step = 0.1, value = 0.8),
      hr(),  
      sliderInput("data", 
        label = "Data: number of successes and trials:",
        min = 0, max = 10, step = 1, value = c(0, 0)),
        
      checkboxInput("showPost",
        label = "Display the posterior",
        value = FALSE),

     sliderInput("maxValue", 
        label = "Change upper limits for trials and concentration",
        min = 10, max = 500, step = 10, value = 10),
        
        p(em("Code by Mike Meredith"), align="center")
    ),
    
    mainPanel(

      h3("Display beta distributions, binomial likelihoods, and beta posteriors"),
      p("Choose which parameters to use, then control the curve with the mode and concentration sliders, or the shape sliders."),
    
      plotOutput("distPlot"),
      tableOutput("results"),
      
      h3("Notes"),
      p("The mode+concentration parameterisation cannot display U-shaped forms of the beta distribution; use the shape parameters for that."),
      p("The beta distribution has two shape parameters, usually denoted by 'alpha' and 'beta', but here we use the R argument names, 'shape1' and 'shape2'."),
      p("Both shape parameters must be > 0; when the slider is at 0, the value used is 0.0001"),
      p("The data consist of the number of trials and the number of successes, eg, 3 heads in 5 coin tosses, or a species observed on 3 visits to a site out of 5."),
      p("The likelihood curve is scaled so that the area under the curve = 1. This is done by multiplying the output from dbinom by the number of trials + 1."),
      br()
    )
  )
))
