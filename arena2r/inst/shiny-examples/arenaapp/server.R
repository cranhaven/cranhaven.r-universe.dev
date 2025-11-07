
library(arena2r)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinythemes)

# Define server logic
server <- function(input, output, session) {
  # Reactive Functions:
  # Simulation Results
  sim_results = reactive({

    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    } else {

      get_simulation_results(source = inFile, source_type = "shinyInput")

    }

  })

  # Head of Simulation Results
  sim_head_results = reactive({

    results = sim_results()

    if(is.null(results)){
      return(NULL)
    } else {

      head(results)

    }

  })

  # Statistics Summary
  sim_summary = reactive({

    results = sim_results()

    if(is.null(results)){
      return(NULL)
    } else {

      suppressWarnings(get_statistics_summary(sim_results = results))

    }

  })

  # Statists List
  statistics_list = reactive({
    results = sim_results()

    if(is.null(results)){
      return(NULL)
    } else {

      unique(results$Statistic)

    }

  })

  # Atualizar Selectize Inputs:
  observe({
    variable_list = statistics_list()
    updateSelectizeInput(session, 'VariavelXScatter', choices = variable_list, server = TRUE)
    updateSelectizeInput(session, 'VariavelYScatter', choices = variable_list, server = TRUE)
    updateSelectizeInput(session, 'variavelConfInt', choices = variable_list, server = TRUE)

  })

#  Outputs - Plots
  output$confint_plot = renderPlot({

    if(is.null(statistics_list())){
      return(NULL)
    } else {
      plot_confint(sim_results = sim_results(),
                   response_variable = input$variavelConfInt)
    }


  })

  output$box_plot = renderPlot({

    if(is.null(statistics_list())){
      return(NULL)
    } else {
      plot_box(sim_results = sim_results(),
                   response_variable = input$variavelConfInt)
    }


  })

  output$scatter_plot <- renderPlot({

    if(is.null(statistics_list())){
      return(NULL)
    } else {
      plot_scatter(sim_results = sim_results(),
                   x_variable = input$VariavelXScatter,
                   y_variable = input$VariavelYScatter)
    }


  })

#  Outputs - Rendered Tables
  output$summary <- renderDataTable({
    sim_summary()
  })

  output$head_results <- renderDataTable({
    sim_head_results()
  })

  #  Outputs - Summary Stats
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("statistics_summary", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sim_summary(), file, row.names = FALSE)
    }
  )


}
