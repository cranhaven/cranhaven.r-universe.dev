## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(shinysurveys)

## ----full-simple-app, eval = FALSE--------------------------------------------
#  
#  # Load packages
#  library(shiny)
#  library(shinysurveys)
#  
#  # Define questions in the format of a shinysurvey
#  survey_questions <- data.frame(
#    question = c("What is your favorite food?",
#                 "What's your name?"),
#    option = NA,
#    input_type = "text",
#    input_id = c("favorite_food", "name"),
#    dependence = NA,
#    dependence_value = NA,
#    required = c(TRUE, FALSE)
#  )
#  
#  # Define shiny UI
#  ui <- fluidPage(
#    surveyOutput(survey_questions,
#                 survey_title = "Hello, World!",
#                 survey_description = "A demo survey")
#  )
#  
#  # Define shiny server
#  server <- function(input, output, session) {
#    renderSurvey()
#  
#    observeEvent(input$submit, {
#      response_data <- getSurveyData()
#      print(response_data)
#    })
#  
#  }
#  
#  # Run the shiny application
#  shinyApp(ui, server)

## ----complex-example, eval = FALSE--------------------------------------------
#  # Load packages
#  library(shiny)
#  library(shinysurveys)
#  
#  # Register a "check" input type
#  extendInputType("check", {
#    shiny::checkboxGroupInput(
#      inputId = surveyID(),
#      label = surveyLabel(),
#      choices = surveyOptions(),
#    )
#  })
#  
#  # Define question in the format of a shinysurvey
#  ice_cream_question <- data.frame(
#    question = "Please indicate which of the following are your top three favorite ice cream flavors.",
#    option = c("Chocolate", "Vanilla", "Strawberry",
#               "Mint Chocolate Chip", "Rocky Road", "Cookie Batter",
#               "Hazelnut", "Cookies N' Cream", "Pistachio"),
#    input_type = "check",
#    input_id = "favorite_ice_cream",
#    dependence = NA,
#    dependence_value = NA,
#    required = TRUE
#  )
#  
#  # Define shiny UI
#  ui <- fluidPage(
#    surveyOutput(ice_cream_question,
#                 survey_title = "Hello, World!")
#  )
#  
#  # Define shiny server
#  server <- function(input, output, session) {
#    renderSurvey()
#  
#    observeEvent(input$submit, {
#      response_data <- getSurveyData()
#      print(response_data)
#    })
#  
#  }
#  
#  # Run the shiny application
#  shinyApp(ui, server)
#  

## ----dependency-survey, eval = FALSE------------------------------------------
#  
#  # Load packages
#  library(shiny)
#  library(shinysurveys)
#  
#  # Define questions in the format of a shinysurvey
#  dep_questions <- tail(teaching_r_questions, 12)
#  
#  # Define shiny UI
#  ui <- fluidPage(
#    surveyOutput(dep_questions,
#                 survey_title = "Hello, World!")
#  )
#  
#  # Define shiny server
#  server <- function(input, output, session) {
#    renderSurvey()
#  
#    observeEvent(input$submit, {
#      response_data <- getSurveyData()
#      print(response_data)
#    })
#  
#  }
#  
#  # Run the shiny application
#  shinyApp(ui, server)

