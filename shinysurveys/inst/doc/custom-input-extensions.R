## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(shinysurveys)
library(tibble)

## ----define-question----------------------------------------------------------
# Define a question as normal with the `input_type` set to "slider", which is not natively supported by {shinysurveys}.

slider_question <- data.frame(
  question = "On a scale from 1-10, how much do you love sushi?",
  option = NA,
  input_type = "slider",
  input_id = "sushi_scale",
  dependence = NA,
  dependence_value = NA,
  required = TRUE
  )

## ----echo = FALSE-------------------------------------------------------------
slider_question

## ----demo-slider-without-extension, error = TRUE------------------------------
library(shiny)
library(shinysurveys)

ui <- fluidPage(
  surveyOutput(df = slider_question,
               survey_title = "Testing the Slider Input")
)


## ----extend-input-type-slider-------------------------------------------------

# Register a slider input to {shinysurveys} with a custom minimum and maximum value.

extendInputType(input_type = "slider", {
  shiny::sliderInput(
    inputId = surveyID(),
    label = surveyLabel(),
    min = 1,
    max = 10,
    value = 5
    ) 
})


## ----demo-slider-input--------------------------------------------------------

# By defining the input type above, this works! Yay!
ui <- fluidPage(
  surveyOutput(df = slider_question,
               survey_title = "Testing the Slider Input")
)


## ----define-date-question-----------------------------------------------------

# Define a question as normal with the `input_type` set to "date", which is not natively supported by {shinysurveys}.
date_question <- data.frame(
  question = "When do you graduate?",
  option = NA,
  input_type = "date",
  input_id = "grad_date",
  dependence = NA,
  dependence_value = NA,
  required = FALSE
  )

## ----echo = FALSE-------------------------------------------------------------
date_question

## ----demo-date-without-extension, error = TRUE--------------------------------
library(shiny)
library(shinysurveys)

ui <- fluidPage(
  surveyOutput(df = date_question,
               survey_title = "Testing the Date Input")
)


## ----extend-input-type-date---------------------------------------------------

# Register a date input to {shinysurveys}, limiting possible dates to a twenty-day period.

extendInputType("date", {
  shiny::dateInput(
    inputId = surveyID(),
    value = Sys.Date(),
    label = surveyLabel(),
    min = Sys.Date()-10,
    max = Sys.Date()+10
  )
})


## ----demo-date-input----------------------------------------------------------

# By defining the input type above, this works! Yay!
ui <- fluidPage(
  surveyOutput(df = date_question,
               survey_title = "Testing the Date Input")
)


