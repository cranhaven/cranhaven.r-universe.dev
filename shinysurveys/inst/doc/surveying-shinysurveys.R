## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(tibble)

## ----setup--------------------------------------------------------------------
library(shinysurveys)

## ---- echo = FALSE------------------------------------------------------------
shinysurveys::teaching_r_questions[1:10,]

## ----display unique questions, echo = FALSE-----------------------------------
unique(shinysurveys::teaching_r_questions$question)

## ----what is your age preview, echo = FALSE-----------------------------------
subset(teaching_r_questions, input_id == "age")

## ----what is your gender mc, echo = FALSE-------------------------------------
subset(teaching_r_questions, input_id == "gender" | input_id == "self_describe_gender")

## ----what is your gender text, echo = FALSE-----------------------------------
subset(teaching_r_questions, input_id == "self_describe_gender") 

## ----show first language options, echo = FALSE--------------------------------
subset(teaching_r_questions, question == "What was your first language?")

## ----learned to program in R, echo = FALSE------------------------------------
subset(teaching_r_questions, input_id == "learned_r")

## ----setup-matrix-question, echo = FALSE--------------------------------------

matrix_question <- data.frame(
   question = c(rep("I love sushi.", 3), rep("I love chocolate.",3)),
   option = c(rep(c("Disagree", "Neutral", "Agree"), 2)),
   input_type = rep("matrix", 6),
   input_id = rep("matId", 6),
   dependence = NA,
   dependence_value = NA,
   required = FALSE
 )

## ----display-matrix-question, echo = FALSE------------------------------------
matrix_question

## ----run survey, eval = FALSE-------------------------------------------------
#  library(shinysurveys)
#  ui <- shiny::fluidPage(
#    shinysurveys::surveyOutput(df = shinysurveys::teaching_r_questions,
#                               survey_title = "A minimal title",
#                               survey_description = "A minimal description")
#  )
#  
#  server <- function(input, output, session) {
#    shinysurveys::renderSurvey()
#  }
#  
#  shiny::shinyApp(ui = ui, server = server)

## ----what is your gender - question dependencies, echo = FALSE----------------
subset(teaching_r_questions, input_id == "gender" | input_id == "self_describe_gender")

