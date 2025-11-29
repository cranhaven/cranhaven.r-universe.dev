## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# library(GitAI)

## -----------------------------------------------------------------------------
# my_project <- initialize_project("gitai-demo") |>
#   set_database(
#     provider  = "Pinecone",
#     index     = "gitai"
#   ) |>
#   set_llm(seed = 1014, api_args = list(temperature = 0))

## -----------------------------------------------------------------------------
# my_project <- my_project |>
#   set_github_repos(
#     repos = c(
#       "r-world-devs/GitStats",
#       "r-world-devs/GitAI",
#       "r-world-devs/cohortBuilder",
#       "r-world-devs/shinyCohortBuilder",
#       "r-world-devs/shinyQueryBuilder",
#       "r-world-devs/queryBuilder",
#       "r-world-devs/shinyGizmo",
#       "r-world-devs/shinyTimelines",
#       "openpharma/DataFakeR"
#     ),
#     orgs = c(
#       "insightsengineering",
#       "openpharma",
#       "pharmaverse",
#       "tidymodels",
#       "r-lib",
#       "rstudio",
#       "tidyverse"
#     )
#   ) |>
#   add_files(c(
#     "DESCRIPTION",
#     "*.md",
#     "*.Rmd"
#   ))

## -----------------------------------------------------------------------------
# my_project <- my_project |>
#   set_prompt(r"(
#     Write up to ten paragraphs of summary for a project based on given input.
#     Be precise and to the point in your answers.
#     Mention core functionality and all main features of the project.
#     If available, mention brifly the technology used in the project
#     (like R, Python, etc).
#     If available, mention brifly if a project is an R package, shiny app,
#     or other type of tool.
#   )")

## -----------------------------------------------------------------------------
# ellmer:::chat_perform_value
# custom_function <- function(provider, req) {
# 
#   req <- req |>
#     httr2::req_timeout(60 * 10) |>
#     httr2::req_retry(
#       max_tries = 10,
#       retry_on_failure = TRUE
#     )
# 
#   req |>
#     httr2::req_perform() |>
#     httr2::resp_body_json()
# }
# unlockBinding("chat_perform_value", asNamespace("ellmer"))
# assign("chat_perform_value", custom_function, envir = asNamespace("ellmer"))
# lockBinding("chat_perform_value", asNamespace("ellmer"))

## -----------------------------------------------------------------------------
# results <- process_repos(my_project)

## -----------------------------------------------------------------------------
# my_project <- initialize_project("gitai-demo") |>
#   set_database(
#     provider  = "Pinecone",
#     index     = "gitai"
#   ) |>
#   set_llm(seed = 1014, api_args = list(temperature = 0))
# 
# my_project |>
#   find_records(
#     "How can I create fake data based on SQL tables?",
#     top_k = 1
#   ) |>
#   purrr::walk(~ cat(
#     .x$metadata$text |>
#       stringr::str_sub(end = 1000) |>
#       stringr::str_wrap(width = 80) |>
#       paste0("...")
#   ))
# #> DataFakeR is an R package designed to generate fake data for relational
# #> databases while preserving the structure and constraints of the original data.
# #> The package is particularly useful for developers and data scientists who need
# #> to create realistic datasets for testing, development, or demonstration purposes
# #> without exposing sensitive information. The current version, 0.1.3, includes
# #> several enhancements and bug fixes, making it a robust tool for data simulation.
# #> The core functionality of DataFakeR revolves around its ability to read a
# #> schema description in YAML format, which defines the structure of the database
# #> tables, including columns, data types, constraints, and relationships. Users can
# #> source this schema from an existing database or define it manually. The package
# #> supports various data types, including character, numeric, integer, logical,
# #> and date, allowing for a wide range of data generation scenarios. One of the
# #> standout features of DataFakeR is its support for determinist...

## -----------------------------------------------------------------------------
# library(shiny)
# library(shinychat)
# library(GitAI)
# 
# gitai <- initialize_project("gitai-demo") |>
#   set_database(index = "gitai") |>
#   set_llm(seed = 1014, api_args = list(temperature = 0)) |>
#   set_prompt(r"(
#     As a helpful assistant, answer user question
#     using only the provided input.
#     Use only provided with the query known input
#     that is most relevent to the user's query.
#     Do not use any other information
#     apart from the input provided with the query.
#     Be concise but provide all important information.
#     Also awalys provide link to mentioned git repositories
#     with visible full URL for example: https://github.com/some_repository.
#     Do not mask it with any other text.
#     )")
# 
# ui <- bslib::page_fluid(
#   bslib::layout_sidebar(
#     sidebar = shiny::sliderInput(
#       "top_k",
#       "Use top K results",
#       step = 1,
#       min = 1,
#       max = 10,
#       value = 5
#     ),
# 
#     chat_ui("chat")
#   )
# )
# 
# server <- function(input, output, session) {
# 
#   user_chatbot <- gitai$llm$clone()
# 
#   shiny::observeEvent(input$chat_user_input, {
# 
#     query <- input$chat_user_input
# 
#     stream <- user_chatbot$stream_async(
#       paste(
#         "User query:", query, "\n\n",
#         "Known input provided for the answer:\n\n",
#         gitai$db$find_records(query = query, top_k = input$top_k)
#       )
#     )
#     chat_append("chat", stream)
#   })
# }
# 
# shinyApp(ui, server)

