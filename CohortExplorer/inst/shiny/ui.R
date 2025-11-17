shinyUI(fluidPage(fluidRow(titlePanel(
  shiny::textOutput("cohortName")
),
column(
  2,
  shinyWidgets::pickerInput(
    inputId = "selectedDatabaseId",
    label = "Database id",
    choices = listOfFiles$databaseId %>% unique() %>% sort(),
    selected = initialSelectedDatabaseId,
    multiple = FALSE
  ),
  shinyWidgets::pickerInput(
    inputId = "selectedCohortId",
    label = "Cohort id",
    choices = listOfFiles$cohortId %>% unique() %>% sort(),
    selected = initialSelectedCohortId,
    multiple = FALSE
  ),
  tags$label(class = "control-label", `for` = "subjectId", "Subject ID"),
  textOutput("subjectId"),
  tags$label(class = "control-label", `for` = "age", "Age at First Index Date"),
  textOutput("age"),
  tags$label(class = "control-label", `for` = "gender", "Gender"),
  textOutput("gender"),
  actionButton("previousButton", "<"),
  actionButton("nextButton", ">"),
  checkboxGroupInput(
    "cdmTables",
    label = "Domains",
    choices = camelCaseToTitleCase(tables),
    selected = camelCaseToTitleCase(
      c(
        "visitOccurrence",
        "conditionOccurrence",
        "drugEra",
        "procedureOccurrence",
        "measurement",
        "observation"
      )
    )
  ),
  textAreaInput(
    "filterRegex",
    div(
      "Concept Name Filter (keep)",
      actionLink("filterInfo", "", icon = icon("info-circle"))
    ),
    placeholder = "regex",
    value = regexFilterInfo
  ),
  textAreaInput(
    "deleteRegex",
    div(
      "Concept Name Filter (remove)",
      actionLink("filterInfo", "", icon = icon("info-circle"))
    ),
    placeholder = "regex"
  ),
  numericInput(
    inputId = "daysFromCohortStart",
    label = "Absolute days from Start",
    min = 0,
    max = 9999,
    step = 1,
    value = 9999
  ),
  numericInput(
    inputId = "daysToCohortStart",
    label = "Absolute days to Start",
    min = 0,
    max = 9999,
    step = 1,
    value = 9999
  ),
  checkboxInput("showPlot", "Show Plot", value = TRUE),
  checkboxInput("showTable", "Show Table", value = TRUE),
  checkboxInput("shiftDates", "Shift Dates", value = FALSE),
  checkboxInput("showSourceCode", "Show source code", value = FALSE),
),
column(
  10,
  conditionalPanel(
    condition = "input.showTable==1",
    conditionalPanel(
      condition = "input.showPlot==1",
      plotly::plotlyOutput("plotSmall", height = "400px")
    ),
    shinycssloaders::withSpinner(reactable::reactableOutput(outputId = "eventTable")),
    csvDownloadButton(ns = "eventTable", "eventTable")
  ),
  conditionalPanel(condition = "input.showTable==0 & input.showPlot==1",
                   plotly::plotlyOutput("plotBig", height = "800px"),)
)
)))
