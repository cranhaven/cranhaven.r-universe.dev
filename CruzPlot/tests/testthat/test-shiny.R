library(shiny)
library(shinydashboard)

test_that("dashboard works", {
  ui <- dashboardPage(
    dashboardHeader(title = "CruzPlot", titleWidth = "200"),

    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Create and Save Map", tabName = "createmap", icon = icon("th", lib = "font-awesome")),
        menuItem("Plot DAS Data", tabName = "DASplot", icon = icon("th")),
        menuItem("Plot Non-DAS Data", tabName = "nonDASplot", icon = icon("th")),
        menuItem(HTML(paste0("Color and Formatting", "<br/>", "Options")), tabName = "dispColor", icon = icon("th")),
        menuItem("Species Information", tabName = "dispSp", icon = icon("th")),
        menuItem("CruzPlot Manual", tabName = "dispManual", icon = icon("th")),
        tags$br(),
        fileInput("load_app_envir_file", "Load workspace"),
        column(
          width = 12,
          textOutput("load_app_text"),
          downloadButton("save_app_envir", "Save workspace", style = "color: black")
        ),
        tags$br(), tags$br(), tags$br(),
        numericInput("map_size", "Map height (pixels)", value = 600, min = 0, step = 100),
        tags$br(),
        actionButton("stop", "Close CruzPlot")
      ), width = "200"
    ),

    dashboardBody(
      helpText("hi")
    )
  )

  expect_identical(class(ui), "shiny.tag")
})


test_that("renderPlot args are as required", {
  d <- renderPlot({
    plot(1:10)
  }, height = 100, units = "px", res = 72)

  expect_true(inherits(d, c("shiny.render.function", "function")))
})
