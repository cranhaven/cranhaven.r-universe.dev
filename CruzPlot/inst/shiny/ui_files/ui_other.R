# UI code for CruzPlot's disply tabs

# Display color/Format options
ui.dispColor <- function() {
  tabItem(
    tabName = "dispColor",
    fluidRow(
      box(
        title = "Color/Format Options", status = "primary", solidHeader = TRUE,  width = 12,
        plotOutput("plotDisplay")
      ),
      column(12, actionButton("display_redraw", "Redraw display"))
    )
  )
}

# Display species codes and names
ui.dispSp <- function() {
  tabItem(
    tabName = "dispSp",
    fluidRow(
      box(
        title = "Species Information", status = "primary", solidHeader = TRUE,  width = 12,
        radioButtons("sp_type", "Select species codes to display",
                     choices = list("Mammals" = 1, "Turtles" = 2, "All" = 3)),
        textOutput("sp_message"),
        conditionalPanel(condition = "input.sp_type == 1", dataTableOutput("sp1")),
        conditionalPanel(condition = "input.sp_type == 2", dataTableOutput("sp2")),
        conditionalPanel(condition = "input.sp_type == 3", dataTableOutput("sp3"))
      )
    )
  )
}

# Display CruzPlot manual
ui.dispManual <- function() {
  tabItem(
    tabName = "dispManual",
    tags$h5("The height of the manual window is controlled by the 'Map height' input in the sidebar. ",
            "If the manual opens in a separate window, you can click 'Open in Browser' to display manual in-app"),
    uiOutput("manual_out")
  )
}
