ui_down_inmet <- function(id){
  tagList(
    fluidRow(
      column(
        12,
        tags$div(
          class = "button-container",
          tags$div(
            class = "button",
            selectInput(
              NS(id,"AnosDown"),
              label = "Select the download year:",
              choices = 2000:format(Sys.Date(),"%Y")
            )
          ),#div
          tags$div(
            class = "button",
            id = "action_button",
            downloadButton(
              NS(id,"Down_met"),
              label = "Download"
            )
          )#div
        )#div
      ),#colunm
      column(
        12,
        helpText(
          "Note: To activate the download, please select a station from the table below."
        )
      ),#colunm
      column(
        12,
        tags$div(
          class = "table",
          reactableOutput(
            NS(id,"table_est")
          )#div
        )#div
      )#column
    )#fluidrow
  )#taglist
}
