ui_load_file <- function(id){
  tagList(
    fluidRow(
      column(
        3,
        tags$div(
          class = "Paineis",
          tags$h4(class = "h4_title",
                  "Input data"),
          pan_load_file(id)
        )#div
      ),#column
      column(
        9,
        br(),
        tags$div(
          class = "table",
          reactableOutput(
            NS(id,"table_padrao")
          )#div
        )#tabpanel
      )#column
    )
  )
}
