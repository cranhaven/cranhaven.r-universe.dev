

ui_eto_load <- function(id){
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
        tags$div(
          class = "table",
          reactableOutput(NS(id,"table_evapo"))
        )#id
      )#column
    )
  )
}


ui_eto_calc <- function(id){
  tagList(
    fluidRow(
      column(
        3,
        panel_ui_calc(id)
      ),#column
      column(
        9,
        tags$div(class="downprocess",
                 actionButton(NS(id,"DownETO"),
                              class = "classdown",
                              "Download CSV",
                              icon = icon("download"))),
        tags$div(
          class="table",
          reactableOutput(NS(id,"table_calc_eto"))
        )
      )#column
    )
  )#taglist
}

