ui_proc <- function(id){
  tagList(
    navbarPage(
      title = "Results",
      tabPanel(
        title = "Data Frames",
        fluidRow(
          useShinyjs(),
          ui_process("processtabs")
        )#fluidRow
      ),#tabpanel
      tabPanel(
        title = "Plots",
        fluidRow(
          ui_graficos("ui_grafic")
        )#fluidRow
      )#tabpanel
    )#navbar
  )
}


ui_process <- function(id){
  tagList(
    useShinyjs(),
    column(
      12,
      tabsetPanel(
        tabPanel(
          'Daily',
          fluidRow(
            column(
              3,
              tags$div(
                class = "Paineis",
                tags$h4(class = "h4_title",
                        "Grouping"),
                panel_daily(id)
              )#div
            ),#column
            column(
              9,
              tags$div(class="downprocess",
                       actionButton(NS(id,"DownBD"),
                                    class = "classdown",
                                    "Download CSV",
                                    icon = icon("download"))),
              tags$div(
                class = "table",
                reactableOutput(
                  NS(id,"table_diario")
                )
              )#div
            )#column
          )#fluidrow
        ),#tabpanel
        tabPanel(
          'Monthly',
          fluidRow(
            column(
              3,
              tags$div(
                class = "Paineis",
                tags$h4(class = "h4_title",
                        "Grouping"),
                panel_monthly(id)
              )#div
            ),#column
            column(
              9,
              tags$div(class="downprocess",
                       actionButton(NS(id,"DownBM"),
                                    class = "classdown",
                                    "Download CSV",
                                    icon = icon("download"))),
              tags$div(
                class = "table",
                reactableOutput(
                  NS(id, "table_mensal")
                )
              )#div
            )#column
          )#fluidrow
        ),#tabpanel
        tabPanel(
          'Yearly',
          fluidRow(
            column(
              3,
              tags$div(
                class = "Paineis",
                tags$h4(class = "h4_title",
                        "Grouping"),
                panel_yearly(id)
              )#div
            ),#column
            column(
              9,
              tags$div(class="downprocess",
                       actionButton(NS(id,"DownBA"),
                                    class = "classdown",
                                    "Download CSV",
                                    icon = icon("download"))),
              tags$div(
                class = "table",
                reactableOutput(
                  NS(id, "table_anual")
                )
              )#div
            )#column
          )#fluidrow
        ),#tabpanel
      )#tabsetpanel
    )#column
  )
}

colPick <- function(inputId,label,selected = "#112446"){
  colorPickr(
    inputId = inputId,
    label = label,
    selected = selected,
    theme = "monolith",
    opacity = TRUE,
    update = "changestop",
    interaction = list(
      clear = FALSE,
      save = FALSE
    ))
}




ui_graficos <- function(id){
  tagList(
    column(
      3,
      tags$div(
        class = "Paineis",
        tags$h4(class = "h4_title",
                "Settings"),
        painel_selection(id),
        painel_labels(id),
        painel_config_chart(id)
      )#div
    ),#column,
    column(
      9,
      tags$div(
        class = "plot",
        plotlyOutput(
          NS(id,"plotlygraf")
        )
      )#div
    )#column
  )#taglist
}

