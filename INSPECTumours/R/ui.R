#' @importFrom  shiny tagList tags navbarPage div img tabPanel includeMarkdown
#' withMathJax navbarMenu
#' @importFrom knitr knit
#' @importFrom waiter autoWaiter
ui <- function() {
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),

    autoWaiter(),

    navbarPage(
      title = div("INSPECT",
                  div(
                    id = "AZlogo",
                    img(src = "www/AZ_RGB_H_COL.PNG", height = "30px")
                  )),
      id = "main_panel",
      tabPanel("Load data", mod_load_ui("load_data"), value = "load"),
      navbarMenu(
        "Analysis",
        tabPanel(
          "Tumour classification",
          mod_classification_ui("classification"),
          value = "analysis"
        ),
        tabPanel(
          "Statistical analysis - drug efficacy test (optional)",
          mod_stat_analysis_ui("stat_analysis")
        )
      ),
      tabPanel("Report",
               mod_report_ui("report")),
      tabPanel("Info",
               withMathJax(includeMarkdown(
                 system.file("info.md", package = "INSPECTumours")
               )))

    )

  )
}
