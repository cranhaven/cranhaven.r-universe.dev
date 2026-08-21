#' Project and report tools
#'
#' @param id Module namespace ID.
#' @noRd
project_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Project Settings", class = "bg-primary text-white", bs_icon("gear")),
        card_body(
          selectInput(
            ns("app_language"), "Language:",
            choices = c("English" = "en", "Turkish" = "tr"),
            selected = "en"
          ),
          checkboxInput(
            ns("include_project_data"),
            "Include data in project file",
            value = TRUE
          ),
          downloadButton(
            ns("download_project"), "Save FAfA Project",
            class = "btn-primary w-100 mb-3"
          ),
          fileInput(
            ns("project_file"), "Project file:",
            accept = c(".fafa", "application/octet-stream")
          ),
          actionButton(
            ns("load_project"), "Load Project",
            icon = icon("folder"), class = "btn-success w-100"
          ),
          textOutput(ns("project_status"))
        )
      ),
      tagList(
        card(
          card_header("Reproducible Reports", bs_icon("file-text")),
          card_body(
            div(
              class = "d-flex flex-wrap gap-2",
              downloadButton(ns("download_r_script"), "Download R Script"),
              downloadButton(ns("download_html_report"), "Download HTML Report"),
              downloadButton(ns("download_pdf_report"), "Download PDF Report"),
              downloadButton(ns("download_apa7_report"), "Download APA 7 Word", class = "btn-primary"),
              downloadButton(ns("download_diagnostic"), "Download Diagnostic Report")
            ),
            p(
              class = "text-muted small mt-3 mb-0",
              "Diagnostic reports contain package and system information only; uploaded data, variable names, model syntax, and file paths are excluded."
            )
          )
        ),
        card(
          card_header("Workflow Audit", bs_icon("table")),
          card_body(
            div(
              class = "d-flex gap-2 mb-2",
              downloadButton(ns("download_audit"), "Download CSV", class = "btn-sm"),
              actionButton(ns("clear_audit"), "Clear Audit", class = "btn-outline-secondary btn-sm")
            ),
            div(
              style = "max-height:430px; overflow-y:auto;",
              tableOutput(ns("audit_table"))
            )
          )
        )
      )
    )
  )
}
