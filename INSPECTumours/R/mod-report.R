#' mod_report UI Function
#' @param id
#'
#' @noRd
#'
#' @importFrom shiny NS tagList numericInput selectInput checkboxInput
#' actionButton div tags span icon fluidPage textOutput br downloadButton
mod_report_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h3("Generate a report"),
    textOutput(ns("header_text")),
    br(),
    downloadButton(ns("report"), "Save")
  )

}

#' mod_report Server Function
#' @param id,input,output,session internal parameters for {shiny}
#' @param r reactiveValues with data
#'
#' @noRd
#'
#' @importFrom shiny showNotification removeNotification observe
#' renderText downloadHandler
#' @importFrom pander pander
#' @importFrom utils write.table zip
#' @importFrom rmarkdown render
#' @noRd
mod_report_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # enable/disable download button
    observe({
      toggleState(id = "report",
                  condition = !is.null(r$classify_final_filtered))
    })

    output$header_text <- renderText({
      if (is.null(r$classify_final_filtered)) {
        "Will be available after classification"
      }
    })


    w <- set_waiter("Rendering report")

    output$report <- downloadHandler(

      filename = function() {
        paste0("inspect_report_",
               format(Sys.time(), "%Y_%m_%d__%H_%M"),
               ".zip")
      },
      content = function(file) {
        # Use a temporary directory for report files, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        old_dir <- getwd()
        on.exit(setwd(old_dir), add = TRUE)

        tmp_dir <- tempdir()
        setwd(tmp_dir)

        # show loading spinner
        w$show()
        on.exit(w$hide(), add = TRUE)

        tryCatch({

          # classification table
          df_final <- "classification_final.csv"
          write.table(
            r$classify_final,
            file = df_final,
            sep = ",",
            dec = ".",
            row.names = FALSE
          )

          # html report
          temp_report <- file.path(tmp_dir, "report.rmd")
          file.copy(system.file("report.rmd",
                                package = "INSPECTumours"),
                    temp_report,
                    overwrite = TRUE)

          html_report <- "report.html"
          render(
            temp_report,
            output_format = "html_document",
            output_file = html_report,
            envir = new.env(parent = globalenv()),
            params = list(r = r)
          )

          #create the zip file
          zip(zipfile = file,
              files = c(df_final,
                        html_report))

        },

        error = function(e) {
          print(e)
          shinyalert("Error while generating a report", type = "error")
        }

        )
      }
    )

  })
}
