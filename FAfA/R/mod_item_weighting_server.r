#' Item Weighting Server Module
#' @param id Module namespace ID.
#' @param data Input data (reactive)
#' @param error_recorder Optional function used for anonymous diagnostics.
#' @export
item_weighting_server <- function(id, data, error_recorder = NULL) {
  moduleServer(id, function(input, output, session) {
    weighted_data_rv <- reactiveVal()

    observeEvent(input$calculate_weighted_scores_button, {
      req(data())
      # utils.R içindeki fonksiyonu kontrol et
      if (!exists("item_weighting")) {
        showNotification("Function 'item_weighting' missing.", type="error"); return()
      }

      tryCatch({
        res <- item_weighting(data())
        weighted_data_rv(res)
        showNotification("Calculated!", type="message")
      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("Item weighting", "Analysis error")
        showNotification(e$message, type="error")
      })
    })

    output$weighted_scores_table_output <- renderTable({
      req(weighted_data_rv())
      utils::head(weighted_data_rv(), 10)
    }, striped=TRUE)

    output$download_weighted_data_button <- downloadHandler(
      filename = "weighted_scores.csv",
      content = function(file) {
        req(weighted_data_rv())
        write.csv(weighted_data_rv(), file, row.names = FALSE)
      }
    )
  })
}
