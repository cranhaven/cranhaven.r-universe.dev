prepare_itemrest_summary <- function(summary_table, language = "en") {
  summary_table <- as.data.frame(summary_table, check.names = FALSE)
  duplicate_columns <- c("REMOVED_THIS_STEP", "REMOVED_ITEMS")
  if (all(duplicate_columns %in% names(summary_table))) {
    first_values <- as.character(summary_table[[duplicate_columns[[1]]]])
    second_values <- as.character(summary_table[[duplicate_columns[[2]]]])
    if (identical(first_values, second_values)) {
      summary_table[[duplicate_columns[[1]]]] <- NULL
    }
  }

  if (identical(fafa_language(language), "tr")) {
    labels <- c(
      ITERATION = "ADIM",
      REMOVED_THIS_STEP = "BU_ADIMDA_\u00c7IKARILANLAR",
      REMOVED_ITEMS = "\u00c7IKARILAN_MADDELER",
      N_REMOVED = "\u00c7IKARILAN_SAYISI",
      REMAINING_ITEMS = "KALAN_MADDELER",
      N_REMAINING = "KALAN_SAYISI",
      TOTAL_EXPLAINED_VAR = "TOPLAM_A\u00c7IKLANAN_VARYANS",
      FACTOR_LOADING_RANGE = "FAKT\u00d6R_Y\u00dcK\u00dc_ARALI\u011eI",
      CRONBACHS_ALPHA = "CRONBACH_ALFA"
    )
    matched <- match(names(summary_table), names(labels))
    names(summary_table)[!is.na(matched)] <- unname(labels[matched[!is.na(matched)]])
  }

  summary_table
}

#' ItemRest Analysis Server Module
#'
#' @param id Module namespace ID.
#' @param data Reactive containing the input dataset.
#' @param error_recorder Optional function used for anonymous diagnostics.
#' @param language Optional reactive interface language.
#' @import shiny
#' @importFrom stats na.omit
#' @importFrom utils capture.output
#' @export
mod_itemrest_server <- function(id, data, error_recorder = NULL,
                                language = NULL) {
  moduleServer(id, function(input, output, session) {

    # Store results: 'res' is the object, 'console_output' is the text printed to console
    analysis_results <- reactiveVal(list(res = NULL, console_output = NULL))
    local_text <- function(english, turkish) {
      fafa_text(language, english, turkish)
    }

    observeEvent(input$run_itemrest, {
      req(data())

      # Check if ItemRest package is available
      if (!requireNamespace("ItemRest", quietly = TRUE)) {
        showNotification(
          local_text(
            "Package 'ItemRest' is required but not installed.",
            "'ItemRest' paketinin kurulmas\u0131 gerekir."
          ),
          type = "error"
        )
        return()
      }

      # Data preparation
      df <- data()
      if(!all(sapply(df, is.numeric))) {
        showNotification(
          local_text(
            "ItemRest requires all variables to be numeric.",
            "Madde \u00e7\u0131karma analizi i\u00e7in t\u00fcm de\u011fi\u015fkenler say\u0131sal olmal\u0131d\u0131r."
          ),
          type = "error"
        )
        return()
      }
      df_clean <- stats::na.omit(df)

      # Prepare arguments
      num_factors_arg <- if(is.na(input$n_factors)) NULL else input$n_factors

      showNotification(
        local_text(
          "Running ItemRest automation strategies...",
          "Otomatik madde \u00e7\u0131karma stratejileri \u00e7al\u0131\u015ft\u0131r\u0131l\u0131yor..."
        ),
        type = "message", duration = NULL, id = "ir_progress"
      )

      tryCatch({
        # capture.output ile konsola basılan her şeyi yakalıyoruz
        captured_txt <- utils::capture.output({
          res <- ItemRest::itemrest(
            data = df_clean,
            cor_method = input$cor_method,
            n_factors = num_factors_arg,
            extract = input$extraction_method,
            rotate = input$rotation_method
          )
        })

        # Hem nesneyi hem de yakalanan metni sakla
        analysis_results(list(res = res, console_output = captured_txt))

        removeNotification("ir_progress")
        showNotification(
          local_text("Analysis Complete!", "Analiz tamamland\u0131!"),
          type = "message"
        )

      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("Item dropout", "Analysis error")
        removeNotification("ir_progress")
        showNotification(
          paste(local_text("Analysis Failed:", "Analiz ba\u015far\u0131s\u0131z:"), e$message),
          type = "error", duration = 10
        )
      })
    })

    # Output 1: Optimal Strategy Text
    output$optimal_strategy_text <- renderPrint({
      req(analysis_results())
      out_data <- analysis_results()
      res <- out_data$res
      txt <- out_data$console_output

      if (!is.null(res) && "optimal_strategy" %in% names(res) && !is.null(res$optimal_strategy)) {
        print(res$optimal_strategy)
      }
      else if (!is.null(txt) && length(txt) > 0) {
        cat(paste(txt, collapse = "\n"))
      }
      else {
        cat(local_text(
          "Optimal strategy details not found in result object or console output.",
          "En uygun stratejinin ayr\u0131nt\u0131lar\u0131 sonu\u00e7 nesnesinde veya konsol \u00e7\u0131kt\u0131s\u0131nda bulunamad\u0131."
        ))
      }
    })

    # Output 2: Removal Summary Table
    output$removal_summary_table <- renderTable({
      req(analysis_results())
      out_data <- analysis_results()
      res <- out_data$res

      if (!is.null(res) && "removal_summary" %in% names(res) && !is.null(res$removal_summary)) {
        return(prepare_itemrest_summary(res$removal_summary, fafa_language(language)))
      } else {
        message_text <- local_text(
          "Summary table not returned. Please check the text output above.",
          "\u00d6zet tablo olu\u015fturulamad\u0131. L\u00fctfen yukar\u0131daki metin \u00e7\u0131kt\u0131s\u0131n\u0131 kontrol edin."
        )
        column_name <- local_text("Message", "\u0130leti")
        result <- data.frame(message_text, check.names = FALSE)
        names(result) <- column_name
        return(result)
      }
    }, striped = TRUE, hover = TRUE, digits = 3)

  })
}
