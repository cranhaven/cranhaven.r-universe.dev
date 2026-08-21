#' Data Selection Server Logic
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @param language Reactive or character interface language (`"en"` or `"tr"`).
#' @export
data_selection_server <- function(id, data, language = NULL) {
  moduleServer(id, function(input, output, session) {

    local_text <- function(english, turkish) {
      fafa_text(language, english, turkish)
    }

    # The shared data changes only after confirmation or project loading.
    analyzed_data <- reactive({
      req(data())
      data()
    })

    # Preview Table
    output$mydatatable <- renderTable({
      validate(need(data(), local_text(
        "Please upload your dataset.",
        "L\u00fctfen veri dosyan\u0131z\u0131 y\u00fckleyin."
      )))
      utils::head(data(), 10)
    })

    # --- Value Boxes (bslib) ---

    output$n_var_box <- renderUI({
      req(analyzed_data())
      value_box(
        title = local_text("Variables", "De\u011fi\u015fkenler"), value = ncol(analyzed_data()),
        showcase = bsicons::bs_icon("columns"), theme = "primary"
      )
    })

    output$n_obs_box <- renderUI({
      req(analyzed_data())
      value_box(
        title = local_text("Sample Size", "\u00d6rneklem B\u00fcy\u00fckl\u00fc\u011f\u00fc"), value = nrow(analyzed_data()),
        showcase = bsicons::bs_icon("people"), theme = "success"
      )
    })

    get_numeric_data <- reactive({
      req(analyzed_data())
      df <- analyzed_data()
      df[, sapply(df, is.numeric), drop = FALSE]
    })

    output$min_val_box <- renderUI({
      req(get_numeric_data())
      val <- tryCatch(min(get_numeric_data(), na.rm = TRUE), error = function(e) NA)
      val <- if(is.infinite(val)) "NA" else round(val, 2)
      value_box(
        title = local_text("Min Value", "En K\u00fc\u00e7\u00fck De\u011fer"), value = val,
        showcase = bsicons::bs_icon("arrow-down"), theme = "info"
      )
    })

    output$max_val_box <- renderUI({
      req(get_numeric_data())
      val <- tryCatch(max(get_numeric_data(), na.rm = TRUE), error = function(e) NA)
      val <- if(is.infinite(val)) "NA" else round(val, 2)
      value_box(
        title = local_text("Max Value", "En B\u00fcy\u00fck De\u011fer"), value = val,
        showcase = bsicons::bs_icon("arrow-up"), theme = "warning"
      )
    })

    output$cat_range_box <- renderUI({
      req(get_numeric_data())
      num_data <- get_numeric_data()
      res <- "N/A"
      if(ncol(num_data) > 0) {
        min_v <- min(num_data, na.rm=TRUE); max_v <- max(num_data, na.rm=TRUE)
        if(!is.infinite(min_v)) res <- paste0(min_v, " - ", max_v)
      }
      value_box(
        title = local_text("Range", "Aral\u0131k"), value = res,
        showcase = bsicons::bs_icon("rulers"), theme = "secondary"
      )
    })
  })
}
