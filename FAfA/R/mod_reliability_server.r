#' Reliability Analysis Server Module
#' @param id Module namespace ID.
#' @param data Input data (reactive)
#' @param factor_dictionary Shared reactive value containing factor-to-item mappings.
#' @param error_recorder Optional function used for anonymous diagnostics.
#' @export
reliability_server <- function(id, data, factor_dictionary = NULL,
                               error_recorder = NULL) {
  moduleServer(id, function(input, output, session) {

    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
    if (is.null(factor_dictionary)) factor_dictionary <- reactiveVal(list())

    reliability_output_rv <- reactiveVal(NULL)
    reliability_cache <- new_session_cache()

    observeEvent(data(), {
      clear_session_cache(reliability_cache)
      reliability_output_rv(NULL)
    }, ignoreNULL = TRUE)

    observe({
      req(data())
      updateSelectizeInput(session, "reliability_item_select", choices = names(data()))
      updateSelectizeInput(session, "reliability_factor_select", choices = names(factor_dictionary()))
      used_items <- unique(unlist(factor_dictionary(), use.names = FALSE))
      available_items <- setdiff(names(data()), used_items)
      updateSelectizeInput(
        session, "reliability_factor_items",
        choices = available_items,
        selected = intersect(
          input$reliability_factor_items %||% character(0),
          available_items
        )
      )
    })

    observeEvent(factor_dictionary(), {
      dictionary <- factor_dictionary()
      selected_dimensions <- intersect(
        isolate(input$reliability_factor_select %||% character(0)),
        names(dictionary)
      )
      updateSelectizeInput(
        session, "reliability_factor_select",
        choices = names(dictionary),
        selected = selected_dimensions
      )
      if (length(dictionary)) {
        syntax <- paste(vapply(names(dictionary), function(f) {
          paste0(f, " =~ ", paste(dictionary[[f]], collapse = " + "))
        }, character(1)), collapse = "\n")
        updateTextAreaInput(session, "cfa_model_for_reliability_input", value = syntax)
      }
    }, ignoreInit = FALSE)

    observeEvent(input$add_reliability_dimension, {
      req(data())
      factor_name <- trimws(input$reliability_factor_name %||% "")
      factor_items <- input$reliability_factor_items %||% character(0)

      if (!nzchar(factor_name)) {
        showNotification("Enter a dimension name.", type = "warning")
        return()
      }
      if (!length(factor_items)) {
        showNotification("Select at least one item for the dimension.", type = "warning")
        return()
      }

      name_holder <- data.frame(value = 1)
      names(name_holder) <- factor_name
      safe_name <- names(normalize_variable_names(name_holder))[[1]]
      dictionary <- factor_dictionary()
      dictionary[[safe_name]] <- factor_items
      factor_dictionary(dictionary)

      selected_dimensions <- union(
        input$reliability_factor_select %||% character(0),
        safe_name
      )
      updateSelectizeInput(
        session, "reliability_factor_select",
        choices = names(dictionary),
        selected = selected_dimensions
      )
      session$onFlushed(function() {
        updateSelectizeInput(
          session, "reliability_factor_select",
          choices = names(dictionary),
          selected = selected_dimensions
        )
      }, once = TRUE)
      updateTextInput(session, "reliability_factor_name", value = "")
      updateSelectizeInput(
        session, "reliability_factor_items",
        selected = character(0)
      )
      showNotification(
        paste0("Dimension added: ", safe_name),
        type = "message"
      )
    })

    observeEvent(input$run_reliability_button, {
      req(data(), input$reliability_coefficient_select)

      raw_data <- data()

      selected_factors <- input$reliability_factor_select %||% character(0)
      dictionary <- factor_dictionary()
      selected_dictionary <- dictionary[selected_factors]

      if (input$reliability_coefficient_select == "s_alpha" &&
          !length(selected_dictionary)) {
        selected_dictionary <- parse_factor_dictionary(
          input$cfa_model_for_reliability_input %||% ""
        )
        selected_dictionary <- Filter(
          function(items) length(items) && all(items %in% names(raw_data)),
          selected_dictionary
        )
      }

      if (input$reliability_coefficient_select == "s_alpha") {
        strata_spec <- tryCatch(
          build_stratified_alpha_spec(selected_dictionary, names(raw_data)),
          error = function(e) {
            reliability_output_rv(paste("Error:", conditionMessage(e)))
            NULL
          }
        )
        if (is.null(strata_spec)) return()
        factor_items <- strata_spec$items
      } else {
        factor_items <- unique(unlist(selected_dictionary, use.names = FALSE))
      }

      if (length(factor_items) > 0) {
        missing_items <- setdiff(factor_items, names(raw_data))
        if (length(missing_items)) {
          reliability_output_rv(paste("Error: Saved factor items are not in the active dataset:", paste(missing_items, collapse = ", ")))
          return()
        }
        current_data <- raw_data[, factor_items, drop = FALSE]
      } else if (!is.null(input$reliability_item_select) && length(input$reliability_item_select) > 0) {
        current_data <- raw_data[, input$reliability_item_select, drop = FALSE]
      } else {
        current_data <- raw_data
      }
      if(!all(sapply(current_data, is.numeric))) {
        showNotification("Warning: Non-numeric columns present. They will be excluded automatically if possible.", type="warning")
      }

      cache_key <- session_cache_key(
        input$reliability_coefficient_select,
        input$correlation_type_radio,
        input$cr_correlation_type_radio,
        input$cfa_model_for_reliability_input,
        selected_dictionary,
        selected_factors,
        names(current_data)
      )
      cached_result <- session_cache_get(reliability_cache, cache_key)
      if (!is.null(cached_result)) {
        reliability_output_rv(cached_result)
        showNotification("Saved session result was used.", type = "message")
        return()
      }

      progress_id <- showNotification("Calculating...", duration = NULL, type = "message")
      on.exit(removeNotification(progress_id), add = TRUE)

      tryCatch({
        cor_arg <- if (input$reliability_coefficient_select == "cr") {
          input$cr_correlation_type_radio %||% "cor"
        } else {
          input$correlation_type_radio %||% "cor"
        }

        strata_arg <- NULL
        if (input$reliability_coefficient_select == "s_alpha") {
          strata_arg <- strata_spec$strata
        }

        res <- reliability_func(
          x                 = current_data,
          method            = input$reliability_coefficient_select,
          cor_kind          = cor_arg,
          defined_structure = input$cfa_model_for_reliability_input,
          strata_define     = strata_arg
        )
        reliability_output_rv(res)
        session_cache_set(reliability_cache, cache_key, res)

      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("Reliability", "Analysis error")
        reliability_output_rv(paste("Error:", e$message))
      })
    })

    output$reliability_result_output <- renderText({
      reliability_output_rv() %||% "Result will appear here."
    })
  })
}
