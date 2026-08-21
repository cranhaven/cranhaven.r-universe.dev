#' Wrangling Server Modules
#' @import shiny
#' @export
#' @param id Module namespace ID.
#' @param data Input data (reactive)
#' @param project_state Optional shared project-state object.
#' @param restore_state Optional reactive used while loading a project.
#' @param error_recorder Optional function used for anonymous diagnostics.

# 1. Exclude Variables Server
wrangling_server_ex_var <- function(id, data, project_state = NULL,
                                    restore_state = NULL,
                                    error_recorder = NULL) {
  moduleServer(id, function(input, output, session) {

    # Tracks names of currently excluded variables
    excluded_vars_rv <- reactiveVal(character(0))

    # Return the current active columns so downstream modules can use one
    # consistent reactive chain.
    data_after_exclusion_rv <- reactive({
      req(data())
      excl <- excluded_vars_rv()
      remaining <- setdiff(names(data()), excl)
      data()[, remaining, drop = FALSE]
    })

    # A new upload starts with all variables active.
    observeEvent(data(), {
      req(data())
      excluded_vars_rv(character(0))
    }, ignoreNULL = TRUE)

    if (!is.null(project_state)) {
      observe({
        project_state$exclusion <- list(excluded = excluded_vars_rv())
      })
    }

    if (!is.null(restore_state)) {
      observeEvent(restore_state(), {
        if (!identical(restore_state()$stage, "exclusion")) return()
        req(data())
        saved <- restore_state()$module_state$exclusion$excluded %||% character(0)
        excluded_vars_rv(intersect(saved, names(data())))
      }, ignoreNULL = TRUE)
    }

    # Keep both checkbox lists in sync with current exclusion state
    observe({
      req(data())
      excl  <- excluded_vars_rv()
      avail <- setdiff(names(data()), excl)
      updateCheckboxGroupInput(session, "available_vars_checkbox",
                               choices = avail, selected = NULL)
      updateCheckboxGroupInput(session, "excluded_vars_checkbox",
                               choices = excl, selected = NULL)
    })

    # Summary counts
    output$variable_summary <- renderUI({
      req(data())
      excl    <- excluded_vars_rv()
      total   <- length(names(data()))
      n_excl  <- length(excl)
      n_act   <- total - n_excl
      div(
        class = "d-flex justify-content-around text-center py-1",
        div(h5(total, class = "mb-0 text-primary"), tags$small("Total")),
        div(h5(n_excl, class = "mb-0 text-danger"),  tags$small("Excluded")),
        div(h5(n_act,  class = "mb-0 text-success"), tags$small("Active"))
      )
    })

    # --- Exclude ---
    observeEvent(input$exclude_button, {
      req(data())
      selected <- input$available_vars_checkbox
      if (is.null(selected) || length(selected) == 0) {
        showNotification("Please select at least one variable to exclude.",
                         type = "warning")
        return()
      }
      new_excl   <- union(excluded_vars_rv(), selected)
      n_remaining <- length(names(data())) - length(new_excl)
      if (n_remaining == 0) {
        showNotification("Cannot exclude all variables.", type = "error")
        return()
      }
      excluded_vars_rv(new_excl)
      showNotification(
        paste0(length(selected), " variable(s) excluded. ",
               "Active dataset: ", n_remaining, " variable(s)."),
        type     = "message",
        duration = 4
      )
    })

    # --- Recover selected ---
    observeEvent(input$recover_button, {
      selected <- input$excluded_vars_checkbox
      if (is.null(selected) || length(selected) == 0) {
        showNotification("Please select at least one variable to recover.",
                         type = "warning")
        return()
      }
      new_excl <- setdiff(excluded_vars_rv(), selected)
      excluded_vars_rv(new_excl)
      req(data())
      n_act <- length(names(data())) - length(new_excl)
      showNotification(
        paste0(length(selected), " variable(s) recovered. ",
               "Active dataset: ", n_act, " variable(s)."),
        type     = "message",
        duration = 4
      )
    })

    # --- Reset all ---
    observeEvent(input$reset_button, {
      excluded_vars_rv(character(0))
      showNotification("All variables restored to active dataset.",
                       type = "message")
    })

    # --- Download active data ---
    output$download_excluded_data_button <- downloadHandler(
      filename = function() "active_dataset.csv",
      content  = function(file) {
        d <- data_after_exclusion_rv()
        write.csv(d, file, row.names = FALSE)
      }
    )

    return(data_after_exclusion_rv)
  })
}

# Reverse-scoring
wrangling_server_recode <- function(id, data, project_state = NULL,
                                    restore_state = NULL,
                                    error_recorder = NULL,
                                    language = NULL) {
  moduleServer(id, function(input, output, session) {
    recoded_data_rv <- reactiveVal(NULL)
    recode_summary_rv <- reactiveVal(NULL)
    recode_rules_rv <- reactiveVal(NULL)
    local_text <- function(english, turkish) {
      fafa_text(language, english, turkish)
    }

    observeEvent(data(), {
      req(data())
      recoded_data_rv(data())
      recode_summary_rv(NULL)
      recode_rules_rv(NULL)
      numeric_variables <- names(data())[vapply(data(), is.numeric, logical(1))]
      updateSelectizeInput(session, "reverse_variables", choices = numeric_variables,
                           selected = character(0))
    }, ignoreNULL = TRUE)

    if (!is.null(project_state)) {
      observe({
        project_state$recode <- list(
          rules = recode_rules_rv(),
          recoded_data = recoded_data_rv()
        )
      })
    }

    if (!is.null(restore_state)) {
      observeEvent(restore_state(), {
        if (!identical(restore_state()$stage, "recode")) return()
        req(data())
        saved <- restore_state()$module_state$recode %||% list()
        rules <- saved$rules
        restored <- saved$recoded_data

        if (!is.data.frame(restored) || !identical(names(restored), names(data()))) {
          restored <- data()
          if (is.data.frame(rules) && nrow(rules)) {
            for (i in seq_len(nrow(rules))) {
              if (rules$Variable[[i]] %in% names(restored)) {
                restored <- reverse_score_variables(
                  restored,
                  rules$Variable[[i]],
                  lower = rules$Minimum[[i]],
                  upper = rules$Maximum[[i]]
                )$data
              }
            }
          }
        }

        recode_rules_rv(rules)
        recode_summary_rv(rules)
        recoded_data_rv(restored)
      }, ignoreNULL = TRUE)
    }

    observeEvent(input$apply_reverse_scoring, {
      req(data())
      tryCatch({
        use_observed_limits <- isTRUE(input$detect_recode_limits)
        lower <- if (use_observed_limits) NULL else input$recode_minimum
        upper <- if (use_observed_limits) NULL else input$recode_maximum
        new_result <- reverse_score_variables(
          data(), input$reverse_variables, lower = lower, upper = upper
        )
        rules <- recode_rules_rv()
        if (!is.null(rules)) {
          rules <- rules[!rules$Variable %in% new_result$specifications$Variable, , drop = FALSE]
        }
        rules <- rbind(rules, new_result$specifications)

        recoded <- data()
        for (i in seq_len(nrow(rules))) {
          recoded <- reverse_score_variables(
            recoded,
            rules$Variable[[i]],
            lower = rules$Minimum[[i]],
            upper = rules$Maximum[[i]]
          )$data
        }
        recoded_data_rv(recoded)
        recode_summary_rv(rules)
        recode_rules_rv(rules)
        updateSelectizeInput(session, "reverse_variables", selected = character(0))
        showNotification(
          local_text(
            "Selected items were reverse-scored and the active data was updated.",
            "Se\u00e7ilen maddeler ters puanland\u0131 ve etkin veri g\u00fcncellendi."
          ),
          type = "message"
        )
      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("Data", "Reverse scoring error")
        showNotification(conditionMessage(e), type = "error", duration = 8)
      })
    })

    observeEvent(input$reset_reverse_scoring, {
      req(data())
      recoded_data_rv(data())
      recode_summary_rv(NULL)
      recode_rules_rv(NULL)
      updateSelectizeInput(session, "reverse_variables", selected = character(0))
      showNotification(
        local_text(
          "Reverse-scoring changes were reset.",
          "Ters puanlama de\u011fi\u015fiklikleri s\u0131f\u0131rland\u0131."
        ),
        type = "message"
      )
    })

    output$recode_status <- renderText({
      summary <- recode_summary_rv()
      if (is.null(summary)) {
        local_text(
          "No items have been reverse-scored.",
          "Hen\u00fcz ters puanlanan madde yok."
        )
      } else {
        paste(
          nrow(summary),
          local_text("item(s) reverse-scored.", "madde ters puanland\u0131.")
        )
      }
    })
    output$recode_summary <- renderTable({
      summary <- recode_summary_rv()
      if (is.null(summary)) return(NULL)
      if (identical(fafa_language(language), "tr")) {
        names(summary) <- c("De\u011fi\u015fken", "Alt_S\u0131n\u0131r", "\u00dcst_S\u0131n\u0131r", "Form\u00fcl")
      }
      summary
    }, striped = TRUE, bordered = TRUE)

    output$download_recoded_data <- downloadHandler(
      filename = function() paste0("recoded_data_", Sys.Date(), ".csv"),
      content = function(file) {
        req(recoded_data_rv())
        write.csv(recoded_data_rv(), file, row.names = FALSE)
      }
    )

    recoded_data_rv
  })
}

# Dataset splitting
wrangling_server_split <- function(id, data, error_recorder = NULL) {
  moduleServer(id, function(input, output, session) {
    split_datasets_rv <- reactiveValues(first_half = NULL, second_half = NULL)

    observeEvent(data(), {
      split_datasets_rv$first_half <- NULL
      split_datasets_rv$second_half <- NULL
    }, ignoreNULL = TRUE)

    observeEvent(input$split_data_button, {
      req(data())
      tryCatch({
        df <- data()
        seed <- suppressWarnings(as.integer(input$split_seed))
        if (is.na(seed)) stop("Random seed must be a whole number.")
        set.seed(seed)
        n1 <- floor((input$split_percentage_slider / 100) * nrow(df))
        idx <- sample.int(nrow(df), n1)
        split_datasets_rv$first_half  <- df[ idx, , drop = FALSE]
        split_datasets_rv$second_half <- df[-idx, , drop = FALSE]
        showNotification(paste0("Data split successfully (seed = ", seed, ")."), type = "message")
      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("Data", "Split error")
        showNotification(e$message, type = "error")
      })
    })

    output$download_first_subset_button <- downloadHandler(
      filename = "subset1.csv",
      content  = function(file) {
        req(split_datasets_rv$first_half)
        write.csv(split_datasets_rv$first_half, file, row.names = FALSE)
      }
    )
    output$download_second_subset_button <- downloadHandler(
      filename = "subset2.csv",
      content  = function(file) {
        req(split_datasets_rv$second_half)
        write.csv(split_datasets_rv$second_half, file, row.names = FALSE)
      }
    )
  })
}

# Outlier management
wrangling_server_outliers <- function(id, data, project_state = NULL,
                                      restore_state = NULL,
                                      error_recorder = NULL) {
  moduleServer(id, function(input, output, session) {
    outlier_info_rv <- reactiveValues(table = NULL, count = NULL,
                                      data_clean = NULL, indices = NULL, checked = FALSE)

    observeEvent(data(), {
      outlier_info_rv$table <- NULL
      outlier_info_rv$count <- NULL
      outlier_info_rv$data_clean <- NULL
      outlier_info_rv$indices <- NULL
      outlier_info_rv$checked <- FALSE
    }, ignoreNULL = TRUE)

    if (!is.null(project_state)) {
      observe({
        project_state$outliers <- list(
          table = outlier_info_rv$table,
          count = outlier_info_rv$count,
          data_clean = outlier_info_rv$data_clean,
          indices = outlier_info_rv$indices,
          checked = outlier_info_rv$checked,
          removed = !is.null(outlier_info_rv$data_clean)
        )
      })
    }

    if (!is.null(restore_state)) {
      observeEvent(restore_state(), {
        if (!identical(restore_state()$stage, "outliers")) return()
        saved <- restore_state()$module_state$outliers %||% list()
        outlier_info_rv$table <- saved$table
        outlier_info_rv$count <- saved$count
        restored_clean <- saved$data_clean
        if (!is.data.frame(restored_clean) && isTRUE(saved$removed)) {
          req(data())
          valid_indices <- saved$indices[
            saved$indices >= 1 & saved$indices <= nrow(data())
          ]
          restored_clean <- if (length(valid_indices)) {
            data()[-valid_indices, , drop = FALSE]
          } else {
            data()
          }
        }
        outlier_info_rv$data_clean <- restored_clean
        outlier_info_rv$indices <- saved$indices
        outlier_info_rv$checked <- isTRUE(saved$checked)
      }, ignoreNULL = TRUE)
    }

    observeEvent(input$check_outliers_button, {
      req(data())
      tryCatch({
        numeric_columns <- vapply(data(), is.numeric, logical(1))
        numeric_data <- data()[, numeric_columns, drop = FALSE]
        if (ncol(numeric_data) < 2) stop("At least two numeric variables are required for outlier detection.")
        res <- assumptions(numeric_data, mah_p_threshold = input$mah_p_value_threshold_input)
        outlier_info_rv$table   <- res$Mah_significant
        outlier_info_rv$count   <- res$n_outlier
        outlier_info_rv$indices <- res$Mah_significant$Row_Number_In_Data
        outlier_info_rv$checked <- TRUE
        showNotification("Outlier check complete.", type = "message")
      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("Outliers", "Detection error")
        showNotification(e$message, type = "error")
      })
    })

    observeEvent(input$remove_outliers_button, {
      req(data())
      if (!isTRUE(outlier_info_rv$checked)) {
        showNotification("Run the outlier check before removing observations.", type = "warning")
        return()
      }
      outlier_info_rv$data_clean <- data()
      if (length(outlier_info_rv$indices) > 0) {
        outlier_info_rv$data_clean <- data()[-outlier_info_rv$indices, , drop = FALSE]
        showNotification("Outliers removed!", type = "message")
      } else {
        showNotification("No outliers were found; the active dataset was retained.", type = "message")
      }
    })

    output$outliers_table     <- renderTable({ outlier_info_rv$table })
    output$outlier_count_text <- renderText({ paste("Outliers found:", outlier_info_rv$count) })

    output$download_data_no_outliers_button <- downloadHandler(
      filename = "data_no_outliers.csv",
      content  = function(file) {
        d <- outlier_info_rv$data_clean
        if (is.null(d)) {
          req(data())
          d <- data()
          showNotification("No removal was applied; the current dataset was downloaded.", type = "warning")
        }
        write.csv(d, file, row.names = FALSE)
      }
    )

    return(reactive(outlier_info_rv$data_clean %||% data()))
  })
}
