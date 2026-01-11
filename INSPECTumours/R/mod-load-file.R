#' mod_load_file UI Function
#' @param id the module id
#' @param name label for fileInput
#'
#' @noRd
#'
#' @importFrom shinyalert useShinyalert
#' @importFrom shinyjs useShinyjs reset
#' @importFrom shiny fileInput
#' @importFrom purrr map
mod_load_file_ui <- function(id, name) {
  ns <- NS(id)
  tagList(useShinyalert(),
          useShinyjs(),
          fileInput(ns("file"), name, accept = c(".csv", ".xlsx")))
}


#' mod_load_file Server Function
#' @param r reactiveValues with data
#' @param id,input,output,session internal parameters for {shiny}
#'
#' @noRd
#'
#' @importFrom dplyr summarise group_by %>% bind_rows filter rename_with mutate
#' rename
#' @importFrom shiny reactiveValues showModal modalDialog
#' selectizeInput modalButton removeModal
#' @importFrom rlang .data
#' @importFrom shinytoastr toastr_warning
#' @importFrom stats setNames
#' @return validated dataframe
mod_load_file_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    loaded_data <- reactiveVal(NULL)

    check_ncols <- reactiveVal(NULL)
    check_case <- reactiveVal(NULL)
    check_control <- reactiveVal(NULL)

    dynamic_ids <- reactiveVal()

    crit_cols <-
      c("animal_id",
        "day",
        "tumour_volume",
        "treatment",
        "study")

    observeEvent(input$file, {

      file <- input$file

      check_ncols(NULL)
      check_case(NULL)
      check_control(NULL)

      tryCatch({
        df <- load_data(file$datapath, file$name)
      },
      error = function(e) {
        print(e)
        shinyalert("Error loading data", type = "error")
      })

      req(df)

      missing_values <- any(is.na(df))
      if (missing_values) {
        notify_error_and_reset_input("Some values are missing")
      }

      n_crit_cols <- length(crit_cols)
      if (ncol(df) < n_crit_cols) {
        notify_error_and_reset_input(
          paste(
            "There are",
            n_crit_cols,
            "critical columns to conduct analysis. Look at the Info tab."
          )
        )

      }

      if (!missing_values & ncol(df) >= n_crit_cols) {
        df$file_name <- rep(file$name, nrow(df))
        loaded_data(df)
        check_ncols(TRUE)
      }

    })

    observeEvent(check_ncols(), {
      unvalidated_list <- crit_cols[!crit_cols %in% colnames(loaded_data())]
      if (length(unvalidated_list) > 0) {

        guesses <- guess_match(colnames(loaded_data()), unvalidated_list)

        ns <- session$ns

        showModal(
          modalDialog(
            title = "Please validate your data",
            paste("There are",
                  length(crit_cols),
                  "mandatory columns to conduct analysis"),
            easyClose = FALSE,
            lapply(seq_len(length(guesses)), function(i)
              selectizeInput(
                inputId = ns(names(guesses)[i]),
                label = names(guesses)[i],
                choices = colnames(loaded_data()),
                selected = if (guesses[[i]] != "") guesses[[i]] else NULL
              )),
            footer = tagList(actionButton(ns("cancel"), "Cancel"),
                             actionButton(ns("ok_case"), "OK")),
            size = "s"
          )
        )
        dynamic_ids(names(guesses))
      } else {
        check_case(TRUE)
      }
    })

    observeEvent(input$cancel, {
      removeModal()
      reset("file")
    })

    observeEvent(input$ok_case, {

      user_inputs <- map(dynamic_ids(), ~ input[[.x]]) %>%
        setNames(dynamic_ids())
      if (anyDuplicated(user_inputs) == 0) {
        df <- loaded_data()
        df_renamed <- rename(df, !!!user_inputs)
        loaded_data(df_renamed)
        check_case(TRUE)
        removeModal()
      } else {
        toastr_warning("Please select unique column names",
                       position = "bottom-center", closeButton = TRUE)
      }

    })

    observeEvent(check_case(), {
      df <- loaded_data()
      df$treatment <- gsub("control", "Control", df$treatment)
      loaded_data(df)
      control_df <- loaded_data() %>%
        group_by(.data$study) %>%
        summarise(control = ifelse("Control" %in% .data$treatment, TRUE, FALSE))
      if (FALSE %in% control_df$control) {
        notify_error_and_reset_input("It should be a control group for each study")
      } else {
        check_control(TRUE)
      }
    })


    observeEvent(check_control(), {
      # tumour volumes < than 15 mm3 are replaced with a minimum value of 15 mm3
      tv_numeric <- is.numeric(loaded_data()$tumour_volume)
      if (!tv_numeric) {
        notify_error_and_reset_input("Tumour volume must be numeric")
      }
      req(tv_numeric)
      df <- loaded_data() %>%
        mutate(tumour_volume = ifelse(.data$tumour_volume < 15,
                                      15,
                                      .data$tumour_volume)) %>%
        rename_with(tolower) %>%
        mutate(log_tv = log10(.data$tumour_volume))
      if (id == "new") {
        if (is.null(r$new_data)) {
          r$new_data <- df
        } else {
          r$new_data <- bind_rows(r$new_data, df)
        }

        r$new_control_data <- filter(r$new_data, .data$treatment == "Control")

      } else {
        r$h_control_data <- df
      }

    })

  })
}
