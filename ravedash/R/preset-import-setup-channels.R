#' @rdname rave-ui-preset
#' @export
presets_import_setup_channels <- function(
  id = "import_channels",
  label = "Channel information",
  import_setup_id = "import_setup",
  import_blocks_id = "import_blocks"
){

  comp <- RAVEShinyComponent$new(id = id)
  comp$depends <- c(import_setup_id, import_blocks_id)
  comp$no_save <- c("", "msg", "actions", "actions_alt", "snapshot",
                    "do_import")

  all_formats <- raveio::IMPORT_FORMATS[c(1,2,3,4,7)]

  comp$ui_func <- function(id, value, depends){

    shidashi::card2(
      title = label,
      inputId = comp$get_sub_element_id(with_namespace = TRUE),
      tools = list(
        shidashi::as_badge("STEP 3")
      ),
      class_body = "",
      body_main = shiny::div(
        class = 'padding-10',
        ravedash::flex_group_box(
          title = "iEEG channels",
          shidashi::flex_item(
            shiny::selectInput(
              inputId = comp$get_sub_element_id("electrode_file", TRUE),
              label = "Data file(s)",
              choices = "auto"
            )
          ),
          shidashi::flex_item(
            shiny::textInput(
              inputId = comp$get_sub_element_id("electrodes", TRUE),
              label = "Channel numbers",
              placeholder = "E.g. 1-84, 100"
            )
          ),
          shidashi::flex_item(
            shiny::selectInput(
              inputId = comp$get_sub_element_id("unit", TRUE),
              label = "Physical unit",
              choices = c("NA", "uV", "mV", "V"),
              selected = "NA",
              multiple = FALSE
            )
          ),
          shidashi::flex_item(
            shiny::numericInput(
              inputId = comp$get_sub_element_id("sample_rate", TRUE),
              label = "Sample rate (Hz)",
              value = NA,
              min = 1
            )
          ),
          shidashi::flex_break(),
          shidashi::flex_item(
            shiny::tags$small(
              shiny::uiOutput(
                outputId = comp$get_sub_element_id("snapshot", TRUE)
              )
            )
          )
        )
      ),
      body_side = shiny::div(
        class = "bg-gray fill padding-10",
        shiny::textOutput(
          outputId = comp$get_sub_element_id("msg", TRUE)
        )
      ),
      class_foot = "padding-10",
      footer = shiny::div(
        class = "float-right",
        dipsaus::actionButtonStyled(
          comp$get_sub_element_id("actions_alt", with_namespace = TRUE),
          label = "Skip validation & import", type = "default"
        ),
        dipsaus::actionButtonStyled(
          comp$get_sub_element_id("actions", with_namespace = TRUE),
          label = "Validate & import"
        )
      )
    )

  }


  comp$server_func <- function(input, output, session) {

    comp_import_blocks <- comp$get_dependent_component(import_blocks_id)
    block_setups <- comp_import_blocks$get_tool("block_setups")

    local_reactives <- shiny::reactiveValues(
      valid_setup = FALSE,
      validation_message = "Waiting...",
      refresh = NULL,
      info = NULL,
      preproc = NULL,
      snapshot = NULL
    )

    output[[comp$get_sub_element_id("msg", FALSE)]] <- shiny::renderText({
      if(isTRUE(local_reactives$valid_setup)) {
        "Subject folder has been created. Please choose session blocks."
      } else {
        local_reactives$validation_message
      }
    })

    disable_ui <- function(){
      dipsaus::updateActionButtonStyled(
        session = session,
        inputId = comp$get_sub_element_id("actions", with_namespace = FALSE),
        disabled = TRUE
      )
      dipsaus::updateActionButtonStyled(
        session = session,
        inputId = comp$get_sub_element_id("actions_alt", with_namespace = FALSE),
        disabled = TRUE
      )

      local_reactives$valid_setup <- FALSE
      local_reactives$info <- NULL
      shidashi::card2_open(id)
    }

    shiny::bindEvent(
      ravedash::safe_observe({
        # return(list(
        # valid = TRUE,
        # initialized = FALSE,
        # project_name = project_name,
        # subject_code = subject_code
        # any_imported
        # current_blocks
        # current_format
        # blocks
        # format
        # ))
        info <- block_setups()
        is_valid <- TRUE
        if(!is.list(info)){
          local_reactives$validation_message <- "Waiting..."
          is_valid <- FALSE
        }
        if(!isTRUE(info$valid)){
          local_reactives$validation_message <- "Please finish the previous steps."
          is_valid <- FALSE
        }

        if(!is_valid) {
          disable_ui()
          return()
        }

        # enable UI
        project_name <- info$project_name
        subject_code <- info$subject_code
        blocks <- info$current_blocks
        format <- info$current_format

        subject <- raveio::RAVESubject$new(project_name = project_name,
                                           subject_code = subject_code, strict = FALSE)
        preproc <- subject$preprocess_settings

        info$preproc <- preproc
        local_reactives$info <- info

        # get electrode files
        fs <- lapply(file.path(preproc$raw_path, blocks), function(f){
          list.files(f)
        })
        common_names <- table(unlist(fs))
        common_names <- names(common_names)[common_names == length(blocks)]

        all_electrode_files <- c("auto", common_names)

        shiny::updateSelectInput(
          session = session,
          inputId = comp$get_sub_element_id("electrode_file", FALSE),
          choices = all_electrode_files,
          selected = preproc$data$electrode_file %OF% all_electrode_files
        )

        dipsaus::updateActionButtonStyled(
          session = session,
          inputId = comp$get_sub_element_id("actions", with_namespace = FALSE),
          disabled = FALSE
        )
        dipsaus::updateActionButtonStyled(
          session = session,
          inputId = comp$get_sub_element_id("actions_alt", with_namespace = FALSE),
          disabled = FALSE
        )
        shidashi::card2_close(id)

        # preproc <- raveio::RAVEPreprocessSettings$new(subject = )

      }),
      block_setups(),
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    output[[comp$get_sub_element_id("snapshot", FALSE)]] <- shiny::renderUI({
      local_reactives$snapshot
    })


    shiny::bindEvent(
      ravedash::safe_observe({
        info <- local_reactives$info
        if(!is.list(info) || !isTRUE(info$valid)) { return() }
        preproc <- info$preproc
        if(is.null(preproc)){ return() }
        format <- info$current_format
        blocks <- info$current_blocks

        # get potential electrodes
        if(format == 1) {
          fs <- list.files(file.path(preproc$raw_path, blocks[[1]]))
          es <- gsub("(^.*[^0-9]|^)([0-9]+)\\.(mat|h5)", "\\2", fs)
          es <- es[grepl("^[0-9]+$", es)]

          local_reactives$snapshot <- shiny::p(
            "With given data format (single .mat/.h5 file per electrode), ",
            "I found the following potential electrodes in the first block (",
            blocks[[1]], "): ", dipsaus::deparse_svec(as.integer(es))
          )
        } else if (format == 3) {
          electrode_file <- comp$get_sub_element_input("electrode_file")
          if(!length(electrode_file) || electrode_file == "auto") {
            electrode_file <- list.files(file.path(preproc$raw_path, blocks[[1]]),
                                         pattern = "\\.edf$", ignore.case = TRUE)
          }
          if(!length(electrode_file)) {
            local_reactives$snapshot <- shiny::p(
              "Cannot find any EDF file in the first block (",
              blocks[[1]], ")"
            )
          } else {
            edf_path <- file.path(preproc$raw_path, blocks[[1]], electrode_file)
            if(length(edf_path) > 1){
              edf_path <- edf_path[which.max(file.size(edf_path))]
            }
            tryCatch({
              header <- raveio::read_edf_header(edf_path)

              local_reactives$snapshot <- shiny::p(
                "With given data format (EDF), I found the following file in ",
                "the first block (", blocks[[1]], "): ", basename(edf_path),
                ". Here is the header information: ",
                shiny::tags$ul(
                  shiny::tags$li(
                    "Total number of channels: ", header$nSignals
                  ),
                  shiny::tags$li(
                    "Recording length: ",
                    sprintf("%.4f seconds", header$recordedPeriod)
                  ),
                  shiny::tags$li(
                    "Potential sample rates: ",
                    paste(unique(header$sampleRate2), " Hz", collapse = ", ")
                  ),
                  shiny::tags$li(
                    "Potential physical units: ",
                    paste(unique(header$unit2), collapse = ",")
                  ),
                  shiny::tags$li(
                    "Channel labels: ",
                    paste(header$sHeaders$label, collapse = ", ")
                  )
                )
              )
            }, error = function(e){
              if(isTRUE(electrode_file == "auto")) {
                local_reactives$snapshot <- shiny::p(
                  "Cannot read the EDF file ", basename(edf_path),
                  " in the first block (",
                  blocks[[1]], "). Please manually choose the EDF file."
                )
              } else {
                local_reactives$snapshot <- shiny::p(
                  "Cannot read the EDF file ", basename(edf_path),
                  " in the first block (",
                  blocks[[1]], "). Please make sure the file is valid."
                )
              }

            })


          }
        } else if (format == 5) {
          # Ignore electrode_file
          electrode_file <- list.files(file.path(preproc$raw_path, blocks[[1]]),
                                       pattern = "\\.nev$", ignore.case = TRUE)
          if(!length(electrode_file)) {
            local_reactives$snapshot <- shiny::p(
              "Cannot find any NEV file in the first block (",
              blocks[[1]], ")"
            )
          } else {
            tryCatch({
              brfile <- raveio::BlackrockFile$new(
                path = file.path(preproc$raw_path, blocks[[1]], electrode_file[[1]]),
                block = blocks[[1]], nev_data = FALSE
              )
              elec_table <- brfile$electrode_table
              duration <- brfile$recording_duration

              nsinfo <- lapply(split(elec_table, elec_table$NSType), function(x) {
                ns_type <- x$NSType[[1]]
                shiny::tags$li(
                  sprintf(
                    "%s: %s [%.0f Hz, %.2f sec]", ns_type,
                    dipsaus::deparse_svec(x$Electrode),
                    x$SampleRate[[1]],
                    duration[[ns_type]]
                  )
                )
              })

              local_reactives$snapshot <- shiny::p(
                "With given data format (BlackRock), I found the following NSX files in ",
                "the first block (", blocks[[1]], "): ",
                paste(names(brfile$has_nsx)[brfile$has_nsx], collapse = ", "),
                ". Here is the header information: ",
                shiny::tags$ul(
                  shiny::tags$li(
                    "Total number of channels: ", nrow(elec_table)
                  ),
                  nsinfo
                ),
                "If no physical unit specified, all the signals will use [uV] by default."
              )
            }, error = function(e){
              local_reactives$snapshot <- shiny::p(
                "Cannot read the BlackRock files ",
                gsub("\\.nev", ".*", basename(electrode_file), ignore.case = TRUE),
                " in the first block (",
                blocks[[1]], "). Please check if the file version is at least 2.3."
              )
              ravedash::logger_error_condition(e)
            })


          }
        } else {
          local_reactives$snapshot <- NULL
        }

        etypes <- preproc$electrode_types
        lfp_sel <- etypes %in% "LFP"
        electrodes <- dipsaus::deparse_svec(preproc$electrodes[lfp_sel])
        shiny::updateTextInput(
          session = session,
          inputId = comp$get_sub_element_id("electrodes", FALSE),
          value = electrodes
        )

        srate <- preproc$sample_rates[lfp_sel]
        if(length(srate)) {
          shiny::updateNumericInput(
            session = session,
            inputId = comp$get_sub_element_id("sample_rate", FALSE),
            value = srate[[1]]
          )
        }

        physical_unit <- preproc$data$physical_unit %OF% c("NA", "uV", "mV", "V")
        shiny::updateSelectInput(
          session = session,
          inputId = comp$get_sub_element_id("unit", FALSE),
          selected = physical_unit
        )

      }),
      local_reactives$info,
      comp$get_sub_element_input("electrode_file"),
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    check_before_import <- function(skip_validation = TRUE) {
      shidashi::clear_notifications()

      tryCatch({


        info <- local_reactives$info
        if(!is.list(info) || !isTRUE(info$valid)) {
          stop("The inputs are invalid. Please check your inputs.")
        }
        preproc <- info$preproc
        if(is.null(preproc)){
          stop("The inputs are invalid. Please check your inputs.")
        }
        format <- info$current_format
        blocks <- info$current_blocks
        electrode_file <- comp$get_sub_element_input("electrode_file")
        sample_rate <- comp$get_sub_element_input("sample_rate")
        physical_unit <- comp$get_sub_element_input("unit")
        electrodes <- dipsaus::parse_svec(comp$get_sub_element_input("electrodes"))

        if(!isTRUE(format %in% seq_along(all_formats))) {
          stop("The format is invalid.")
        }
        if(!length(blocks)) {
          stop("The session block has zero length.")
        }
        if(!isTRUE(sample_rate > 1)) {
          stop("The sample rate must be positive.")
        }
        if(!length(electrodes)) {
          stop("No electrode will be imported.")
        }

        preproc <- info$preproc

        any_imported <- any(preproc$data_imported)

        # set blocks
        # f1 <- tempfile()
        # f2 <- tempfile()
        # file.copy(preproc$path, f1, overwrite = TRUE)
        # file.copy(preproc$backup_path, f2, overwrite = TRUE)
        # on.exit({
        #   file.copy(f1, preproc$path, overwrite = TRUE)
        #   file.copy(f2, preproc$backup_path, overwrite = TRUE)
        #   unlink(f1)
        #   unlink(f2)
        # })
        # preproc$data$checklevel <- 0L
        # lapply(preproc$electrodes, function(e){
        #   preproc$data[[as.character(e)]]$data_imported <- FALSE
        # })
        # preproc$set_blocks(blocks)
        # preproc$set_electrodes(electrodes, type = "LFP")
        # preproc$set_sample_rates(sample_rate, type = "LFP")
        # preproc$save()

        # validation_result <- raveio::validate_raw_file(
        #   subject_code = preproc$subject$subject_code,
        #   blocks = blocks, electrodes = electrodes,
        #   format = format
        # )

        settings <- raveio::load_yaml(comp$container$settings_path)
        settings <- comp$container$collect_settings(c(
          import_setup_id,
          import_blocks_id,
          id
        ), map = settings)
        settings$skip_validation <- skip_validation
        raveio::save_yaml(settings, comp$container$settings_path, sorted = TRUE)

        dipsaus::shiny_alert2(
          title = "Validating...",
          text = "Validating the input data... (some might take a while)",
          auto_close = FALSE, buttons = FALSE, icon = "info"
        )
        on.exit({
          Sys.sleep(time = 0.5)
          dipsaus::close_alert2()
        }, add = TRUE)

        result <- raveio::pipeline_run(
          names = "validation_result",
          pipe_dir = comp$container$pipeline_path,
          scheduler = "none",
          type = "smart",
          async = FALSE
        )

        validation_result <- raveio::pipeline_read(
          var_names = "validation_result",
          pipe_dir = comp$container$pipeline_path
        )

        Sys.sleep(time = 0.5)
        dipsaus::close_alert2()

        if(isFALSE(validation_result)) {
          reasons <- attr(validation_result,"reason")
          shidashi::show_notification(
            title = "Validation failure",
            type = "danger",
            autohide = FALSE, close = TRUE,
            message = shiny::div(
              shiny::p(
                "Found the following issues in the data. Please correct them."
              ),
              shiny::tagList(
                lapply(names(reasons), function(nm){
                  items <- reasons[[nm]]
                  if(length(items)) {
                    shiny::p(
                      nm,
                      shiny::tags$ul(
                        lapply(items, shiny::tags$li)
                      )
                    )
                  } else {
                    shiny::p(nm)
                  }

                })
              )
            )
          )
          return()
        }

        # format <- info$current_format
        # blocks <- info$current_blocks
        # electrode_file <- comp$get_sub_element_input("electrode_file")
        # sample_rate <- comp$get_sub_element_input("sample_rate")
        # physical_unit <- comp$get_sub_element_input("unit")
        # electrodes <- dipsaus::parse_svec(comp$get_sub_element_input("electrodes"))

        shiny::showModal(
          shiny::modalDialog(
            title = "Ready to import data",
            easyClose = FALSE, size = "l",
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              dipsaus::actionButtonStyled(
                inputId = comp$get_sub_element_id("do_import", TRUE),
                label = "Import data"
              )
            ),
            shiny::div(
              "Please make sure the following information is correct before proceeding: ",
              shiny::tags$ul(
                shiny::tags$li(
                  "Subject: ", preproc$subject$subject_id
                ),
                shiny::tags$li(
                  "Session blocks: ", paste(blocks, collapse = ", ")
                ),
                shiny::tags$li(
                  sprintf("Session format: %s (%s)",
                          all_formats[[format]],
                          names(all_formats)[[format]])
                ),
                shiny::tags$li(
                  "Electrode channels: ", dipsaus::deparse_svec(electrodes)
                ),
                shiny::tags$li(
                  sprintf("Sample rate: %.4f Hz", sample_rate)
                ),
                shiny::tags$li(
                  sprintf("Physical unit: %s", physical_unit)
                )
              ),

              {
                if(any_imported){
                  "* The subject has been imported before. Proceed and you will need to re-process all other modules, including Wavelet."
                } else {
                  NULL
                }
              }

            )
          )
        )

      }, error = function(e){
        shidashi::show_notification(
          title = "Validation failure",
          type = "danger",
          autohide = FALSE, close = TRUE,
          message = shiny::div(
            shiny::p(
              "Found the following issues. Please correct them."
            ),
            shiny::p(
              e$message
            )
          )
        )
      })
    }

    shiny::bindEvent(
      ravedash::safe_observe({
        check_before_import(skip_validation = FALSE)
      }),
      comp$get_sub_element_input("actions")
    )

    shiny::bindEvent(
      ravedash::safe_observe({
        check_before_import(skip_validation = TRUE)
      }),
      comp$get_sub_element_input("actions_alt")
    )


    shiny::bindEvent(
      ravedash::safe_observe({
        info <- local_reactives$info
        preproc <- info$preproc

        format <- info$current_format
        blocks <- info$current_blocks
        electrode_file <- comp$get_sub_element_input("electrode_file")
        sample_rate <- comp$get_sub_element_input("sample_rate")
        physical_unit <- comp$get_sub_element_input("unit")
        electrodes <- dipsaus::parse_svec(comp$get_sub_element_input("electrodes"))

        settings <- raveio::load_yaml(comp$container$settings_path)
        settings <- comp$container$collect_settings(c(
          import_setup_id,
          import_blocks_id,
          id
        ), map = settings)

        settings$skip_validation <- TRUE
        settings$force_import <- TRUE
        raveio::save_yaml(settings, comp$container$settings_path, sorted = TRUE)


        dipsaus::shiny_alert2(
          title = "Importing in progress",
          text = "It's time to stand up and stretch yourself...",
          icon = "info",
          auto_close = FALSE,
          danger_mode = FALSE,
          buttons = FALSE
        )

        result <- raveio::pipeline_run(
          pipe_dir = comp$container$pipeline_path,
          scheduler = "none",
          type = "smart"
        )

        result$promise$then(
          onFulfilled = function(...){
            dipsaus::close_alert2()
            shiny::removeModal()
            dipsaus::shiny_alert2(
              title = "Success!",
              text = "Finished importing the data. Please proceed to the next modules.",
              icon = "success",
              danger_mode = FALSE,
              auto_close = TRUE,
              buttons = "Got it!"
            )
          },
          onRejected = function(e) {
            dipsaus::close_alert2()
            ravedash::logger_error_condition(e)
            dipsaus::shiny_alert2(
              title = "Error",
              text = paste(c(
                "Found errors while trying to import data: ",
                e$message
              ), collapse = "\n"),
              icon = "error",
              danger_mode = TRUE,
              auto_close = TRUE,
              buttons = "Dismiss"
            )
          }
        )
        # pipeline_set(.list = settings)

        # raveio::rave_import(
        #   project_name = preproc$subject$project_name,
        #   subject_code = preproc$subject$subject_code,
        #   blocks = blocks, electrodes = electrodes,
        #   sample_rate = sample_rate, format = format,
        #   conversion = ifelse(isTRUE(physical_unit == "NA"), NA, physical_unit),
        #   data_type = "LFP", add = FALSE, skip_validation = TRUE
        # )
      }),
      comp$get_sub_element_input("do_import"),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

  }


  # ------------------------------- Validators --------------------------------



  return(comp)
}
