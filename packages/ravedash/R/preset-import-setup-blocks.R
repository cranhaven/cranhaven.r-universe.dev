#' @rdname rave-ui-preset
#' @export
presets_import_setup_blocks <- function(
  id = "import_blocks",
  label = "Format & session blocks",
  import_setup_id = "import_setup",
  max_components = 5
){

  comp <- RAVEShinyComponent$new(id = id)
  comp$depends <- import_setup_id
  comp$no_save <- c("", "msg", "actions", "format_details", "action_dbl_confirm",
                    "block_preview", "")

  all_formats <- raveio::IMPORT_FORMATS[c(1,2,3,4,7)]
  regexps <- c(
    "\\.(h5|mat)$",
    "\\.(h5|mat)$",
    "\\.(edf)$",
    "\\.(eeg|dat|vmrk|vhdr)$",
    "\\.(nev|ns[1-6])$"
  )

  comp$ui_func <- function(id, value, depends){

    shidashi::card2(
      title = label,
      inputId = comp$get_sub_element_id(with_namespace = TRUE),
      tools = list(
        shidashi::as_badge("STEP 2")
      ),
      class_body = "",
      body_main = shiny::div(
        class = 'padding-10',
        shiny::fluidRow(
          shiny::column(
            width = 6L,
            shiny::selectInput(
              inputId = comp$get_sub_element_id("session_block", TRUE),
              label = "Sessions/Blocks",
              choices = character(0L),
              selected = character(0L),
              multiple = TRUE
            ),
            shiny::selectInput(
              inputId = comp$get_sub_element_id("format", TRUE),
              label = "Formats",
              choices = names(all_formats),
              selected = character(0L),
              multiple = FALSE
            ),
            shiny::tags$small(
              shiny::p(shiny::textOutput(
                outputId = comp$get_sub_element_id("format_details", TRUE)
              )),
              shiny::p("Please check the file format to make sure your data (preview has been printed in the right panel) is consistent with the configuration.")
            )
          ),
          shiny::column(
            width = 6L,
            shiny::div(
              class = "row overflow-y-scroll max-height-350",
              shiny::column(
                width = 12L,
                shiny::verbatimTextOutput(
                  outputId = comp$get_sub_element_id("block_preview", TRUE)
                )
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
          comp$get_sub_element_id("actions", with_namespace = TRUE),
          label = "Confirm"
        )
      )
    )

  }


  comp$server_func <- function(input, output, session) {

    comp_import_setup <- comp$get_dependent_component(import_setup_id)
    basic_setups <- comp_import_setup$get_tool("basic_setups")

    local_reactives <- shiny::reactiveValues(
      valid_setup = FALSE,
      validation_message = "Waiting...",
      refresh = NULL
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
      local_reactives$valid_setup <- FALSE

      shiny::updateSelectInput(
        session = session,
        inputId = comp$get_sub_element_id("session_block", FALSE),
        label = "Sessions/Blocks",
        choices = character(0L)
      )

      shidashi::card2_open(id)
    }

    shiny::bindEvent(
      ravedash::safe_observe({
        # return(list(
        # valid = TRUE,
        # initialized = FALSE,
        # project_name = project_name,
        # subject_code = subject_code
        # # format = format
        # ))
        info <- basic_setups()
        is_valid <- TRUE
        if(!is.list(info)){
          local_reactives$validation_message <- "Waiting..."
          is_valid <- FALSE
        }
        if(!isTRUE(info$valid)){
          local_reactives$validation_message <- "Please select valid project and subject in the previous step."
          is_valid <- FALSE
        }
        if(!isTRUE(info$initialized)) {
          local_reactives$validation_message <- "Subject folders have not been created yet. Please create them in the previous step."
          is_valid <- FALSE
        }

        if(!is_valid) {
          disable_ui()
          return()
        }

        # enable UI
        project_name <- info$project_name
        subject_code <- info$subject_code
        subject <- raveio::RAVESubject$new(project_name = project_name,
                                           subject_code = subject_code, strict = FALSE)
        preproc <- subject$preprocess_settings
        format_selection <- preproc$data$format %OF% seq_along(all_formats)

        shiny::updateSelectInput(
          session = session,
          inputId = comp$get_sub_element_id("format", FALSE),
          selected = names(all_formats)[[format_selection]]
        )

        if(any(preproc$data_imported)) {
          shiny::updateSelectInput(
            session = session,
            inputId = comp$get_sub_element_id("session_block", FALSE),
            label = "Sessions/Blocks (read-only)",
            choices = preproc$all_blocks,
            selected = preproc$blocks
          )
        } else {
          shiny::updateSelectInput(
            session = session,
            inputId = comp$get_sub_element_id("session_block", FALSE),
            label = "Sessions/Blocks",
            choices = preproc$all_blocks,
            selected = preproc$blocks
          )
        }

        dipsaus::updateActionButtonStyled(
          session = session,
          inputId = comp$get_sub_element_id("actions", with_namespace = FALSE),
          disabled = FALSE
        )
        shidashi::card2_close(id)


        # preproc <- raveio::RAVEPreprocessSettings$new(subject = )

      }),
      basic_setups(),
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    # validations here
    block_setups <- shiny::bindEvent(
      shiny::reactive({
        info <- basic_setups()
        if(!is.list(info) || !isTRUE(info$initialized)) {
          return(list(
            valid = FALSE,
            reason = "Waiting for previous steps"
          ))
        }
        info$blocks <- comp$get_sub_element_input("session_block")
        info$format <- which(
          names(all_formats) %in% comp$get_sub_element_input("format")
        )

        if(!length(info$blocks)) {
          return(list(
            valid = FALSE,
            reason = "No block(s) selected"
          ))
        }

        project_name <- info$project_name
        subject_code <- info$subject_code
        subject <- raveio::RAVESubject$new(project_name = project_name,
                                           subject_code = subject_code, strict = FALSE)
        preproc <- subject$preprocess_settings

        if(any(preproc$data_imported)) {
          info$any_imported <- TRUE
          info$current_blocks <- preproc$blocks
          info$current_format <- preproc$data$format %OF% seq_along(all_formats)

        } else {
          info$any_imported <- FALSE
          if(length(preproc$blocks)) {
            info$current_blocks <- preproc$blocks
          } else {
            info$current_blocks <- info$blocks
          }
          info$current_format <- preproc$data$format %OF% c(info$format, seq_along(all_formats))
        }

        # raveio::save_yaml(info, stdout())
        return(info)

      }),
      basic_setups(),
      local_reactives$refresh,
      comp$get_sub_element_input("session_block"),
      comp$get_sub_element_input("format"),
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )

    shiny::bindEvent(
      ravedash::safe_observe({
        info <- block_setups()
        if(!isTRUE(info$valid)) {
          dipsaus::updateActionButtonStyled(
            session = session,
            inputId = comp$get_sub_element_id("actions", with_namespace = FALSE),
            disabled = TRUE,
            label = info$reason
          )
          return()
        } else {
          if(setequal(info$current_blocks, info$blocks) &&
             isTRUE(info$current_format == info$format)) {

            settings <- raveio::load_yaml(comp$container$settings_path)
            comp$collect_settings(map = settings)

            raveio::save_yaml(
              settings,
              comp$container$settings_path,
              sorted = TRUE
            )

            dipsaus::updateActionButtonStyled(
              session = session,
              inputId = comp$get_sub_element_id("actions", with_namespace = FALSE),
              disabled = TRUE,
              label = "No change detected"
            )
            return()
          }

          dipsaus::updateActionButtonStyled(
            session = session,
            inputId = comp$get_sub_element_id("actions", with_namespace = FALSE),
            disabled = FALSE,
            label = "Confirm"
          )
        }
      }),
      block_setups(),
      ignoreNULL = TRUE,
      ignoreInit = FALSE
    )




    output[[comp$get_sub_element_id("format_details", FALSE)]] <- shiny::renderText({

      info <- block_setups()
      if(!is.list(info) || !isTRUE(info$valid)) { return() }
      fmt_idx <- info$format
      if(length(fmt_idx) != 1 || !fmt_idx %in% seq_along(all_formats)) { return() }

      switch (
        as.character(fmt_idx),
        '1' = {
          paste0("In each block folder, one Matlab/HDF5 file stands for one electrode. ",
                 "File name should match with format XXX1.h5 or xxx2.mat. ",
                 "Each file only contains a one-dimensional vector. ",
                 "The vector lengths stand for total time points and they must be the same across all electrode files. ")
          # shiny::div(
          #   shiny::p(),
          #   shiny::tags$pre(
          #     dipsaus::print_directory_tree(
          #       c('block1', 'block2'),
          #       root = '<subject folder>',
          #       child = c(
          #         'datafile_e1.mat <vector of time>',
          #         'datafile_e2.mat <same length>',
          #         'datafile_e3.mat',
          #         '...'
          #       ),
          #       collapse = '\n'
          #     )
          #   )
          # )
        },
        '2' = {
          paste0("A single Matlab/HDF5 file containing all electrode information. ",
                 "Data must be a matrix. One of the dimension must be electrodes, ",
                 "the other dimension must be time points. ",
                 "ALL blocks must share the same file & data name.")
          # shiny::div(
          #   shiny::p("A single Matlab/HDF5 file containing all electrode information. ",
          #            "Data must be a matrix. One of the dimension must be electrodes, ",
          #            "the other dimension must be time points. ",
          #            "ALL blocks must share the same file & data name; for example:"),
          #   shiny::tags$pre(
          #     dipsaus::print_directory_tree(
          #       c('block1', 'block2'),
          #       root = '<subject folder>',
          #       child = c(
          #         'datafile.mat <one big matrix>'
          #       ),
          #       collapse = '\n'
          #     )
          #   ))
        },
        '5' = {
          paste0("In each block folder, one Neuro-Event file [.nev] and corresponding NSX files [.ns1, .ns2, ..., .ns6] containing electrode data.")
        },
        {
          paste0("In each block folder, one EDF(+)/EEG file containing all electrode data.")
          # shiny::div(
          #   shiny::p("In each block folder, one EDF(+) file containing all electrode data; for example:"),
          #   shiny::tags$pre(
          #     dipsaus::print_directory_tree(
          #       c('block1', 'block2'),
          #       root = '<subject folder>',
          #       child = c(
          #         'datafile.edf <ONLY one EDF file per block>'
          #       ),
          #       collapse = '\n'
          #     )
          #   ))
        }
      )
    })

    output[[comp$get_sub_element_id("block_preview", FALSE)]] <- shiny::renderPrint({
      info <- block_setups()
      if(!is.list(info) || !isTRUE(info$valid)) {
        return("Please choose valid blocks and format")
      }
      fmt_idx <- info$format
      blocks <- info$blocks

      project_name <- info$project_name
      subject_code <- info$subject_code
      dirs <- raveio::rave_directories(subject_code = subject_code,
                                       project_name = project_name)

      if(!dir.exists(dirs$raw_path)) {
        return("Cannot find raw data path")
      }

      root_str <- sprintf("%s (subject folder)", subject_code)

      regexp <- regexps[[fmt_idx]]

      for(block in blocks) {
        fs <- list.files(file.path(dirs$raw_path, block), pattern = regexp, recursive = FALSE, all.files = FALSE, full.names = FALSE, ignore.case = TRUE)
        if(length(fs) > max_components) {
          fs <- c(fs[seq_len(max_components - 1)], "...")
        }
        print(dipsaus::print_directory_tree(sprintf("%s (session folder)", block),
                                            root = root_str, dir_only = TRUE, child = fs))
        root_str <- " "
      }
    })

    # blocks, format, any_imported
    set_data <- function(preproc, info){
      ravedash::logger("Current subject: ", preproc$subject$subject_id, level = "info")
      new_blocks <- info$blocks
      format <- info$format

      if(any(preproc$data_imported)) {

        if(!isFALSE(preproc$data$stringent)){

          if(!setequal(preproc$blocks, new_blocks)) {
            ravedash::logger("Subject is set with less stringent validation.", level = "info")
            preproc$data$stringent <- FALSE
          }

        }

      }

      ravedash::logger("Setting session blocks: ", paste(new_blocks, collapse = ", "), level = "info")
      preproc$data$blocks <- new_blocks

      ravedash::logger("Setting session format: {all_formats[[format]]} ({names(all_formats)[[format]]})", level = "info", use_glue = TRUE)
      preproc$data$format <- format

      preproc$save()

      settings <- raveio::load_yaml(comp$container$settings_path)
      comp$collect_settings(map = settings)

      raveio::save_yaml(
        settings,
        comp$container$settings_path,
        sorted = TRUE
      )

      shidashi::clear_notifications()
      shidashi::show_notification(
        title = "Set blocks and format",
        subtitle = "Success!",
        type = "success",
        message = shiny::p(
          "The subject [", preproc$subject$subject_id, "] has been edited: ",
          shiny::tags$ul(
            shiny::tags$li("Session blocks: ", paste(new_blocks, collapse = ", ")),
            shiny::tags$li("Session format: ", all_formats[[format]], " (",
                           names(all_formats)[[format]], ")")
          )
        ),
        autohide = TRUE
      )

      local_reactives$refresh <- Sys.time()

    }

    shiny::bindEvent(
      ravedash::safe_observe({

        info <- block_setups()
        if(!is.list(info) || !isTRUE(info$valid)) {
          shidashi::show_notification(
            title = "Error", type = "danger", autohide = FALSE,
            message = paste(c(
              "One or more errors found while trying to configure the subject: ",
              info$reason
            ), collapse = ""),
            class = "_rave-import-LFP-notif-error_"
          )
          return()
        }


        format_idx <- info$format
        if(!length(format_idx)){
          shidashi::show_notification(
            title = "Error", type = "danger", autohide = FALSE,
            message = paste(c(
              "Cannot recognize the format: ",
              info$format
            ), collapse = ""),
            class = "_rave-import-LFP-notif-error_"
          )
          return()
        }

        new_blocks <- info$blocks
        project_name <- info$project_name
        subject_code <- info$subject_code
        subject <- raveio::RAVESubject$new(project_name = project_name,
                                           subject_code = subject_code, strict = FALSE)
        preproc <- subject$preprocess_settings

        if(info$any_imported && !setequal(preproc$blocks, new_blocks)) {
          shidashi::show_notification(
            title = "Block inconsistent", type = "warning",
            autohide = FALSE, close = TRUE,
            message = shiny::div(
              shiny::p(
                "It seems you have imported this subject with different session/block settings before. Please proceed and press the `OK` button if this change is intentional. Please be aware that this feature (changing the session blocks after importing the data) is experimental and will result in less stringent validation."
              ),
              shiny::tags$ul(
                shiny::tags$li(
                  "Existing blocks: ", paste(preproc$blocks, collapse = ", ")
                ),
                shiny::tags$li(
                  "New blocks: ", paste(new_blocks, collapse = ", ")
                )
              ),
              shiny::actionButton(
                comp$get_sub_element_id("action_dbl_confirm", with_namespace = TRUE),
                label = "OK",
              )
            )
          )
          return()
        }

        set_data(preproc, info)
      }),
      comp$get_sub_element_input("actions"),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

    shiny::bindEvent(
      ravedash::safe_observe({

        info <- block_setups()
        project_name <- info$project_name
        subject_code <- info$subject_code
        subject <- raveio::RAVESubject$new(project_name = project_name,
                                           subject_code = subject_code, strict = FALSE)
        preproc <- subject$preprocess_settings
        set_data(preproc, info)

      }),
      comp$get_sub_element_input("action_dbl_confirm")
    )

    comp$set_tool("block_setups", block_setups, server_needed = TRUE)

  }


  # ------------------------------- Validators --------------------------------



  return(comp)
}
