#' @rdname rave-ui-preset
#' @export
presets_import_setup_native <- function(
  id = "import_setup",
  label = "Select project & subject"
){

  comp <- RAVEShinyComponent$new(id = id)
  comp$no_save <- c("", "format_details", "actions", "new_project_name",
                    "new_project_name_dismiss", "new_project_name_confirm")


  get_projects <- function(refresh = FALSE){
    raveio::get_projects(refresh = refresh)
  }

  comp$ui_func <- function(id, value, depends){

    all_projects <- c(get_projects(TRUE), "[New Project]")
    raw_root <- raveio::raveio_getopt("raw_data_dir")
    all_subjects <- list.files(raw_root, pattern = "^[^\\/ ]+$", full.names = FALSE, recursive = FALSE, all.files = FALSE)
    all_subjects <- all_subjects[grepl("^[a-zA-Z0-9]", all_subjects)]
    all_subjects <- all_subjects[dir.exists(file.path(raw_root, all_subjects))]

    ravedash::input_card(
      title = label,
      class_header = "",
      tools = list(
        shidashi::as_badge("STEP 1")
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6L,
          shiny::selectInput(
            inputId = comp$get_sub_element_id("project_name", with_namespace = TRUE),
            label = "Project name",
            choices = all_projects,
            selected = comp$get_default("project_name")
          )
        ),
        shiny::column(
          width = 6L,
          shiny::selectInput(
            inputId = comp$get_sub_element_id("subject_code", with_namespace = TRUE),
            label = "Subject code",
            choices = all_subjects,
            selected = comp$get_default("subject_code", missing = character())
          )
          # shiny::textInput(
          #   inputId = comp$get_sub_element_id("subject_code", with_namespace = TRUE),
          #   label = "Subject code",
          #   placeholder = "Letters, digits, dash (-), and/or underscore (_)",
          #   value = comp$get_default("subject_code", missing = "")
          # )
        )
      ),
      # shiny::radioButtons(
      #   comp$get_sub_element_id("format", with_namespace = TRUE),
      #   label = "Format convention", inline = TRUE,
      #   choices = c("Native", "BIDS"),
      #   selected = comp$get_default("format", constraint = c("Native", "BIDS"))
      # ),
      # shiny::selectInput(
      #   inputId = comp$get_sub_element_id("format", with_namespace = TRUE),
      #   label = "Data source/format",
      #   choices = names(all_formats),
      #   selected = comp$get_default("format", constraint = names(all_formats))
      # ),
      # shiny::uiOutput(
      #   comp$get_sub_element_id("format_details", with_namespace = TRUE)
      # ),
      footer = shiny::div(
        class = "float-right",
        dipsaus::actionButtonStyled(
          comp$get_sub_element_id("actions", with_namespace = TRUE),
          label = "Import data"
        )
        # shiny::uiOutput(
        #   comp$get_sub_element_id("actions", with_namespace = TRUE)
        # )
      )
    )

  }


  comp$server_func <- function(input, output, session) {

    local_reactives <- shiny::reactiveValues(
      force_refresh = NULL
    )

    # Pop up modal when project name is "[New Project]"
    shiny::bindEvent(
      ravedash::safe_observe({

        project_name <- comp$get_sub_element_input("project_name")

        if(isTRUE(project_name == "[New Project]")){

          ravedash::logger("Opening up a modal to create new project", level = "trace")

          shiny::showModal(shiny::modalDialog(
            title = "Create new project",
            easyClose = FALSE,
            shiny::div(
              class = 'fill-width',
              shiny::textInput(
                inputId = comp$get_sub_element_id("new_project_name", with_namespace = TRUE),
                label = "Enter a valid project name:",
                placeholder = "Letters, digits, `-`, and `_`"
              )
            ),
            footer = shiny::tagList(
              shiny::actionButton(
                inputId = comp$get_sub_element_id("new_project_name_dismiss", TRUE),
                label = "Dismiss"
              ),
              dipsaus::actionButtonStyled(
                inputId = comp$get_sub_element_id("new_project_name_confirm", TRUE),
                label = "Create"
              )
            )
          ))
        }
      }),
      comp$get_sub_element_input("project_name"),
      ignoreNULL = TRUE,
      ignoreInit = FALSE
    )

    # Check and remove modal if "dismiss" button is pressed
    shiny::bindEvent(
      ravedash::safe_observe({
        # check current project size
        all_projects <- get_projects(refresh = TRUE)
        if(!length(all_projects)) {
          ravedash::logger("Dismiss button is pressed but there is no project", level = "trace")
          shidashi::show_notification(
            "You haven't created any project yet. Please create one so you can start to import subjects.", type = "warning", close = TRUE, autohide = TRUE
          )
          return()
        }

        previous_project <- comp$get_default("project_name", constraint = all_projects)
        ravedash::logger("Dismiss button is pressed, fallback to previous project: {previous_project}", level = "trace", use_glue = TRUE)
        shiny::updateSelectInput(
          session = session, selected = previous_project,
          inputId = comp$get_sub_element_id("project_name", with_namespace = FALSE)
        )
        shiny::removeModal()

      }),
      comp$get_sub_element_input("new_project_name_dismiss"),
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Create new project name
    shiny::bindEvent(
      ravedash::safe_observe({
        # check current project size
        all_projects <- get_projects(refresh = TRUE)
        new_project_name <- comp$get_sub_element_input("new_project_name")

        msg <- NULL

        if(!length(new_project_name)) {
          new_project_name <- ""
        }
        new_project_name <- trimws(new_project_name, which = "both")
        if(new_project_name == ''){
          msg <- "The project name cannot be blank"
        } else if( tolower(new_project_name) %in% tolower(all_projects) ){
          msg <- "Project has already existed"
        } else if (!grepl("^[a-zA-Z0-9][a-zA-Z0-9_-]{0,}$", new_project_name)){
          msg <- "The subject code is invalid: can only contain letters, digits, dash (-), or underscore (_). The first letter should only contain letters and digits. For example, `my-project_0123` is valid, but `_project`, `project!@#$` are invalid."
        }
        if(length(msg)){
          ravedash::logger("Invalid new project name: `{new_project_name}`", level = "trace", use_glue = TRUE)
          shidashi::show_notification(
            title = "The project name is invalid",
            message = msg, type = "warning", close = TRUE, autohide = FALSE
          )
          return()
        }

        ravedash::logger("Creating new project name: `{new_project_name}`", level = "trace", use_glue = TRUE)
        project <- raveio::RAVEProject$new(project_name = new_project_name, strict = FALSE)
        raveio::dir_create2(project$path)

        shidashi::clear_notifications()
        shidashi::show_notification(message = sprintf("A new RAVE project folder [%s] has been created!", new_project_name), type = "success", title = "Success!", subtitle = "New Project")
        shiny::updateSelectInput(
          session = session, selected = new_project_name,
          choices = c(get_projects(refresh = TRUE), "[New Project]"),
          inputId = comp$get_sub_element_id("project_name", with_namespace = FALSE)
        )
        shiny::removeModal()

      }),
      comp$get_sub_element_input("new_project_name_confirm"),
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Run pipeline
    input_ready <- shiny::bindEvent(
      shiny::reactive({
        valid <- comp$sv$is_valid()
        if(!valid){
          return(list(
            valid = FALSE,
            reason = "Project/subject is invalid. Please fix the inputs above"
          ))
        }

        project_name <- comp$get_sub_element_input("project_name")
        subject_code <- comp$get_sub_element_input("subject_code")
        # format <- comp$get_sub_element_input("format")

        if(!length(project_name) || project_name == "" ||
           !length(subject_code) || trimws(subject_code) == "" ){
          return(list(valid = FALSE, reason = "Blank project/subject found. Please enter the inputs"))
        }
        dirs <- raveio::rave_directories(subject_code = subject_code, project_name = project_name)

        if(dir.exists(dirs$proprocess_path)) {

          settings <- raveio::load_yaml(comp$container$settings_path)
          comp$collect_settings(map = settings)

          raveio::save_yaml(
            settings,
            comp$container$settings_path,
            sorted = TRUE
          )

          return(list(
            valid = TRUE,
            initialized = TRUE,
            project_name = project_name,
            subject_code = subject_code
          ))
        } else {
          # if( format == "Native" ) {
            if(!dir.exists(dirs$raw_path)) {
              return(list(
                valid = FALSE,
                reason = sprintf("Cannot find raw folder for subject `%s`", subject_code)
              ))
            }
          # } else if(format == "BIDS") {
          #   if(!length(dirs$bids_subject_path) || !dir.exists(dirs$bids_subject_path)) {
          #     return(list(
          #       valid = FALSE,
          #       reason = sprintf("Cannot find raw folder for BIDS subject `%s`", subject_code)
          #     ))
          #   }
          # }
          return(list(
            valid = TRUE,
            initialized = FALSE,
            project_name = project_name,
            subject_code = subject_code
            # format = format
          ))
        }

      }),
      comp$get_sub_element_input("project_name"),
      comp$get_sub_element_input("subject_code"),
      # comp$get_sub_element_input("format"),
      local_reactives$force_refresh,
      ignoreNULL = TRUE
    )

    # Update `actions` button's label text
    shiny::bindEvent(
      ravedash::safe_observe({
        validation <- input_ready()
        if(!validation$valid){
          dipsaus::updateActionButtonStyled(
            session = session,
            inputId = comp$get_sub_element_id("actions", with_namespace = FALSE),
            disabled = TRUE,
            label = validation$reason
          )
        } else if(validation$initialized){
          dipsaus::updateActionButtonStyled(
            session = session,
            inputId = comp$get_sub_element_id("actions", with_namespace = FALSE),
            disabled = TRUE,
            label = "Subject already exists"
          )
        } else {
          dipsaus::updateActionButtonStyled(
            session = session,
            inputId = comp$get_sub_element_id("actions", with_namespace = FALSE),
            disabled = FALSE,
            label = "Create subject"
          )
        }
      }),
      input_ready(),
      ignoreNULL = TRUE,
      ignoreInit = FALSE
    )

    shiny::bindEvent(
      ravedash::safe_observe({
        validator <- input_ready()
        if(!isTRUE(validator$valid) || !isFALSE(validator$initialized)) { return() }
        # Create a new subject!
        project_name <- validator$project_name
        subject_code <- validator$subject_code
        ravedash::logger("Initializing RAVE subject folders {project_name}/{subject_code}", level = "info", use_glue = TRUE)
        preprocess <- raveio::RAVEPreprocessSettings$new(subject = sprintf("%s/%s", project_name, subject_code), read_only = FALSE)
        preprocess$subject$initialize_paths(include_freesurfer = FALSE)
        preprocess$save()
        local_reactives$force_refresh <- Sys.time()
      }),
      comp$get_sub_element_input("actions"),
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Expose `basic_setups` to other presets
    comp$set_tool("basic_setups", value = input_ready, server_needed = TRUE)

  }


  # ------------------------------- Validators --------------------------------
  comp$add_rule(
    sub_id = "project_name",
    rule = function(value){
      if(!length(value)){
        return("The project name cannot be blank")
      }
      if(value == "[New Project]"){
        return("Please enter the new project name in the pop-up modal message")
      }
      invisible()
    }
  )

  comp$add_rule(
    sub_id = "new_project_name",
    rule = function(value){
      if(
        length(value) == 1 &&
        isTRUE(comp$get_sub_element_input("project_name") == "[New Project]")
      ){
        if(!nchar(value)){ return() }
        all_projects <- tolower(get_projects())
        if( tolower(value) %in% all_projects ){
          return("Project has already existed (case-insensitive)")
        }
        result <- grepl("^[a-zA-Z0-9][a-zA-Z0-9_-]{0,}$", value)
        if(!result){
          return("The subject code is invalid: can only contain letters, digits, dash (-), or underscore (_). The first letter should only contain letters and digits.")
        }
      }
      invisible()
    }
  )

  comp$add_rule(
    sub_id = "subject_code",
    rule = function(value){
      if(!length(value)){
        value <- ""
      }
      if(trimws(value) == "") { return() }

      if(!grepl("^[a-zA-Z0-9][a-zA-Z0-9_-]{0,}$", value)) {
        return("The subject code is invalid: can only contain letters, digits, dash (-), or underscore (_). The first letter should only contain letters and digits.")
      }

    }
  )



  # # input validator
  # validator_step1_inputs <- shinyvalidate::InputValidator$new(session = session)
  #
  # validator_step1_inputs$add_rule(inputId = "loader_project_name", shinyvalidate::sv_required())
  # validator_step1_inputs$add_rule(inputId = "loader_project_name", shinyvalidate::sv_regex("^[a-zA-Z0-9][a-zA-Z0-9_-]{0,}$", "The project name is invalid: can only contain letters, digits, dash (-), or underscore (_). The first letter should only contain letters and digits."))
  #
  # validator_step1_inputs$add_rule(inputId = "loader_subject_code", shinyvalidate::sv_optional())
  # validator_step1_inputs$add_rule(inputId = "loader_subject_code", shinyvalidate::sv_regex("^[a-zA-Z0-9][a-zA-Z0-9_-]{0,}$", "The subject code is invalid: can only contain letters, digits, dash (-), or underscore (_). The first letter should only contain letters and digits."))
  #
  # validator_step1_inputs$add_validator(validator_step1_modal)
  # validator_step1_inputs$enable()



  return(comp)
}
