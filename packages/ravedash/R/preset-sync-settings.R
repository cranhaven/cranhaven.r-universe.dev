



#' @rdname rave-ui-preset
#' @export
presets_loader_sync_project_subject <- function(
    id = "loader_sync_project_subject",
    label = "Sync subject from most recently loaded",
    varname = "loader_sync_project_subject",
    loader_project_id = "loader_project_name",
    loader_subject_id = "loader_subject_code",
    from_module = NULL,
    project_varname = "project_name",
    subject_varname = "subject_code"
) {
  force(from_module)

  if(!is.null(from_module)) {
    if(
      length(from_module) != 1 ||
      is.na(from_module) ||
      !is.character(from_module)
    ) {
      stop("`presets_loader_sync_project_subject`: `from_module` must be either NULL or a character of length 1 (module ID)")
    }
  }

  comp <- RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- c(loader_project_id, loader_subject_id)
  comp$no_save <- id

  comp$ui_func <- function(id, value, depends){
    shiny::actionLink(inputId = id, label = label)
  }
  comp$server_func <- function(input, output, session){
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)

    shiny::bindEvent(
      safe_observe({

        # check if the module exists
        if(is.null(from_module)) {
          session_cache <- session_getopt(
            c("project_name", "subject_code"), default = NULL)
          project_name <- session_cache$project_name
          subject_code <- session_cache$subject_code
        } else {
          pipeline <- tryCatch({
            raveio::pipeline(pipeline_name = from_module)
          }, error = function(e){ NULL })
          if(is.null(pipeline)) { return() }
          project_name <- pipeline$get_settings(key = project_varname, default = NULL)
          subject_code <- pipeline$get_settings(key = subject_varname, default = NULL)
        }


        if(
          inherits(loader_project, "RAVEShinyComponent") &&
          length(project_name) == 1 && !is.na(project_name) &&
          is.character(project_name)
        ) {
          shiny::updateSelectInput(
            session = session,
            inputId = loader_project$get_sub_element_id(),
            selected = project_name
          )
        }
        if(
          inherits(loader_subject, "RAVEShinyComponent") &&
          length(subject_code) == 1 &&
          !is.na(subject_code) &&
          is.character(subject_code)
        ) {
          # get all subjects
          all_subcodes <- list.dirs(raveio::raveio_getopt("raw_data_dir"),
                                    full.names = FALSE, recursive = FALSE)
          all_subcodes <- all_subcodes[grepl("^[a-zA-Z0-9]", all_subcodes)]
          shiny::updateSelectInput(
            session = session,
            inputId = loader_subject$get_sub_element_id(),
            choices = all_subcodes,
            selected = subject_code
          )
        }

      }),
      comp$get_sub_element_input(),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

  }

  comp
}
