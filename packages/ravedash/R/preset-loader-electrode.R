

#' @rdname rave-ui-preset
#' @export
presets_loader_electrodes <- function(
  id = "loader_electrode_text",
  varname = "loaded_electrodes",
  label = "Electrodes",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code"
){
  comp <- RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- c(loader_project_id, loader_subject_id)

  comp$ui_func <- function(id, value, depends){
    shiny::textInput(
      inputId = id,
      label = label,
      placeholder = "E.g. 1-84,90-100",
      value = ""
    )
  }
  comp$server_func <- function(input, output, session){
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)

    get_subject <- loader_subject$get_tool("get_subject")

    shiny::bindEvent(
      observe({
        if(!loader_subject$sv$is_valid()){ return() }
        subject <- get_subject()
        if(is.null(subject)){ return() }

        # electrodes
        # check if subject is last input
        electrode_text <- dipsaus::deparse_svec(subject$electrodes)
        if(isTRUE(loader_subject$get_settings_value(use_cache = TRUE) == subject$subject_code)) {
          electrode_text <- comp$get_settings_value(default = electrode_text, use_cache = TRUE)
        }
        shiny::updateTextInput(
          session = session,
          inputId = id,
          value = electrode_text
        )

      }),
      loader_project$current_value,
      loader_subject$current_value,
      ignoreNULL = TRUE
    )

  }

  comp

}
