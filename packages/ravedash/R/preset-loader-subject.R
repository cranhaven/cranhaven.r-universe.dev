

#' @rdname rave-ui-preset
#' @export
presets_loader_subject <- function(
  id = "loader_subject_code", varname = "subject_code",
  label = "Subject",
  loader_project_id = "loader_project_name",
  checks = c("notch", "wavelet")
){
  force(checks)
  comp <- RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- loader_project_id

  comp$ui_func <- function(id, value, depends){
    shiny::selectInput(
      inputId = id,
      label = label,
      choices = value,
      selected = value,
      multiple = FALSE
    )
  }
  comp$server_func <- function(input, output, session){
    loader_project <- comp$get_dependent_component(loader_project_id)

    get_subject <- function(){
      project_name <- loader_project$current_value
      subject_code <- comp$current_value
      subject_id <- sprintf("%s/%s", project_name, subject_code)
      if(length(subject_id) != 1){
        return(NULL)
      }
      subject <- comp$container$get_cache("loader_subject_instance", default = NULL)
      if(!inherits(subject, "RAVESubject") ||
         !identical(subject$subject_id, subject_id)) {

        subject <- raveio::as_rave_subject(subject_id, strict = FALSE)
        comp$container$set_cache(key = "loader_subject_instance",
                                 value = subject, expire_after = 10)
      }
      return(subject)

    }

    shiny::bindEvent(
      observe({
        if(!loader_project$sv$is_valid()){ return() }
        project_name <- input[[loader_project$id]]
        project <- raveio::as_rave_project(project_name)
        all_subjects <- project$subjects()
        selected <- comp$get_settings_value(
          default = input[[comp$id]],
          constraint = all_subjects,
          use_cache = TRUE
        )

        shiny::updateSelectInput(session = session, inputId = comp$id,
                                 choices = all_subjects, selected = selected)
      }),
      input[[loader_project$id]],
      ignoreNULL = TRUE
    )

    comp$set_tool("get_subject", value = get_subject, server_needed = TRUE)

  }
  comp$add_rule(function(subject_code){
    if(length(subject_code) != 1 || is.na(subject_code) ||
       startsWith(subject_code, "_")){
      return("Invalid subject code.")
    }

    loader_project <- comp$get_dependent_component(loader_project_id)

    project_name <- loader_project$current_value
    if(is.null(project_name)){
      return("No project name found.")
    }
    subject_id <- sprintf("%s/%s", project_name, subject_code)
    subject <- comp$container$get_cache("loader_subject_instance", default = NULL)
    if(!inherits(subject, "RAVESubject") ||
       !identical(subject$subject_id, subject_id)) {

      subject <- raveio::as_rave_subject(subject_id, strict = FALSE)
      comp$container$set_cache(key = "loader_subject_instance",
                               value = subject, expire_after = 10)
    }


    if(!dir.exists(subject$path)){
      return("Subject directory is broken or missing.")
    }
    if("notch" %in% checks) {
      if(!any(subject$preprocess_settings$notch_filtered)) {
        return("Please run notch filter on this subject first.")
      }
    }
    if("wavelet" %in% checks) {
      if(!any(subject$preprocess_settings$has_wavelet)) {
        return("Please run wavelet on this subject first.")
      }
    }
    if("3dviewer" %in% checks) {
      fspath <- subject$freesurfer_path
      if(length(fspath) != 1 || is.na(fspath) || !isTRUE(dir.exists(fspath))) {
        return("Please generate or import surface/volume reconstruction first.")
      }
    }

    return(NULL)
  })

  comp

}


#' @rdname rave-ui-preset
#' @export
presets_loader_subject_only <- function(
    id = "loader_subject_code",
    varname = "subject_code",
    label = "Subject",
    multiple = FALSE
){
  force(multiple)
  comp <- RAVEShinyComponent$new(id = id, varname = varname)

  comp$ui_func <- function(id, value, depends){
    choices <- list.dirs(raveio::raveio_getopt("raw_data_dir"),
                         full.names = FALSE, recursive = FALSE)
    choices <- choices[grepl("^[a-zA-Z][a-zA-Z0-9_-]{0,}$", choices)]

    if(multiple) {
      value <- value[value %in% choices]
    } else {
      value <- value %OF% choices
    }

    shiny::selectInput(
      inputId = id,
      label = label,
      choices = choices,
      selected = value,
      multiple = multiple
    )
  }
  comp
}

