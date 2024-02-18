

#' @rdname rave-ui-preset
#' @export
presets_loader_project <- function(
  id = "loader_project_name", varname = "project_name",
  label = "Project"
){

  comp <- RAVEShinyComponent$new(id = id, varname = varname)

  comp$ui_func <- function(id, value, depends){
    choices <- raveio::get_projects(refresh = FALSE)
    shiny::selectInput(
      inputId = id,
      label = label,
      choices = choices,
      selected = value %OF% choices,
      multiple = FALSE
    )
  }
  comp$add_rule(function(value){
    if(length(value) != 1 || is.na(value)){
      return("Missing project name. Please choose one.")
    }
    project <- raveio::as_rave_project(value, strict = FALSE)
    if(!dir.exists(project$path)){
      return(raveio::glue("Cannot find path to project `{value}`"))
    }
    return(NULL)
  })

  comp

}


