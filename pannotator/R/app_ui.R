#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  #call function in app_config.R to see if a project file was passed in on run_app()
  was_projectSettingsFile_passed_in()

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    # Your application UI logic
    fluidPage(
      br(),
      fluidRow(
        column(myEnv$config$mapPanelWidth, wellPanel(
          tags$h4("Mapping Panel", style = "font-size: 13px; text-align: center; margin: 0;"), id="map_panel", mod_leaflet_map_ui("leaflet_map"))),
        column(myEnv$config$panoPanelWidth, wellPanel(tags$h4("Image Panel", style = "font-size: 13px; text-align: center; margin: 0;"), id="image_panel", mod_360_image_ui("pano360_image"))),
        column(myEnv$config$formPanelWidth, wellPanel(tags$h4("Annotation Panel", style = "font-size: 13px; text-align: center; margin: 0;"), id="form_panel",  style = "padding: 20px;", mod_control_form_ui("control_form"), div(id = "add_here"), ))
      )
    )
  )

}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  add_resource_path(
    "temp_dir",
    tempdir() #tools::R_user_dir("pannotator")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "pannotator"
    )
  )
}



was_projectSettingsFile_passed_in <- function() {

  # check if a projectSettingsFile is passed in with run_app() ie. run_app(projectSettingsFile = "C:/E/test-project.yml")
  projectOptions <- golem::get_golem_options()
  #View(projectOptions)
  #if(length(projectOptions) > 0){
  if (!is.null(projectOptions$projectSettingsFile)) {
    #print("project SettingsFile passed in:")
    #print(projectOptions$projectSettingsFile)

    rm(list = ls(envir = myEnv), envir = myEnv)
    myEnv$config_dir <- normalizePath(dirname(projectOptions$projectSettingsFile))
    myEnv$project_config_file <- normalizePath(file.path(projectOptions$projectSettingsFile))
    #print(myEnv$project_config_file)
    #config_file <- file.path(config_dir, "config.yml")
    #print(config_dir)
    r$config  <- configr::read.config(myEnv$project_config_file)
    myEnv$config <- configr::read.config(myEnv$project_config_file)

    #print(normalizePath(myEnv$config$projectFolder))
    myEnv$data_dir <- normalizePath(myEnv$config$projectFolder)

    # List all PDF files in the directory
    #pdf_files <- list.files(app_sys("app/www"), pattern = "\\.pdf$", full.names = TRUE)
    # Delete each PDF file
    #sapply(pdf_files, file.remove)
    # copy help files to www location for linking in the browser
    for (i in 1:4) {
      lookupFile <- paste0("lookup", i, "HelpFile")
      destFile <- paste0("help",i,".pdf")
      fromPath <- normalizePath(file.path(myEnv$data_dir, myEnv$config[[lookupFile]]))
      #toPath <- normalizePath(file.path(app_sys("app/www"), myEnv$config[[lookupFile]]))
      toPath <- normalizePath(file.path(tempdir(), destFile))
      #print(toPath)
      file.copy(fromPath, toPath, overwrite = TRUE)
    }
  }

}

#TODO need to come back tot his and sort out checking whether projectSettingsFile was passed in order

#' @noRd
.onLoad <- function(libname, pkgname) {
  #print("on load called")
  # Call this function during the app's initialization
  initialize_config()

  rm(list = ls(envir = myEnv), envir = myEnv)
  myEnv$config_dir <- normalizePath(tools::R_user_dir("pannotator", which = "config"))
  myEnv$data_dir <- normalizePath(tools::R_user_dir("pannotator", which = "data"))
  myEnv$project_config_file <- normalizePath(file.path(myEnv$config_dir, "default-project-config.yml"))

  r$config  <- configr::read.config(myEnv$project_config_file)
  myEnv$config <- configr::read.config(myEnv$project_config_file)

  # List all PDF files in the directory
  pdf_files <- list.files(tempdir(), pattern = "\\.pdf$", full.names = TRUE)
  # Delete each PDF file
  sapply(pdf_files, file.remove)

  # copy help files to temp location for linking in the browser
  for (i in 1:4) {
    lookupFile <- paste0("lookup", i, "HelpFile")
    fromPath <- normalizePath(file.path(myEnv$data_dir, "/", myEnv$config[[lookupFile]]))
    toPath <- file.path(tempdir(), myEnv$config[[lookupFile]])
    file.copy(fromPath, toPath, overwrite = TRUE)
  }
  #print(tempdir())
}
