#' Run the Shiny Application
#'
#' `run_app()` starts the Shiny application contained in this package, with an option to load settings from a YAML file using additional option: projectSettingsFile = "pathToYamlFile".
#'
#' @details This function initializes and runs the Shiny app developed with the golem framework.
#' It optionally loads application-specific settings from a YAML file, which can be useful for project-specific configurations.
#'
#' @param ... Additional arguments to pass to `golem_opts`. See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @return No return value, called for side effects. Launches the Shiny app.
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#'
#' @examples
#' if (interactive()) {
#'   # Example: Run the application with default settings
#'   run_app()
#'
#'   # Example: Run the application with a specific YAML configuration file
#'   run_app(projectSettingsFile = "C:/test-project.yml")
#' }
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
