#' Start voiceR Shiny App
#'
#' Launches the voiceR Shiny app, providing the opportunity to analyze multiple audio files via a graphical user interface (no-code interface).
#' @return This function launches the voiceR Shiny app.
#' @examples
#' if(interactive()){
#'    voiceRApp()
#' }
#'
#'
#' @importFrom shiny runApp withProgress incProgress downloadHandler reactiveValues reactive renderText observeEvent
#' @importFrom shinyjs html hide show
#' @importFrom shinyFiles getVolumes shinyDirChoose
#' @importFrom stringr str_remove_all str_split str_detect str_extract str_replace
#' @importFrom tuneR readMP3 readWave
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom soundgen analyze
#' @importFrom seewave duration rms env zapsilw
#' @importFrom DT renderDataTable
#' @importFrom plotly ggplotly renderPlotly plot_ly layout subplot
#' @importFrom ggplot2 ggplot_build ylim labs
#' @importFrom ggthemes theme_fivethirtyeight
#' @importFrom rmarkdown render html_document
#' @export
voiceRApp <- function() {
  #Get app directory for the shiny app
  appDir <- system.file("AppShiny", package = "voiceR")
  #If appDir is blank throw error
  if (appDir == "") {
    stop("Could not find shiny-app directory. Try re-installing `voiceR`.", call. = FALSE)
  }
  #run the shiny app
  runApp(appDir, display.mode = "normal")
}
