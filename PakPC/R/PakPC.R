#' @title PakPC
#'
#' @description Shiny App to Analyze Pakistan's Population Census Data
#'  
#' @return Pivot tables and graphs of PakPC2017 and PakPC2023 R packages data
#' 
#' @import htmltools magrittr PakPC2017 PakPC2023 rpivotTable shiny
#' @importFrom DT DTOutput datatable formatCurrency renderDT
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboardPlus box dashboardHeader dashboardSidebar dashboardPage messageItem notificationItem taskItem 
#' @examples
#' if(interactive()) {
#'     library(PakPC)
#'     PakPC()
#'   }
#' 
#' @export

PakPC <- function() {
  appDir <- system.file("shinyapp", package = "PakPC")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `PakPC`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}