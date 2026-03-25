#' @title PakNAcc
#'
#' @description Shiny App for National Accounts
#' 
#' @return Pivot tables and graphs for Pakistan's Quarterly National Accounts data
#' 
#' @import collapse  htmltools magrittr rpivotTable shiny
#' @importFrom DT DTOutput datatable formatCurrency renderDT
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboardPlus box dashboardHeader dashboardSidebar dashboardPage messageItem notificationItem taskItem 
#' @examples
#' if(interactive()) {
#'     library(PakNAcc)
#'     PakNAcc()
#'   }
#' 
#' @export

PakNAcc <- function() {
  appDir <- system.file("shinyapp", package = "PakNAcc")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `PakNAcc`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}