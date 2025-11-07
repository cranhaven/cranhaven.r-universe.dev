#' UI function for single module dashboard
#'
#' @param request shiny request object
#' @param id Module id
#' @param tab_item Tab item UI
#'
#' @return dashboardPage UI
single_module_ui <- function(request, id, tab_item) {

  DB_header <- shinydashboardPlus::dashboardHeader(
    title = id
  )
  DB_sidebar <- shinydashboardPlus::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tablist",
      shinydashboard::menuItem(id, tabName = id, selected = TRUE)
    )
  )
  DB_body <- shinydashboard::dashboardBody(
    htmltools::tags$script(src = "https://kit.fontawesome.com/01cb805c1e.js"),
    shinydashboard::tabItems(
      tab_item
    )
  )

  if (!requireNamespace("waiter", quietly = TRUE)) {
    rlang::inform("Install the waiter package to get a spinner for loading")
    shinydashboardPlus::dashboardPage(
      DB_header,
      DB_sidebar,
      DB_body#,
      #DB_rightsidebar,
      #freshTheme = NCO_theme,
      #preloader = list(
      #  html = htmltools::tagList(waiter::spin_1(), "Loading ..."),
      #  color = "#367fa9"
      #)
    )
  } else {


    shinydashboardPlus::dashboardPage(
      DB_header,
      DB_sidebar,
      DB_body,
      #DB_rightsidebar,
      #freshTheme = NCO_theme,
      preloader = list(
        html = htmltools::tagList(waiter::spin_1(), "Loading ..."),
        color = "#367fa9"
      )
    )
  }
}
