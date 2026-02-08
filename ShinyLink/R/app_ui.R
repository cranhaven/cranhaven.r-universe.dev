#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyWidgets prettyToggle prettySwitch prettyCheckboxGroup
#' @importFrom shinyWidgets actionBttn switchInput awesomeRadio materialSwitch
#' @importFrom shinyWidgets prettyRadioButtons sliderTextInput checkboxGroupButtons chooseSliderSkin
#' @importFrom shinydashboard dropdownMenu sidebarMenu menuItem menuSubItem dashboardBody tabItems tabItem
#' @importFrom shinydashboardPlus dashboardPage dashboardSidebar box dashboardHeader taskItem dropdownBlock descriptionBlock boxPad dashboardFooter dashboardControlbar skinSelector
#'
#' @noRd
app_ui <- function(request) {
  if (!requireNamespace("shinybusy", quietly = TRUE)) {
    stop("Package 'shinybusy' is required. Please install it.")
  }
  if (!requireNamespace("shinyjs", quietly = TRUE)) {
    stop("Package 'shinyjs' is required. Please install it.")
  }
  if (!requireNamespace("waiter", quietly = TRUE)) {
    stop("Package 'waiter' is required. Please install it.")
  }
  tagList(
    shinybusy::add_busy_bar(color = "#00a65a", height = "8px" ),

    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
      preloader = list(html = tagList(waiter::spin_1(), "Loading ..."), color = "#3c8dbc"),
      options = list(sidebarExpandOnHover = TRUE),
      # controlbar = dashboardControlbar(),
      controlbar = dashboardControlbar(collapsed = TRUE, skinSelector()),
      title = "ShinyLink",
      # Header -----------------------------------------------------------------
      header = dashboardHeader(
        title = "ShinyLink Ver 0.5.4",
        leftUi = tagList(
          dropdownBlock(
            id = "mydropdown",
            title = "Global options",
            icon = icon("server"),
            sliderInput(
              inputId = "n",
              label = "Number of observations",
              min = 10,
              max = 100,
              value = 30
            ),
            prettyToggle(
              inputId = "na",
              label_on = "NAs kept",
              label_off = "NAs removed",
              icon_on = icon("check"),
              icon_off = icon("trash-can")
            )
          ),
          dropdownBlock(
            id = "mydropdown3",
            title = "Advanced options",
            icon = icon("sliders"),
            prettySwitch(
              inputId = "switch5",
              label = "Fill switch with status:",
              fill = TRUE,
              status = "primary"
            ),
            prettyCheckboxGroup(
              inputId = "checkgroup6",
              label = "Click me!",
              thick = TRUE,
              choices = c("Click me !", "Me !", "Or me !"),
              animation = "pulse",
              status = "info"
            )
          ),
          dropdownBlock(
            id = "mydropdown2",
            title = "Output options",
            icon = icon("list-check"),
            prettySwitch(
              inputId = "switch4",
              label = "Fill switch with status:",
              fill = TRUE,
              status = "primary"
            ),
            prettyCheckboxGroup(
              inputId = "checkgroup2",
              label = "Click me!",
              thick = TRUE,
              choices = c("Click me !", "Me !", "Or me !"),
              animation = "pulse",
              status = "info"
            )
          )
        ),
        dropdownMenu(
          # The `name` provided ('tasks') is deprecated in Font Awesome 6:
          # consider using 'bars-progress' or 'fas fa-bars-progress' instead
          # This needs to be addressed in the dependency "shinydashboardPlus"
          type = "tasks",
          icon = icon("bars-progress"),
          badgeStatus = "danger",
          taskItem(value = 20, color = "aqua", "Refactor code"),
          taskItem(value = 40, color = "green", "Design new layout"),
          taskItem(value = 60, color = "yellow", "Another task"),
          taskItem(value = 80, color = "red", "Write documentation")
        )
      ),

      # Sidebar ----------------------------------------------------------------
      sidebar = dashboardSidebar(
        minified = TRUE,
        collapsed = FALSE,

        sidebarMenu(
          id = "tabs",
          menuItem(
            "Data Uploading",
            tabName = "upload",
            icon = icon("upload")
          ),
          menuItem(
            "Data Cleaning",
            icon = icon("brush"),
            startExpanded = TRUE,
            menuSubItem(
              "Remove Duplicate Rows",
              tabName = "duplicate",
              icon = icon("copy")
            ),
            menuSubItem(
              "Assign Variables",
              tabName = "assignment",
              icon = icon("pen")
            ),
            menuSubItem(
              "Recode Race & Gender",
              tabName = "gender_race",
              icon = icon("venus-mars")
            ),
            menuSubItem(
              "Format Dates",
              tabName = "date_format",
              icon = icon("calendar")
            )
          ),
          menuItem(
            "Simple Match",
            icon = icon("link"),
            startExpanded = TRUE,
            menuSubItem(
              "Matching Settings",
              tabName = "simple_settings",
              icon = icon("gear")
            ),
            menuSubItem(
              "Matching Results",
              tabName = "simple_results",
              icon = icon("square-poll-vertical")
            ),
            menuSubItem(
              "Matching Details",
              tabName = "simple_details",
              icon = icon("download")
            ),
            menuSubItem(
              "Manual Inspection",
              tabName = "manual_inspection",
              icon = icon("user-plus")
            )
          ),
          menuItem(
            "Advanced Match",
            icon = icon("microchip"),
            startExpanded = TRUE,
            menuSubItem(
              "Advanced Settings",
              tabName = "advanced_parameters",
              icon = icon("sliders")
            ),
            menuSubItem(
              "Matching Details",
              tabName = "advanced_details",
              icon = icon("download")
            ),
            menuSubItem(
              "Impute Missing Gender ",
              tabName = "imputation",
              icon = icon("circle-question")
            )
            # menuSubItem(
            #   "Matching Results",
            #   tabName = "advanced_results",
            #   icon = icon("hard-drive")
            # ),
          )
        )
        # mod_sidebar_ui("mod_sidebar") # Not as a sidebar module for now
        # In the module other sub-module like duplicate can not using 'updateTabItems'
        # to access the namespace to update the tabs.
      ),
      # Body -------------------------------------------------------------------
      body = dashboardBody(
        shinyjs::useShinyjs(),
        tabItems(
          tabItem(tabName = "upload",
                  mod_uploading_ui("uploading")),
          tabItem(tabName = "duplicate",
                  mod_cleaning_duplicate_ui("cleaning_duplicate")),
          tabItem(tabName = "assignment",
                  mod_cleaning_assignment_ui("cleaning_assignment")),
          tabItem(tabName = "gender_race",
                  mod_cleaning_gender_ui("cleaning_gender")),
          tabItem(tabName = "date_format",
                  mod_cleaning_date_ui("cleaning_date")),
          tabItem(tabName = "simple_settings",
                  mod_simple_settings_ui("simple_settings")),


          tabItem(tabName = "simple_results",
                  mod_simple_results_ui("simple_results")),
          tabItem(tabName = "manual_inspection",
                  mod_manual_inspection_ui("manual_inspection")),


          tabItem(tabName = "simple_details",
                  mod_simple_details_ui("simple_details")),
          tabItem(tabName = "advanced_parameters",
                  mod_advanced_parameters_ui("advanced_parameters")),
          # tabItem(tabName = "advanced_results",
          #         mod_advanced_results_ui("advanced_results")),
          tabItem(tabName = "advanced_details",
                  mod_advanced_details_ui("advanced_details")),
          tabItem(tabName = "imputation",
                  mod_cleaning_imputation_ui("cleaning_imputation"))
      )),

      # Footer -----------------------------------------------------------------
      footer = dashboardFooter(right = "Nelson Scientific Labs, LLC., 2023")

      # End of UI --------------------------------------------------------------
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
  add_resource_path("www", app_sys("app/www"))

  tags$head(favicon(),
            bundle_resources(path = app_sys("app/www"),
                             app_title = "ShinyLink - Record Linkage ")
            # Add here other external resources
            # for example, you can add
            # shinyalert::useShinyalert()
            )
}
