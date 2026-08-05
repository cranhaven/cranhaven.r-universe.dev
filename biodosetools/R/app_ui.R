#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    dashboardPage(
      title = "Biodose Tools",
      skin = "purple",
      header = dashboard_header(),
      sidebar = dashboard_sidebar(),
      body = dashboard_body()
    )
  )
}

#' The dashboard header
#'
#' @import shiny shinydashboard
#' @noRd
dashboard_header <- function() {
  dashboardHeader(
    title = span(
      img(src = "www/icon_small.svg", height = 35),
      "Biodose Tools"
    ),
    tags$li(
      a(
        icon("github"),
        height = 40,
        href = "https://github.com/biodosetools-team/biodosetools",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  )
}

#' The dashboard sidebar
#'
#' @import shiny shinydashboard
#' @noRd
dashboard_sidebar <- function() {
  dashboardSidebar(
    # About section
    sidebarMenu(
      menuItem(
        text = "About this app",
        tabName = "home",
        icon = icon("home"),
        selected = TRUE
      )
    ),

    # Aberration assays section
    p(class = "menu-title", "ABERRATION ASSAYS"),
    sidebarMenu(

      # Dicentrics
      menuItem(
        text = "Dicentrics",
        tabName = "menu-item-dicent",
        icon = icon("dna"),
        startExpanded = FALSE,
        # Modules
        menuItem(
          text = "Characteristic limits",
          tabName = "tab-limits-dicent",
          icon = icon("circle-radiation")
        ),
        menuItem(
          text = "Fitting",
          tabName = "tab-fitting-dicent",
          icon = icon("cog"),
          selected = FALSE
        ),
        menuItem(
          text = "Dose estimation",
          icon = icon("calculator"),
          # Sub-modules
          menuItem(
            text = "Standard",
            tabName = "tab-estimation-dicent",
            icon = icon("radiation")
          ),
          menuItem(
            text = "Criticality accidents",
            tabName = "tab-mixed-fields",
            icon = icon("mix")
          )
        )
      ),


      # Translocations
      menuItem(
        text = "Translocations",
        tabName = "menu-item-trans",
        icon = icon("palette"),
        # Modules
        menuSubItem(
          text = "Fitting",
          tabName = "tab-fitting-trans",
          icon = icon("cog")
        ),
        menuSubItem(
          text = "Dose estimation",
          tabName = "tab-estimation-trans",
          icon = icon("calculator")
        )
      ),
      # Micronuclei
      #if (golem::app_dev()) {
      menuItem(
        text = "Micronuclei",
        tabName = "menu-item-micro",
        icon = icon("dot-circle"),
        # Modules
        menuItem(
          text = "Fitting",
          tabName = "tab-fitting-micro",
          icon = icon("cog")
        ),
        menuItem(
          text = "Dose estimation",
          tabName = "tab-estimation-micro",
          icon = icon("calculator")
        )
      ),

      # Other tools section
   p(class = "menu-title", "OTHER TOOLS"),
      menuItem(
        text = "Interlab comparison",
        tabName = "tab-interlab",
        icon = icon("share")
      )
      # menuItem(
      #   text = "Multiple donors",
      #   tabName = "tab-compare",
      #   icon = icon("person"),
      #   badgeLabel = "wip",
      #   badgeColor = "green"
      # )
    ),

    p(
      class = "sticky-footer",
      a(
        paste("Version", utils::packageVersion(pkg = "biodosetools")),
        href = "https://github.com/biodosetools-team/biodosetools/blob/master/NEWS.md"
      )
    )
  )
}

#' The dashboard home page
#'
#' @import shiny shinydashboard
#' @noRd
dashboard_home <- function() {
  tabItem(
    tabName = "home",
    div(
      style = "padding-bottom: 20px;",
      h2("About this project", style = "margin-left: 10%;"),
      {
        md_path <- system.file("app/www/about_body.md", package = "biodosetools")
        tmp_md <- file.path(tempdir(), "about_body.md")
        file.copy(md_path, tmp_md, overwrite = TRUE)
        includeMarkdown(tmp_md)
      },

      # Buttons
      div(
        style = "margin-left: 10%;",

        # GitHub icon
        actionButton(
          inputId = "github_link",
          label = "Source code",
          icon = icon("github"),
          class = "home-button",
          onclick = "window.open('https://github.com/biodosetools-team/biodosetools/', '_blank')"
        ),
        widget_sep(),

        # Wiki/Documentation icon
        actionButton(
          inputId = "wiki_link",
          label = "Documentation",
          icon = icon("book"),
          class = "home-button",
          onclick = "window.open('https://biodosetools-team.github.io/documentation/', '_blank')"
        ),
        widget_sep(),

        # Report bugs
        actionButton(
          inputId = "wiki_link",
          label = "Report issue",
          icon = icon("bug"),
          class = "home-button",
          onclick = "window.open('https://github.com/biodosetools-team/biodosetools/issues/new/', '_blank')"
        )
      )
    )
  )
}

#' The dashboard body
#'
#' @import shiny shinydashboard
#' @noRd
dashboard_body <- function() {
  dashboardBody(
    tabItems(
      dashboard_home(),

      # Modules
      mod_fitting_dicent_ui(id = "fitting_dicent_ui", label = "tab-fitting-dicent"),
      mod_estimation_dicent_ui(id = "estimation_dicent_ui", label = "tab-estimation-dicent"),
      mod_fitting_trans_ui(id = "fitting_trans_ui", label = "tab-fitting-trans"),
      mod_estimation_trans_ui(id = "estimation_trans_ui", label = "tab-estimation-trans"),
      mod_fitting_micro_ui(id = "fitting_micro_ui", label = "tab-fitting-micro"),
      mod_estimation_micro_ui(id = "estimation_micro_ui", label = "tab-estimation-micro"),

      mod_mixed_ui(id = "mixed_ui", label = "tab-mixed-fields"),#new
      mod_limits_dicent_ui(id = "limits_dicent_ui", label = "tab-limits-dicent"),#new
      mod_interlab_ui(id = "interlab_ui", label = "tab-interlab")#new
      #mod_compare_ui(id = "compare_ui", label = "tab-compare")#new

    ),
        tags$head(tags$style(HTML('
             /* tabBox background */
             .nav-tabs-custom>.nav-tabs {
                 background-color: #DCDEE0;
             }
          .nav-tabs-custom > .nav-tabs > li.header {
              font-size: 15px;
              color: black;
              font-weight:bold;
          }
                                  ')))
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
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(ico = "favicon", ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Biodose Tools"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
