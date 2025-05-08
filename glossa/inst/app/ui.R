# Header ----
header <- bs4Dash::bs4DashNavbar(
  # * Logo and header options ----
  title = bs4Dash::dashboardBrand(
    title = strong("GLOSSA"),
    color = NULL,
    href = "https://iMARES-group.github.io/glossa/",
    image = "logo_glossa.png",
    opacity = 1
  ),

  # Logo on browser window
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/logo_glossa.png")),


  # Header options
  titleWidth = NULL,
  fixed = TRUE, # Fix navbar to top
  skin = "light",
  status = "white",
  border = TRUE,
  compact = FALSE,
  sidebarIcon = icon("bars"),

  # * Right ui buttons ----
  rightUi = tagList(
    # New analysis button
    tags$li(
      class = 'dropdown',
      bs4Dash::actionButton(
        inputId = "new_analysis_header",
        label = "New analysis",
        icon = icon("circle-plus"),
        status = "primary",
        outline = FALSE
      )
    ),

    # Help button
    tags$li(
      class = 'dropdown',
      bs4Dash::actionButton(
        inputId = "show_help",
        label = NULL,
        icon = icon("question"),
        status = "secondary",
        outline = TRUE,
        style = "border: none"
      )
    ),

    # Stop app button
    tags$li(
      class = 'dropdown',
      bs4Dash::actionButton(
        inputId = "stop_app",
        label = NULL,
        icon = icon("power-off"),
        status = "danger",
        outline = TRUE,
        style = "border: none",
        onclick = "setTimeout(function(){window.close();},500);"
      )
    )
  )
)

# Sidebar ----
sidebar <- bs4Dash::bs4DashSidebar(
  # Sidebar options
  skin = "light",
  status = "primary",
  collapsed = FALSE,
  minified = TRUE,
  expandOnHover = TRUE,
  fixed = TRUE,
  id = "sidebar",
  customArea = NULL,

  # * Sidebar tabs ----
  bs4Dash::sidebarMenu(
    id = "sidebar_menu",

    # Home tab
    bs4Dash::menuItem(
      tabName = "home",
      icon = icon("house"),
      text = strong("Home")
    ),

    bs4Dash::sidebarHeader("Modelling"),
    # New Analysis tab
    bs4Dash::menuItem(
      tabName = "new_analysis",
      icon = icon("circle-plus", class = "fa-solid"),
      text = "New analysis"
    ),

    # Results tab
    bs4Dash::menuItem(
      tabName = "reports",
      icon = icon("chart-simple"),
      text = "Reports"
    ),

    # Export tab
    bs4Dash::menuItem(
      tabName = "exports",
      icon = icon("download"),
      text = "Exports"
    ),

    bs4Dash::sidebarHeader("Resources"),
    # Documentation tab
    bs4Dash::menuItem(
      tabName = "documentation",
      icon = icon("book"),
      text = "Documentation"
    ),

    # How to cite tab
    bs4Dash::menuItem(
      tabName = "how_to_cite",
      icon = icon("bookmark", class = "fa-solid"),
      text = "How to cite"
    ),

    # Updates tab
    bs4Dash::menuItem(
      tabName = "updates",
      icon = icon("bell", class = "fa-solid"),
      text = "Lastest updates"
    ),

    # Contact tab
    bs4Dash::menuItem(
      tabName = "contact",
      icon = icon("envelope", class = "fa-solid"),
      text = "Contact"
    )
  )
)

# Body tab ----
body <- bs4Dash::bs4DashBody(
  waiter::useWaiter(), # include waiter dependencies

  bs4Dash::tabItems(
    # * home tab ----
    bs4Dash::tabItem(
      tabName = "home",
      # Start first row
      fluidRow(
        #  ** Welcome box ----
        bs4Dash::box(
          title = strong("Welcome to GLOSSA"),
          status = NULL,
          width = 6,
          height = 250,
          solidHeader = FALSE,
          background = NULL,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = icon("handshake", class = "fa-regular", style = "font-size:2rem"),

          fluidRow(
            # welcome text
            bs4Dash::column(
              width = 6,
              "Welcome to GLOSSA (Global Species Spatiotemporal Analysis). Explore species distributions worldwide, from past to future, across diverse climate scenarios."
            ),
            # welcome figure
            bs4Dash::column(
              width = 6,
              align = "center",
              img(src = "logo_glossa.png", height = "130px")
            )
          ),

          fluidRow(
            div(
              style = "position:absolute; bottom:10px; left:50%; transform: translateX(-50%); width:100%",
              bs4Dash::column(
                width = 12,
                align = "center",
                bs4Dash::actionButton(
                  inputId = "new_analysis_home",
                  label = "New analysis",
                  icon = icon("circle-plus"),
                  status = "primary",
                  outline = TRUE,
                  width = "100%"
                )
              )
            )
          )
        ), # End welcome box

        # ** Demo box ----
        bs4Dash::box(
          title = strong("Getting started with GLOSSA"),
          status = NULL,
          width = 3,
          height = 250,
          solidHeader = FALSE,
          background = "lightblue",
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = icon("rocket", style = "font-size:2rem"),

          fluidRow(
            bs4Dash::column(
              width = 12,
              "New to GLOSSA? Visit our Getting Started page for a quick and easy introduction. Learn how to model species distribution with GLOSSA in just a few steps!"
            )
          ),

          fluidRow(
            div(
              style = "position:absolute; bottom:10px; left:50%; transform: translateX(-50%); width:100%",
              bs4Dash::column(
                width = 12,
                align = "center",
                bs4Dash::actionButton(
                  inputId = "try_demo",
                  label = "Get started",
                  icon = NULL,
                  status = "secondary",
                  outline = FALSE,
                  width = "100%",
                  onclick ="window.open('https://iMARES-group.github.io/glossa/', '_blank')"
                )
              )
            )
          )
        ), # End demo box

        # ** Group box ----
        bs4Dash::box(
          title = strong("Meet our research team"),
          status = NULL,
          width = 3,
          height = 250,
          solidHeader = FALSE,
          background = "purple",
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = icon("people-group", style = "font-size:2rem"),

          fluidRow(
            bs4Dash::column(
              width = 12,
              "Discover the team behind GLOSSA at the iMARES group, ICM-CSIC in Barcelona, Spain. Learn more about our research, and connect with our team. Click below to discover more about our group."
            )
          ),

          fluidRow(
            div(
              style = "position:absolute; bottom:10px; left:50%; transform: translateX(-50%); width:100%",
              bs4Dash::column(
                width = 12,
                align = "center",
                bs4Dash::actionButton(
                  inputId = "know_the_group",
                  label = "About us",
                  icon = NULL,
                  status = "secondary",
                  outline = FALSE,
                  width = "100%",
                  onclick ="window.open('https://imares.science/', '_blank')"
                )
              )
            )
          )
        ) # End group box
      ), # End first row

      # Start second row
      fluidRow(
        # ** Documentation and guidelines ----
        bs4Dash::box(
          title = strong("Documentation and guidelines"),
          status = NULL,
          width = 6,
          height = 180,
          solidHeader = FALSE,
          background = NULL,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = icon("book", style = "font-size:2rem"),

          fluidRow(
            bs4Dash::column(
              width = 12,
              "Curious about data preparation, result exportation, and our modeling process? Explore our comprehensive GLOSSA documentation. Discover step-by-step tutorials and example datasets to make the most of it. Here, you’ll find the most updated reference to GLOSSA."
            )
          ),

          fluidRow(
            div(
              style = "position:absolute; bottom:10px; left:50%; transform: translateX(-50%); width:100%",
              bs4Dash::column(
                width = 12,
                align = "center",
                bs4Dash::actionButton(
                  inputId = "documentation_and_guidelines",
                  label = "Explore documentation",
                  icon = NULL,
                  status = "primary",
                  outline = TRUE,
                  width = "100%",
                  onclick ="window.open('https://imares-group.github.io/pages/documentation/', '_blank')"
                )
              )
            )
          )
        ), # End documentation and guidelines

        # ** Download data tutorials ----
        bs4Dash::box(
          title = strong("Preparing your data"),
          status = NULL,
          width = 6,
          height = 180,
          solidHeader = FALSE,
          background = NULL,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = icon("globe", style = "font-size:2rem"),

          fluidRow(
            bs4Dash::column(
              width = 12,
              "Prepare your data in the correct format for GLOSSA. We provide examples using GBIF for occurrence data and ISIMIP for environmental layers. Follow our guidelines to ensure smooth integration with the GLOSSA modeling framework."
            )
          ),

          fluidRow(
            div(
              style = "position:absolute; bottom:10px; left:50%; transform: translateX(-50%); width:100%",
              bs4Dash::column(
                width = 12,
                align = "center",
                fluidRow(
                  bs4Dash::column(
                    width = 6,
                    bs4Dash::actionButton(
                      inputId = "tutorial_1",
                      label = "Prepare species occurrences",
                      icon = NULL,
                      status = "primary",
                      outline = TRUE,
                      width = "100%",
                      onclick ="window.open('https://imares-group.github.io/glossa/pages/tutorials_examples/', '_blank')"
                    )
                  ),
                  bs4Dash::column(
                    width = 6,
                    bs4Dash::actionButton(
                      inputId = "tutorial_2",
                      label = "Prepare environmental data",
                      icon = NULL,
                      status = "primary",
                      outline = TRUE,
                      width = "100%",
                      onclick ="window.open('https://imares-group.github.io/glossa/pages/tutorials_examples/', '_blank')"
                    )
                  )
                )
              )
            )
          )
        ) # End download data tutorials box
      ) # End second row
    ),

    # * New Analysis tab ----
    bs4Dash::tabItem(
      tabName = "new_analysis",

      # Data upload and visualization
      fluidRow(
        bs4Dash::column(
          width = 6,
          fluidRow(
            id = "data_upload",
            bs4Dash::box(
              title = strong("Data upload"),
              status = NULL,
              width = 12,
              height = NULL,
              solidHeader = FALSE,
              background = NULL,
              maximizable = FALSE,
              collapsible = FALSE,
              headerBorder = FALSE,
              elevation = 2,
              label = shiny::actionButton(
                "data_upload_info",
                label = NULL,
                icon = icon("circle-info", class = "fa-solid fa-circle-info", style = "color:#007bff;"),
                class = "btn btn-default action-button btn-xs",
                style="background-color:transparent;border-radius:0px;border-width:0px"
              ),

              # ** Sidebar options ----
              # Advanced options
              sidebar = bs4Dash::boxSidebar(
                startOpen = FALSE,
                id = "advanced_options_sidebar",
                background = "#6c757d",
                icon = NULL,

                strong("Occurrences thinning"),

                tags$head(tags$style(HTML(".not_bold label {font-weight:normal !important;;}"))),
                div(numericInput(
                  inputId = "decimal_digits",
                  label = "Rounding precision",
                  value = NULL, min = 0, step = 1, width = "90%"), class="not_bold"),

                strong("Layers processing"),

                shinyWidgets::prettySwitch(
                  inputId = "scale_layers",
                  label = "Standardize covariates",
                  status = "primary",
                  fill = TRUE
                ),

                strong("Polygon processing"),

                div(numericInput(
                  inputId = "buff_poly",
                  label = tags$span("Enlarge polygon (buffer arc degrees)", shiny::actionButton("preview_buff_poly", label = NULL, icon = icon("circle-play", class = "fa-solid fa-circle-play", style = "color:#efefef;"), class = "btn btn-default action-button btn-s", style="background-color:transparent;border-radius:0px;border-width:0px")),
                  value = NULL, min = 0, max = 10, step = 100, width = "90%"), class="not_bold"),

                shinyWidgets::pickerInput(
                  inputId = "model_choice",
                  label = "Model",
                  choices = c("BART"),
                  selected = "BART",
                  width = "90%"
                ),

                numericInput(
                  inputId = "seed",
                  label = "Set a seed",
                  value = NULL,
                  width = "90%"
                )
              ), # End of sidebar options

              # ** Data upload ----
              fluidRow(
                bs4Dash::column(
                  width = 3,
                  glossa::file_input_area_ui(
                    "pa_files",
                    label = "Occurrences",
                    button_label = "Add CSV files",
                    multiple = TRUE,
                    accept = c(".csv", ".txt", ".tsv"),
                    icon_name = "map-location-dot"
                  )
                ),
                bs4Dash::column(
                  width = 3,
                  glossa::file_input_area_ui(
                    "fit_layers",
                    label = "Environmental data",
                    button_label = "Add ZIP layers",
                    multiple = FALSE,
                    accept = ".zip",
                    icon_name = "layer-group"
                  )
                ),
                bs4Dash::column(
                  width = 3,
                  glossa::file_input_area_ui(
                    "proj_layers",
                    label = "Projection layers",
                    button_label = "Add ZIP layers (optional)",
                    multiple = TRUE,
                    accept = ".zip",
                    icon_name = "forward"
                  )
                ),
                bs4Dash::column(
                  width = 3,
                  glossa::file_input_area_ui(
                    "study_area_poly",
                    label = "Study area",
                    button_label = "Add polygon (optional)",
                    multiple = FALSE,
                    accept = c(".gpkg", ".shp", ".kml", ".json", ".geojson"),
                    icon_name = "crop"
                  )
                )
              ),

              # ** Analysis options ----
              tags$span(strong("Select one or more options to compute"), shiny::actionButton("analysis_options_options_info", label = NULL, icon = icon("circle-info", class = "fa-solid fa-circle-info", style = "color:#007bff;"), class = "btn btn-default action-button btn-xs", style="background-color:transparent;border-radius:0px;border-width:0px")),

              tags$style(HTML("
              .pretty .state label span {
              font-weight: normal !important;
              }")),
              fluidRow(
                style = "display: flex; justify-content: center; flex-wrap: wrap",
                bs4Dash::column(
                  width = 4,
                  prettyCheckboxGroup(
                    inputId = "analysis_options_nr",
                    label = tags$span("Native range", shiny::actionButton("analysis_options_nr_info", label = NULL, icon = icon("circle-info", class = "fa-solid fa-circle-info", style = "color:#007bff;"), class = "btn btn-default action-button btn-xs", style="background-color:transparent;border-radius:0px;border-width:0px")),
                    choiceNames = c("Model fitting", "Model projection"),
                    choiceValues = c("fit_layers", "projections"),
                    selected = NULL,
                    status = "primary",
                    shape = "curve"
                  )
                ),
                bs4Dash::column(
                  width = 4,
                  prettyCheckboxGroup(
                    inputId = "analysis_options_sh",
                    label = tags$span("Suitable habitat", shiny::actionButton("analysis_options_sh_info", label = NULL, icon = icon("circle-info", class = "fa-solid fa-circle-info", style = "color:#007bff;"), class = "btn btn-default action-button btn-xs", style="background-color:transparent;border-radius:0px;border-width:0px")),
                    choiceNames = c("Model fitting", "Model projection"),
                    choiceValues = c("fit_layers", "projections"),
                    selected = NULL,
                    status = "primary",
                    shape = "curve"
                  )
                ),
                bs4Dash::column(
                  width = 4,
                  prettyCheckboxGroup(
                    inputId = "analysis_options_other",
                    label = tags$span("Others", shiny::actionButton("analysis_options_others_info", label = NULL, icon = icon("circle-info", class = "fa-solid fa-circle-info", style = "color:#007bff;"), class = "btn btn-default action-button btn-xs", style="background-color:transparent;border-radius:0px;border-width:0px")),
                    choiceNames = c("Functional responses", "Variable importance", "Cross-validation"),
                    choiceValues = c("functional_responses", "variable_importance", "cross_validation"),
                    selected = NULL,
                    status = "primary",
                    shape = "curve"
                  )
                )
              ),

              fluidRow(
                bs4Dash::column(
                  width = 6,
                  bs4Dash::actionButton(
                    inputId = "toggle_advanced_options",
                    label = "Advanced options",
                    icon = NULL,
                    status = "primary",
                    outline = TRUE,
                    width = "100%"
                  )
                ),
                bs4Dash::column(
                  width = 3,
                  bs4Dash::actionButton(
                    "reset_input",
                    "Reset",
                    icon = NULL,
                    status = "danger",
                    width = "100%",
                    outline = TRUE
                  )
                ),
                bs4Dash::column(
                  width = 3,
                  bs4Dash::actionButton(
                    "run_button",
                    "Run Job",
                    icon = icon("play"),
                    status = "primary",
                    width = "100%"
                  )
                )
              )
            )
          )
        ),

        bs4Dash::column(
          width = 6,

          # ** Previsualization map ----
          fluidRow(
            bs4Dash::box(
              title = strong("Previsualization"),
              status = NULL,
              width = 12,
              height = 430,
              solidHeader = FALSE,
              background = NULL,
              maximizable = TRUE,
              collapsible = FALSE,
              headerBorder = FALSE,
              elevation = 2,
              label = NULL,

              sidebar = bs4Dash::boxSidebar(
                startOpen = FALSE,
                id = "previsualization_plot_sidebar",
                background = "#adb5bd",
                icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),

                shinyWidgets::pickerInput(
                  inputId = "previsualization_plot_species",
                  label = "Species occurrences",
                  choices = NULL,
                  width = "90%"
                ),

                shinyWidgets::pickerInput(
                  inputId = "previsualization_plot_layer",
                  label = "Environmental data",
                  choices = NULL,
                  width = "90%"
                ),

                shinyWidgets::prettySwitch(
                  inputId = "previsualization_plot_extent",
                  label = "Extent polygon",
                  status = "primary",
                  fill = TRUE
                )
              ),

              leaflet::leafletOutput("previsualization_plot", height = "100%")
            )
          )
        )
      ), # End first row

      # Start second row
      fluidRow(
        # ** Select predictor variables ----
        bs4Dash::column(
          width = 6,
          fluidRow(
            bs4Dash::box(
              title = strong("Predictor variables"),
              status = NULL,
              width = 12,
              solidHeader = FALSE,
              background = NULL,
              collapsible = FALSE,
              headerBorder = FALSE,
              elevation = 2,
              label = shiny::actionButton(
                "predictor_variables_info",
                label = NULL,
                icon = icon("circle-info", class = "fa-solid fa-circle-info", style = "color:#007bff;"),
                class = "btn btn-default action-button btn-xs",
                style="background-color:transparent;border-radius:0px;border-width:0px"
              ),

              bs4Dash::column(
                width = 12,
                uiOutput(outputId = "predictor_selector")
              )
            )
          )
        ),

        # ** File validation table ----
        bs4Dash::column(
          width = 6,
          fluidRow(
            bs4Dash::box(
              title = strong("Uploaded files"),
              status = NULL,
              width = 12,
              solidHeader = FALSE,
              background = NULL,
              collapsible = FALSE,
              headerBorder = FALSE,
              elevation = 2,
              label = NULL,

              bs4Dash::column(
                width = 12,
                DT::DTOutput("uploaded_files")
              )
            )
          )
        )
      ) # End second row
    ),

    # * Reports tab ----
    bs4Dash::tabItem(
      tabName = "reports",

      fluidRow(
        bs4Dash::box(
          title = NULL,
          status = NULL,
          width = 4,
          height = 95,
          solidHeader = FALSE,
          background = NULL,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = NULL,

          # ** Select species ----
          bs4Dash::column(
            width = 12,
            shinyWidgets::pickerInput(
              inputId = "sp",
              label = NULL,
              choices = NULL,
              choicesOpt = list()
            )
          )
        ),

        # ** Sparkline plots ----
        bs4Dash::column(
          width = 8,
          uiOutput("spark_boxes")
        )
      ),


      fluidRow(
        bs4Dash::column(
          width = 8,

          # ** Prediction map ----
          fluidRow(
            bs4Dash::box(
              title = strong("GLOSSA predictions"),
              status = NULL,
              width = 12,
              height = 465,
              solidHeader = FALSE,
              background = NULL,
              maximizable = TRUE,
              collapsible = FALSE,
              headerBorder = FALSE,
              elevation = 2,
              label = glossa::export_plot_ui("export_pred_plot"),

              sidebar = bs4Dash::boxSidebar(
                startOpen = FALSE,
                id = "pred_plot_sidebar",
                background = "#adb5bd",
                icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
                shinyWidgets::pickerInput(
                  inputId = "pred_plot_layers",
                  label = NULL,
                  choices = NULL,
                  width = "90%"
                ),

                shinyWidgets::pickerInput(
                  inputId = "pred_plot_model",
                  label = NULL,
                  choices = NULL,
                  width = "90%"
                ),

                shinyWidgets::pickerInput(
                  inputId = "pred_plot_value",
                  label = NULL,
                  choices = NULL,
                  width = "90%",
                  options = list(size = 5)
                ),

                uiOutput("pred_plot_scenario_picker"), # Only shows for projections

                uiOutput("pred_plot_year_slider"), # Only shows for projections

                shinyWidgets::prettySwitch(
                  inputId = "pa_points",
                  label = "Show points used for model fitting",
                  status = "primary",
                  fill = TRUE
                )
              ),

              plotOutput("prediction_plot", height = "100%")
            )
          )
        ),

        # ** Environmental data plot ----
        bs4Dash::column(
          width = 4,
          fluidRow(
            bs4Dash::box(
              title = strong("Environmental variables"),
              status = NULL,
              width = 12,
              height = 200,
              solidHeader = FALSE,
              background = NULL,
              maximizable = TRUE,
              collapsible = FALSE,
              headerBorder = FALSE,
              elevation = 2,
              label = glossa::export_plot_ui("export_layers_plot"),

              sidebar = bs4Dash::boxSidebar(
                startOpen = FALSE,
                id = "layers_plot_sidebar",
                background = "#adb5bd",
                icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
                shinyWidgets::pickerInput(
                  inputId = "layers_plot_mode",
                  label = NULL,
                  choices = NULL,
                  width = "90%"
                ),

                shinyWidgets::pickerInput(
                  inputId = "layers_plot_cov",
                  label = NULL,
                  choices = NULL,
                  width = "90%",
                  options = list(size = 5)
                ),

                uiOutput("layers_plot_scenario_picker"), # Only shows for projections

                uiOutput("layers_plot_year_slider"), # Only shows for projections

              ),

              plotOutput("cov_layers_plot", height = "100%")
            )
          ),

          # ** Deleted occurrences plot ----
          fluidRow(
            bs4Dash::box(
              title = strong("Presence validation"),
              status = NULL,
              width = 12,
              height = 200,
              solidHeader = FALSE,
              background = NULL,
              maximizable = TRUE,
              collapsible = FALSE,
              headerBorder = FALSE,
              elevation = 2,
              label = glossa::export_plot_ui("export_observations_plot"),

              sidebar = bs4Dash::boxSidebar(
                startOpen = FALSE,
                id = "observations_plot_sidebar",
                background = "#adb5bd",
                icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;")
              ),

              plotOutput("observations_plot", height = "100%")
            )
          )
        )
      ),

      fluidRow(
        # ** Functional responses ----
        bs4Dash::box(
          title = strong("Functional responses"),
          status = NULL,
          width = 4,
          height = 200,
          solidHeader = FALSE,
          background = NULL,
          maximizable = TRUE,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = glossa::export_plot_ui("export_fr_plot"),

          sidebar = bs4Dash::boxSidebar(
            startOpen = FALSE,
            id = "fr_plot_sidebar",
            background = "#adb5bd",
            icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
            shinyWidgets::pickerInput(
              inputId = "fr_plot_cov",
              label = NULL,
              choices = NULL,
              width = "90%",
              options = list(size = 5)
            )
          ),

          plotOutput("fr_plot", height = "100%")
        ),

        # ** Variable importance ----
        bs4Dash::box(
          title = strong("Variable importance"),
          status = NULL,
          width = 4,
          height = 200,
          solidHeader = FALSE,
          background = NULL,
          maximizable = TRUE,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = glossa::export_plot_ui("export_varimp_plot"),

          sidebar = bs4Dash::boxSidebar(
            startOpen = FALSE,
            id = "varimp_plot_sidebar",
            background = "#adb5bd",
            icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
            shinyWidgets::pickerInput(
              inputId = "varimp_plot_mode",
              label = NULL,
              choices = NULL,
              width = "90%"
            ),
          ),

          plotOutput("varimp_plot", height = "100%")
        ),

        # ** Cross-validation ----
        bs4Dash::box(
          title = strong("Cross-validation"),
          status = NULL,
          width = 4,
          height = 200,
          solidHeader = FALSE,
          background = NULL,
          maximizable = TRUE,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = glossa::export_plot_ui("export_cv_plot"),

          sidebar = bs4Dash::boxSidebar(
            startOpen = FALSE,
            id = "cv_plot_sidebar",
            background = "#adb5bd",
            icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
            shinyWidgets::pickerInput(
              inputId = "cv_plot_mode",
              label = NULL,
              choices = NULL,
              width = "90%",
              options = list(size = 5)
            )
          ),

          plotOutput("cv_plot", height = "100%")
        )
      ),

      fluidRow(column(width = 12, strong("Model summary"))),

      # Model summary
      fluidRow(
        # ** ROC curve ----
        bs4Dash::box(
          title = strong("ROC curve"),
          status = NULL,
          width = 4,
          height = 200,
          solidHeader = FALSE,
          background = NULL,
          maximizable = TRUE,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = glossa::export_plot_ui("export_roc_plot"),

          sidebar = bs4Dash::boxSidebar(
            startOpen = FALSE,
            id = "roc_plot_sidebar",
            background = "#adb5bd",
            icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
            shinyWidgets::pickerInput(
              inputId = "roc_plot_mode",
              label = NULL,
              choices = NULL,
              width = "90%",
              options = list(size = 5)
            )
          ),

          plotOutput("roc_plot", height = "100%")
        ),

        # ** Classified values ----
        bs4Dash::box(
          title = strong("Classified values"),
          status = NULL,
          width = 4,
          height = 200,
          solidHeader = FALSE,
          background = NULL,
          maximizable = TRUE,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = glossa::export_plot_ui("export_class_val_plot"),

          sidebar = bs4Dash::boxSidebar(
            startOpen = FALSE,
            id = "class_val_plot_sidebar",
            background = "#adb5bd",
            icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
            shinyWidgets::pickerInput(
              inputId = "class_val_plot_mode",
              label = NULL,
              choices = NULL,
              width = "90%",
              options = list(size = 5)
            )
          ),

          plotOutput("class_val_plot", height = "100%")
        ),

        # ** Distribution of fitted values ----
        bs4Dash::box(
          title = strong("Distribution of fitted values"),
          status = NULL,
          width = 4,
          height = 200,
          solidHeader = FALSE,
          background = NULL,
          maximizable = TRUE,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = glossa::export_plot_ui("export_fv_plot"),

          sidebar = bs4Dash::boxSidebar(
            startOpen = FALSE,
            id = "fv_plot_sidebar",
            background = "#adb5bd",
            icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
            shinyWidgets::pickerInput(
              inputId = "fv_plot_mode",
              label = NULL,
              choices = NULL,
              width = "90%",
              options = list(size = 5)
            )
          ),

          plotOutput("fv_plot", height = "100%")
        )
      )
    ),
    # * Exports tab ----
    bs4Dash::tabItem(
      tabName = "exports",

      fluidRow(
        bs4Dash::column(
          width = 6, offset = 3,
          bs4Dash::box(id = "export_details",
                       title = strong("Export details"),
                       status = NULL,
                       width = 12,
                       solidHeader = FALSE,
                       background = NULL,
                       collapsible = FALSE,
                       headerBorder = FALSE,
                       elevation = 2,
                       label = bs4Dash::actionButton(
                         "export_all",
                         label = "Select all",
                         status = "primary",
                         outline = FALSE
                       ),

                       selectInput(
                         "export_sp",
                         label = "Species name",
                         choices = NULL,
                         selected = NULL,
                         multiple = TRUE
                       ),

                       selectInput(
                         "export_results",
                         label = "Results",
                         choices = NULL,
                         selected = NULL,
                         multiple = TRUE
                       ),

                       selectInput(
                         "export_models",
                         label = "Model",
                         choices = NULL,
                         selected = NULL,
                         multiple = TRUE
                       ),

                       selectInput(
                         "export_values",
                         label = "Fields",
                         choices = NULL,
                         selected = NULL,
                         multiple = TRUE
                       ),

                       selectInput(
                         "export_layer_format",
                         label = "Layers file type",
                         choices = NULL,
                         selected = NULL,
                         multiple = FALSE
                       ),

                       strong("Other results"),

                       shinyWidgets::prettySwitch(
                         inputId = "export_model_data",
                         label = "Export data used to fit the models",
                         status = "primary",
                         fill = TRUE
                       ),

                       shinyWidgets::prettySwitch(
                         inputId = "export_mod_summary",
                         label = "Confusion matrix",
                         status = "primary",
                         fill = TRUE
                       ),

                       shinyWidgets::prettySwitch(
                         inputId = "export_var_imp",
                         label = "Variable importance",
                         status = "primary",
                         fill = TRUE
                       ),

                       shinyWidgets::prettySwitch(
                         inputId = "export_fr",
                         label = "Functional responses",
                         status = "primary",
                         fill = TRUE
                       ),

                       shinyWidgets::prettySwitch(
                         inputId = "export_cv",
                         label = "Cross-validation metrics",
                         status = "primary",
                         fill = TRUE
                       ),

                       shinyWidgets::prettySwitch(
                         inputId = "export_pa_cutoff",
                         label = "P/A probability cutoff value",
                         status = "primary",
                         fill = TRUE
                       ),

                       glossa::downloadActionButton(
                         outputId = "export_button",
                         label = "Save GLOSSA results",
                         icon = NULL,
                         status = "primary",
                         outline = FALSE,
                         width = "100%"
                       )
          ) # End box
        ) # End column
      ) # End fluidRow
    ), # End exports tab

    # * Documentation tab
    bs4Dash::tabItem(
      tabName = "documentation",

      fluidRow(
        bs4Dash::column(
          width = 8, offset = 2,
          bs4Dash::box(title = NULL,
                       status = NULL,
                       width = 12,
                       solidHeader = FALSE,
                       background = NULL,
                       collapsible = FALSE,
                       headerBorder = FALSE,
                       elevation = 2,
                       label = NULL,

                       shiny::includeMarkdown("Rmd/documentation.Rmd")
          ) # End box
        ) # End column
      ) # End fluidRow
    ), # End documentation tab

    # * How to cite tab ----
    bs4Dash::tabItem(
      tabName = "how_to_cite",

      fluidRow(
        bs4Dash::column(
          width = 8, offset = 2,
          bs4Dash::box(title = NULL,
                       status = NULL,
                       width = 12,
                       solidHeader = FALSE,
                       background = NULL,
                       collapsible = FALSE,
                       headerBorder = FALSE,
                       elevation = 2,
                       label = NULL,

                       shiny::includeMarkdown("Rmd/how_to_cite.Rmd")
          ) # End box
        ) # End column
      ) # End fluidRow
    ), # End how_to_cite tab

    # * Lastest updates tab ----
    bs4Dash::tabItem(
      tabName = "updates",

      fluidRow(
        bs4Dash::column(
          width = 8, offset = 2,
          bs4Dash::box(title = NULL,
                       status = NULL,
                       width = 12,
                       solidHeader = FALSE,
                       background = NULL,
                       collapsible = FALSE,
                       headerBorder = FALSE,
                       elevation = 2,
                       label = NULL,

                       shiny::includeMarkdown("Rmd/lastest_updates.Rmd")
          ) # End box
        ) # End column
      ) # End fluidRow
    ),

    # * Contact us tab ----
    bs4Dash::tabItem(
      tabName = "contact",

      fluidRow(
        bs4Dash::column(
          width = 8, offset = 2,
          bs4Dash::box(title = NULL,
                       status = NULL,
                       width = 12,
                       solidHeader = FALSE,
                       background = NULL,
                       collapsible = FALSE,
                       headerBorder = FALSE,
                       elevation = 2,
                       label = NULL,

                       shiny::includeMarkdown("Rmd/contact_us.Rmd")
          ) # End box
        ) # End column
      ) # End fluidRow
    ) # End contact tab
  )
)

# UI ----
bs4Dash::bs4DashPage(
  header,
  sidebar,
  body,
  controlbar = NULL,
  title = NULL,
  freshTheme = NULL,
  # waiter pre-loader
  preloader = list(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading ...")), color = "#3b444b"),
  options = NULL,
  fullscreen = FALSE,
  help = NULL,
  dark = NULL,
  scrollToTop = FALSE,
  # authors and corporation logos
  footer = bs4Dash::bs4DashFooter	(
    fixed = FALSE,
    left = span(
      "Developed by ",
      a(href = "https://imares.science/", target = "_blank", "@iMARES")
    ),
    right = tagList(
      img(src = "logo_csic.png", height="25px", align="center"),
      img(src = "logo_icm.png", height="25px", align="center"),
      img(src = "logo_so.png", height="25px", align="center"),
      img(src = "logo_imares.png", height="25px", align="center"),
      img(src = "logo_prooceans.png", height="25px", align="center")
    )
  )
)
