#' Confirmatory Factor Analysis (CFA) UI Module
#'
#' @param id Module namespace ID.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinycssloaders withSpinner
#' @export
cfa_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(5, 7),

      # Model builder
      card(
        card_header(
          class = "bg-info text-white",
          "Model Builder",
          bs_icon("magic")
        ),
        card_body(
          selectInput(
            ns("builder_model_type"),
            "Model Type:",
            choices = c(
              "First-order / Complex" = "first_order",
              "Second-order" = "second_order",
              "Bifactor" = "bifactor"
            ),
            selected = "first_order"
          ),
          helpText(
            "An indicator may be assigned to more than one factor for complex or cross-loading models."
          ),
          h6("Define Observed Factors (=~)", class = "text-primary"),
          textInput(ns("builder_factor_name"), "Factor Name:", placeholder = "e.g. F1, F2, Factor1 ..."),
          selectizeInput(
            ns("builder_items"),
            "Select Indicators:",
            choices = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Select variables...",
              closeAfterSelect = FALSE,
              plugins = list("remove_button")
            )
          ),
          actionButton(
            ns("btn_add_to_model"),
            "Add / Update Factor",
            icon = icon("code"),
            class = "btn-secondary btn-sm w-100 mb-3"
          ),

          conditionalPanel(
            condition = sprintf(
              "input['%s'] === 'second_order'",
              ns("builder_model_type")
            ),
            h6("Define Second-order Factor", class = "text-primary"),
            textInput(
              ns("builder_higher_factor_name"),
              "Second-order Factor Name:",
              value = "HO",
              placeholder = "e.g. HO, General"
            ),
            selectizeInput(
              ns("builder_lower_factors"),
              "Select First-order Factors:",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Select first-order factors...",
                closeAfterSelect = FALSE,
                plugins = list("remove_button")
              )
            ),
            actionButton(
              ns("btn_add_second_order"),
              "Add Second-order Factor",
              icon = icon("code"),
              class = "btn-primary btn-sm w-100 mb-3"
            )
          ),

          conditionalPanel(
            condition = sprintf(
              "input['%s'] === 'bifactor'",
              ns("builder_model_type")
            ),
            h6("Build Bifactor Model", class = "text-primary"),
            helpText("Define the group factors above, then select them here."),
            textInput(
              ns("builder_general_factor_name"),
              "General Factor Name:",
              value = "G"
            ),
            selectizeInput(
              ns("builder_bifactor_groups"),
              "Select Group Factors:",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Select group factors...",
                closeAfterSelect = FALSE,
                plugins = list("remove_button")
              )
            ),
            checkboxInput(
              ns("builder_bifactor_orthogonal"),
              "Set the general and group factors as orthogonal",
              value = TRUE
            ),
            actionButton(
              ns("btn_build_bifactor"),
              "Create Bifactor Syntax",
              icon = icon("code"),
              class = "btn-primary btn-sm w-100 mb-3"
            )
          ),

          h6("Add Covariance (~~)", class = "text-primary"),
          selectizeInput(
            ns("builder_cov_items"),
            "Select 2 Variables:",
            choices = NULL,
            multiple = TRUE,
            options = list(
              maxItems = 2,
              placeholder = "Select 2 variables or factors...",
              closeAfterSelect = FALSE,
              plugins = list("remove_button")
            )
          ),
          actionButton(
            ns("btn_add_cov"),
            "Add Covariance (~~)",
            icon = icon("link"),
            class = "btn-secondary btn-sm w-100"
          )
        )
      ),

      # Syntax and estimation settings
      card(
        card_header(
          class = "bg-primary text-white",
          "CFA Analysis Setup",
          bs_icon("sliders")
        ),
        card_body(
          textAreaInput(
            ns("cfa_model_syntax_input"),
            label = "Model Syntax (lavaan):",
            placeholder = "e.g.,\nF1 =~ item1 + item2\nV1 ~~ V2",
            rows = 10,
            resize = "both",
            width = "100%"
          ),
          div(class = "d-flex gap-2 mb-3",
              fileInput(ns("upload_model_syntax"), "Load syntax (.txt/.lav)",
                        accept = c(".txt", ".lav"), buttonLabel = "Browse", placeholder = "No file"),
              downloadButton(ns("download_model_syntax"), "Save Syntax", class = "align-self-end mb-3")
          ),
          radioButtons(
            ns("cfa_correlation_type_radio"),
            label = "Data Type:",
            choices = c("Continuous (Pearson)" = "pea", "Ordinal (Polychoric)" = "poly"),
            inline = TRUE,
            selected = "pea"
          ),
          selectInput(
            ns("cfa_estimator_select"),
            label = "Estimator:",
            choices = c("ML", "MLR", "GLS"),
            selected = "ML"
          ),
          actionButton(
            ns("run_cfa_button"),
            "Run CFA Analysis",
            icon = icon("play"),
            class = "btn-success w-100"
          )
        )
      )
    ),

    navset_card_tab(
      full_screen = TRUE,

      nav_panel(
        title = "Path Diagram",
        icon = bs_icon("diagram-3"),
        layout_sidebar(
          sidebar = sidebar(
            title = "Plot Settings",
            width = 250,
            selectInput(ns("plot_layout"), "Layout Style:",
                        choices = c("Tree (Hierarchical)" = "tree",
                                    "Tree (Left-Right)" = "tree2",
                                    "Spring (Force-directed)" = "spring",
                                    "Circle" = "circle")),
            sliderInput(ns("plot_rotation"), "Rotation:", min = 1, max = 4, value = 2, step = 1),
            sliderInput(ns("plot_man_size"), "Box Width:", min = 4, max = 20, value = 10),
            sliderInput(ns("plot_edge_label_cex"), "Label Size:", min = 0.5, max = 1.5, value = 0.8, step = 0.1),
            checkboxInput(ns("plot_show_labels"), "Show Estimates", value = TRUE)
            ,selectInput(ns("plot_download_format"), "Download format:",
                         choices = c("PNG (300 dpi)" = "png", "SVG" = "svg", "JPG (300 dpi)" = "jpg"),
                         selected = "png")
          ),
          withSpinner(plotOutput(ns("cfa_path_diagram_output"), height = "600px"), type = 8, color = "#2C3E50"),
          downloadButton(ns("download_path_diagram_button"), "Download Diagram", class = "btn-sm")
        )
      ),

      nav_panel(
        title = "Fit Measures",
        icon = bs_icon("table"),
        tableOutput(ns("cfa_fit_measures_table")),
        div(
          class = "d-flex flex-wrap gap-2",
          downloadButton(ns("download_fit_measures_button"), "Download CSV", class = "btn-sm"),
          downloadButton(ns("download_cfa_apa7"), "Download APA 7 Word", class = "btn-sm btn-primary")
        )
      ),

      nav_panel(
        title = "Factor Loadings",
        icon = bs_icon("list-ol"),
        tableOutput(ns("cfa_factor_loadings_table")),
        downloadButton(ns("download_factor_loadings_button"), "Download CSV", class = "btn-sm")
      ),

      nav_panel(
        title = "Modification Indices",
        icon = bs_icon("tools"),
        tableOutput(ns("cfa_modification_indices_table"))
      ),

      nav_panel(
        title = "Dynamic Fit Index",
        icon = bs_icon("activity"),
        p(
          "Generate model- and sample-specific CFI and RMSEA cutoffs through simulation. Conventional cutoffs remain visible for comparison."
        ),
        p(
          class = "small text-muted",
          "Model-specific cutoffs are simulated by FAfA from the fitted lavaan model. No additional package is required."
        ),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          selectInput(
            ns("dynamic_fit_scale"),
            "Simulation scale:",
            choices = c(
              "Automatic" = "auto",
              "Normal continuous" = "normal",
              "Non-normal continuous" = "nonnormal",
              "Categorical / ordinal" = "categorical"
            ),
            selected = "auto"
          ),
          numericInput(
            ns("dynamic_fit_reps"),
            "Simulation replications:",
            value = 250,
            min = 50,
            max = 5000,
            step = 50
          ),
          textInput(
            ns("dynamic_fit_mad"),
            "MAD levels:",
            value = "0.038, 0.05, 0.06"
          ),
          numericInput(
            ns("dynamic_fit_seed"),
            "Random seed:",
            value = 2026,
            min = 1,
            max = .Machine$integer.max,
            step = 1
          )
        ),
        actionButton(
          ns("run_dynamic_fit_button"),
          "Run Dynamic Fit Index",
          icon = icon("play"),
          class = "btn-success mb-2"
        ),
        p(class = "text-muted", textOutput(ns("dynamic_fit_status"))),
        h5("Conventional Cutoffs"),
        tableOutput(ns("conventional_fit_table")),
        h5("Model-Specific Dynamic Cutoffs", class = "mt-4"),
        tableOutput(ns("dynamic_fit_cutoffs_table")),
        div(
          class = "d-flex flex-wrap gap-2 mt-2",
          downloadButton(ns("download_dynamic_fit_csv"), "Download CSV", class = "btn-sm")
        ),
        p(
          class = "small text-muted mt-3",
          "Reference: McNeish and Wolf (2023), Dynamic Fit Index Cutoffs for Confirmatory Factor Analysis Models; dynamic R package version 1.1.0 (AGPL-3).",
          tags$a(
            href = "https://doi.org/10.1037/met0000425",
            target = "_blank",
            rel = "noopener noreferrer",
            "Open article"
          ),
          " | ",
          tags$a(
            href = "https://github.com/melissagwolf/dynamic",
            target = "_blank",
            rel = "noopener noreferrer",
            "Package repository"
          )
        )
      )
    )
  )
}
