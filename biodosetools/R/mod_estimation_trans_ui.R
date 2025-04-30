#' Translocations Dose Estimation UI Module
#'
#' @param id Namespace for the {shiny} module.
#' @param label Internal label of the {shiny} module.
#'
#' @import shiny shinydashboard shinyWidgets rhandsontable
#' @noRd
mod_estimation_trans_ui <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabItem(
    class = "tabitem-container",
    tabName = label,
    h2("Translocations: Dose estimation"),
    fluidRow(
      # Box: Curve fitting options ----
      box(
        width = 5,
        title = span(
          "Curve fitting data options",
          help_modal_button(
            ns("help_fit_data"),
            ns("help_fit_data_modal")
          )
        ),
        status = "info",
        collapsible = TRUE,

        # Help modal
        bsplus::bs_modal(
          id = ns("help_fit_data_modal"),
          title = "Help: Fitting data input",
          size = "large",
          body = tagList(
            # Option selection
            radioGroupButtons(
              inputId = ns("help_fit_data_option"),
              label = NULL,
              choices = c(
                "Manual input" = "manual",
                "Load data"    = "load"
              ),
              selected = "load"
            ),
            # Contents
            conditionalPanel(
              condition = "input.help_fit_data_option == 'manual'",
              ns = ns,
              include_help("estimation/fit_data_input.md")
            ),
            conditionalPanel(
              condition = "input.help_fit_data_option == 'load'",
              ns = ns,
              include_help("estimation/fit_data_load.md")
            ),
            include_help("trans/fit_data_estimation.md")
          )
        ),
        fluidRow(
          col_12(
            # Load data from file
            awesomeCheckbox(
              inputId = ns("load_fit_data_check"),
              status = "info",
              label = "Load fit data from RDS file",
              value = TRUE
            ),

            # Manual input ----
            conditionalPanel(
              condition = "!input.load_fit_data_check",
              ns = ns,
              div(
                class = "side-widget-tall",
                selectInput(
                  ns("formula_select"),
                  width = 165,
                  label = "Fitting formula",
                  choices = list_fitting_formulas(),
                  selected = "lin-quad"
                )
              ),
              widget_sep(),
              actionButton(
                ns("button_gen_table"),
                class = "options-button",
                style = "margin-left: -10px; margin-bottom: 0px;",
                label = "Generate tables"
              ),
              br(),
              br(),
              selectInput(
                ns("frequency_select"),
                label = "Translocation frequency",
                choices = list(
                  "Measured by FISH" = "measured_freq",
                  "Full genome"      = "full_gen_freq"
                ),
                selected = "measured_freq"
              )
            ),
            conditionalPanel(
              condition = "!input.load_fit_data_check & input.frequency_select == 'measured_freq'",
              ns = ns,
              numericInput(
                ns("fit_genome_factor"),
                "Genomic conversion factor",
                value = NA,
                min = 0,
                max = 1,
                step = 0.001
              )
            ),
            conditionalPanel(
              condition = "!input.load_fit_data_check",
              ns = ns,
              widget_label("Coefficients"),
              div(
                class = "hot-improved",
                rHandsontableOutput(ns("fit_coeffs_hot"))
              ),
              br(),
              awesomeCheckbox(
                inputId = ns("use_var_cov_matrix"),
                status = "info",
                label = "Provide variance-covariance matrix",
                value = FALSE
              )
            ),
            conditionalPanel(
              condition = "input.use_var_cov_matrix",
              ns = ns,
              widget_label("Variance-covariance matrix"),
              div(
                class = "hot-improved",
                rHandsontableOutput(ns("fit_var_cov_mat_hot"))
              ),
              br()
            ),

            # Load from file ----
            conditionalPanel(
              condition = "input.load_fit_data_check",
              ns = ns,
              fileInput(
                ns("load_fit_data"),
                label = "File input",
                accept = c(".rds")
              )
            ),

            # Buttons
            actionButton(
              ns("button_view_fit_data"),
              class = "options-button",
              label = "Preview data"
            )
          )
        )
      ),
      # tabBox: Curve fitting overview ----

      tabBox(
        id = ns("fit_results_tabs"),
        width = 7,
        side = "left",
        tabPanel(
          title = "Result of curve fit",
          h5("Fit formula"),
          uiOutput(ns("fit_formula_tex")),
          br(),
          h5("Translocation frequency"),
          uiOutput(ns("fit_trans_frequency_message")),
          br(),
          h5("Full genome coefficients"),
          div(
            class = "hot-improved",
            rHandsontableOutput(ns("fit_coeffs"))
          )
        ),
        tabPanel(
          title = "Summary statistics",
          conditionalPanel(
            condition = "input.load_fit_data_check",
            ns = ns,
            h5("Model-level statistics"),
            div(
              class = "hot-improved",
              rHandsontableOutput(ns("fit_model_statistics"))
            ),
            br()
          ),
          h5("Correlation matrix"),
          div(
            class = "hot-improved",
            rHandsontableOutput(ns("fit_cor_mat"))
          ),
          br(),
          h5("Variance-covariance matrix"),
          div(
            class = "hot-improved",
            rHandsontableOutput(ns("fit_var_cov_mat"))
          )
        )
      )
    ),
    fluidRow(
      # Box: Stains color options ----
      box(
        width = 5,
        title = span(
          "Stains color options",
          help_modal_button(
            ns("help_colors_data"),
            ns("help_colors_modal")
          )
        ),
        status = "info",
        collapsible = TRUE,

        # Help modal
        bsplus::bs_modal(
          id = ns("help_colors_modal"),
          title = "Help: Stain color data input",
          size = "large",
          body = tagList(
            include_help("trans/colors_data_input.md"),
            div(
              class = "hot-improved",
              rHandsontableOutput(ns("help_chromosome_hot"))
            ),
            include_help("trans/colors_data_input_b.md")
          )
        ),
        fluidRow(
          col_12(
            awesomeRadio(
              inputId = ns("trans_sex"),
              status = "info",
              label = "Sex",
              choices = c(
                "Male"   = "male",
                "Female" = "female"
              ),
              selected = "male"
            ),
            selectizeInput(
              inputId = ns("trans_chromosome_select"),
              label = "Chromosomes",
              choices = c(1:21, "X", "Y"),
              options = list(
                placeholder = "Select stained chromosomes"
              ),
              multiple = TRUE
            ),
            widget_label("Stain color scheme"),
            awesomeCheckbox(
              inputId = ns("trans_m_fish_scheme"),
              status = "info",
              label = "Use M-Fish",
              value = FALSE
            ),
            conditionalPanel(
              condition = "!input.trans_m_fish_scheme",
              ns = ns,
              selectizeInput(
                inputId = ns("trans_color_select"),
                label = "Stain colors",
                choices = c(
                  "Red",
                  "Green",
                  "Yellow",
                  "Orange",
                  "Purple",
                  "Magenta",
                  "Cyan"
                ),
                options = list(
                  placeholder = "Select observed colors"
                  # TODO: use renderUI to force maxItems to be length(trans_color_select)
                  # maxItems = 5
                ),
                multiple = TRUE
              )
            ),
            br(),
            actionButton(
              ns("button_upd_chrom_table"),
              class = "options-button",
              "Generate table"
            )
          )
        )
      ),
      col_7_inner(
        # Box: Chromosome-color table ----
        box(
          width = 12,
          title = "Chromosome data",
          status = "primary",
          collapsible = TRUE,
          fluidRow(
            col_12(
              div(
                class = "hot-improved",
                rHandsontableOutput(outputId = ns("chromosome_table"))
              ),
              br(),
              actionButton(
                ns("button_calc_genome_factor"),
                class = "inputs-button",
                label = "Calculate factor"
              )
            )
          )
        ),

        # Box: Conversion factor to full genome ----
        box(
          width = 12,
          title = "Genomic conversion factor",
          status = "success",
          collapsible = TRUE,
          fluidRow(
            col_12(
              uiOutput(ns("genome_factor"))
            )
          )
        )
      )
    ),
    fluidRow(
      # Box: Data input options ----
      box(
        width = 5,
        title = span(
          "Data input options",
          help_modal_button(
            ns("help_cases_data"),
            ns("help_cases_data_modal")
          )
        ),
        status = "info",
        collapsible = TRUE,

        # Help modal
        bsplus::bs_modal(
          id = ns("help_cases_data_modal"),
          title = "Help: Cases data input",
          size = "large",
          body = tagList(
            # Option selection
            radioGroupButtons(
              inputId = ns("help_cases_data_option"),
              label = NULL,
              choices = c(
                "Manual input" = "manual",
                "Load data"    = "load",
                "Confounders"  = "confounders"
              )
            ),
            # Contents
            conditionalPanel(
              condition = "input.help_cases_data_option == 'manual'",
              ns = ns,
              include_help("trans/cases_data_input.md")
            ),
            conditionalPanel(
              condition = "input.help_cases_data_option == 'load'",
              ns = ns,
              include_help("trans/cases_data_load.md")
            ),
            conditionalPanel(
              condition = "input.help_cases_data_option == 'confounders'",
              ns = ns,
              include_help("trans/cases_data_confounders.md")
            )
          )
        ),
        fluidRow(
          col_12(
            # Name of translocations
            textInput(
              inputId = ns("trans_name"),
              label = "Name of translocations",
              placeholder = "Input type of translocations"
            ),

            # Load data from file
            awesomeCheckbox(
              inputId = ns("load_case_data_check"),
              status = "info",
              label = "Load data from file",
              value = FALSE
            ),

            # Inputs
            conditionalPanel(
              condition = "!input.load_case_data_check",
              ns = ns,
              # numericInput(
              #   ns("num_cases"),
              #   label = "Number of cases",
              #   value = 1
              # ),
              numericInput(
                ns("num_aberrs"),
                label = "Maximum number of translocations per cell",
                value = 5
              )
            ),
            conditionalPanel(
              condition = "input.load_case_data_check",
              ns = ns,
              fileInput(
                ns("load_case_data"),
                label = "File input",
                accept = c("txt/csv", "text/comma-separated-values", "text/plain", ".csv", ".txt", ".dat")
              )
            ),

            # Confounders selection
            widget_label("Confounders"),
            awesomeCheckbox(
              inputId = ns("trans_confounders"),
              status = "info",
              label = "Use confounders",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.trans_confounders",
              ns = ns,
              selectInput(
                inputId = ns("trans_confounders_type"),
                label = NULL,
                choices = c(
                  "Using Sigurdson" = "sigurdson",
                  "Using own data" = "manual"
                ),
                selected = "sigurdson"
              )
            ),

            # Case description
            textAreaInput(
              inputId = ns("case_description"),
              label = "Case description",
              placeholder = "Short summary of the case"
            ),

            # Buttons
            actionButton(
              ns("button_upd_table"),
              class = "options-button",
              label = "Generate table"
            )
          )
        )
      ),

      # Box: hot Cases & confounders input ----
      col_7_inner(
        box(
          width = 12,
          title = "Data input",
          status = "primary",
          collapsible = TRUE,

          # Confounders ----
          conditionalPanel(
            condition = "input.trans_confounders & input.trans_confounders_type == 'sigurdson'",
            ns = ns,
            div(
              class = "side-widget",
              style = "padding-right: 10px;",
              numericInput(
                ns("trans_confounder_age"),
                width = 75,
                "Age",
                value = 25,
                min = 0,
                step = 1
              )
            ),
            div(
              class = "side-widget",
              style = "max-width: 140px; margin-right: 5px;",
              widget_label("Sex", -4, 14),
              switchInput(
                inputId = ns("trans_confounder_sex"),
                onStatus = "primary",
                value = FALSE
              )
            ),
            div(
              class = "side-widget",
              style = "max-width: 140px; margin-right: 5px;",
              widget_label("Smoker", -4, 14),
              switchInput(
                inputId = ns("trans_confounder_smoke"),
                onStatus = "primary",
                value = FALSE
              )
            ),
            div(
              class = "side-widget",
              style = "padding-right: 10px;",
              selectInput(
                ns("trans_confounder_ethnicity"),
                label = "Ethnicity",
                width = "150px",
                choices = list(
                  "Ethnicities" = c(
                    "White" = "white",
                    "Asian" = "asian",
                    "Black" = "black",
                    "Other" = "other"
                  ),
                  "None" = c(
                    "Not specified" = "none"
                  )
                ),
                selected = "none"
              )
            ),
            div(
              class = "side-widget",
              style = "padding-right: 10px;",
              selectInput(
                ns("trans_confounder_region"),
                label = "Lab region",
                width = "180px",
                choices = list(
                  "Regions" = c(
                    "North America"  = "n-america",
                    "Western Europe" = "w-europe",
                    "Central Europe" = "c-europe",
                    "Eastern Europe" = "e-europe",
                    "Asia"           = "asia"
                  ),
                  "None" = c(
                    "Not specified" = "none"
                  )
                ),
                selected = "none"
              )
            )
          ),
          conditionalPanel(
            condition = "input.trans_confounders & input.trans_confounders_type == 'manual'",
            ns = ns,
            numericInput(
              ns("trans_expected_aberr_value"),
              label = "Translocation frequency per cell",
              value = 0.00339,
              step = 0.00001
            )
          ),

          # Cases table ----
          div(
            class = "hot-improved",
            rHandsontableOutput(ns("case_data_hot"))
          ),
          # Button
          br(),
          actionButton(
            ns("button_upd_params"),
            class = "inputs-button",
            label = "Calculate parameters"
          )
        ),

        # Box: Estimation options ----
        box(
          width = 12,
          title = span(
            "Dose estimation options",
            help_modal_button(
              ns("help_estimation_options"),
              ns("help_estimation_options_modal")
            )
          ),
          status = "info",
          collapsible = TRUE,

          # Help modal
          bsplus::bs_modal(
            id = ns("help_estimation_options_modal"),
            title = "Help: Dose estimation options",
            size = "large",
            body = tagList(
              # Option selection
              radioGroupButtons(
                inputId = ns("help_estimation_options_option"),
                label = NULL,
                choices = c(
                  "Exposure"             = "exposure",
                  "Assessment"           = "assess",
                  "Error calculation"    = "error",
                  "Survival coefficient" = "surv_coeff"
                )
              ),
              # Contents
              conditionalPanel(
                condition = "input.help_estimation_options_option == 'exposure'",
                ns = ns,
                include_help("estimation/dose_exposure.md")
              ),
              conditionalPanel(
                condition = "input.help_estimation_options_option == 'assess'",
                ns = ns,
                include_help("estimation/dose_assessment.md")
              ),
              conditionalPanel(
                condition = "input.help_estimation_options_option == 'error'",
                ns = ns,
                include_help("estimation/dose_error.md"),
                include_help("dicent/dose_error_methods.md")
              ),
              conditionalPanel(
                condition = "input.help_estimation_options_option == 'surv_coeff'",
                ns = ns,
                include_help("estimation/fraction_coeff_select.md")
              )
            )
          ),

          # Type of exposure selection
          div(
            class = "side-widget-tall",
            selectInput(
              ns("exposure_select"),
              label = "Exposure",
              width = "175px",
              choices = list(
                "Acute"      = "acute",
                "Protracted" = "protracted"
              ),
              selected = "acute"
            )
          ),
          widget_sep(),

          # Assessment selection
          div(
            class = "side-widget-tall",
            selectInput(
              ns("assessment_select"),
              label = "Assessment",
              width = "175px",
              choices = list(
                "Whole-body"    = "whole-body",
                "Partial-body"  = "partial-body"
              ),
              selected = "whole-body"
            )
          ),
          br(),
          br(),

          # Whole-body error method selection
          div(
            class = "side-widget-tall",
            selectInput(
              ns("error_method_whole_select"),
              label = "Whole-body error method",
              width = "250px",
              choices = list(
                "Merkle's method (83%-83%)" = "merkle-83",
                "Merkle's method (95%-95%)" = "merkle-95",
                "Delta method (95%)"        = "delta"
              ),
              selected = "merkle-83"
            )
          ),
          widget_sep(),

          # Partial-body error method selection
          div(
            class = "side-widget-tall",
            conditionalPanel(
              condition = "input.assessment_select == 'partial-body'",
              ns = ns,
              selectInput(
                ns("error_method_partial_select"),
                label = "Partial-body error method",
                width = "250px",
                choices = list(
                  "Dolphin (95%)" = "dolphin"
                ),
                selected = "dolphin"
              )
            )
          ),

          # Protracted timing
          conditionalPanel(
            condition = "input.exposure_select == 'protracted'",
            ns = ns,
            br(),

            # Irradiation time
            div(
              class = "side-widget-tall",
              numericInput(
                ns("protracted_time"),
                label = "Irradiation time (h)",
                width = "175px",
                value = 0.5,
                step = 0.1,
                min = 0
              )
            ),
            widget_sep(),

            # Rejoining time
            div(
              class = "side-widget-tall",
              numericInput(
                ns("protracted_life_time"),
                label = "Rejoining time (h)",
                width = "175px",
                value = 2,
                step = 0.1,
                min = 2,
                max = 5
              )
            )
          ),

          # Coefficient conditional input
          conditionalPanel(
            condition = "input.assessment_select != 'whole-body'",
            ns = ns,
            br(),

            # Coefficient input selection
            div(
              class = "side-widget-tall",
              selectInput(
                ns("fraction_coeff_select"),
                label = "Survival coefficient",
                width = "175px",
                choices = list(
                  "D0" = "d0",
                  "Gamma" = "gamma"
                ),
                selected = "d0"
              )
            ),
            widget_sep(),
            div(
              class = "side-widget",
              # Input gamma
              conditionalPanel(
                condition = "input.fraction_coeff_select == 'gamma'",
                ns = ns,
                div(
                  class = "side-widget-tall",
                  numericInput(
                    width = "175px",
                    ns("gamma_coeff"), "Gamma",
                    value = 0.3706479, step = 0.01
                  )
                ),
                div(
                  class = "side-widget-tall",
                  numericInput(
                    width = "150px",
                    ns("gamma_error"), "Error of gamma",
                    value = 0.009164707, step = 0.0001
                  )
                )
              ),
              # Input D0
              conditionalPanel(
                condition = "input.fraction_coeff_select == 'd0'",
                ns = ns,
                div(
                  class = "side-widget-tall",
                  numericInput(
                    width = "150px",
                    ns("d0_coeff"), "D0",
                    value = 2.7, step = 0.01,
                    min = 2.7, max = 3.5
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.assessment_select == 'whole-body'",
            ns = ns,
            br()
          ),
          conditionalPanel(
            condition = "input.assessment_select != 'whole-body'",
            ns = ns,
            br()
          ),
          actionButton(
            ns("button_estimate"),
            class = "options-button",
            label = "Estimate dose"
          )
        )
      )
    ),
    fluidRow(
      col_6_inner(
        # tabBox: Estimation results ----
        uiOutput(ns("estimation_results_ui")),

        # Box: Export data and results ----
        box(
          width = 12,
          title = span(
            "Save results",
            help_modal_button(
              ns("help_fit_data_save"),
              ns("help_fit_data_save_modal")
            )
          ),
          status = "warning",
          collapsible = TRUE,

          # Help Modal
          bsplus::bs_modal(
            id = ns("help_fit_data_save_modal"),
            title = "Help: Export results",
            size = "large",
            body = tagList(
              # Contents
              include_help("save/estimation_data_save_report.md")
            )
          ),

          # Case description
          textAreaInput(
            inputId = ns("results_comments"),
            label = "Comments",
            placeholder = "Comments to be included on report"
          ),

          # Download report
          downloadButton(
            ns("save_report"),
            class = "export-button side-widget-download",
            label = "Download report"
          ),
          div(
            class = "side-widget-format",
            selectInput(
              ns("save_report_format"),
              label = NULL,
              width = "85px",
              choices = list(".pdf", ".docx"),
              selected = ".pdf"
            )
          )
        )
      ),
      # Box: Plot curves ----
      box(
        width = 6,
        title = "Curve plot",
        status = "success",
        collapsible = TRUE,

        # Plot
        plotOutput(ns("plot")),
        # Download plot
        downloadButton(
          ns("save_plot"),
          class = "results-button side-widget-download",
          label = "Save plot"
        ),
        div(
          class = "side-widget-format",
          selectInput(
            ns("save_plot_format"),
            label = NULL,
            width = "75px",
            choices = list(".png", ".pdf"),
            selected = ".png"
          )
        )
      )
    )
  )
}
