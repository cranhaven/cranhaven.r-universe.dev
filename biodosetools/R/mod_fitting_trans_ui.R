#' Translocations Fitting UI Module
#'
#' @param id Namespace for the {shiny} module.
#' @param label Internal label of the {shiny} module.
#'
#' @import shiny shinydashboard shinyWidgets rhandsontable
#' @noRd
mod_fitting_trans_ui <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabItem(
    class = "tabitem-container",
    tabName = label,
    h2("Translocations: Dose-effect fitting"),
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
              label = "Generate table"
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
        width = 6,
        title = span(
          "Data input options",
          help_modal_button(
            ns("help_count_data"),
            ns("help_count_data_modal")
          )
        ),
        status = "info",
        collapsible = TRUE,

        # Help modal
        bsplus::bs_modal(
          id = ns("help_count_data_modal"),
          title = "Help: Count data input",
          size = "large",
          body = tagList(
            # Option selection
            radioGroupButtons(
              inputId = ns("help_count_data_option"),
              label = NULL,
              choices = c(
                "Manual input"    = "manual",
                "Load data"       = "load",
                "Aggregated data" = "aggr"
              )
            ),
            # Contents
            conditionalPanel(
              condition = "input.help_count_data_option == 'manual'",
              ns = ns,
              include_help("fitting/count_data_input.md")
            ),
            conditionalPanel(
              condition = "input.help_count_data_option == 'load'",
              ns = ns,
              include_help("fitting/count_data_load.md")
            ),
            conditionalPanel(
              condition = "input.help_count_data_option == 'aggr'",
              ns = ns,
              include_help("fitting/count_data_aggregated.md")
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

            # Load file checkbox
            awesomeCheckbox(
              inputId = ns("load_count_data_check"),
              status = "info",
              label = "Load data from file",
              value = FALSE
            ),

            # Full/aggregated data checkbox
            awesomeCheckbox(
              inputId = ns("use_aggr_count_data_check"),
              status = "info",
              width = "100%",
              label = "Only provide total number of translocations",
              value = FALSE
            ),

            # Manual input ----
            conditionalPanel(
              condition = "!input.load_count_data_check",
              ns = ns,
              numericInput(
                ns("num_doses"),
                label = "Number of doses",
                value = 10
              )
            ),
            conditionalPanel(
              condition = "!input.load_count_data_check & !input.use_aggr_count_data_check",
              ns = ns,
              numericInput(
                ns("num_aberrs"),
                label = "Maximum number of translocations per cell",
                value = 5
              )
            ),
            # Load from file ----
            conditionalPanel(
              condition = "input.load_count_data_check",
              ns = ns,
              fileInput(
                ns("load_count_data"),
                label = "File input",
                accept = c("txt/csv", "text/comma-separated-values", "text/plain", ".csv", ".txt", ".dat")
              )
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

      # Box: Fitting options ----
      box(
        width = 6,
        title = span(
          "Fitting options",
          help_modal_button(
            ns("help_fitting_options"),
            ns("help_fitting_options_modal")
          )
        ),
        status = "info",
        collapsible = TRUE,

        # Help modal
        bsplus::bs_modal(
          id = ns("help_fitting_options_modal"),
          title = "Help: Fitting options",
          size = "large",
          body = tagList(
            # Option selection
            radioGroupButtons(
              inputId = ns("help_fitting_options_option"),
              label = NULL,
              choices = c(
                "Fitting formula"  = "formula",
                "Fitting model"    = "model"
              )
            ),
            # Contents
            conditionalPanel(
              condition = "input.help_fitting_options_option == 'formula'",
              ns = ns,
              include_help("fitting/fitting_options_formula.md")
            ),
            conditionalPanel(
              condition = "input.help_fitting_options_option == 'model'",
              ns = ns,
              include_help("fitting/fitting_options_model.md")
            )
          )
        ),
        fluidRow(
          col_12(
            # Fitting formula
            selectInput(
              ns("formula_select"),
              label = "Fitting formula",
              choices = list_fitting_formulas(),
              selected = "lin-quad"
            ),
            # Fitting model
            selectInput(
              ns("family_select"),
              label = "Fitting model",
              choices = list(
                "Automatic" = "automatic",
                "Poisson" = "poisson",
                "Quasi-Poisson" = "quasipoisson"
              ),
              selected = "automatic"
            )
          )
        )
      )
    ),

    # Box: Irradiation conditions input ----
    fluidRow(
      box(
        width = 12,
        title = "Irradiation conditions",
        status = "primary",
        collapsible = TRUE,
        col_6(
          class = "col-inner-textinput-left",
          textInput(
            inputId = ns("irr_cond_irradiator_name"),
            label = "Name of the irradiator used",
            placeholder = NULL
          ),
          textAreaInput(
            inputId = ns("irr_cond_radiation_quality"),
            label = "Radiation quality",
            placeholder = "X or gamma rays and energy"
          ),
          textInput(
            inputId = ns("irr_cond_dose_rate"),
            label = "Dose rate (Gy/min)",
            placeholder = NULL
          ),
          textInput(
            inputId = ns("irr_cond_dose_quantity"),
            label = "Dose quantity",
            placeholder = "In water or air kerma"
          )
        ),
        col_6(
          class = "col-inner-textinput-right",
          textInput(
            inputId = ns("irr_cond_whole_blood"),
            label = "Whole blood or isolated lymphocytes",
            placeholder = NULL
          ),
          textInput(
            inputId = ns("irr_cond_temperature"),
            label = "Temperature",
            placeholder = "Temperature during the irradiation"
          ),
          textInput(
            inputId = ns("irr_cond_time"),
            label = "Time incubations",
            placeholder = "Time incubations after sample irradiation"
          ),
          textAreaInput(
            inputId = ns("irr_cond_beam_characteristics"),
            label = "Beam characteristics",
            placeholder = "Beam quality indicators, filtration (X-rays), energy (Gamma rays), ..."
          )
        )
      )
    ),

    # Box: hot Count data input ----
    fluidRow(
      box(
        width = 12,
        title = "Data input",
        status = "primary",
        collapsible = TRUE,
        div(
          class = "hot-improved",
          rHandsontableOutput(ns("count_data_hot"))
        ),

        # Buttons
        br(),
        div(
          style = "display: inline-block;",
          conditionalPanel(
            condition = "!input.use_aggr_count_data_check",
            ns = ns,
            actionButton(
              ns("button_upd_params"),
              class = "inputs-button",
              label = "Calculate parameters"
            ),
            widget_sep()
          )
        ),
        downloadButton(
          ns("save_count_data"),
          class = "side-widget-download",
          label = "Save count data"
        ),
        div(
          class = "side-widget-format",
          selectInput(
            ns("save_count_data_format"),
            label = NULL,
            width = "75px",
            choices = list(".csv", ".tex"),
            selected = ".csv"
          )
        ),
        widget_sep_vert(),
        actionButton(
          ns("button_fit"),
          class = "inputs-button",
          label = "Calculate fitting"
        ),
        div(
          class = "side-widget-tall",
          # Translocation frequency
          selectInput(
            ns("frequency_select"),
            label = NULL,
            width = "180px",
            choices = list(
              "Measured by FISH" = "measured_freq",
              "Full genome"      = "full_gen_freq"
            ),
            selected = "measured_freq"
          )
        )
      )
    ),
    fluidRow(
      col_6_inner(
        # tabBox: Fit results ----
        tabBox(
          id = ns("fit_results_tabs"),
          width = 12,
          side = "left",
          tabPanel(
            title = "Result of curve fit",
            h5("Fit formula"),
            uiOutput(ns("fit_formula_tex")),
            h5("Model"),
            uiOutput(ns("fit_model_summary")),
            br(),
            h5("Coefficients"),
            div(
              class = "hot-improved",
              rHandsontableOutput(ns("fit_coeffs"))
            )
          ),
          tabPanel(
            title = "Summary statistics",
            h5("Model-level statistics"),
            div(
              class = "hot-improved",
              rHandsontableOutput(ns("fit_model_statistics"))
            ),
            br(),
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
        ),

        # Box: Export data and results ----
        box(
          width = 12,
          title = span(
            "Export results",
            help_modal_button(
              ns("help_fit_data_save"),
              ns("help_fit_data_save_modal")
            )
          ),
          status = "warning",
          collapsible = TRUE,

          # Help modal
          bsplus::bs_modal(
            id = ns("help_fit_data_save_modal"),
            title = "Help: Export results",
            size = "large",
            body = tagList(
              # Option selection
              radioGroupButtons(
                inputId = ns("help_fit_data_save_option"),
                label = NULL,
                choices = c(
                  "Fitting data" = "data",
                  "Report"       = "report"
                )
              ),
              # Contents
              conditionalPanel(
                condition = "input.help_fit_data_save_option == 'data'",
                ns = ns,
                include_help("save/fit_data_save.md")
              ),
              conditionalPanel(
                condition = "input.help_fit_data_save_option == 'report'",
                ns = ns,
                include_help("save/fit_data_save_report.md")
              )
            )
          ),

          # Download fit data & report
          downloadButton(
            ns("save_fit_data"),
            class = "side-widget-download",
            label = "Save fitting data"
          ),
          div(
            class = "side-widget-format",
            selectInput(
              ns("save_fit_data_format"),
              label = NULL,
              width = "75px",
              choices = list(".rds"),
              selected = ".rds"
            )
          ),
          widget_sep_vert(),
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
      # Box: Plot box ----
      box(
        width = 6,
        title = "Curve plot",
        status = "success",
        collapsible = TRUE,

        # Plot
        plotOutput(
          ns("plot")
        ),
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
