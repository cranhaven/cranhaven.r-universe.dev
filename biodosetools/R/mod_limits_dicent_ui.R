#' Characteristic limits UI Module
#'
#' @param id Namespace for the {shiny} module.
#' @param label Internal label of the {shiny} module.
#'
#' @import shiny shinydashboard shinyWidgets rhandsontable
#' @noRd
mod_limits_dicent_ui <- function(id, label) {
  # proband = case# <----
  ns <- NS(id)
  tabItem(
    class = "tabitem-container",
    tabName = label,
    h2("Dicentrics: Characteristic limits"),
    fluidRow(
    col_6(
#DATA INPUT OPTIONS ------------------------------------------------------------
      box(
        width = 13,
        title = span(
          "1. Data input options",
          help_modal_button(
            ns("help_limits_data"),
            ns("help_limits_data_modal")
          )
        ),
        status = "info",
        collapsible = TRUE,
        # Help modal
        bsplus::bs_modal(
          id = ns("help_limits_data_modal"),
          title = "Help: Characteristic limits data input",
          size = "large",
          body = tagList(
            #Option selection
            radioGroupButtons(
              inputId = ns("help_limits_option"),
              label = NULL,
              choices = c(
                "Compare case vs control"    = "poisson_test",
                "Characteristic limits"       = "limits")
            ),
            # Contents
            conditionalPanel(
              condition = "input.help_limits_option == 'limits'",
              ns = ns,
              include_help("limits/limits_input.md")
            ),
            conditionalPanel(
              condition = "input.help_limits_option == 'poisson_test'",
              ns = ns,
              include_help("limits/poisson_test_input.md")
            )
          )
        ),

          selectInput(
            ns("limits_select"),
            label = "Choose method",
            choices = list(
              "Compare case vs control" = "p_poisson_test",
              "Characteristic limits" = "limits"
            ),
            selected = "p_poisson_test"
         ),

        # Choice of method
        conditionalPanel(
          condition = "input.limits_select=='p_poisson_test'",
          ns = ns,
          fluidRow(
          col_6(
            h5("Control"),
            numericInput(
              ns("num_aberrs_control_comp"),
              label = "Number of dicentrics ",
              value = 1,
              min = 0
            ),
            h5("Case"),
            numericInput(
              ns("num_aberrs_proband_comp"),
              label = "Number of dicentrics",
              value = 1,
              min = 0
            ),
            actionButton(
              ns("button_compare_control"),
              class = "options-button",
              label = "Calculate"
            )
          ),
          col_6(
            h5("_"),
            numericInput(
              ns("num_cells_control_comp"),
              label = "Number of cells ",
              value = 1000,
              min = 0
            ),
            h5("_"),
            numericInput(
              ns("num_cells_proband_comp"),
              label = "Number of cells",
              value = 1000,
              min = 0
            )))),

        conditionalPanel(
          condition = "input.limits_select=='limits'",
          ns = ns,
         fluidRow(
           col_6(
            numericInput(
              ns("typeI_error"),
              label = paste0("Type I error (", rlang::as_utf8_character("\u03B1"), ")"),
              value = 0.05,
              step = 0.01,
              min = 0
            )
          ),
          col_6(
            numericInput(
              ns("typeII_error"),
              label = paste0("Type II error (", rlang::as_utf8_character("\u03B2"), ")"),
              value = 0.10,
              step = 0.01,
              min = 0
            )
          ))))
      ,
#RESULTS LIMITS-----------------------------------------------------------------
        conditionalPanel(
            condition = "input.limits_select=='limits'",
            ns = ns,
            tabBox(
              id = ns("fit_results_tabs"),
              width = 13,
              side = "left",
              tabPanel(
                title = "Characteristic limits",
                br(),
                    div(
                      style = "display: flex; justify-content: center;",
                      class = "hot-improved",
                      rHandsontableOutput(ns("decision_threshold"))
              ),
              widget_sep(),
              br(),
              widget_sep(),
              h6("** See the help button (?) in the Data input options tab for more information about the parameters."),
              downloadButton(
                ns("save_limits"),
                class = "side-widget-download",
                label = "Save data"
              ),
              div(
                class = "side-widget-format",
                selectInput(
                  ns("save_limits_format"),
                  label = NULL,
                  width = "75px",
                  choices = list(".csv", ".tex"),
                  selected = ".csv"
                )
              )

            ),
            tabPanel(
              title = ".RDS file data",
              conditionalPanel(
                condition = "input.load_fit_data_check_limits",
                ns = ns,
                h5("Fit formula"),
                uiOutput(ns("fit_formula_tex")),
                h5("Coefficients"),
                div(
                  style = "display: flex; justify-content: center;",
                  class = "hot-improved",
                  rHandsontableOutput(ns("fit_coeffs_limits"))
                )
            ),
            conditionalPanel(
              condition = "!input.load_fit_data_check_limits",
              ns = ns,
              div(
                style = "display: flex; justify-content: center;",
                h5("Please upload a .rds file to access this panel")
              )
            ))))
      ),

#DATA INPUT LIMITS---------------------------------------------------------------
     conditionalPanel(
          condition = "input.limits_select=='limits'",
          ns = ns,
          col_6(
            box(
              width = 13,
              title = "2. Data Input",
              status = "info",
              collapsible = TRUE,
                awesomeCheckbox(
                  inputId = ns("no_curve"),
                  status = "info",
                  label = "Without curve data",
                  value = TRUE
                ),
                awesomeCheckbox(
                  inputId = ns("curve_check_limits"),
                  status = "info",
                  label = "Enter manually the curve coefficients",
                  value = FALSE
                ),
                awesomeCheckbox(
                  inputId = ns("load_fit_data_check_limits"),
                  status = "info",
                  label = "Load fit data from .rds file",
                  value = FALSE
                ),

  # Manual curve input ---------------------------------------------------------
                conditionalPanel(
                  condition = "input.curve_check_limits",
                  ns = ns,
                  div(
                    class = "side-widget-tall",
                    selectInput(
                      ns("formula_select_limits"),
                      width = 165,
                      label = "Select Fitting formula",
                      choices = list_fitting_formulas(),
                      selected = "lin-quad"
                    )
                  ),
                  widget_sep(),
                  br(),
                  br(),
                  widget_label("Enter the coefficients manually"),
                  div(
                    style = "display: flex; justify-content: center;",
                    class = "hot-improved",
                    rHandsontableOutput(ns("fit_coeffs_hot_limits"))
                  ),
                  br()
                ),

  # Load from file -------------------------------------------------------------
                conditionalPanel(
                  condition = "input.load_fit_data_check_limits",
                  ns = ns,
                  fileInput(
                    ns("load_fit_data_limits"),
                    label = "File input",
                    accept = c(".rds")
                  )
                ),

  # Provide cells and dics -----------------------------------------------------
                selectInput(
                  ns("input_control"),
                  label = "Choose control data input",
                  choices = list(
                    "Dicentrics per cell" = "mu",
                    "Number of dicentrics and cells" = "full"
                  ),
                  selected = "full"
                )
                ,
                conditionalPanel(
                  condition = "input.input_control=='full'",
                  ns = ns,
                  fluidRow(
                    col_6(
                      numericInput(
                        ns("num_aberrs_control_full"),
                        label = "Number dicentrics control",
                        value = 1,
                        min = 0
                      )
                    ),
                    col_6(
                      numericInput(
                        ns("num_cells_control_full"),
                        label = "Number cells control",
                        value = 1000,
                        min = 0
                      )
                    )),

                  awesomeCheckbox(
                    inputId = ns("provide_cells_proband_check"),
                    status = "info",
                    label = "Provide cell number of case",
                    value = FALSE
                  )
                  ,
                  conditionalPanel("input.provide_cells_proband_check",
                                   ns=ns,
                                   numericInput(
                                     ns("num_cells_proband_full"),
                                     label = "Number cells case",
                                     value = 0,
                                     min = 0
                                   )
                  )
                ),
                conditionalPanel(
                  condition = "input.input_control=='mu'",
                  ns = ns,

                  numericInput(
                    ns("mean_control"),
                    label = "Dicentrics per cell",
                    value = 0.001,
                    step = 0.001,
                    min = 0
                  )
                  ,
                  awesomeCheckbox(
                    inputId = ns("provide_cells_proband_check_mu"),
                    status = "info",
                    label = "Provide cells number of case",
                    value = FALSE
                  )
                  ,
                  conditionalPanel("input.provide_cells_proband_check_mu",
                                   ns=ns,
                                   numericInput(
                                     ns("num_cells_proband_mu"),
                                     label = "Number cells case",
                                     value = 1,
                                     min = 0
                                   )
                  )
                ),
                # Buttons
                conditionalPanel("input.no_curve",
                                  ns=ns,
                                  actionButton(
                                    ns("button_get_limits"),
                                    class = "options-button",
                                    label = "Get characteristic limits"
                                  )),
                conditionalPanel("input.curve_check_limits",
                                 ns=ns,
                                 actionButton(
                                   ns("button_get_limits_curve_manual"),
                                   class = "options-button",
                                   label = "Get characteristic limits"
                                 )),
                conditionalPanel("input.load_fit_data_check_limits",
                                 ns=ns,
                                 actionButton(
                                   ns("button_get_limits_file"),
                                   class = "options-button",
                                   label = "Get characteristic limits"
                                 ))
                   ))),
#RESULTS POISSON----------------------------------------------------------------
    conditionalPanel(
      condition = "input.limits_select=='p_poisson_test'",
      ns = ns,
      col_6(
        tabBox(
          width = 13,
          #title = "Summary statistics",
          tabPanel(
            "Summary statistics",
            div(
              style = "display: flex; justify-content: center;",
              class = "hot-improved",
              rHandsontableOutput(ns("p_value"))
            )
          ))))
  ))

}
