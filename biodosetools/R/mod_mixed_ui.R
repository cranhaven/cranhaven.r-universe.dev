#' Dicentrics Dose Estimation Mixed fiewlds UI Module
#'
#' @param id Namespace for the {shiny} module.
#' @param label Internal label of the {shiny} module.
#'
#' @import shiny shinydashboard shinyWidgets rhandsontable
#' @noRd
mod_mixed_ui <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tabItem(
    class = "tabitem-container",
    tabName = label,
    h2("Dicentrics: Dose estimation for Criticality accidents"),
    fluidRow(
      #UPLOAD FILES-------------------------------------------------------------------
    #  col_6(
        box(
          width = 5,
          title = span(
            HTML("1. Curve fitting data options"),
            help_modal_button(
              ns("help_mixed_fields"),
              ns("help_mixed_fields_modal")
            )
          ),
          status = "info",
          collapsible = TRUE,

          # Help modal
          bsplus::bs_modal(
            id = ns("help_mixed_fields_modal"),
            title = "Help: Mixed fields Dose estimation",
            size = "large",
            body = tagList(
              include_help("mixed/mixed.md")
            )
          ),
            awesomeCheckbox(
              inputId = ns("curve_manual_mixed"),
              status = "info",
              label = "Enter manually the curve coefficients",
              value = FALSE
            ),

            # Manual curve input -------------------------------------------------------
            conditionalPanel(
              condition = "input.curve_manual_mixed",
              ns = ns,
#              awesomeCheckbox(
#                inputId = ns("check_neutrons_mx_manual"),
#                status = "info",
#                label = "Use a mixed neutron curve",
#                value = FALSE
#              ),
              h5("Gamma curve"),
              div(
                class = "side-widget-tall",
                selectInput(
                  ns("formula_select_mixed_gamma"),
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
                rHandsontableOutput(ns("fit_coeffs_hot_mixed_gamma"))
              ),
              br(),
              awesomeCheckbox(
                inputId = ns("use_var_cov_matrix_g"),
                status = "info",
                label = "Provide variance-covariance matrix",
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.use_var_cov_matrix_g",
                ns = ns,
                widget_label("Variance-covariance matrix"),
                div(
                  style = "display: flex; justify-content: center;",
                  class = "hot-improved",
                  rHandsontableOutput(ns("fit_var_cov_mat_hot_gamma"))
                )),
              h5("Neutron curve"),
              div(
                class = "side-widget-tall",
                selectInput(
                  ns("formula_select_mixed_neutrons"),
                  width = 165,
                  label = "Select Fitting formula",
                  choices = list_fitting_formulas(),
                  selected = "lin"
                )
              ),
              widget_sep(),
              br(),
              br(),
              widget_label("Enter the coefficients manually"),
              div(
                style = "display: flex; justify-content: center;",
                class = "hot-improved",
                rHandsontableOutput(ns("fit_coeffs_hot_mixed_neutrons"))
              ),
              br(),
              awesomeCheckbox(
                inputId = ns("use_var_cov_matrix_n"),
                status = "info",
                label = "Provide variance-covariance matrix",
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.use_var_cov_matrix_n",
                ns = ns,
                widget_label("Variance-covariance matrix"),
                div(
                  style = "display: flex; justify-content: center;",
                  class = "hot-improved",
                  rHandsontableOutput(ns("fit_var_cov_mat_hot_neutrons"))
                ))
#              ,conditionalPanel(
#                condition = "input.check_neutrons_mx_manual",
#                ns = ns,
#                numericInput(
#                  ns("p_m"),
#                  label = "Photon component mixed curve (0-100) %",
#                  value = 0,
#                  step = 1
 #               ))
            ),

          # Load from file -------------------------------------------------------------
          conditionalPanel(
            condition = "!input.curve_manual_mixed",
            ns = ns,

#              awesomeCheckbox(
#                inputId = ns("check_neutrons_mx_file"),
#                status = "info",
#                label = "Use a mixed neutron curve",
#                value = FALSE
#              ),

              fileInput(
                ns("load_fit_data_gamma"),
                label = "Upload Gamma rays file (.rds)",
                accept = c(".rds")
              ),
              fileInput(
                ns("load_fit_data_neutrons"),
                label = "Upload Neutrons file (.rds)",
                accept = c(".rds")
              )
#              ,conditionalPanel(
#                condition = "input.check_neutrons_mx_file",
#                ns = ns,
#                numericInput(
#                  ns("p_f"),
#                  label = "Photon component mixed curve (0-100) %",
#                  value = 0,
#                  step = 1
#                ))
            )
        )
        ,
        #INPUT CURVES BOX---------------------------------------------------------------
        conditionalPanel(
          condition = "!input.curve_manual_mixed",
          ns = ns,
          tabBox(
            width = 7,
            #title = "Result of curve fit",
            tabPanel(
              "Gamma",
              h5("Fit formula"),
              uiOutput(ns("gamma_formula")),
              h5("Coefficients"),
              div(
                style = "display: flex; justify-content: center;",
                div(class = "hot-improved",
                    rHandsontableOutput(ns("gamma_coeffs"))
                )))
            ,
            tabPanel(
              "Neutron",
              h5("Fit formula"),
              uiOutput(ns("neutrons_formula")),
              h5("Coefficients"),
              div(
                style = "display: flex; justify-content: center;",
                div(class = "hot-improved",
                    rHandsontableOutput(ns("neutrons_coeffs"))
                ))))


)),

      #DATA INPUT BOX-----------------------------------------------------------------
      fluidRow(
        box(
          width = 5,
          title =  HTML("2. Data input"),
          status = "info",
          collapsible = TRUE,

            awesomeCheckbox(
              inputId = ns("option_2_check"),
              status = "info",
              label = "Only provide total number of dicentrics",
              value = FALSE)
            ,
            conditionalPanel(
              condition = "input.option_2_check",
              ns = ns,
              numericInput(
                ns("num_ratio"),
                label = "Ratio (gamma/neutron)",
                value = 1,
                step = 0.01
              ),
              numericInput(
                ns("num_case_mx_only"),
                label = "Number of cases",
                value = 1,
                min = 1
              )
              # numericInput(
              #   ns("num_dics_mx"),
              #   label = "Number of dicentrics",
              #   value = 0
              # ),
              # numericInput(
              #   ns("num_cells_mx"),
              #   label = "Number of cells",
              #   value = 0
              # )
            ),

            conditionalPanel(
              condition = "!input.option_2_check",
              ns = ns,
              numericInput(
                ns("num_ratio"),
                label = "Ratio (gamma/neutron)",
                value = 1,
                step = 0.01
              ),
               numericInput(
                 ns("num_max_dic_cell"),
                 label = "Maximum number of dicentrics per cell",
                 value = 6
               ),
               numericInput(
                 ns("num_case_mx"),
                 label = "Number of cases",
                 value = 1,
                 min = 1
               )
             ),
          textAreaInput(
            inputId = ns("case_description_mx"),
            label = "Case description",
            placeholder = "Case description to be included on report"
          )
          ),
        col_7_inner(
        box(
          width = 12,
          title = HTML("3. Data input"),
          status = "primary",
          collapsible = TRUE,

          uiOutput(ns("correct_dose_input")),
        ),
        # #DOSE ESTIMATION BOX----------------------------------------------------
         tabBox(
           width = 12,
           #title = "Dose estimation",
           tabPanel(
             "Gamma",
             h5("Yield (dicentrics/cells)"),
             div(
               style = "display: flex; justify-content: center;",
               div(class = "hot-improved",
                   rHandsontableOutput(ns("value_g_y"))
               )),
             h5("Dose estimation (Gy)"),
             div(
               style = "display: flex; justify-content: center;",
               div(class = "hot-improved",
                   rHandsontableOutput(ns("value_g_d"))
               ))
           )
           ,
           tabPanel(
             "Neutron",
             h5("Yield (dics/cell)"),
             div(
               style = "display: flex; justify-content: center;",
               div(class = "hot-improved",
                   rHandsontableOutput(ns("value_n_y"))
               )),
             h5("Dose estimation [Gy]"),
             div(
               style = "display: flex; justify-content: center;",
               div(class = "hot-improved",
                   rHandsontableOutput(ns("value_n_d"))
               )))
           ,
           tabPanel(
             "Total",
             h5("Yield (dics/cell)"),
             div(
               style = "display: flex; justify-content: center;",
               div(class = "hot-improved",
                   rHandsontableOutput(ns("value_t_y"))
               )),
             h5("Dose estimation [Gy]"),
             div(
               style = "display: flex; justify-content: center;",
               div(class = "hot-improved",
                   rHandsontableOutput(ns("value_t_d"))
               )))
         )
         ),

#CURVE PLOT BOX-----------------------------------------------------------------

  tabBox(
    width = 12,
    title = "Curve plot",
    tabPanel(
      "Gamma",
      div(
        class = "side-widget-tall",
        uiOutput(ns("cases_tab"))),
      widget_sep(),
      br(),
      br(),

      # numericInput(
      #     ns("dose_to_plot"),
      #     label = HTML("Select dose to plot:",
      #                  as.character(actionLink(ns("dose_info"), icon("question", style = "color:black")))),
      #     value = 1,
      #     min = 1,
      #     width = '140px'
      #   ),
      # Modal Dialog (Popup)
      #tags$script(HTML("$('#dose_info').css('cursor', 'pointer');"))


      # Plot output
      plotOutput(ns("curve_plot_mx_g")),

      actionButton(
        ns("plot_button"),
        class = "options-button",
        label = "Plot curves"
      ),
      # Download plot

      downloadButton(
        ns("save_plot_g"),
        class = "results-button side-widget-download",
        label = "Save plot"
      ),
      div(
        class = "side-widget-format",
        selectInput(
          ns("save_plot_format_gamma"),
          label = NULL,
          width = "75px",
          choices = list(".jpg", ".png",".pdf"),
          selected = ".jpg"
        )
      )

    )
    ,
    tabPanel(
      "Neutron",
      # Plot output
      plotOutput(ns("curve_plot_mx_n")),

      # Download plot
      downloadButton(
        ns("save_plot_n"),
        class = "results-button side-widget-download",
        label = "Save plot"
      ),
      div(
        class = "side-widget-format",
        selectInput(
          ns("save_plot_format_neutrons"),
          label = NULL,
          width = "75px",
          choices = list(".jpg", ".png",".pdf"),
          selected = ".jpg"
        )
      ))),
  #SAVE RESULTS BOX---------------------------------------------------------------
  box(
    width = 6,
    title = span(
      HTML("4. Save results"),
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
        include_help("mixed/estimation_data_save_report_mx.md")
      )
    ),

    # Case description
    textAreaInput(
      inputId = ns("results_comments"),
      label = "Comments",
      placeholder = "Comments to be included on report"
    ),
    downloadButton(
      ns("save_results_mx"),
      class = "export-button side-widget-download",
      label = "Save data"
    ),
    div(
      class = "side-widget-format",
      selectInput(
        ns("save_results_mx_format"),
        label = NULL,
        width = "75px",
        choices =  list(".rds", ".xlsx"),
        selected = ".rds"
      )
    ),

    # Download report
    downloadButton(
      ns("save_report_mx"),
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
      ))))

  )

}
