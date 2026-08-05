#' Interlab UI Module
#'
#' @param id Namespace for the {shiny} module.
#' @param label Internal label of the {shiny} module.
#'
#' @import shiny shinydashboard shinyWidgets rhandsontable
#' @noRd

mod_interlab_ui <- function(id, label) {
  ns <- NS(id)

  tabItem(
    class = "tabitem-container",
    tabName = label,
    h2("Interlab comparison"),
    fluidRow(
      col_6(
      box(
        width = 13,
        title = span(
          "1. Data input",
          help_modal_button(
            ns("help_interlab"),
            ns("help_interlab_modal")
          )
        ),
        status = "info",
        collapsible = TRUE,
        # Help modal
        bsplus::bs_modal(
          id = ns("help_interlab_modal"),
          title = "Help: Interlaboratory comparison",
          size = "large",
          body = tagList(
            #Option selection
            radioGroupButtons(
              inputId = ns("help_interlab_option"),
              label = NULL,
              choices = c(
                "Z-Score" = "zscore",
                "Deviation from reference" = "dev")
            ),
            conditionalPanel(
              condition = "input.help_interlab_option === 'zscore'",
              ns = ns,
              include_help("interlab/interlab_z.md")
            ),
            conditionalPanel(
              condition = "input.help_interlab_option === 'dev'",
              ns = ns,
              include_help("interlab/interlab_dev.md")
            )
          )
        ),
        col_12(
        # selectInput(
        #   ns("technique"),
        #   width = 165,
        #   label = "Select technique",
        #   choices = c("Dicentrics", "Translocations", "Micronuclei"),
        #   selected = "Dicentrics"
        #   ),
        numericInput(
          ns("num_labs"),
          label = "How many labs do participate in the comparison?",
          value = 2,
          step = 1,
          min = 2
        ),
        uiOutput(ns("file_inputs_interlab")),
        actionButton(
          ns("button_table_interlab"),
          class = "options-button",
          label = "Dose estimation summary"
        )
      )
    )
    ),
    col_6(
      tabBox(
        width = 13,
        tabPanel(
          "Data summary",
          div(
            style = "display: flex; justify-content: center;",
            div(class = "hot-improved",
                rHandsontableOutput(ns("summary_table_ilc"))
            )),
          widget_sep(),
          br(),
          br(),
          downloadButton(
            ns("save_summary_inter_ilc"),
            class = "side-widget-download",
            label = "Save data"
          ),
          div(
            class = "side-widget-format",
            selectInput(
              ns("save_summary_format_ilc"),
              label = NULL,
              width = "75px",
              choices = list(".csv", ".xlsx",".tex"),
              selected = ".xlsx"
            )),
          widget_sep(),
          br(),
          br(),
          actionButton(
            ns("button_plot_summary"),
            class = "options-button",
            label = "Plot summary"
          )
        ),
        tabPanel(
          "Curves",
          div(
            style = "display: flex; justify-content: center;",
            div(class = "hot-improved",
                rHandsontableOutput(ns("summary_table_curve"))
            )),
          widget_sep(),
          br(),
          br(),
          downloadButton(
            ns("save_summary_inter_curve"),
            class = "side-widget-download",
            label = "Save data"
          ),
          div(
            class = "side-widget-format",
            selectInput(
              ns("save_summary_format_curve"),
              label = NULL,
              width = "75px",
              choices = list(".csv", ".xlsx",".tex"),
              selected = ".xlsx"
              )
            ),
          widget_sep(),
          br(),
          br(),
          actionButton(
            ns("button_plot_curves"),
            class = "options-button",
            label = "Plot curves"
          )
        ),
        tabPanel(
          "Doses",
          div(
            style = "display: flex; justify-content: center;",
            div(class = "hot-improved",
                rHandsontableOutput(ns("dose_table"))
            ))
        ),
        tabPanel(
          "Frequencies",
          div(
            style = "display: flex; justify-content: center;",
            div(class = "hot-improved",
                rHandsontableOutput(ns("freq_table"))
              )
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.button_plot_summary",
      ns = ns,
      tabBox(
        width = 13,
        title = "Plots summary",
        tabPanel(
          "Dose estimates ",
          plotOutput(ns("dose_boxplot")),
          # Download plot
          downloadButton(
            ns("save_boxplot_interlab_dose"),
            class = "results-button side-widget-download",
            label = "Save plot"
          ),
          div(
            class = "side-widget-format",
            selectInput(
              ns("save_boxplot_format_interlab_dose"),
              label = NULL,
              width = "75px",
              choices = list(".jpg", ".png",".pdf"),
              selected = ".jpg"
            )
          )
        ),
        tabPanel(
          "Yield",
          plotOutput(ns("yield_boxplot")),
          # Download plot
          downloadButton(
            ns("save_boxplot_interlab_yield"),
            class = "results-button side-widget-download",
            label = "Save plot"
          ),
          div(
            class = "side-widget-format",
            selectInput(
              ns("save_boxplot_format_interlab_yield"),
              label = NULL,
              width = "75px",
              choices = list(".jpg", ".png",".pdf"),
              selected = ".jpg"
            )
          )
        ),
        tabPanel(
          "U-test ",
          plotOutput(ns("u_test_plot")),
          # Download plot
          downloadButton(
            ns("save_plot_interlab_u"),
            class = "results-button side-widget-download",
            label = "Save plot"
          ),
          div(
            class = "side-widget-format",
            selectInput(
              ns("save_plot_format_interlab_u"),
              label = NULL,
              width = "75px",
              choices = list(".jpg", ".png",".pdf"),
              selected = ".jpg"
            )
          )
        ),
        tabPanel(
          "Dispersion index ",
          plotOutput(ns("DI_plot")),
          # Download plot
          downloadButton(
            ns("save_plot_interlab_DI"),
            class = "results-button side-widget-download",
            label = "Save plot"
          ),
          div(
            class = "side-widget-format",
            selectInput(
              ns("save_plot_format_interlab_DI"),
              label = NULL,
              width = "75px",
              choices = list(".jpg", ".png",".pdf"),
              selected = ".jpg"
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.button_plot_curves",
      ns = ns,
      tabBox(
        width = 13,
        title = "Plots curves",
        tabPanel(
          "Manual curves",
          plotOutput(ns("curves_manual")),
          # Download plot
          downloadButton(
            ns("save_plot_interlab_cm"),
            class = "results-button side-widget-download",
            label = "Save plot"
          ),
          div(
            class = "side-widget-format",
            selectInput(
              ns("save_plot_format_interlab_cm"),
              label = NULL,
              width = "75px",
              choices = list(".jpg", ".png",".pdf"),
              selected = ".jpg"
            )
          )
        ),
        tabPanel(
          "Automatic curves",
          plotOutput(ns("curves_auto")),
          # Download plot
          downloadButton(
            ns("save_plot_interlab_ca"),
            class = "results-button side-widget-download",
            label = "Save plot"
          ),
          div(
            class = "side-widget-format",
            selectInput(
              ns("save_plot_format_interlab_ca"),
              label = NULL,
              width = "75px",
              choices = list(".jpg", ".png",".pdf"),
              selected = ".jpg"
            )
          )
        ),
        tabPanel(
          "Manual Barplot ",
          plotOutput(ns("barplot_manual")),
          # Download plot
          downloadButton(
            ns("save_plot_interlab_bm"),
            class = "results-button side-widget-download",
            label = "Save plot"
          ),
          div(
            class = "side-widget-format",
            selectInput(
              ns("save_plot_format_interlab_bm"),
              label = NULL,
              width = "75px",
              choices = list(".jpg", ".png",".pdf"),
              selected = ".jpg"
            )
          )
        ),
        tabPanel(
          "Automatic Barplot ",
          plotOutput(ns("barplot_auto")),
          # Download plot
          downloadButton(
            ns("save_plot_interlab_ba"),
            class = "results-button side-widget-download",
            label = "Save plot"
          ),
          div(
            class = "side-widget-format",
            selectInput(
              ns("save_plot_format_interlab_ba"),
              label = NULL,
              width = "75px",
              choices = list(".jpg", ".png",".pdf"),
              selected = ".jpg"
            )
          )
        )
      )
    ),
    fluidRow(
        col_6(
        box(
          width = 13,
          title = "2. Z-score data input",
          status = "info",
          collapsible = TRUE,
            col_6(
            selectInput(
              ns("dose_or_freq"),
              width = 165,
              label = "Select Input type",
              choices = c("Dose", "Frequency"),
              selected = "Dose"
            ),
            selectInput(
              ns("select_method_zscore"),
              width = 165,
              label = HTML("Select algorithm",
                           as.character(actionLink(ns("zscore_info"), icon("question", style = "color:black")))),
              choices = c("algA", "algB", "QHampel"),
              selected = "algA"
            ),

            div(
              class = "side-widget-tall",
              uiOutput(ns("doses_tab"))),
            widget_sep(),
            br(),
            br()),
          col_6(
            uiOutput(ns("ref_vals"))
            ),
          col_12(
            actionButton(
              ns("button_zscore"),
              class = "options-button",
              label = "Calculate and plot"
            )
          )
        )
        ),
        col_6(
          tabBox(
            width = 13,
            title = "Z-Score",
            tabPanel(
              "Table",
              div(
                style = "display: flex; justify-content: center;",
                div(
                class = "hot-improved",
                rHandsontableOutput(ns("zscore_summary"))
                )
              ),
            widget_sep(),
            br(),
            br(),
            downloadButton(
              ns("save_summary_zscore"),
              class = "side-widget-download",
              label = "Save data"
            ),
            div(
              class = "side-widget-format",
              selectInput(
                ns("save_summary_format_zscore"),
                label = NULL,
                width = "75px",
                choices = list(".csv", ".xlsx",".tex"),
                selected = ".xlsx"
                )
              )
            )
          )
        )
    ),
    conditionalPanel(
      condition = "input.dose_or_freq === 'Dose'",
      ns = ns,
      conditionalPanel(
        condition = "input.button_zscore",
        ns = ns,

        tabBox(
          width = 13,
          title = "Plots ILC",
          tabPanel(
            "Dose estimation",
            plotOutput(ns("plot_triage_interlab_dose")),
            # Download plot
            downloadButton(
              ns("save_plot_triage_interlab_dose"),
              class = "results-button side-widget-download",
              label = "Save plot"
            ),
            div(
              class = "side-widget-format",
              selectInput(
                ns("save_plot_format_triage_interlab_dose"),
                label = NULL,
                width = "75px",
                choices = list(".jpg", ".png",".pdf"),
                selected = ".jpg"
              )
            )
          ),
          tabPanel(
            "Z-score",
            plotOutput(ns("plot_interlab_dose")),
            # Download plot
            downloadButton(
              ns("save_plot_interlab_dose"),
              class = "results-button side-widget-download",
              label = "Save plot"
            ),
            div(
              class = "side-widget-format",
              selectInput(
                ns("save_plot_format_interlab_dose"),
                label = NULL,
                width = "75px",
                choices = list(".jpg", ".png",".pdf"),
                selected = ".jpg"
              )
            )
          ),
          tabPanel(
            "Deviation from ref.",
            plotOutput(ns("plot_interlab_deviation")),
            # Download plot
            downloadButton(
              ns("save_plot_interlab_dev"),
              class = "results-button side-widget-download",
              label = "Save plot"
            ),
            div(
              class = "side-widget-format",
              selectInput(
                ns("save_plot_format_interlab_dev"),
                label = NULL,
                width = "75px",
                choices = list(".jpg", ".png",".pdf"),
                selected = ".jpg"
              )
            )
          ),
          tabPanel(
            "Z-score all samples",
            plotOutput(ns("plot_zscore_all_dose")),
            # Download plot
            downloadButton(
              ns("save_plot_zscore_all_dose"),
              class = "results-button side-widget-download",
              label = "Save plot"
            ),
            div(
              class = "side-widget-format",
              selectInput(
                ns("save_plot_format_zscore_all_dose"),
                label = NULL,
                width = "75px",
                choices = list(".jpg", ".png",".pdf"),
                selected = ".jpg"
              )
            )
          ),
          tabPanel(
            "Deviation all samples",
            plotOutput(ns("plot_deviation_all")),
            # Download plot
            downloadButton(
              ns("save_plot_deviation_all"),
              class = "results-button side-widget-download",
              label = "Save plot"
            ),
            div(
              class = "side-widget-format",
              selectInput(
                ns("save_plot_format_deviation_all"),
                label = NULL,
                width = "75px",
                choices = list(".jpg", ".png",".pdf"),
                selected = ".jpg"
              )
            )
          )
        )
      )),
      conditionalPanel(
        condition = "input.dose_or_freq === 'Frequency'",
       ns = ns,
       conditionalPanel(
         condition = "input.button_zscore",
         ns = ns,
        tabBox(
          width = 13,
          title = "Plots ILC",
          # tabPanel(
          #   "Dose estimation",
          #   plotOutput(ns("plot_triage_interlab_freq")),
          #   # Download plot
          #   downloadButton(
          #     ns("save_plot_triage_interlab_freq"),
          #     class = "results-button side-widget-download",
          #     label = "Save plot"
          #   ),
          #   div(
          #     class = "side-widget-format",
          #     selectInput(
          #       ns("save_plot_format_triage_interlab_freq"),
          #       label = NULL,
          #       width = "75px",
          #       choices = list(".jpg", ".pdf"),
          #       selected = ".jpg"
          #     )
          #   )
          # ),
          tabPanel(
            "Z-score",
            plotOutput(ns("plot_interlab_freq")),
            # Download plot
            downloadButton(
              ns("save_plot_interlab_freq"),
              class = "results-button side-widget-download",
              label = "Save plot"
            ),
            div(
              class = "side-widget-format",
              selectInput(
                ns("save_plot_format_interlab_freq"),
                label = NULL,
                width = "75px",
                choices = list(".jpg", ".png",".pdf"),
                selected = ".jpg"
              )
            )
          ),
          tabPanel(
            "Z-score all samples",
            plotOutput(ns("plot_zscore_all_freq")),
            # Download plot
            downloadButton(
              ns("save_plot_zscore_all_freq"),
              class = "results-button side-widget-download",
              label = "Save plot"
            ),
            div(
              class = "side-widget-format",
              selectInput(
                ns("save_plot_format_zscore_all_freq"),
                label = NULL,
                width = "75px",
                choices = list(".jpg", ".png",".pdf"),
                selected = ".jpg"
              )
            )
          )
        )
      )
      ),
  fluidRow(
  box(
    width = 6,
    title = span(
      HTML("3. Save results"),
      help_modal_button(
        ns("help_interlab_save"),
        ns("help_interlab_save_modal")
      )
    ),
    status = "warning",
    collapsible = TRUE,

    # Help Modal
    bsplus::bs_modal(
      id = ns("help_interlab_save_modal"),
      title = "Help: Export results",
      size = "large",
      body = tagList(
        # Contents
        include_help("interlab/save_report_interlab.md")
      )
    ),

    # Case description
    textAreaInput(
      inputId = ns("results_comments_i"),
      label = "Comments",
      placeholder = "Comments to be included on report"
    ),

    # Download report
    downloadButton(
      ns("save_report_i"),
      class = "export-button side-widget-download",
      label = "Download report"
    ),
    div(
      class = "side-widget-format",
      selectInput(
        ns("save_report_format_i"),
        label = NULL,
        width = "85px",
        choices = list(".pdf"),
        selected = ".pdf"
      ))
    ))
)}
