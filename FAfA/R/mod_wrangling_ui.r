#' Wrangling UI Modules
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd

# 1. Exclude Variables
wrangling_ui_ex_var <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(6, 6),

      # --- LEFT: Available Variables ---
      card(
        card_header(
          class = "bg-success text-white",
          bs_icon("check2-square"), " Available Variables"
        ),
        card_body(
          p(class = "text-muted small mb-2",
            "Tick the variables you want to remove from the active dataset, then click Exclude."),
          div(
            style = paste(
              "max-height: 380px; overflow-y: auto;",
              "border: 1px solid #dee2e6; border-radius: 6px; padding: 8px 12px;"
            ),
            checkboxGroupInput(
              ns("available_vars_checkbox"),
              label    = NULL,
              choices  = NULL,
              selected = NULL
            )
          ),
          hr(class = "my-2"),
          actionButton(
            ns("exclude_button"),
            label = "Exclude Selected",
            icon  = icon("minus-circle"),
            class = "btn-danger w-100"
          )
        )
      ),

      # --- RIGHT: Excluded Variables + Summary + Actions ---
      card(
        card_header(
          class = "bg-danger text-white",
          bs_icon("x-circle"), " Excluded Variables"
        ),
        card_body(
          # Variable count summary
          uiOutput(ns("variable_summary")),
          hr(class = "my-2"),

          # List of excluded vars (tick to recover)
          p(class = "text-muted small mb-2",
            "Tick variables to bring back, then click Recover."),
          div(
            style = paste(
              "max-height: 240px; overflow-y: auto;",
              "border: 1px solid #dee2e6; border-radius: 6px; padding: 8px 12px;"
            ),
            checkboxGroupInput(
              ns("excluded_vars_checkbox"),
              label    = NULL,
              choices  = NULL,
              selected = NULL
            )
          ),
          hr(class = "my-2"),

          # Recover / Reset
          div(
            class = "d-flex gap-2 mb-2",
            actionButton(
              ns("recover_button"),
              label = "Recover Selected",
              icon  = icon("undo"),
              class = "btn-success flex-fill"
            ),
            actionButton(
              ns("reset_button"),
              label = "Reset All",
              icon  = icon("refresh"),
              class = "btn-outline-secondary flex-fill"
            )
          ),

          # Download active dataset
          downloadButton(
            ns("download_excluded_data_button"),
            label = "Download Active Data",
            class = "w-100"
          )
        )
      )
    )
  )
}

# Reverse-scoring
wrangling_ui_recode <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Reverse-score Items", class = "bg-info text-white", bs_icon("pencil-square")),
        card_body(
          p("Select negatively worded items, then confirm the response scale or let the program detect item limits."),
          selectizeInput(
            ns("reverse_variables"),
            "Items to reverse-score:",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "Select numeric items...")
          ),
          checkboxInput(
            ns("detect_recode_limits"),
            "Detect minimum and maximum separately for each item",
            value = FALSE
          ),
          conditionalPanel(
            condition = "!input.detect_recode_limits",
            ns = ns,
            div(
              class = "d-flex gap-2",
              numericInput(ns("recode_minimum"), "Scale minimum:", value = 1, step = 1),
              numericInput(ns("recode_maximum"), "Scale maximum:", value = 5, step = 1)
            )
          ),
          div(
            class = "p-3 mb-3 rounded",
            style = "background:#f1f5f9; color:#334155;",
            tags$b("How it is calculated"), tags$br(),
            "New score = scale minimum + scale maximum - original score.", tags$br(),
            "For a 1-5 item this becomes: 1 + 5 - score = 6 - score.", tags$br(),
            "Observed limits are optional; fixed scale limits are safer when the sample does not contain every response category."
          ),
          div(
            class = "d-flex gap-2",
            actionButton(ns("apply_reverse_scoring"), "Reverse & Update",
                         icon = icon("refresh"), class = "btn-info flex-fill"),
            actionButton(ns("reset_reverse_scoring"), "Reset",
                         icon = icon("undo"), class = "btn-outline-secondary flex-fill")
          ),
          hr(),
          downloadButton(ns("download_recoded_data"), "Download Recoded Data", class = "w-100")
        )
      ),
      card(
        card_header("Applied Scoring Rules"),
        card_body(
          textOutput(ns("recode_status")),
          tableOutput(ns("recode_summary"))
        )
      )
    )
  )
}

# Dataset splitting
wrangling_ui_split <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = 8,
      style = "margin: 0 auto;",
      card(
        card_header("Split Dataset", class = "bg-warning text-dark", bs_icon("scissors")),
        card_body(
          p("Randomly split data into two subsets (e.g., for EFA/CFA)."),
          sliderInput(ns("split_percentage_slider"), "First Subset %:",
                      min = 10, max = 90, value = 50, step = 5, post = "%"),
          numericInput(ns("split_seed"), "Random seed:", value = 1234, min = 0, step = 1),
          actionButton(ns("split_data_button"), "Split Data",
                       icon = icon("random"), class = "btn-warning w-100"),
          hr(),
          div(class = "d-flex gap-2",
              downloadButton(ns("download_first_subset_button"),  "Save Part 1", class = "flex-fill"),
              downloadButton(ns("download_second_subset_button"), "Save Part 2", class = "flex-fill")
          )
        )
      )
    )
  )
}

# Outlier management
wrangling_ui_outliers <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Outlier Detection", class = "bg-dark text-white", bs_icon("search")),
        card_body(
          p("Detect multivariate outliers using Mahalanobis Distance."),
          numericInput(ns("mah_p_value_threshold_input"), "P-value Threshold:",
                       value = 0.001, min = 0.00001, max = 0.05, step = 0.0001),
          actionButton(ns("check_outliers_button"), "Find Outliers",
                       icon = icon("search"), class = "btn-primary w-100 mb-2"),
          actionButton(ns("remove_outliers_button"), "Remove & Update",
                       icon = icon("user-minus"), class = "btn-danger w-100"),
          hr(),
          downloadButton(ns("download_data_no_outliers_button"), "Download Clean Data", class = "w-100")
        )
      ),
      card(
        card_header("Outlier Results"),
        card_body(
          textOutput(ns("outlier_count_text")),
          withSpinner(tableOutput(ns("outliers_table")), type = 8, color = "#2C3E50")
        )
      )
    )
  )
}
