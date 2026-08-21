#' Reliability UI
#' @noRd
reliability_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Reliability Setup", class = "bg-info text-white"),
        card_body(
          selectizeInput(
            ns("reliability_item_select"),
            "Select Items (leave empty for all):",
            choices  = NULL,
            multiple = TRUE,
            options  = list(placeholder = "Select specific variables...")
          ),
          selectizeInput(
            ns("reliability_factor_select"),
            "Use saved CFA dimensions:",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "Select one or more dimensions...")
          ),
          hr(),
          radioButtons(ns("reliability_coefficient_select"), "Coefficient:",
                       choices = c(
                         "Cronbach's Alpha"                  = "alpha",
                         "McDonald's Omega Total"            = "omega",
                         "McDonald's Omega Hierarchical"     = "omega_h",
                         "Armor's Theta"                     = "theta",
                         "Stratified Alpha"                  = "s_alpha",
                         "Composite Reliability & AVE (CFA)" = "cr"
                       )
          ),

          # Theta: correlation type
          conditionalPanel("input.reliability_coefficient_select == 'theta'", ns = ns,
                           radioButtons(ns("correlation_type_radio"), "Correlation:",
                                        choices = c("Pearson" = "cor", "Polychoric" = "poly"), inline = TRUE)
          ),

          # Stratified Alpha and CR use the same factor builder.
          conditionalPanel(
            "input.reliability_coefficient_select == 's_alpha' || input.reliability_coefficient_select == 'cr'",
            ns = ns,
            div(
              class = "border rounded p-3 mb-3 bg-light",
              h6("Model Definition"),
              textInput(
                ns("reliability_factor_name"),
                "Dimension name:",
                placeholder = "e.g. F1, F2"
              ),
              selectizeInput(
                ns("reliability_factor_items"),
                "Items in dimension:",
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Select items...")
              ),
              actionButton(
                ns("add_reliability_dimension"),
                "Add Dimension",
                icon = icon("plus"),
                class = "btn-primary w-100 mb-3"
              ),
              textAreaInput(
                ns("cfa_model_for_reliability_input"),
                "Model Syntax (lavaan):",
                rows = 5,
                placeholder = "Dimensions added above appear here automatically."
              ),
              helpText(
                "For Stratified Alpha, strata codes are generated automatically from these dimensions."
              )
            )
          ),

          # CR & AVE also needs the data type.
          conditionalPanel(
            "input.reliability_coefficient_select == 'cr'",
            ns = ns,
            radioButtons(ns("cr_correlation_type_radio"), "Data Type:",
                         choices = c("Continuous (Pearson)" = "cor", "Ordinal (Polychoric)" = "poly"),
                         inline = TRUE)
          ),

          actionButton(ns("run_reliability_button"), "Calculate",
                       icon = icon("calculator"), class = "btn-info w-100")
        )
      ),
      card(
        card_header("Result"),
        card_body(
          withSpinner(
            verbatimTextOutput(ns("reliability_result_output")),
            type = 8, color = "#2C3E50"
          )
        )
      )
    )
  )
}
