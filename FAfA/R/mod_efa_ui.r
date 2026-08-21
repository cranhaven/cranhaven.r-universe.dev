#' EFA UI Modules
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd

# 1. Factor Retention
efa_ui_fac_ret <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        class = "factor-retention-card",
        card_header("Retention Methods", class = "bg-primary text-white", bs_icon("gear")),
        card_body(
          div(
            class = "factor-retention-method-select",
            selectizeInput(
              ns("dimension_methods"), "Select Method:",
              choices = c(
                "Scree Plot (Eigenvalues)"         = "scree_plot",
                "Optimal Parallel Analysis (MRFA)" = "pa_mrfa",
                "Traditional Parallel Analysis"    = "pa_traditional",
                "Parallel Analysis (Lubbe, 2019)"  = "pa_lubbe",
                "Hull Method"                       = "hull_method",
                "MAP (Original)"                    = "map_method_tra",
                "MAP (Revised)"                     = "map_method_rev",
                "EGA (TMFG)"                        = "EGA_tmfg",
                "EGA (Glasso)"                      = "EGA_glasso",
                "Empirical Kaiser (EKC)"            = "EK_C",
                "Comparison Data (CD)"              = "comp_data_method"
              ),
              selected = "hull_method",
              options = list(maxOptions = 100)
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] === 'pa_lubbe'", ns("dimension_methods")),
            div(
              class = "mt-3",
              p(
                class = "text-muted small mb-2",
                "The marginal distribution of each item is preserved through permutation. The same automatic correlation estimator is used for the observed and reference eigenvalues."
              ),
              layout_columns(
                col_widths = c(4, 4, 4),
                numericInput(
                  ns("lubbe_iterations"), "Permutations:",
                  value = 100, min = 20, max = 5000, step = 20
                ),
                numericInput(
                  ns("lubbe_quantile"), "Reference quantile:",
                  value = 0.95, min = 0.50, max = 0.99, step = 0.01
                ),
                numericInput(
                  ns("lubbe_seed"), "Random seed:",
                  value = 2026, min = 0, step = 1
                )
              ),
              tags$small(
                tags$strong("Reference: "),
                tags$a(
                  "Lubbe (2019)",
                  href = "https://doi.org/10.1037/met0000171",
                  target = "_blank",
                  rel = "noopener noreferrer"
                ),
                ", Psychological Methods, 24(3), 339-351."
              )
            )
          ),
          actionButton(ns("run_factor_ret"), "Run Analysis",
                       icon = icon("play"), class = "btn-success w-100")
        )
      ),
      card(
        card_header("Results"),
        card_body(
          withSpinner(tableOutput(ns("dim_ret_results")), type = 8, color = "#2C3E50"),
          conditionalPanel(
            condition = sprintf("input['%s'] === 'scree_plot'", ns("dimension_methods")),
            withSpinner(
              plotOutput(ns("scree_plot"), height = "360px"),
              type = 8, color = "#2C3E50"
            ),
            div(
              class = "d-flex flex-wrap gap-2 mt-2",
              downloadButton(ns("download_scree_png"), "Download PNG (300 dpi)", class = "btn-sm"),
              downloadButton(ns("download_scree_svg"), "Download SVG", class = "btn-sm")
            )
          )
        )
      )
    )
  )
}

# 2. EFA Setup
efa_ui_analysis <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      class = "efa-configuration-card",
      card_header("EFA Configuration", class = "bg-primary text-white", bs_icon("sliders")),
      card_body(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          radioButtons(ns("cor_kind"), "Correlation:",
                       choices = c("Pearson" = "pea", "Polychoric" = "poly"),
                       selected = "poly"),
          numericInput(ns("number_factor"), "No. of Factors:", value = 1, min = 1),
          selectInput(ns("fact_method"), "Extraction:",
                      choices = c(
                        "Minimum Residuals"         = "minres",
                        "Maximum Likelihood"        = "ml",
                        "Principal Axis"            = "pa",
                        "Unweighted Least Squares"  = "uls",
                        "Weighted Least Squares"    = "wls",
                        "Minimum Rank"              = "minrank",
                        "Minimum Chi-Square"        = "minchi",
                        "Generalized Least Squares" = "gls"
                      ), selected = "minres"),
          selectInput(ns("rotating_method"), "Rotation:",
                      choices = list(
                        "None"       = list("None" = "none"),
                        "Oblique"    = list(
                          "Oblimin"    = "oblimin",
                          "Promax"     = "promax",
                          "Quartimin"  = "quartimin",
                          "BiquartMin" = "biquartimin",
                          "GeominQ"    = "geominQ",
                          "BentlerQ"   = "bentlerQ",
                          "Simplimax"  = "simplimax",
                          "Cluster"    = "cluster"
                        ),
                        "Orthogonal" = list(
                          "Varimax"    = "varimax",
                          "Quartimax"  = "quartimax",
                          "Equamax"    = "equamax",
                          "BentlerT"   = "bentlerT",
                          "GeominT"    = "geominT",
                          "Bifactor"   = "bifactor"
                        )
                      ), selected = "oblimin")
        ),
        hr(),
        actionButton(ns("run_efa"), "Run EFA",
                     icon = icon("play"), class = "btn-success w-100 btn-lg")
      )
    )
  )
}

# 3. EFA Report
efa_ui_report <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Sampling Adequacy & Sphericity", bs_icon("check-circle")),
      card_body(
        layout_columns(
          col_widths = c(5, 7),
          div(
            tags$p(class = "text-muted small mb-1", "KMO Measure of Sampling Adequacy"),
            htmlOutput(ns("kmo_result"))
          ),
          div(
            tags$p(class = "text-muted small mb-1", "Bartlett's Test of Sphericity"),
            tableOutput(ns("bartlett"))
          )
        )
      )
    ),
    card(
      card_header("Factor Solution & Visualisation", bs_icon("table")),
      navset_card_tab(
        nav_panel(
          title = "Heatmap", icon = bs_icon("grid-3x3"),
          div(
            class = "d-flex flex-wrap align-items-end gap-2 mb-2",
            div(
              style = "min-width:260px; flex:1;",
              selectInput(
                ns("heatmap_palette"), "Colour palette:",
                choices = c(
                  "Blue - White - Red"     = "blue_red",
                  "Black - White"          = "grayscale",
                  "Purple - White - Green" = "purple_green",
                  "Orange - White - Blue"  = "orange_blue",
                  "Teal - White - Rose"    = "teal_rose"
                ),
                selected = "blue_red"
              )
            ),
            div(
              style = "min-width:210px; flex:0 1 240px;",
              checkboxInput(
                ns("heatmap_show_values"),
                "Show correlation values",
                value = TRUE
              )
            ),
            downloadButton(
              ns("download_heatmap_png"), "Download PNG (300 dpi)",
              class = "btn-sm mb-3"
            ),
            downloadButton(
              ns("download_heatmap_svg"), "Download SVG",
              class = "btn-sm mb-3"
            )
          ),
          withSpinner(
            plotOutput(ns("heat_map"), height = "480px"),
            type = 8, color = "#2C3E50"
          ),
          div(
            class = "mt-2 p-2 rounded",
            style = "background:#f1f5f9; font-size:0.82rem; color:#475569;",
            bs_icon("info-circle"), " ",
            textOutput(ns("cor_range_text"), inline = TRUE)
          )
        ),
        nav_panel(
          title = "Loadings", icon = bs_icon("list-ol"),
          tableOutput(ns("efa_result_str")),
          div(
            class = "d-flex flex-wrap gap-2 mt-2",
            downloadButton(ns("download_efa_loadings"), "Download CSV", class = "btn-sm"),
            downloadButton(ns("download_efa_apa7"), "Download APA 7 Word", class = "btn-sm btn-primary")
          )
        ),
        nav_panel("Variance Explained", tableOutput(ns("efa_result_expl_var"))),
        nav_panel("Factor Correlations (Phi)", tableOutput(ns("efa_result_interf_cor")))
      )
    )
  )
}
