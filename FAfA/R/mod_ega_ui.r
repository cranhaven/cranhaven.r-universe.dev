#' EGA UI Module
#' @noRd
ega_ui <- function(id) {
  ns <- NS(id)
  detected_cores <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if (!is.finite(detected_cores) || detected_cores < 1L) detected_cores <- 2L
  default_boot_cores <- max(1L, ceiling(detected_cores / 2L))

  tagList(
    layout_columns(
      col_widths = c(4, 8),

      # Sol Taraf: Ayarlar
      card(
        card_header("EGA Setup", class = "bg-primary text-white", bs_icon("diagram-2")),
        card_body(
          selectInput(ns("ega_estimation_method_select"), "Network Model:",
                      choices = c("TMFG" = "TMFG", "Glasso" = "glasso"), selected = "TMFG"),
          selectInput(ns("ega_algorithm_select"), "Community Detection:",
                      choices = c(
                        "Walktrap (default)"  = "walktrap",
                        "Louvain"             = "louvain",
                        "Leiden"              = "leiden",
                        "Fast Greedy"         = "fast_greedy",
                        "Edge Betweenness"    = "edge_betweenness",
                        "Label Propagation"   = "label_prop"
                      ), selected = "walktrap"),
          radioButtons(ns("ega_correlation_type_radio"), "Correlation:",
                       choices = c("Auto" = "cor_auto", "Pearson" = "pearson"), inline = TRUE),
          actionButton(ns("run_ega_button"), "Run EGA",
                       icon = icon("project-diagram"), class = "btn-success w-100")
        )
      ),

      # Sağ Taraf: Plot
      card(
        class = "ega-network-card",
        card_header("Network Plot"),
        withSpinner(plotOutput(ns("ega_network_plot_output"), height = "500px"), type = 8, color = "#2C3E50"),
        selectInput(ns("ega_plot_download_format"), "Download format:",
                    choices = c("PNG (300 dpi)" = "png", "SVG" = "svg", "JPG (300 dpi)" = "jpg"),
                    selected = "png"),
        downloadButton(ns("download_ega_plot_button"), "Download Plot")
      )
    ),

    card(
      card_header("Dimensions & Structure"),
      navset_card_tab(
        nav_panel(
          title = "Item Allocation", icon = bs_icon("diagram-3"),
          verbatimTextOutput(ns("ega_dimensionality_summary_output")),
          withSpinner(
            tableOutput(ns("ega_item_community_table_output")),
            type = 8, color = "#2C3E50"
          )
        ),
        nav_panel(
          title = "Adjacency Matrix", icon = bs_icon("table"),
          div(
            style = "overflow-x: auto;",
            tableOutput(ns("ega_network_table_output"))
          ),
          div(
            class = "d-flex flex-wrap gap-2 mt-2",
            downloadButton(ns("download_ega_network_button"), "Download CSV", class = "btn-sm"),
            downloadButton(ns("download_ega_apa7"), "Download APA 7 Word", class = "btn-sm btn-primary")
          )
        )
      )
    ),

    card(
      class = "mt-3",
      card_header(
        "Bootstrap Exploratory Graph Analysis (bootEGA)",
        class = "bg-info text-white"
      ),
      card_body(
        p(
          "Estimate dimensional stability with EGAnet::bootEGA(), ",
          "dimensionStability(), and itemStability()."
        ),
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Bootstrap EGA Setup"),
            card_body(
              selectInput(
                ns("bootega_type"),
                "Bootstrap type:",
                choices = c(
                  "Parametric" = "parametric",
                  "Resampling (non-parametric)" = "resampling"
                ),
                selected = "parametric"
              ),
              numericInput(
                ns("bootega_iterations"),
                "Bootstrap samples (iter):",
                value = 500,
                min = 20,
                step = 10
              ),
              numericInput(
                ns("bootega_cores"),
                "Processor cores (ncores):",
                value = default_boot_cores,
                min = 1,
                max = detected_cores,
                step = 1
              ),
              numericInput(
                ns("bootega_seed"),
                "Random seed:",
                value = 2026,
                min = 1,
                step = 1
              ),
              checkboxInput(
                ns("bootega_typical_structure"),
                "Estimate the typical network structure",
                value = FALSE
              ),
              helpText(
                "EGAnet recommends 500 bootstrap samples. Structural consistency ",
                "is the proportion of bootstrap samples in which an empirical ",
                "dimension is exactly replicated."
              ),
              actionButton(
                ns("run_bootega_button"),
                "Run Bootstrap Exploratory Graph Analysis",
                icon = icon("play"),
                class = "btn-success w-100"
              )
            )
          ),
          card(
            card_header("Bootstrap EGA Results"),
            card_body(
              textOutput(ns("bootega_status")),
              navset_card_tab(
                nav_panel(
                  title = "Bootstrap Summary",
                  h5("Bootstrap distribution summary", class = "mt-3"),
                  tableOutput(ns("bootega_summary_table")),
                  h5("Dimension frequency", class = "mt-3"),
                  tableOutput(ns("bootega_frequency_table")),
                  div(
                    class = "d-flex flex-wrap gap-2 mt-2",
                    downloadButton(ns("download_bootega_summary"), "Download Summary CSV", class = "btn-sm"),
                    downloadButton(ns("download_bootega_frequency"), "Download Frequency CSV", class = "btn-sm"),
                    downloadButton(
                      ns("download_bootega_apa7"),
                      "Download APA 7 Word",
                      class = "btn-sm btn-primary"
                    )
                  )
                ),
                nav_panel(
                  title = "Dimension Stability",
                  p(
                    class = "mt-3 text-muted",
                    "Structural consistency reports exact dimension replication; ",
                    "average item stability summarizes item assignment stability within each dimension."
                  ),
                  tableOutput(ns("bootega_dimension_stability_table")),
                  downloadButton(
                    ns("download_bootega_dimension_stability"),
                    "Download Dimension Stability CSV",
                    class = "btn-sm"
                  )
                ),
                nav_panel(
                  title = "Item Stability",
                  tableOutput(ns("bootega_item_stability_table")),
                  withSpinner(
                    plotOutput(ns("bootega_item_stability_plot"), height = "500px"),
                    type = 8,
                    color = "#2C3E50"
                  ),
                  selectInput(
                    ns("bootega_plot_download_format"),
                    "Download format:",
                    choices = c(
                      "PNG (300 dpi)" = "png",
                      "SVG" = "svg",
                      "JPG (300 dpi)" = "jpg"
                    ),
                    selected = "png"
                  ),
                  div(
                    class = "d-flex flex-wrap gap-2 mt-2",
                    downloadButton(ns("download_bootega_item_stability"), "Download Item Stability CSV", class = "btn-sm"),
                    downloadButton(ns("download_bootega_item_plot"), "Download Item Stability Plot", class = "btn-sm")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
