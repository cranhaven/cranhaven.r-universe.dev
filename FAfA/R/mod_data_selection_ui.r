#' Data Selection UI Module
#'
#' @param id A character string, the module id.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinycssloaders withSpinner
#' @noRd
data_selection_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      # Sol kolon (Yükleme) 4 birim, Sağ kolon (İstatistik ve Tablo) 8 birim
      col_widths = c(4, 8),

      # --- SOL KOLON: Sadece Veri Yükleme ---
      card(
        card_header(
          "Data Upload",
          class = "bg-primary text-white",
          bs_icon("upload")
        ),
        card_body(
          markdown("
            Please upload your data file. Supported formats: **.csv, .txt, .dat, .sav, .xlsx, .xls**.

            *Ensure missing values are represented as NA.*
            "),
          fileInput(
            ns("file1"),
            label = "Choose Data File:",
            accept = c(".csv", ".txt", ".dat", ".sav", ".xlsx", ".xls"),
            placeholder = "No file selected"
          ),
          checkboxInput(
            ns("has_header_checkbox"),
            label = "My data has a header row",
            value = TRUE
          ),
          actionButton(ns("analyze_data"), "Analyze Data", icon = icon("play"), class = "btn-success w-100")
        )
      ),

      # --- SAĞ KOLON: İstatistikler ve Önizleme ---
      tagList(
        # 1. Summary Statistics (Daha kompakt - Tek Satır)
        card(
          card_header("Summary Statistics", bs_icon("bar-chart")),
          card_body(
            layout_columns(
              # Kutuları tek satıra sığdırmak için genişlikleri ayarladık (Toplam 12)
              # Değişken ve Gözlem (3'er birim), Min/Max/Range (2'şer birim)
              col_widths = c(3, 3, 2, 2, 2),
              row_heights = "auto",
              uiOutput(ns("n_var_box")),     # Variables
              uiOutput(ns("n_obs_box")),     # Sample Size
              uiOutput(ns("min_val_box")),   # Min
              uiOutput(ns("max_val_box")),   # Max
              uiOutput(ns("cat_range_box"))  # Category Range
            )
          ),
          # Kartın gereksiz uzamasını engellemek için min-height ayarı
          style = "min-height: 150px;"
        ),

        # 2. Data Preview (Buraya taşındı)
        card(
          card_header("Data Preview", bs_icon("table")),
          card_body(
            withSpinner(tableOutput(ns("mydatatable")), type = 8, color = "#2C3E50"),
            style = "max-height: 500px; overflow-y: auto;" # Tablo çok uzarsa scroll çıksın
          )
        )
      )
    )
  )
}
