#######################################

# LAPOP Map World / Americas-Only

#######################################

#' @include lapop_fonts.R
NULL

#'
#' LAPOP World and Americas Map Graph
#'
#' The `lapop_map()` generates a stylized choropleth map using ISO2 country codes
#' from both continuous and factor variables. It is designed to map cross-country
#' results from `lpr_cc()` and supports either a full world map (`survey = "CSES"`)
#' # or an Americas-only map (`survey = "AmericasBarometer"`).
#'
#' @param data A data frame containing ISO2 country codes and a value to map.
#' @param pais_lab String. Column name containing ISO2 country codes (e.g., `"US"`, `"BR"`).
#' @param outcome String. Column name containing the numeric or categorical variable to visualize.
#' @param survey Either `"CSES"` (full world map) or `"AmericasBarometer"` (Americas only).
#' @param zoom Numeric (0–1). Controls how tightly the map zooms when `survey = "AmericasBarometer"`. Default is `1`.
#' @param palette Vector of up to 5 colors for continuous and factor variables.
#' @param main_title Character. Title of graph.  Default: None.
#' @param source_info Character. Information on dataset used (country, years, version, etc.),
#' which is added to the bottom-left corner of the graph. Default: LAPOP ("Source: LAPOP Lab" will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "percentage of Mexicans who say...)".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param selected_countries Character or NULL. ISO2 code of the currently selected country (e.g. from
#' `input$pais` in Shiny). When not `NULL`, countries with no data are rendered with diagonal stripes
#' instead of solid gray. Default: `NULL` (solid `"#dddddf"`).
#'
#' @return A `ggplot2` choropleth map object.
#'
#' @examples
#' \donttest{
#' # Standalone — solid gray for no-data countries
#' lapop_fonts()
#'  data_cont <- data.frame(
#'   vallabel = c("US", "AR", "VE", "CH", "EC", "BO"),
#'   prop = c(37, 52, 80, 17, 69, 94)
#' )
#' lapop_map(data_cont, pais_lab = "vallabel", outcome = "prop", zoom = 0.9,
#'           survey = "AmericasBarometer", main_title = "Latin America and Caribbean Countries",
#'           subtitle = "% of respondents")
#'
#' }
#' if (interactive()) {
#'   lapop_map(data_cont, pais_lab = "vallabel", outcome = "prop", zoom = 0.9,
#'             survey = "AmericasBarometer", selected_countries = "BR")
#' }
#' @export
#' @import ggplot2
#' @import ggtext
#' @import grid
#' @importFrom sf st_bbox st_drop_geometry
#' @importFrom dplyr filter left_join rename
#'
#' @author Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

lapop_map <- function(data,
                      outcome = "value",
                      pais_lab = "pais_lab",
                      survey = c("CSES", "AmericasBarometer"),
                      zoom = 1,
                      main_title = "",
                      subtitle = "",
                      palette = c("#F2A344", "#D97A1E", "#BF5A00", "#8A3900", "#4A1E00"),
                      source_info = "LAPOP",
                      lang = "en",
                      selected_countries = NULL) {

  survey <- match.arg(survey)
  zoom <- max(0, min(1, zoom))

  # ---------------------------------------------------------------
  # ISO2 vectors
  # ---------------------------------------------------------------
  americas_iso2 <- c(
    "US","CA","MX",
    "CR","SV","GT","HN","NI","PA","BZ",
    "DO","HT","TT","JM","CU","BB","BS","GD","LC","VC",
    "AR","BO","BR","CL","CO","EC","GY","PE","PY","SR","UY","VE"
  )

  if (is.null(world_sf)) {
    stop("world map data is not loaded. Restart R and load `lapop` again.")
  }

  world <- world_sf

  if (survey == "AmericasBarometer") {
    world <- world %>% dplyr::filter(pais_lab %in% americas_iso2)
  }

  # ---------------------------------------------------------------
  # Merge user data
  # ---------------------------------------------------------------
  if (!all(c(outcome, pais_lab) %in% names(data))) {
    stop("`outcome` and `pais_lab` must refer to columns in `data`.")
  }

  df <- data.frame(
    pais_lab = data[[pais_lab]],
    value = data[[outcome]],
    stringsAsFactors = FALSE
  )

  merged <- world %>% dplyr::left_join(df, by = "pais_lab") %>% sf::st_as_sf()
  outcome_is_factor <- is.factor(merged$value) || is.character(merged$value)

  # ---------------------------------------------------------------
  # Stripe flag: TRUE only when a country is selected (Shiny context)
  # Standalone use: selected_countries = NULL → always solid gray
  # ---------------------------------------------------------------
  use_stripes <- !is.null(selected_countries) && length(selected_countries) > 0 && any(nchar(selected_countries) > 0)

  na_data  <- merged %>% dplyr::filter(is.na(value))
  val_data <- merged %>% dplyr::filter(!is.na(value))

  # ---------------------------------------------------------------
  # BASE MAP
  # ---------------------------------------------------------------
  p <- ggplot2::ggplot()

  if (use_stripes && nrow(na_data) > 0) {
    if (!requireNamespace("ggpattern", quietly = TRUE)) {
      stop("Package 'ggpattern' is required for stripe rendering. Install with install.packages('ggpattern').")
    }
    p <- p +
      ggpattern::geom_sf_pattern(
        data            = na_data,
        pattern         = "stripe",
        pattern_colour  = "#aaaaaa",
        pattern_fill    = "#aaaaaa",
        pattern_density = 0.4,
        pattern_spacing = 0.03,
        pattern_angle   = 45,
        fill            = "#ffffff",  # white base under stripes
        color           = "black",
        size            = 0.25
      )
  } else if (nrow(na_data) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data  = na_data,
        fill  = "#dddddf",            # solid gray — default / standalone
        color = "black",
        size  = 0.25
      )
  }

  p <- p +
    ggplot2::geom_sf(
      data  = val_data,
      ggplot2::aes(fill = value),
      color = "black",
      size  = 0.25
    )

  # ---------------------------------------------------------------
  # FILL SCALES
  # ---------------------------------------------------------------
  if (outcome_is_factor) {
    p <- p +
      ggplot2::scale_fill_manual(
        values = palette,
        drop   = FALSE,
        na.value = "#dddddf"
      )
  } else {
    p <- p +
      ggplot2::scale_fill_gradientn(
        labels = function(x) formatC(x, format = "g"),
        colors = palette,
        na.value = "#dddddf",
        guide = ggplot2::guide_colorbar(
          direction      = "horizontal",
          barwidth       = grid::unit(50, "pt"),
          barheight      = grid::unit(12, "pt"),
          label.position = "top"
        )
      )
  }

  # ---------------------------------------------------------------
  # Zoom logic for Americas
  # ---------------------------------------------------------------
  if (survey == "AmericasBarometer") {
    p <- p + ggplot2::coord_sf(
      xlim   = c(-170, -25),
      ylim   = c(-60, 80),
      expand = FALSE
    )
  }

  # ---------------------------------------------------------------
  # Caption logic
  # ---------------------------------------------------------------
  caption_text <- ifelse(
    lang == "es" & source_info == "LAPOP", "Fuente: LAPOP Lab",
    ifelse(lang == "en" & source_info == "LAPOP", "Source: LAPOP Lab",
           source_info)
  )

  # ---------------------------------------------------------------
  # Final theme
  # ---------------------------------------------------------------
  p + ggplot2::theme_void() +
    ggplot2::labs(
      title    = main_title,
      subtitle = subtitle,
      fill     = "",
      caption  = caption_text
    ) +
    ggplot2::theme(
      plot.margin        = ggplot2::margin(0, 175, 0, 0),
      legend.position    = "bottom",
      legend.box         = "vertical",
      legend.box.just    = "left",
      legend.justification = "left",
      legend.title       = ggplot2::element_blank(),
      legend.key.width   = grid::unit(16, "pt"),
      legend.key.height  = grid::unit(18, "pt"),
      legend.text        = ggtext::element_markdown(family = "inter-light"),
      plot.title         = ggplot2::element_text(size = 18, family = "inter", face = "bold"),
      plot.caption       = ggplot2::element_text(size = 10.5, vjust = 2, hjust = 0,
                                                 family = "inter", color = "#585860"),
      plot.subtitle      = ggplot2::element_text(size = 13, hjust = 0,
                                                 family = "inter", color = "#585860")
    )
}
