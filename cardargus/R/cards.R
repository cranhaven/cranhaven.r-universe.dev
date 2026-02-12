# cards.R - SVG card generation

#' Create an Information Card in SVG Format
#'
#' Generate a complete information card as an SVG with embedded styles,
#' fonts, badges, logos, and field labels/values.
#'
#' @param title Card title (e.g., "FAR", "FNHIS")
#' @param badges_data List of badge data (label, value, color)
#' @param fields List of field rows, each row is a list of fields with label, value, 
#'   and optionally with_icon. The with_icon parameter can be:
#'   \itemize{
#'     \item TRUE - uses the default house icon
#'     \item FALSE or NULL - no icon
#'     \item A character string - path to an SVG file or raw SVG code
#'   }
#' @param bg_color Background color of the card. Can be a solid color (e.g., "#fab255") 
#'   or a CSS gradient (e.g., "linear-gradient(to right, #1a5a3a, #2e7d32)" or 
#'   "linear-gradient(135deg, #667eea, #764ba2)").
#' @param width Card width in pixels
#' @param padding Padding inside the card
#' @param corner_radius Corner radius for rounded corners
#' @param font Font family
#' @param title_fontsize Title font size
#' @param title_color Color for the card title (default "white")
#' @param label_fontsize Label font size
#' @param value_fontsize Value font size
#' @param label_color Color for field labels (default "white")
#' @param value_bg_color Background color for value boxes
#' @param value_text_color Text color for values
#' @param show_house_icon Show house icon next to empreendimento
#' @param logos Character vector of logo file paths or SVG strings for top right.
#'   Use `get_svg_path("filename.svg")` for bundled logos, or any local path.
#' @param logos_height Height for top-right logos (default 40)
#' @param bottom_logos Character vector of logo file paths or SVG strings for bottom left.
#' @param bottom_logos_height Height for bottom-left logos (default 30)
#' @param gap_to_footer Distance (px) between the last info block and the footer row.
#' @param footer_row_padding_bottom Bottom padding (px) under the footer row.
#' @param footer Footer text (e.g., update timestamp)
#' @param footer_fontsize Footer font size
#' @param gap_to_footer Distance (px) between the last info block and the footer row.
#' @param footer_row_padding_bottom Bottom padding (px) under the footer row (text + logos).
#' @param show_viewer If TRUE (and interactive), preview the SVG in the RStudio Viewer.
#' @param footer_color Color for footer text (default "white")
#' @param uniform_row_height If TRUE, keep the height inside a row.
#' @param show_viewer If TRUE (and interactive), preview the SVG in the Viewer.
#' @return SVG string
#' @export
#' @examples
#' # With default house icon
#' fields <- list(
#'   list(
#'     list(label = "Empreendimento", value = "CAIARA II", with_icon = TRUE)
#'   )
#' )
#' 
#' # With custom icon
#' custom_icon <- '<svg width="50" height="50"><circle cx="25" cy="25" r="20" fill="white"/></svg>'
#' fields <- list(
#'   list(
#'     list(label = "Projeto", value = "Meu Projeto", with_icon = custom_icon)
#'   )
#' )
#' 
#' badges <- list(
#'   list(label = "UH", value = "192"),
#'   list(label = "Recurso Federal", value = "36,4 milhões")
#' )
#' 
#' # With file paths for logos
#' svg_card("FAR", badges, fields, 
#'          bg_color = "#fab255",
#'          logos = c("path/to/logo1.svg", "path/to/logo2.svg"),
#'          bottom_logos = c("path/to/gov_logo.svg"))
#' 
#' # With gradient background
#' svg_card("MCMV", badges, fields,
#'          bg_color = "linear-gradient(to right, #1a5a3a, #2e7d32)")
#' 
#' # Diagonal gradient
#' svg_card("Programa", badges, fields,
#'          bg_color = "linear-gradient(135deg, #667eea, #764ba2)")
svg_card <- function(title = "FAR",
                     badges_data = list(),
                     fields = list(),
                     bg_color = "#fab255",
                     width = 500,
                     padding = 20,
                     corner_radius = 8,
                     font = "Jost",
                     title_fontsize = 16,
                     title_color = "white",
                     label_fontsize = 11,
                     value_fontsize = 11,
                     label_color = "white",
                     value_bg_color = "#f8f8ff",
                     value_text_color = "#212529",
                     show_house_icon = TRUE,
                     logos = list(),
                     logos_height = 40,
                     bottom_logos = list(),
                     bottom_logos_height = 30,
                     footer = NULL,
                     gap_to_footer = 6,
                     footer_row_padding_bottom = 6,
                     footer_fontsize = 8,
                     footer_color = "white",
                     uniform_row_height = TRUE,
                     show_viewer = interactive()
                     ) {
  
  # Generate unique ID for this card
  card_id <- generate_id(title, bg_color, width)
  
  # ===== GRADIENT BACKGROUND SUPPORT =====
  # Detect if bg_color is a CSS gradient and convert to SVG

  if (grepl("gradient\\(", bg_color, ignore.case = TRUE)) {
    bg_grad_id <- paste0("bg_grad_", card_id)
    bg_gradient_def <- css_gradient_to_svg(bg_color, id = bg_grad_id)
    bg_fill <- sprintf("url(#%s)", bg_grad_id)
  } else {
    bg_gradient_def <- ""
    bg_fill <- bg_color
  }
  
  # Calculate content width
  content_width <- width - 2 * padding
  
  # Initialize Y position tracker
  current_y <- padding
  
  # Collect all SVG elements
  elements <- character()
  
  # ===== DEFS SECTION (styles, gradients, etc.) =====
  defs <- sprintf('
    <defs>
      <style>
        @import url("https://fonts.googleapis.com/css2?family=%s:wght@400;500;600;700&amp;display=swap");
        .card-title { font-family: "%s", sans-serif; font-weight: 700; font-size: %dpx; fill: %s; }
        .field-label { font-family: "%s", sans-serif; font-weight: 600; font-size: %dpx; fill: %s; }
        .field-value { font-family: "%s", sans-serif; font-weight: 400; font-size: %dpx; fill: %s; }
        .footer-text { font-family: "%s", sans-serif; font-style: italic; font-size: %dpx; fill: %s; opacity: 0.8; }
      </style>
      <clipPath id="card-clip-%s">
        <rect width="%d" rx="%d" ry="%d"/>
      </clipPath>
      %s
    </defs>',
    gsub(" ", "+", font),
    font, title_fontsize, title_color,
    font, label_fontsize, label_color,
    font, value_fontsize, value_text_color,
    font, footer_fontsize, footer_color,
    card_id, width, corner_radius, corner_radius,
    bg_gradient_def
  )
  
  # ===== TITLE =====
  has_top_logos <- length(logos) > 0
  
  # altura do header = maior entre altura das logos e altura "útil" do título
  # (use um fator para não ficar apertado quando título é pequeno)
  title_block_h <- title_fontsize * 1.2
  header_height <- max(title_block_h, if (has_top_logos) logos_height else title_block_h)
  
  # y do título:
  # - com logos: centraliza no meio do header
  # - sem logos: mantém comportamento antigo (baseline normal)
  if (has_top_logos) {
    title_y <- current_y + header_height / 2
    title_baseline <- "middle"
  } else {
    title_y <- current_y + title_fontsize
    title_baseline <- "alphabetic"
  }
  
  elements <- c(elements, sprintf(
    '<text x="%d" y="%.1f" class="card-title" dominant-baseline="%s">%s</text>',
    padding, title_y, title_baseline, escape_xml(title)
  ))
  
  # avança o cursor pelo header inteiro
  current_y <- current_y + header_height + 15
  
  
  # ===== BADGES (with uniform height) =====
  if (length(badges_data) > 0) {
    badge_x <- padding
    badge_y <- current_y
    
    # First pass: calculate max height for uniform badges
    max_badge_height <- 0
    for (badge in badges_data) {
      metrics_label <- gdtools::str_metrics(badge$label, fontname = font, fontsize = 10)
      metrics_value <- gdtools::str_metrics(badge$value, fontname = font, fontsize = 10)
      height_label <- as.numeric(metrics_label["ascent"]) + as.numeric(metrics_label["descent"])
      height_value <- as.numeric(metrics_value["ascent"]) + as.numeric(metrics_value["descent"])
      badge_h <- max(height_label, height_value) * 2
      if (badge_h > max_badge_height) max_badge_height <- badge_h
    }
    
    # Second pass: create badges with uniform height
    for (i in seq_along(badges_data)) {
      badge <- badges_data[[i]]
      color <- if (!is.null(badge$color)) badge$color else "white"
      badge_result <- create_badge(
        label = badge$label,
        value = badge$value,
        color = color,
        font = font,
        fontsize = value_fontsize,
        style = "",
        shadow_offset = 1,
        height = max_badge_height / 2,  # badge_svg doubles this
        as_string = FALSE
      )
      
      # Replace IDs to make them unique within the card context
      badge_svg_content <- badge_result$svg
      badge_svg_content <- gsub('id="s([^"]+)"', sprintf('id="s\\1_%d"', i), badge_svg_content)
      badge_svg_content <- gsub('id="r([^"]+)"', sprintf('id="r\\1_%d"', i), badge_svg_content)
      badge_svg_content <- gsub('#s([^"\\)]+)', sprintf('#s\\1_%d', i), badge_svg_content)
      badge_svg_content <- gsub('#r([^"\\)]+)', sprintf('#r\\1_%d', i), badge_svg_content)
      
      # Remove the outer svg tags and extract content
      badge_inner <- gsub('<\\?xml[^>]*\\?>', '', badge_svg_content)
      badge_inner <- gsub('<svg[^>]*>', '', badge_inner)
      badge_inner <- gsub('</svg>', '', badge_inner)
      
      # Wrap in a group with translation (no nested svg)
      elements <- c(elements, sprintf(
        '<g transform="translate(%.1f, %.1f)">%s</g>',
        badge_x, badge_y, badge_inner
      ))
      
      badge_x <- badge_x + badge_result$width + 4
    }
    
    current_y <- current_y + max_badge_height + 15
  }
  
  # ===== FIELDS =====
  row_spacing <- 12
  field_spacing <- 10
  value_box_padding_x <- 10
  value_box_padding_y <- 8
  value_box_radius <- 4
  
  for (row in fields) {
    if (length(row) == 0) next
    
    # Calculate field widths
    num_fields <- length(row)
    available_width <- content_width - (num_fields - 1) * field_spacing
    
    # Check for icon in first field
    # with_icon can be: TRUE (default house icon), FALSE/NULL (no icon), or a string (custom SVG)
    first_field_icon <- row[[1]]$with_icon
    has_icon <- !is.null(first_field_icon) && !isFALSE(first_field_icon)
    icon_width <- if (has_icon) 55 else 0
    
    # Determine the icon SVG to use
    if (has_icon) {
      if (isTRUE(first_field_icon)) {
        # Use default house icon
        icon_svg <- icon_house(50, 56)
        icon_height <- 56
      } else if (is.character(first_field_icon)) {
        # Use custom SVG provided
        icon_svg <- first_field_icon
        # Try to extract height from SVG, default to 56
        height_match <- regmatches(first_field_icon, regexpr('height="[0-9.]+"', first_field_icon))
        icon_height <- if (length(height_match) > 0) {
          as.numeric(gsub('[^0-9.]', '', height_match))
        } else {
          56
        }
      } else {
        has_icon <- FALSE
        icon_width <- 0
      }
    }
    
    if (has_icon && num_fields == 1) {
      field_widths <- content_width - icon_width - field_spacing
    } else {
      field_widths <- rep(available_width / num_fields, num_fields)
      if (has_icon) {
        field_widths[1] <- field_widths[1] - icon_width - field_spacing
      }
    }
    
    # Calculate row height based on wrapped text
    row_height <- 0
    field_data <- list()
    
    for (i in seq_along(row)) {
      field <- row[[i]]
      fw <- if (i == 1 && has_icon) field_widths[1] else field_widths[i]
      
      # Wrap the value text
      wrapped <- wrap_text(
        field$value, 
        fw - 2 * value_box_padding_x, 
        font, 
        value_fontsize
      )
      
      field_height <- label_fontsize + 4 + wrapped$height + 2 * value_box_padding_y
      row_height <- max(row_height, field_height)
      
      field_data[[i]] <- list(
        field = field,
        width = fw,
        wrapped = wrapped
      )
    }
    
    # Draw icon if needed
    field_x <- padding
    if (has_icon) {
      icon_y <- current_y + (row_height - icon_height) / 2
      elements <- c(elements, sprintf(
        '<g transform="translate(%.1f, %.1f)">%s</g>',
        field_x, icon_y, icon_svg
      ))
      field_x <- field_x + icon_width + field_spacing
    }
    
    # Draw each field
    for (i in seq_along(row)) {
      fd <- field_data[[i]]
      field <- fd$field
      fw <- fd$width
      wrapped <- fd$wrapped
      
      # Skip icon adjustment for first field (already done)
      if (i == 1 && has_icon) {
        # Already positioned
      } else if (i > 1) {
        field_x <- field_x + field_data[[i-1]]$width + field_spacing
      }
      
      # Draw label
      elements <- c(elements, sprintf(
        '<text x="%.1f" y="%.1f" class="field-label">%s</text>',
        field_x, current_y + label_fontsize, escape_xml(field$label)
      ))
      
      # Draw value box background
      # box_y <- current_y + label_fontsize + 4
      # box_height <- wrapped$height + 2 * value_box_padding_y
      box_y <- current_y + label_fontsize + 4
      
      # If uniform_row_height, make all boxes in this row the same height
      box_height <- if (isTRUE(uniform_row_height)) {
        row_height - (label_fontsize + 4)
      } else {
        wrapped$height + 2 * value_box_padding_y
      }
      
      
      elements <- c(elements, sprintf(
        '<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="%d" fill="%s" stroke="#dee2e6" stroke-width="1"/>',
        field_x, box_y, fw, box_height, value_box_radius, value_bg_color
      ))
      
      # Draw value text (possibly multi-line)
      #text_y <- box_y + value_box_padding_y + value_fontsize * 0.85
      if (isTRUE(uniform_row_height)) {
        text_block_h <- length(wrapped$lines) * wrapped$line_height
        extra <- max(0, box_height - 2 * value_box_padding_y - text_block_h)
        text_y <- box_y + value_box_padding_y + (extra / 2) + value_fontsize * 0.85
      } else {
        text_y <- box_y + value_box_padding_y + value_fontsize * 0.85
      }
      
      for (line in wrapped$lines) {
        elements <- c(elements, sprintf(
          '<text x="%.1f" y="%.1f" class="field-value">%s</text>',
          field_x + value_box_padding_x, text_y, escape_xml(line)
        ))
        text_y <- text_y + wrapped$line_height
      }
      
    }
    
    current_y <- current_y + row_height + row_spacing
  }
  
  # ===== FOOTER (text left) + BOTTOM LOGOS (right, same line) =====
  footer_logos_svg <- ""
  footer_elements <- character()
  #footer_height <- 0
  footer_present <- (!is.null(footer) || length(bottom_logos) > 0)

  # Footer Y positions are computed after final_height is known so we can:
  # - prevent overlaps when footer logos are tall
  # - keep the last field row close to the bottom (small gap)
  # - bottom-align the footer logos with the card edge (inside padding)

  # Calculate final height
  # ===== FINAL HEIGHT (depends on footer/logo heights) =====
  if (footer_present) {
    has_bottom_logos <- (length(bottom_logos) > 0)
    footer_row_height <- max(footer_fontsize, if (has_bottom_logos) bottom_logos_height else 0)
    final_height <- current_y + gap_to_footer + footer_row_height + footer_row_padding_bottom
  } else {
    final_height <- current_y + padding
  }
  
  # ===== TOP-RIGHT LOGOS =====
  top_logos_svg <- ""
  if (length(logos) > 0) {
    logo_result <- create_logo_row(
      logos = logos,
      target_height = logos_height,
      spacing = 10,
      card_width = width,
      x_offset = padding,
      y_offset = padding
    )
    top_logos_svg <- logo_result$svg_content
  }
  
  
  # ===== FOOTER ELEMENTS (placed using final_height) =====
  if (footer_present) {
    has_bottom_logos <- (length(bottom_logos) > 0)
    footer_row_height <- max(footer_fontsize, if (has_bottom_logos) bottom_logos_height else 0)

    # Footer row is anchored to the bottom of the card
    footer_row_y_top <- final_height - footer_row_padding_bottom - footer_row_height
    footer_row_y_bottom <- footer_row_y_top + footer_row_height

    if (!is.null(footer)) {
      footer_y <- if (has_bottom_logos) {
        footer_row_y_top + footer_row_height / 2
      } else {
        footer_row_y_bottom - 2
      }

      footer_baseline <- if (has_bottom_logos) "middle" else "alphabetic"

      footer_elements <- c(footer_elements, sprintf(
        '<text x="%d" y="%.1f" class="footer-text" dominant-baseline="%s">%s</text>',
        padding, footer_y, footer_baseline, escape_xml(footer)
      ))
    }

    if (has_bottom_logos) {
      # Bottom-align logos with the card edge (inside footer_row_padding_bottom)
      logos_y <- final_height - footer_row_padding_bottom - bottom_logos_height
      logo_row <- create_logo_row(
        logos = bottom_logos,
        target_height = bottom_logos_height,
        spacing = 10,
        card_width = width,
        x_offset = padding,
        y_offset = logos_y
      )
      footer_elements <- c(footer_elements, logo_row$svg_content)
    }
  }
# ===== ASSEMBLE FINAL SVG =====
  svg <- sprintf(
    '<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">
  %s
  <!-- Background -->
  <rect width="%d" height="%d" rx="%d" fill="%s"/>
  <!-- Content -->
  %s
  <!-- Top-right logos -->
  %s
  <!-- Footer-row logos (right) -->
  %s
</svg>',
    width, round(final_height), width, round(final_height),
    defs,
    width, round(final_height), corner_radius, bg_fill,
    paste(c(elements, footer_elements), collapse = "\n  "),
    top_logos_svg,
    footer_logos_svg
  )
  
  # In interactive / iterative use, also render in the Viewer while still returning the SVG.
  if (isTRUE(show_viewer) && interactive()) {
    if (!requireNamespace("htmltools", quietly = TRUE)) {
      cli::cli_warn("Package {.pkg htmltools} is recommended to preview SVGs in the Viewer.")
    } else {
      preview <- htmltools::browsable(htmltools::HTML(svg))
      # html_print triggers the Viewer pane in RStudio / Posit
      try(htmltools::html_print(preview), silent = TRUE)
    }
  }

  return(svg)
}

