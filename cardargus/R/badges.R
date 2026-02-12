# badges.R - SVG badge generation functions

#' Create an SVG Badge
#'
#' Generate a badge similar to shields.io style with label and value.
#' Both sides have properly rounded corners.
#'
#' @param label Label text (left side)
#' @param value Value text (right side)
#' @param color Background color for value area
#' @param font Font family name
#' @param style CSS style string
#' @param fontsize Font size in pixels
#' @param horiz_padding Horizontal padding
#' @param extra_right_pad Extra padding on right side
#' @param class CSS class
#' @param shadow_offset Shadow offset in pixels
#' @param corner_radius Corner radius for rounded rectangle
#' @param height Minimum height (optional)
#' @param as_string Return as character string
#' @return SVG string
#' @export
#' @examples
#' create_badge("UH", "192", "white")
#' create_badge("Recurso Federal", "36,4 milhões", "#4CAF50")
create_badge <- function(label, value, color,
                         font = "Jost", style = "margin:2px;",
                         fontsize = 11,
                         horiz_padding = 5,
                         extra_right_pad = 2,
                         class = "",
                         shadow_offset = 2,
                         corner_radius = 3,
                         height = NULL,
                         as_string = TRUE) {
  
  # Generate unique ID
  id_suffix <- generate_id(label, value, color)
  
  # Measure text dimensions
  metrics_label <- gdtools::str_metrics(label, fontname = font, fontsize = fontsize)
  metrics_value <- gdtools::str_metrics(value, fontname = font, fontsize = fontsize)
  
  width_label <- as.numeric(metrics_label["width"])
  width_value <- as.numeric(metrics_value["width"])
  
  # Define widths
  left_width <- width_label + 2 * horiz_padding
  right_width <- width_value + 2 * horiz_padding + 2 * extra_right_pad
  total_width <- left_width + right_width
  
  # Calculate heights
  height_label <- as.numeric(metrics_label["ascent"]) + as.numeric(metrics_label["descent"])
  height_value <- as.numeric(metrics_value["ascent"]) + as.numeric(metrics_value["descent"])
  current_height <- max(height_label, height_value)
  
  # if (!is.null(height)) {
  #   current_height <- max(height, current_height, na.rm = TRUE)
  # }
  # 
  # final_height <- current_height * 2
  
  if (!is.null(height)) {
    final_height <- height * 2  # Usar altura especificada diretamente
  } else {
    final_height <- current_height * 2
  }
  
  # Text positions (exactly as original)
  left_text_x <- horiz_padding + width_label / 2
  right_text_x <- left_width + horiz_padding + extra_right_pad + width_value / 2
  text_y <- final_height * 0.55
  
  # Determine text colors based on background
  if (is_light_color(color)) {
    value_text_shadow <- "#ccc"
    value_text_main <- "#333"
  } else {
    value_text_shadow <- "#010101"
    value_text_main <- "#fff"
  }
  
  # Handle gradient colors
  if (grepl("gradient\\(", color, ignore.case = TRUE)) {
    grad_id <- paste0("grad_", id_suffix)
    svg_grad <- css_gradient_to_svg(color, id = grad_id)
    fill_color <- sprintf("url(#%s)", grad_id)
    defs_block <- sprintf("<defs>\n%s\n</defs>", svg_grad)
  } else {
    fill_color <- color
    defs_block <- ""
  }
  
  # Build SVG (using clipPath for rounded corners on both sides)
  svg <- sprintf(
    '<svg class="%s" style="%s" xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" role="img" aria-label="%s: %s">
      <title>%s: %s</title>
      %s
      <linearGradient id="s%s" x2="0" y2="100%%">
        <stop offset="0" stop-color="#bbb" stop-opacity=".1"/>
        <stop offset="1" stop-opacity=".1"/>
      </linearGradient>
      <clipPath id="r%s">
        <rect width="%d" height="%d" rx="%d" ry="%d" fill="#fff"/>
      </clipPath>
      <g clip-path="url(#r%s)">
        <rect width="%d" height="%d" fill="#555"/>
        <rect x="%d" width="%d" height="%d" fill="%s"/>
        <rect width="%d" height="%d" fill="url(#s%s)"/>
      </g>
      <g fill="#fff" text-anchor="middle" font-family="%s" font-size="%s" dominant-baseline="middle">
        <text x="%.1f" y="%.1f" fill="#010101" text-rendering="geometricPrecision" alignment-baseline="middle" fill-opacity=".3">%s</text>
        <text x="%.1f" y="%.1f" text-rendering="geometricPrecision" alignment-baseline="middle" fill="#fff">%s</text>
        <text x="%.1f" y="%.1f" text-rendering="geometricPrecision" alignment-baseline="middle" fill="%s" fill-opacity=".3">%s</text>
        <text x="%.1f" y="%.1f" text-rendering="geometricPrecision" alignment-baseline="middle" fill="%s">%s</text>
      </g>
    </svg>',
    class, style,
    round(total_width), round(final_height),
    escape_xml(label), escape_xml(value),
    escape_xml(label), escape_xml(value),
    defs_block,
    id_suffix,
    id_suffix,
    round(total_width), round(final_height), corner_radius, corner_radius,
    id_suffix,
    round(total_width), round(final_height),
    round(left_width), round(right_width), round(final_height), fill_color,
    round(total_width), round(final_height),
    id_suffix,
    font, as.character(fontsize),
    left_text_x + shadow_offset, text_y + shadow_offset, escape_xml(label),
    left_text_x, text_y, escape_xml(label),
    right_text_x + shadow_offset, text_y + shadow_offset, value_text_shadow, escape_xml(value),
    right_text_x, text_y, value_text_main, escape_xml(value)
  )
  
  if (as_string) {
    return(svg)
  } else {
    return(list(svg = svg, width = round(total_width), height = round(final_height)))
  }
}

#' Create a row of SVG badges with uniform height
#'
#' Generate multiple badges arranged horizontally with the same height.
#'
#' @param badges_data A list of lists, each containing label, value, and optionally color
#' @param default_color Default color for badges
#' @param spacing Spacing between badges
#' @param font Font family
#' @param fontsize Font size
#' @param uniform_height Force all badges to have the same height (default TRUE)
#' @return SVG string containing all badges
#' @export
#' @examples
#' badges <- list(
#'   list(label = "UH", value = "192"),
#'   list(label = "Recurso Federal", value = "36,4 milhões"),
#'   list(label = "Contrapartida", value = "0,0")
#' )
#' create_badge_row(badges, default_color = "white")
create_badge_row <- function(badges_data, 
                             default_color = "white",
                             spacing = 4,
                             font = "Jost",
                             fontsize = 10,
                             uniform_height = TRUE) {
  
  if (length(badges_data) == 0) {
    return('<svg xmlns="http://www.w3.org/2000/svg" width="0" height="0"></svg>')
  }
  
  # First pass: calculate all dimensions to find max height
  badge_dims <- list()
  max_height <- 0
  
  for (i in seq_along(badges_data)) {
    badge <- badges_data[[i]]
    
    # Measure text dimensions
    metrics_label <- gdtools::str_metrics(badge$label, fontname = font, fontsize = fontsize)
    metrics_value <- gdtools::str_metrics(badge$value, fontname = font, fontsize = fontsize)
    
    height_label <- as.numeric(metrics_label["ascent"]) + as.numeric(metrics_label["descent"])
    height_value <- as.numeric(metrics_value["ascent"]) + as.numeric(metrics_value["descent"])
    current_height <- max(height_label, height_value) * 2
    
    if (current_height > max_height) {
      max_height <- current_height
    }
    
    badge_dims[[i]] <- list(
      label = badge$label,
      value = badge$value,
      color = if (!is.null(badge$color)) badge$color else default_color
    )
  }
  
  # Second pass: generate badges with uniform height
  badge_svgs <- list()
  badge_widths <- numeric()
  
  for (i in seq_along(badges_data)) {
    bd <- badge_dims[[i]]
    
    # Use max_height / 2 as the height parameter since badge_svg doubles it
    target_height <- if (uniform_height) max_height / 2 else NULL
    
    result <- create_badge(
      label = bd$label,
      value = bd$value,
      color = bd$color,
      font = font,
      fontsize = fontsize,
      style = "",
      height = target_height,
      as_string = FALSE
    )
    
    badge_svgs[[i]] <- result$svg
    badge_widths[i] <- result$width
  }
  
  # Calculate total dimensions
  total_width <- sum(badge_widths) + (length(badges_data) - 1) * spacing
  total_height <- max_height
  
  # Position badges
  x_positions <- numeric(length(badges_data))
  current_x <- 0
  for (i in seq_along(badges_data)) {
    x_positions[i] <- current_x
    current_x <- current_x + badge_widths[i] + spacing
  }
  
  # Build combined SVG
  inner_svgs <- ""
  for (i in seq_along(badges_data)) {
    # Extract inner content from each badge SVG
    badge_content <- gsub('<svg[^>]*>', '', badge_svgs[[i]])
    badge_content <- gsub('</svg>', '', badge_content)
    
    inner_svgs <- paste0(inner_svgs, sprintf(
      '<g transform="translate(%.1f, 0)">%s</g>',
      x_positions[i], badge_content
    ))
  }
  
  svg <- sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d">%s</svg>',
    round(total_width), round(total_height), inner_svgs
  )
  
  return(svg)
}

#' Convert CSS gradient to SVG gradient
#'
#' @param css_gradient CSS gradient string
#' @param id ID for the gradient
#' @return SVG gradient definition
#' @keywords internal
css_gradient_to_svg <- function(css_gradient, id = "grad") {

  # Linear gradient parser supporting directional keywords and angles
  # Handles: "linear-gradient(to right, #color1, #color2)"
  #          "linear-gradient(135deg, #color1, #color2)"
  
  # Check for angle in degrees (e.g., "45deg", "135deg")
  angle_match <- regmatches(css_gradient, regexpr("(\\d+)deg", css_gradient))
  
  if (length(angle_match) > 0 && nchar(angle_match) > 0) {
    # Extract angle value and convert to SVG coordinates
    angle <- as.numeric(gsub("deg", "", angle_match))
    # CSS angles: 0deg = to top, 90deg = to right, 180deg = to bottom
    # SVG uses coordinates, so we need to convert
    rad <- (angle - 90) * pi / 180
    x1 <- sprintf("%.0f%%", 50 - 50 * cos(rad))
    y1 <- sprintf("%.0f%%", 50 - 50 * sin(rad))
    x2 <- sprintf("%.0f%%", 50 + 50 * cos(rad))
    y2 <- sprintf("%.0f%%", 50 + 50 * sin(rad))
  } else if (grepl("to right", css_gradient, ignore.case = TRUE)) {
    x1 <- "0%"; y1 <- "0%"; x2 <- "100%"; y2 <- "0%"
  } else if (grepl("to left", css_gradient, ignore.case = TRUE)) {
    x1 <- "100%"; y1 <- "0%"; x2 <- "0%"; y2 <- "0%"
  } else if (grepl("to bottom", css_gradient, ignore.case = TRUE)) {
    x1 <- "0%"; y1 <- "0%"; x2 <- "0%"; y2 <- "100%"
  } else if (grepl("to top", css_gradient, ignore.case = TRUE)) {
    x1 <- "0%"; y1 <- "100%"; x2 <- "0%"; y2 <- "0%"
  } else {
    # Default: top to bottom
    x1 <- "0%"; y1 <- "0%"; x2 <- "0%"; y2 <- "100%"
  }
  
  # Extract colors (hex codes)
  colors <- regmatches(css_gradient, gregexpr("#[0-9A-Fa-f]{3,8}", css_gradient))[[1]]
  if (length(colors) < 2) {
    colors <- c("#ffffff", "#000000")
  }
  
  stops <- ""
  for (i in seq_along(colors)) {
    offset <- (i - 1) / (length(colors) - 1) * 100
    stops <- paste0(stops, sprintf(
      '<stop offset="%d%%" stop-color="%s"/>',
      round(offset), colors[i]
    ))
  }
  
  sprintf(
    '<linearGradient id="%s" x1="%s" y1="%s" x2="%s" y2="%s">%s</linearGradient>',
    id, x1, y1, x2, y2, stops
  )
}
