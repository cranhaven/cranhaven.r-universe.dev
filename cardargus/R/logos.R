# logos.R - SVG logo icons and logo management

#' Load and process external SVG file for embedding
#'
#' Reads an SVG file and processes it to be embedded inside another SVG.
#' Removes XML declarations, adjusts dimensions, and prepares for embedding.
#'
#' @details
#' This function is useful when you want to embed custom logos or icons in cards.
#' You can pass any SVG file path to the `logos`, `bottom_logos`, or `with_icon`
#' parameters of `svg_card()`.
#'
#' @param svg_path Path to the SVG file
#' @param target_height Desired height in pixels
#' @param target_width Optional desired width (calculated from aspect ratio if NULL)
#' @return A list with svg_content, width, and height
#' @export
#' @examples
#' \dontrun{ # Need a external svg file
#' # Load a custom logo
#' logo <- load_svg_for_embed("/path/to/logo.svg", target_height = 40)
#' 
#' # Or just pass the path directly to svg_card():
#' svg_card(
#'   title = "My Card",
#'   logos = c("/path/to/logo1.svg", "/path/to/logo2.svg"),
#'   ...
#' )
#' }
load_svg_for_embed <- function(svg_path, target_height = 40, target_width = NULL) {
  if (!file.exists(svg_path)) {
    stop("SVG file not found: ", svg_path)
  }
  
  # Read SVG content
  svg_content <- paste(readLines(svg_path, warn = FALSE), collapse = "\n")
  
  # Remove XML declaration and comments
  svg_content <- gsub('<\\?xml[^>]*\\?>', '', svg_content)
  svg_content <- gsub('<!--[^>]*-->', '', svg_content)
  
  # Extract original dimensions from viewBox or width/height
  viewbox_match <- regmatches(svg_content, regexec('viewBox="([^"]*)"', svg_content))[[1]]
  width_match <- regmatches(svg_content, regexec('width="([0-9.]+)', svg_content))[[1]]
  height_match <- regmatches(svg_content, regexec('height="([0-9.]+)', svg_content))[[1]]
  
  # Determine original dimensions
  if (length(viewbox_match) > 1) {
    vb_parts <- as.numeric(strsplit(viewbox_match[2], "\\s+")[[1]])
    orig_width <- vb_parts[3]
    orig_height <- vb_parts[4]
  } else if (length(width_match) > 1 && length(height_match) > 1) {
    orig_width <- as.numeric(width_match[2])
    orig_height <- as.numeric(height_match[2])
  } else {
    # Default to square if can't determine
    orig_width <- 100
    orig_height <- 100
  }
  
  # Calculate new dimensions maintaining aspect ratio
  aspect_ratio <- orig_width / orig_height
  new_height <- target_height
  new_width <- if (!is.null(target_width)) target_width else round(target_height * aspect_ratio)
  
  # Remove existing width/height attributes from opening svg tag
  svg_content <- gsub('(<svg[^>]*?)\\s+width="[^"]*"', '\\1', svg_content)
  svg_content <- gsub('(<svg[^>]*?)\\s+height="[^"]*"', '\\1', svg_content)
  
  # Ensure viewBox exists
  if (length(viewbox_match) < 2) {
    svg_content <- gsub('<svg', sprintf('<svg viewBox="0 0 %s %s"', orig_width, orig_height), svg_content)
  }
  
  # Add new width and height
  svg_content <- gsub('<svg', sprintf('<svg width="%.0f" height="%.0f"', new_width, new_height), svg_content)
  
  # Remove problematic namespace declarations that can cause issues when embedded
  svg_content <- gsub('xmlns:inkscape="[^"]*"', '', svg_content)
  svg_content <- gsub('xmlns:sodipodi="[^"]*"', '', svg_content)
  svg_content <- gsub('sodipodi:[a-z]+="[^"]*"', '', svg_content)
  svg_content <- gsub('inkscape:[a-z]+="[^"]*"', '', svg_content)
  
  # Clean up extra spaces
  svg_content <- gsub('\\s+>', '>', svg_content)
  svg_content <- gsub('<svg\\s+', '<svg ', svg_content)
  
  list(
    svg_content = svg_content,
    width = new_width,
    height = new_height
  )
}

#' Create logo row for top-right corner of card
#'
#' Takes a list of SVG logos and arranges them horizontally with proper spacing.
#' Returns the SVG elements positioned for the top-right corner.
#'
#' @param logos List of SVG strings or file paths
#' @param target_height Height for all logos (default 40)
#' @param spacing Horizontal spacing between logos (default 10)
#' @param card_width Total card width to calculate positioning
#' @param x_offset Right margin from card edge
#' @param y_offset Top margin
#' @return A list with svg_content and total_width
#' @export
create_logo_row <- function(logos, target_height = 40, spacing = 10, 
                            card_width = 500, x_offset = 20, y_offset = 20) {
  if (length(logos) == 0) {
    return(list(svg_content = "", total_width = 0))
  }
  
  processed_logos <- list()
  total_width <- 0
  
  for (i in seq_along(logos)) {
    logo <- logos[[i]]
    
    if (is.character(logo) && file.exists(logo)) {
      # It's a file path - load and process
      processed <- load_svg_for_embed(logo, target_height = target_height)
      processed_logos[[i]] <- processed
      total_width <- total_width + processed$width + (if (i > 1) spacing else 0)
    } else if (is.character(logo)) {
      # It's already SVG content - extract dimensions and resize
      width_match <- regmatches(logo, regexec('width="([0-9.]+)', logo))[[1]]
      height_match <- regmatches(logo, regexec('height="([0-9.]+)', logo))[[1]]
      
      if (length(width_match) > 1 && length(height_match) > 1) {
        orig_width <- as.numeric(width_match[2])
        orig_height <- as.numeric(height_match[2])
        aspect_ratio <- orig_width / orig_height
        new_width <- round(target_height * aspect_ratio)
        
        # Update dimensions in SVG
        logo <- gsub('width="[0-9.]+"', sprintf('width="%.0f"', new_width), logo)
        logo <- gsub('height="[0-9.]+"', sprintf('height="%.0f"', target_height), logo)
        
        processed_logos[[i]] <- list(svg_content = logo, width = new_width, height = target_height)
        total_width <- total_width + new_width + (if (i > 1) spacing else 0)
      } else {
        # Can't determine dimensions, use target_height as square
        processed_logos[[i]] <- list(svg_content = logo, width = target_height, height = target_height)
        total_width <- total_width + target_height + (if (i > 1) spacing else 0)
      }
    }
  }
  
  # Build positioned logo SVG content
  start_x <- card_width - x_offset - total_width
  current_x <- start_x
  svg_parts <- c()
  
  for (i in seq_along(processed_logos)) {
    logo_data <- processed_logos[[i]]
    
    # Wrap each logo in a group with translation
    svg_part <- sprintf(
      '<g transform="translate(%.0f, %.0f)">%s</g>',
      current_x, y_offset, logo_data$svg_content
    )
    svg_parts <- c(svg_parts, svg_part)
    
    current_x <- current_x + logo_data$width + spacing
  }
  
  list(
    svg_content = paste(svg_parts, collapse = "\n"),
    total_width = total_width,
    start_x = start_x
  )
}

#' Create logo row for bottom-left corner of card
#'
#' Takes a list of SVG logos and arranges them horizontally for the bottom-left.
#'
#' @param logos List of SVG strings or file paths
#' @param target_height Height for all logos (default 30)
#' @param spacing Horizontal spacing between logos (default 10)
#' @param x_offset Left margin from card edge
#' @param card_height Total card height for y positioning
#' @param y_offset Bottom margin
#' @return A list with svg_content and total_width
#' @export
create_bottom_logo_row <- function(logos, target_height = 30, spacing = 10,
                                   x_offset = 20, card_height = 400, y_offset = 20) {
  if (length(logos) == 0) {
    return(list(svg_content = "", total_width = 0))
  }
  
  processed_logos <- list()
  total_width <- 0
  
  for (i in seq_along(logos)) {
    logo <- logos[[i]]
    
    if (is.character(logo) && file.exists(logo)) {
      processed <- load_svg_for_embed(logo, target_height = target_height)
      processed_logos[[i]] <- processed
      total_width <- total_width + processed$width + (if (i > 1) spacing else 0)
    } else if (is.character(logo)) {
      width_match <- regmatches(logo, regexec('width="([0-9.]+)', logo))[[1]]
      height_match <- regmatches(logo, regexec('height="([0-9.]+)', logo))[[1]]
      
      if (length(width_match) > 1 && length(height_match) > 1) {
        orig_width <- as.numeric(width_match[2])
        orig_height <- as.numeric(height_match[2])
        aspect_ratio <- orig_width / orig_height
        new_width <- round(target_height * aspect_ratio)
        
        logo <- gsub('width="[0-9.]+"', sprintf('width="%.0f"', new_width), logo)
        logo <- gsub('height="[0-9.]+"', sprintf('height="%.0f"', target_height), logo)
        
        processed_logos[[i]] <- list(svg_content = logo, width = new_width, height = target_height)
        total_width <- total_width + new_width + (if (i > 1) spacing else 0)
      } else {
        processed_logos[[i]] <- list(svg_content = logo, width = target_height, height = target_height)
        total_width <- total_width + target_height + (if (i > 1) spacing else 0)
      }
    }
  }
  
  # Position at bottom-left
  y_position <- card_height - y_offset - target_height
  current_x <- x_offset
  svg_parts <- c()
  
  for (i in seq_along(processed_logos)) {
    logo_data <- processed_logos[[i]]
    
    svg_part <- sprintf(
      '<g transform="translate(%.0f, %.0f)">%s</g>',
      current_x, y_position, logo_data$svg_content
    )
    svg_parts <- c(svg_parts, svg_part)
    
    current_x <- current_x + logo_data$width + spacing
  }
  
  list(
    svg_content = paste(svg_parts, collapse = "\n"),
    total_width = total_width,
    y_position = y_position
  )
}

#' House Icon SVG
#'
#' Generate a house/home icon SVG. You can also use any SVG file path
#' instead of built-in icons.
#'
#' @param width Width of the icon
#' @param height Height of the icon
#' @param stroke_color Stroke color
#' @param stroke_width Stroke width
#' @param fill Fill color (default none)
#' @return SVG string
#' @export
#' @examples
#' icon_house(50, 56)
#' 
#' # You can also use a custom SVG file:
#' # svg_card(..., with_icon = "/path/to/my_icon.svg")
icon_house <- function(width = 50, height = 56, 
                       stroke_color = "white", 
                       stroke_width = 35,
                       fill = "none") {
  sprintf(
    '<svg width="%d" height="%d" viewBox="0 0 572 564" fill="%s" xmlns="http://www.w3.org/2000/svg">
    <path d="M286.666 20.5544H286.667C288.134 21.715 288.135 21.7152 289.632 22.8992L289.749 22.991L296.055 27.8855C297.612 29.0963 297.62 29.102 299.224 30.3494L299.286 30.3982C303.942 33.9851 308.638 37.4306 313.189 40.7546V40.7556C321.461 46.8148 329.655 53.0311 337.908 59.3513L337.915 59.3552L337.921 59.3601C344.353 64.2774 350.827 69.114 357.341 73.8875C365.614 79.9503 373.812 86.1666 382.062 92.4851L382.068 92.49L382.074 92.4939C388.507 97.4112 394.981 102.248 401.494 107.021C409.767 113.084 417.965 119.3 426.216 125.619L426.223 125.624L426.229 125.629C432.661 130.546 439.135 135.381 445.647 140.154V140.155C455.07 147.062 464.394 154.154 473.814 161.36V161.361C477.79 164.402 481.786 167.427 485.808 170.431C490.661 174.058 495.497 177.743 500.38 181.478L500.382 181.479C507.012 186.55 513.763 191.711 520.628 196.791L523.576 198.963C525.209 200.168 526.818 201.371 528.404 202.582L528.463 202.627C533.293 206.281 538.402 209.786 544.27 212.95C547.924 215.038 550.174 216.439 551.839 217.864C552.719 218.617 553.229 219.211 553.545 219.699C554.612 226.321 554.459 233.408 554.206 242.494L554.204 242.543L554.203 242.593C554.126 246.06 554.128 249.524 554.14 252.824L554.153 256.064C554.164 263.428 554.097 270.804 553.974 278.204C553.791 289.17 553.734 300.108 553.706 310.974C553.658 328.506 553.512 346.045 553.304 363.599C553.1 380.713 552.944 397.833 552.85 414.958C552.844 415.915 552.838 417.197 552.832 418.186V418.187C552.786 426.524 552.743 434.86 552.7 443.194C552.522 478.111 552.171 511.279 551.815 545.753H21.2832C20.7333 493.984 20.5786 479.892 20.4639 462.669L20.3359 440.362L20.1943 415.014C20.1911 414.454 20.1792 412.399 20.1758 411.807V411.805L20.085 398.968C20.0155 390.411 19.9296 381.858 19.832 373.31L19.6777 360.494C19.4613 343.026 19.3339 325.556 19.2871 308.082V308.077L19.25 299.915C19.2077 293.111 19.1365 286.304 19.0312 279.492L18.9639 275.404C18.8433 268.123 18.8074 260.831 18.8369 253.517V253.508C18.8501 249.686 18.8353 245.777 18.7422 241.813L18.6973 240.111C18.5223 233.825 18.4012 229.443 18.3936 226.112C18.3865 223.051 18.4852 221.56 18.5771 220.816C18.6272 220.74 18.6858 220.648 18.7588 220.542C18.9536 220.258 19.175 219.946 19.457 219.556C22.9656 216.262 27.149 213.504 32.6768 210.43L32.8105 210.356L32.9414 210.28C38.8184 206.877 44.038 202.807 48.1201 199.693C49.7923 198.462 51.4636 197.236 53.1348 196.014L53.165 195.993L53.1943 195.97C60.023 190.936 66.7359 185.779 73.2754 180.769C79.5598 175.964 85.9122 171.229 92.3311 166.531L92.334 166.529C99.3183 161.415 106.203 156.196 113.013 150.852C120.322 145.117 127.778 139.528 135.377 133.967L135.391 133.957C142.144 129.004 148.787 123.974 155.348 118.948L155.349 118.949C163.393 112.788 171.512 106.72 179.708 100.714L179.716 100.708L179.725 100.702C186.417 95.7851 193.002 90.793 199.502 85.8152C207.547 79.6542 215.667 73.586 223.862 67.5798L223.878 67.5681C231.583 61.9076 239.15 56.1311 246.587 50.4353C253.736 44.9702 260.959 39.5928 268.253 34.2781L268.265 34.2693L268.275 34.2615C272.777 30.9703 277.275 27.5745 281.734 24.0115C283.011 22.9996 283.012 22.9988 284.314 21.9666L284.346 21.9412C285.02 21.4043 285.692 20.8622 286.364 20.3191C286.465 20.3971 286.565 20.476 286.666 20.5544Z" stroke="%s" stroke-width="%d"/>
    </svg>',
    width, height, fill, stroke_color, stroke_width
  )
}

#' Building Icon SVG
#'
#' Generate a building/apartment icon SVG.
#'
#' @param width Width of the icon
#' @param height Height of the icon
#' @param stroke_color Stroke color
#' @param stroke_width Stroke width
#' @param fill Fill color
#' @return SVG string
#' @export
#' @examples
#' icon_building(50, 56)
icon_building <- function(width = 50, height = 56, 
                          stroke_color = "white", 
                          stroke_width = 2,
                          fill = "none") {
  sprintf(
    '<svg width="%d" height="%d" viewBox="0 0 50 56" fill="%s" xmlns="http://www.w3.org/2000/svg">
      <rect x="5" y="10" width="40" height="44" stroke="%s" stroke-width="%d" fill="%s"/>
      <rect x="10" y="16" width="8" height="8" stroke="%s" stroke-width="1" fill="%s"/>
      <rect x="21" y="16" width="8" height="8" stroke="%s" stroke-width="1" fill="%s"/>
      <rect x="32" y="16" width="8" height="8" stroke="%s" stroke-width="1" fill="%s"/>
      <rect x="10" y="28" width="8" height="8" stroke="%s" stroke-width="1" fill="%s"/>
      <rect x="21" y="28" width="8" height="8" stroke="%s" stroke-width="1" fill="%s"/>
      <rect x="32" y="28" width="8" height="8" stroke="%s" stroke-width="1" fill="%s"/>
      <rect x="18" y="42" width="14" height="12" stroke="%s" stroke-width="1" fill="%s"/>
    </svg>',
    width, height, fill,
    stroke_color, stroke_width, fill,
    stroke_color, fill,
    stroke_color, fill,
    stroke_color, fill,
    stroke_color, fill,
    stroke_color, fill,
    stroke_color, fill,
    stroke_color, fill
  )
}

#' Construction Icon SVG
#'
#' Generate a construction/crane icon SVG.
#'
#' @param width Width of the icon
#' @param height Height of the icon
#' @param stroke_color Stroke color
#' @param stroke_width Stroke width
#' @param fill Fill color
#' @return SVG string
#' @export
#' @examples
#' icon_construction(50, 56)
icon_construction <- function(width = 50, height = 56, 
                              stroke_color = "white", 
                              stroke_width = 2,
                              fill = "none") {
  sprintf(
    '<svg width="%d" height="%d" viewBox="0 0 50 56" fill="%s" xmlns="http://www.w3.org/2000/svg">
      <!-- Base -->
      <rect x="20" y="48" width="10" height="6" stroke="%s" stroke-width="%d" fill="%s"/>
      <!-- Tower -->
      <line x1="25" y1="48" x2="25" y2="8" stroke="%s" stroke-width="%d"/>
      <!-- Arm -->
      <line x1="25" y1="8" x2="45" y2="8" stroke="%s" stroke-width="%d"/>
      <!-- Cable -->
      <line x1="40" y1="8" x2="40" y2="25" stroke="%s" stroke-width="1"/>
      <!-- Hook -->
      <path d="M38 25 L42 25 L42 30 Q40 33 38 30 Z" stroke="%s" stroke-width="1" fill="%s"/>
      <!-- Counter weight -->
      <line x1="25" y1="8" x2="10" y2="8" stroke="%s" stroke-width="%d"/>
      <rect x="5" y="8" width="8" height="6" stroke="%s" stroke-width="1" fill="%s"/>
      <!-- Support cables -->
      <line x1="25" y1="15" x2="10" y2="8" stroke="%s" stroke-width="1"/>
      <line x1="25" y1="15" x2="40" y2="8" stroke="%s" stroke-width="1"/>
    </svg>',
    width, height, fill,
    stroke_color, stroke_width, fill,
    stroke_color, stroke_width,
    stroke_color, stroke_width,
    stroke_color,
    stroke_color, fill,
    stroke_color, stroke_width,
    stroke_color, fill,
    stroke_color,
    stroke_color
  )
}

#' Map Pin Icon SVG
#'
#' Generate a map pin/location icon SVG.
#'
#' @param width Width of the icon
#' @param height Height of the icon
#' @param stroke_color Stroke color
#' @param stroke_width Stroke width
#' @param fill Fill color
#' @return SVG string
#' @export
#' @examples
#' icon_map_pin(50, 56)
icon_map_pin <- function(width = 50, height = 56, 
                         stroke_color = "white", 
                         stroke_width = 2,
                         fill = "none") {
  sprintf(
    '<svg width="%d" height="%d" viewBox="0 0 50 56" fill="%s" xmlns="http://www.w3.org/2000/svg">
      <path d="M25 4 C14 4 6 12 6 22 C6 35 25 52 25 52 C25 52 44 35 44 22 C44 12 36 4 25 4 Z" 
            stroke="%s" stroke-width="%d" fill="%s"/>
      <circle cx="25" cy="22" r="8" stroke="%s" stroke-width="%d" fill="%s"/>
    </svg>',
    width, height, fill,
    stroke_color, stroke_width, fill,
    stroke_color, stroke_width, fill
  )
}

#' Dollar/Money Icon SVG
#'
#' Generate a dollar/money icon SVG.
#'
#' @param width Width of the icon
#' @param height Height of the icon
#' @param stroke_color Stroke color
#' @param stroke_width Stroke width
#' @param fill Fill color
#' @return SVG string
#' @export
#' @examples
#' icon_money(50, 56)
icon_money <- function(width = 50, height = 56, 
                       stroke_color = "white", 
                       stroke_width = 2,
                       fill = "none") {
  sprintf(
    '<svg width="%d" height="%d" viewBox="0 0 50 56" fill="%s" xmlns="http://www.w3.org/2000/svg">
      <circle cx="25" cy="28" r="20" stroke="%s" stroke-width="%d" fill="%s"/>
      <text x="25" y="35" font-family="Arial, sans-serif" font-size="24" font-weight="bold" 
            fill="%s" text-anchor="middle">$</text>
    </svg>',
    width, height, fill,
    stroke_color, stroke_width, fill,
    stroke_color
  )
}

#' Generic logo placeholder SVG
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function is deprecated. Use [load_svg_for_embed()] instead,
#' or simply pass the SVG file path directly to `svg_card()`.
#'
#' @param svg_content Raw SVG content or path to SVG file
#' @param width Desired width
#' @param height Desired height
#' @return SVG string
#' @keywords internal
custom_logo_svg <- function(svg_content, width = 40, height = 40) {
  cli::cli_warn(c(
    "!" = "{.fn custom_logo_svg} is deprecated.",
    "i" = "Use {.fn load_svg_for_embed} or pass file paths directly to {.fn svg_card}."
  ))
  
  if (file.exists(svg_content)) {
    result <- load_svg_for_embed(svg_content, target_height = height, target_width = width)
    return(result$svg_content)
  }
  
  # It's raw SVG content
  svg_content <- gsub('width="[0-9.]+"', sprintf('width="%d"', width), svg_content)
  svg_content <- gsub('height="[0-9.]+"', sprintf('height="%d"', height), svg_content)
  
  return(svg_content)
}
