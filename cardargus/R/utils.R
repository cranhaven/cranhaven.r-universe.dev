# utils.R - Utility functions for SVG cards

#' Check if a color is light
#'
#' @param color A hex color string or color name
#' @return Logical indicating if the color is light
#' @export
#' @examples
#' is_light_color("#FFFFFF")
#' is_light_color("#000000")
is_light_color <- function(color) {
  # Convert color to RGB
  if (grepl("^#", color)) {
    hex <- gsub("^#", "", color)
    if (nchar(hex) == 3) {
      hex <- paste0(substr(hex, 1, 1), substr(hex, 1, 1),
                    substr(hex, 2, 2), substr(hex, 2, 2),
                    substr(hex, 3, 3), substr(hex, 3, 3))
    }
    r <- strtoi(substr(hex, 1, 2), 16)
    g <- strtoi(substr(hex, 3, 4), 16)
    b <- strtoi(substr(hex, 5, 6), 16)
  } else {
    # Try to get RGB from color name
    rgb_vals <- col2rgb(color)
    r <- rgb_vals[1]
    g <- rgb_vals[2]
    b <- rgb_vals[3]
  }
  
  # Calculate luminance
  luminance <- (0.299 * r + 0.587 * g + 0.114 * b) / 255
  return(luminance > 0.5)
}

#' Compress number to abbreviated format
#'
#' @param x Numeric value to compress
#' @param digits Number of decimal places
#' @return Character string with abbreviated number
#' @export
#' @examples
#' compress_number(1234567)
#' compress_number(36400000)
compress_number <- function(x, digits = 1) {
  if (is.na(x) || is.null(x)) return("S/I")
  if (x == 0) return("0,0")
  
  abs_x <- abs(x)
  
  if (abs_x >= 1e9) {
    value <- round(x / 1e9, digits)
    suffix <- " bilh\u00F5es"
  } else if (abs_x >= 1e6) {
    value <- round(x / 1e6, digits)
    suffix <- " milh\u00F5es"
  } else if (abs_x >= 1e3) {
    value <- round(x / 1e3, digits)
    suffix <- " mil"
  } else {
    value <- round(x, digits)
    suffix <- ""
  }
  
  # Format with Brazilian locale (comma as decimal separator)
  formatted <- format(value, nsmall = digits, decimal.mark = ",", big.mark = ".")
  paste0(trimws(formatted), suffix)
}

#' Get Google Font CSS for embedding in SVG
#'
#' @param font_name Name of the Google Font
#' @param weights Vector of font weights to include
#' @return Character string with CSS @font-face rules
#' @export
#' @examples
#' get_font_css("Jost")
get_font_css <- function(font_name = "Jost", weights = c(400, 500, 600, 700)) {
  # Generate @font-face rules for Google Fonts
  css <- sprintf(
    '@import url("https://fonts.googleapis.com/css2?family=%s:wght@%s&display=swap");',
    gsub(" ", "+", font_name),
    paste(weights, collapse = ";")
  )
  return(css)
}

#' Generate unique ID suffix
#'
#' @param ... Values to hash
#' @return Character string with unique ID
#' @keywords internal
generate_id <- function(...) {
  digest::digest(paste(..., runif(1), sep = "_"), algo = "crc32")
}

#' Escape XML special characters
#'
#' @param text Text to escape
#' @return Escaped text
#' @keywords internal
escape_xml <- function(text) {
  if (is.na(text) || is.null(text)) return("")
  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  text <- gsub("'", "&apos;", text)
  text <- gsub('"', "&quot;", text)
  return(text)
}

#' Calculate text width using gdtools
#'
#' @param text Text to measure
#' @param font Font name
#' @param fontsize Font size in pixels
#' @return Numeric width in pixels
#' @keywords internal
text_width <- function(text, font = "Jost", fontsize = 12) {
  if (is.na(text) || text == "") return(0)
  metrics <- gdtools::str_metrics(text, fontname = font, fontsize = fontsize)
  return(as.numeric(metrics["width"]))
}

#' Calculate text height using gdtools
#'
#' @param text Text to measure
#' @param font Font name
#' @param fontsize Font size in pixels
#' @return Numeric height in pixels
#' @keywords internal
text_height <- function(text, font = "Jost", fontsize = 12) {
  if (is.na(text) || text == "") return(fontsize)
  metrics <- gdtools::str_metrics(text, fontname = font, fontsize = fontsize)
  return(as.numeric(metrics["ascent"]) + as.numeric(metrics["descent"]))
}

#' Wrap text to fit within a given width
#'
#' @param text Text to wrap
#' @param max_width Maximum width in pixels
#' @param font Font name
#' @param fontsize Font size
#' @return List with wrapped lines and total height
#' @keywords internal
wrap_text <- function(text, max_width, font = "Jost", fontsize = 12) {
  if (is.na(text) || text == "") {
    return(list(lines = "", height = fontsize * 1.2))
  }
  
  words <- strsplit(text, " ")[[1]]
  lines <- character()
  current_line <- ""
  
  for (word in words) {
    test_line <- if (current_line == "") word else paste(current_line, word)
    if (text_width(test_line, font, fontsize) <= max_width) {
      current_line <- test_line
    } else {
      if (current_line != "") {
        lines <- c(lines, current_line)
      }
      current_line <- word
    }
  }
  
  if (current_line != "") {
    lines <- c(lines, current_line)
  }
  
  line_height <- fontsize * 1.3
  total_height <- length(lines) * line_height
  
  list(lines = lines, height = total_height, line_height = line_height)
}
