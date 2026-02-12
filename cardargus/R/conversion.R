# conversion.R - SVG conversion utilities
#
# Save and convert SVG cards to PNG/PDF using high-quality renderers.

#' Sanitize SVG for rasterization engines (librsvg / ImageMagick)
#'
#' @description
#' SVGs exported by editors such as Inkscape may contain `sodipodi:*` and/or
#' `inkscape:*` nodes/attributes. If these namespace prefixes appear without the
#' corresponding `xmlns:*` declarations, strict parsers (notably `librsvg`) can
#' fail with errors like:
#' \preformatted{Namespace prefix sodipodi on namedview is not defined}
#'
#' This helper removes non-rendering metadata blocks and attributes commonly
#' responsible for parse failures, producing a more interoperable SVG string.
#' It also removes Google Fonts @import rules which are not supported by
#' rasterization engines.
#'
#' @param svg_input An SVG string or an object coercible to character
#'   (e.g. `htmltools::HTML`).
#'
#' @return A character string containing a sanitized SVG.
#'
#' @keywords internal
sanitize_svg_for_raster <- function(svg_input) {
  svg <- as.character(svg_input)
  
  # Remove problematic metadata blocks (safe for rendering)
  svg <- gsub("<sodipodi:namedview[^>]*?/\\s*>", "", svg, perl = TRUE)
  svg <- gsub("<sodipodi:namedview[\\s\\S]*?</sodipodi:namedview\\s*>", "", svg, perl = TRUE)
  
  svg <- gsub("<inkscape:page[^>]*?/\\s*>", "", svg, perl = TRUE)
  svg <- gsub("<inkscape:page[\\s\\S]*?</inkscape:page\\s*>", "", svg, perl = TRUE)
  
  # Metadata can also trigger strict parsers; safe to remove for raster output
  svg <- gsub("<metadata[\\s\\S]*?</metadata\\s*>", "", svg, perl = TRUE)
  
  # Remove attributes with undefined prefixes
  svg <- gsub("\\s+sodipodi:[a-zA-Z0-9_.-]+\\s*=\\s*\"[^\"]*\"", "", svg, perl = TRUE)
  svg <- gsub("\\s+inkscape:[a-zA-Z0-9_.-]+\\s*=\\s*\"[^\"]*\"", "", svg, perl = TRUE)
  
  # Remove Google Fonts @import rules (not supported by librsvg/magick)
  # These patterns handle various URL formats and encoding (&amp; vs &)
  # Pattern 1: @import url("...googleapis...")
  svg <- gsub('@import\\s+url\\(["\'][^"\']*googleapis[^"\']*["\']\\)\\s*;?', "", svg, perl = TRUE)
  # Pattern 2: @import url(...googleapis...) without quotes
  svg <- gsub('@import\\s+url\\([^)]*googleapis[^)]*\\)\\s*;?', "", svg, perl = TRUE)
  # Pattern 3: @import url("...fonts.google...")  
  svg <- gsub('@import\\s+url\\(["\'][^"\']*fonts\\.google[^"\']*["\']\\)\\s*;?', "", svg, perl = TRUE)
  # Pattern 4: Any remaining @import with google in URL
  svg <- gsub('@import[^;]*google[^;]*;', "", svg, perl = TRUE)
  
  svg
}

#' Ensure output directory exists
#' @param path Output file path.
#' @keywords internal
ensure_output_dir <- function(path) {
  output_dir <- dirname(path)
  if (!dir.exists(output_dir) && output_dir != ".") {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
}

#' Write SVG content to a temporary .svg file
#' @param svg_content Character SVG content.
#' @return Path to a temporary SVG file.
#' @keywords internal
write_svg_tempfile <- function(svg_content) {
  tf <- tempfile(fileext = ".svg")
  writeLines(as.character(svg_content), tf, useBytes = TRUE)
  tf
}

#' Parse a numeric width/height from the root <svg ...> tag
#'
#' @description
#' Tries:
#' 1) width="..." / height="..." (supports px, numbers)
#' 2) viewBox="minx miny width height" as fallback
#'
#' @param svg_content Character SVG.
#' @param attr "width" or "height".
#' @return Numeric value (in CSS px) or NA_real_ if not found.
#' @keywords internal
parse_svg_root_dim <- function(svg_content, attr = c("width", "height")) {
  attr <- match.arg(attr)
  svg <- as.character(svg_content)
  
  # 1) Direct attribute width="..." / height="..."
  m <- regexpr(paste0(attr, '="[^"]+"'), svg)
  if (m[1] != -1) {
    s <- regmatches(svg, m)
    val <- suppressWarnings(as.numeric(sub(".*?([0-9.]+).*", "\\1", s)))
    if (!is.na(val)) return(val)
  }
  
  # 2) Fallback to viewBox (minx miny w h)
  vb <- regmatches(svg, regexec('viewBox="([^"]+)"', svg))[[1]]
  if (length(vb) > 1) {
    parts <- strsplit(trimws(vb[2]), "\\s+")[[1]]
    parts <- suppressWarnings(as.numeric(parts))
    if (length(parts) == 4 && all(!is.na(parts))) {
      if (attr == "width") return(parts[3])
      if (attr == "height") return(parts[4])
    }
  }
  
  NA_real_
}

# ------------------------------------------------------------------------------
# Public API
# ------------------------------------------------------------------------------

#' Save SVG string to file (sanitized + embedded fonts)
#'
#' @description
#' Saves an SVG string to disk. Before saving, the function:
#' 1) sanitizes the SVG to remove problematic Inkscape/Sodipodi metadata that can
#'    break strict XML parsers, and
#' 2) detects and embeds fonts (WOFF2 via @font-face) for deterministic rendering.
#'
#' This function expects the font helpers to be available in the package:
#' `detect_svg_fonts()`, `ensure_cardargus_fonts()`, and `embed_svg_fonts()`.
#'
#' @param svg_content SVG string (or object coercible to character).
#' @param output_path Output file path.
#'
#' @return Path to the saved SVG file.
#' @export
#'
#' @examples
#' svg <- svg_card("FAR", list(), list())
#' save_svg(svg, tempfile(fileext = ".svg"))
#' 
save_svg <- function(svg_content, output_path) {
  ensure_output_dir(output_path)
  
  svg <- as.character(svg_content)
  
  # 1) Remove problematic metadata/namespaces first (prevents XML parsing errors)
  svg <- sanitize_svg_for_raster(svg)
  
  # 2) Detect fonts and embed them (WOFF2 via @font-face)
  #    (Assumes these helpers exist in the package.)
  fonts_used <- detect_svg_fonts(svg)
  if (length(fonts_used)) {
    font_paths <- ensure_cardargus_fonts(fonts_used)
    for (family in names(font_paths)) {
      woff2_path <- font_paths[[family]]
      if (!is.na(woff2_path) && file.exists(woff2_path)) {
        svg <- embed_svg_fonts(svg, font_family = family, woff2_path = woff2_path)
      }
    }
  }
  
  writeLines(svg, output_path, useBytes = TRUE)
  output_path
}

#' Convert SVG to PNG
#'
#' @description
#' Convert an SVG string or SVG file path to a high-quality PNG image.
#' The function sanitizes the SVG and embeds required WOFF2 fonts (downloaded
#' on demand into a user cache) to ensure consistent font rendering.
#'
#' **Important note about DPI**: `rsvg` rasterizes primarily based on pixel
#' dimensions. To make DPI matter, this function scales output pixel size by
#' `(dpi / 96)` when `width` / `height` are not explicitly provided.
#'
#' @param svg_input SVG string or path to an SVG file.
#' @param output_path Output path for the PNG file (optional; a temp file is used if NULL).
#' @param width Output width in pixels (NULL to infer from SVG and scale by DPI).
#' @param height Output height in pixels (NULL to infer from SVG and scale by DPI).
#' @param dpi Resolution in dots per inch (default 300 for high quality).
#' @param background Background color. Use "transparent" or "none" for transparency
#'   (default), or specify a color like "white", "#FFFFFF", etc.
#'
#' @return Path to the generated PNG file.
#' @export
#'
#' @examples
#' svg <- svg_card("FAR", list(), list())
#' file_name <- tempfile(fileext = ".png")
#' png_path <- svg_to_png(svg, file_name, dpi = 300)
#' png_path <- svg_to_png(svg, file_name, dpi = 300, background = "white")
svg_to_png <- function(svg_input,
                       output_path = NULL,
                       width = NULL,
                       height = NULL,
                       dpi = 300,
                       background = "transparent") {
  
  if (!requireNamespace("rsvg", quietly = TRUE) &&
      !requireNamespace("magick", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "No SVG renderer available.",
      "i" = "Install {.pkg rsvg} or {.pkg magick}:",
      " " = "{.code install.packages('rsvg')}"
    ))
  }
  
  # Read SVG (file path vs string)
  if (is.character(svg_input) && length(svg_input) == 1 && file.exists(svg_input)) {
    svg_content <- paste(readLines(svg_input, warn = FALSE), collapse = "\n")
  } else {
    svg_content <- as.character(svg_input)
  }
  
  # Sanitize + embed fonts (download/cache if needed)
  # (prepare_svg_for_raster() should be defined in fonts.R)
  svg_content <- prepare_svg_for_raster(svg_content)
  
  # Output path
  if (is.null(output_path)) output_path <- tempfile(fileext = ".png")
  ensure_output_dir(output_path)
  
  # Normalize background parameter
  bg_transparent <- tolower(background) %in% c("transparent", "none", "")
  
  # DPI scaling (SVG/CSS ref 96 DPI)
  scale <- dpi / 96
  if (is.null(width)) {
    w0 <- parse_svg_root_dim(svg_content, "width")
    if (!is.na(w0)) width <- max(1L, round(w0 * scale))
  }
  if (is.null(height)) {
    h0 <- parse_svg_root_dim(svg_content, "height")
    if (!is.na(h0)) height <- max(1L, round(h0 * scale))
  }
  
  svg_file <- write_svg_tempfile(svg_content)
  
  # Prefer rsvg first
  if (requireNamespace("rsvg", quietly = TRUE)) {
    ok <- tryCatch({
      rsvg::rsvg_png(svg_file, file = output_path, width = width, height = height)
      
      # Apply solid background if requested (flatten)
      if (!bg_transparent && requireNamespace("magick", quietly = TRUE)) {
        img <- magick::image_read(output_path)
        img <- magick::image_background(img, background, flatten = TRUE)
        magick::image_write(img, output_path, format = "png")
      }
      TRUE
    }, error = function(e) FALSE)
    
    if (isTRUE(ok)) return(output_path)
  }
  
  # Fallback: magick (IMPORTANT: set size at rasterization time via image_read_svg)
  if (requireNamespace("magick", quietly = TRUE)) {
    ok <- tryCatch({
      if (is.null(width) || is.null(height)) {
        img <- magick::image_read_svg(svg_file)
      } else {
        img <- magick::image_read_svg(svg_file, width = width, height = height)
      }
      
      if (!bg_transparent) {
        img <- magick::image_background(img, background, flatten = TRUE)
      }
      
      magick::image_write(img, output_path, format = "png")
      TRUE
    }, error = function(e) {
      cli::cli_abort("Failed to convert SVG to PNG: {e$message}")
    })
    
    if (isTRUE(ok)) return(output_path)
  }
  
  cli::cli_abort("Failed to convert SVG to PNG (no working renderer found).")
}

#' Convert SVG to multiple formats
#'
#' @description
#' Convert an SVG string or file to multiple formats. Supported formats are:
#' \itemize{
#'   \item \code{"svg"} - saves the SVG
#'   \item \code{"png"} - rasterizes to PNG via \code{\link{svg_to_png}}
#'   \item \code{"pdf"} - converts to PDF (prefers \pkg{rsvg})
#' }
#'
#' @param svg_input SVG string or file path.
#' @param output_base Base name for output files (without extension).
#' @param formats Vector of formats to generate ("png", "svg", "pdf").
#' @param dpi Resolution for raster formats.
#' @param background Background color for PNG output.
#'
#' @return Named list with paths to generated files.
#' @export
svg_to_formats <- function(svg_input,
                           output_base,
                           formats = c("svg", "png"),
                           dpi = 300,
                           background = "transparent") {
  
  results <- list()
  
  # Get SVG content
  if (is.character(svg_input) && length(svg_input) == 1 && file.exists(svg_input)) {
    svg_content <- paste(readLines(svg_input, warn = FALSE), collapse = "\n")
  } else {
    svg_content <- as.character(svg_input)
  }
  
  # Sanitize + embed fonts ONCE (prepare_svg_for_raster handles this)
  svg_prepared <- prepare_svg_for_raster(svg_content)
  
  for (fmt in formats) {
    output_path <- paste0(output_base, ".", fmt)
    
    if (identical(fmt, "svg")) {
      # Write directly - fonts are already embedded by prepare_svg_for_raster
      ensure_output_dir(output_path)
      writeLines(svg_prepared, output_path, useBytes = TRUE)
      results$svg <- output_path
      
    } else if (identical(fmt, "png")) {
      # svg_to_png will use the already-prepared content
      results$png <- svg_to_png(svg_prepared, output_path, dpi = dpi, background = background)
      
    } else if (identical(fmt, "pdf")) {
      ensure_output_dir(output_path)
      
      if (requireNamespace("rsvg", quietly = TRUE)) {
        svg_file <- write_svg_tempfile(svg_prepared)
        tryCatch({
          rsvg::rsvg_pdf(svg_file, file = output_path)
          results$pdf <- output_path
        }, error = function(e) {
          cli::cli_warn("Failed to create PDF via rsvg: {e$message}")
        })
      } else if (requireNamespace("magick", quietly = TRUE)) {
        svg_file <- write_svg_tempfile(svg_prepared)
        tryCatch({
          img <- magick::image_read_svg(svg_file)
          magick::image_write(img, path = output_path, format = "pdf")
          results$pdf <- output_path
        }, error = function(e) {
          cli::cli_warn("Failed to create PDF via magick: {e$message}")
        })
      } else {
        cli::cli_warn("PDF conversion requires {.pkg rsvg} or {.pkg magick}.")
      }
    }
  }
  
  results
}

#' Convert SVG to PNG with optional margin and background
#'
#' @description
#' Creates a PNG with extra margin around the card.
#' Fonts are embedded before rasterization for consistent appearance.
#'
#' @param svg_input SVG string or path to SVG file.
#' @param output_path Output path for PNG file (optional; a temp file is used if NULL).
#' @param margin Margin in pixels to add around the card.
#' @param margin_color Color of the margin area (default transparent).
#' @param dpi Resolution in dots per inch.
#' @param background Background color for the card rasterization (default transparent).
#'
#' @return Path to the generated PNG file.
#' @export
svg_to_png_with_margin <- function(svg_input,
                                   output_path = NULL,
                                   margin = 20,
                                   margin_color = "transparent",
                                   dpi = 300,
                                   background = "transparent") {
  
  if (!requireNamespace("magick", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "{.fn svg_to_png_with_margin} requires the {.pkg magick} package.",
      "i" = "Install with: {.code install.packages('magick')}"
    ))
  }
  
  # First convert to PNG (rasterize card) using embedded fonts
  temp_png <- tempfile(fileext = ".png")
  svg_to_png(svg_input, temp_png, dpi = dpi, background = background)
  
  if (is.null(output_path)) output_path <- tempfile(fileext = ".png")
  ensure_output_dir(output_path)
  
  img <- magick::image_read(temp_png)
  info <- magick::image_info(img)
  
  new_width  <- info$width + 2 * margin
  new_height <- info$height + 2 * margin
  
  is_transparent_margin <- tolower(margin_color) %in% c("transparent", "none", "")
  
  # Create the background canvas
  bg <- if (is_transparent_margin) {
    magick::image_blank(new_width, new_height, color = "none")
  } else {
    magick::image_blank(new_width, new_height, color = margin_color)
  }
  
  # operator="over" is important to avoid blank composites with alpha
  result <- magick::image_composite(
    bg, img,
    operator = "over",
    offset = sprintf("+%d+%d", margin, margin)
  )
  
  magick::image_write(result, output_path, format = "png")
  output_path
}

#' Batch convert multiple SVG cards to PNG
#'
#' @description
#' Convert a list of SVG strings to PNG files in a given directory.
#' Fonts are embedded automatically for consistent rendering.
#'
#' @param svg_list List of SVG strings.
#' @param output_dir Output directory.
#' @param prefix File name prefix.
#' @param dpi Resolution.
#' @param background Background color for PNG output.
#'
#' @return Character vector of output paths.
#' @export
batch_svg_to_png <- function(svg_list,
                             output_dir = ".",
                             prefix = "card",
                             dpi = 300,
                             background = "transparent") {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  paths <- character(length(svg_list))
  
  for (i in seq_along(svg_list)) {
    output_path <- file.path(output_dir, sprintf("%s_%03d.png", prefix, i))
    paths[i] <- svg_to_png(svg_list[[i]], output_path, dpi = dpi, background = background)
  }
  
  paths
}