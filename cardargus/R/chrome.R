# chrome.R - Chrome-based SVG conversion
#
# Convert SVG to PNG/PDF using headless Chrome via chromote package.
# Provides superior font rendering for Google Fonts.

#' Check if Chrome/Chromium is available for rendering
#'
#' @description
#' Checks whether the chromote package can find and use a Chrome or Chromium
#' installation for headless rendering.
#'
#' @param verbose Print status messages (default FALSE).
#'
#' @return TRUE if Chrome is available, FALSE otherwise.
#' @export
#'
#' @examples
#' if (chrome_available()) {
#'    cat("Using Chrome")
#' } else {
#'    cat("Using Magick")
#' }
#' 
chrome_available <- function(verbose = FALSE) {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    if (verbose) cli::cli_alert_warning("Package {.pkg chromote} is not installed.")
    return(FALSE)
  }
  
  tryCatch({
    if (verbose) cli::cli_alert_success("Chrome found: {.path {find_chrome_path()}}")
    TRUE
  }, error = function(e) {
    if (verbose) cli::cli_alert_warning("Chrome not found: {e$message}")
    FALSE
  })
}

#' Find Chrome executable path
#'
#' @description
#' Attempts to find a Chrome or Chromium executable on the system.
#' Checks common installation paths and environment variables.
#'
#' @return Path to Chrome executable, or NULL if not found.
#' @export
#'
#' @examples
#' path <- find_chrome_path()
#' if (!is.null(path)) {
#'   message("Chrome found at: ", path)
#' }
find_chrome_path <- function() {
  # Check environment variable first
  env_path <- Sys.getenv("CHROMOTE_CHROME", "")
  if (nzchar(env_path) && file.exists(env_path)) {
    return(env_path)
  }
  
  # Try chromote's finder
  if (requireNamespace("chromote", quietly = TRUE)) {
    tryCatch({
      return(chromote::find_chrome())
    }, error = function(e) NULL)
  }
  
  # Manual search for common paths
  candidates <- c(
    # Linux
    "/usr/bin/google-chrome",
    "/usr/bin/google-chrome-stable",
    "/usr/bin/chromium",
    "/usr/bin/chromium-browser",
    "/snap/bin/chromium",
    # macOS
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    # Windows
    file.path(Sys.getenv("PROGRAMFILES"), "Google/Chrome/Application/chrome.exe"),
    file.path(Sys.getenv("PROGRAMFILES(X86)"), "Google/Chrome/Application/chrome.exe"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe")
  )
  
  for (path in candidates) {
    if (file.exists(path)) {
      return(path)
    }
  }
  
  # Try 'which' on Unix-like systems
  if (.Platform$OS.type == "unix") {
    for (cmd in c("google-chrome", "google-chrome-stable", "chromium", "chromium-browser")) {
      result <- tryCatch({
        path <- system2("which", cmd, stdout = TRUE, stderr = FALSE)
        if (length(path) > 0 && file.exists(path[1])) path[1] else NULL
      }, error = function(e) NULL, warning = function(w) NULL)
      if (!is.null(result)) return(result)
    }
  }
  
  NULL
}

#' Ensure Chrome is available, downloading if necessary
#'
#' @description
#' Checks if Chrome is available and optionally downloads a standalone
#' Chrome for Testing if not found. This ensures Chrome-based rendering
#' works without requiring a system-wide Chrome installation.
#'
#' @param download If TRUE and Chrome is not found, attempt to download
#'   Chrome for Testing (default FALSE).
#' @param verbose Print status messages (default TRUE).
#'
#' @return TRUE if Chrome is available (or was successfully downloaded),
#'   FALSE otherwise.
#' @export
#'
#' @details
#' When `download = TRUE`, this function will download "Chrome for Testing",
#' a standalone Chrome distribution designed for automation. The download
#' is approximately 150MB and is cached in the user's data directory.
#'
#' Alternatively, you can:
#' - Install Chrome/Chromium system-wide
#' - Set the `CHROMOTE_CHROME` environment variable to point to an existing installation
#'
#' @examples
#' # Check and report status
#' ensure_chrome()
#'
#' # Download Chrome if not available
#' \dontrun{ # Requires an external Chrome/Chromium installation 
#' ensure_chrome(download = TRUE)
#' }
ensure_chrome <- function(download = FALSE, verbose = TRUE) {
  # Check if already available
  if (chrome_available(verbose = FALSE)) {
    if (verbose) {
      cli::cli_alert_success("Chrome is available: {.path {find_chrome_path()}}")
    }
    return(TRUE)
  }
  
  if (!download) {
    if (verbose) {
      cli::cli_alert_warning("Chrome/Chromium not found on this system.")
      cli::cli_h3("Options to enable Chrome rendering")
      cli::cli_bullets(c(
        "1" = "Install Chrome or Chromium:",
        " " = "Linux: {.code sudo apt install chromium-browser}",
        " " = "macOS: {.code brew install --cask google-chrome}",
        " " = "Windows: Download from {.url https://www.google.com/chrome/}",
        "2" = "Or set path to existing Chrome:",
        " " = '{.code Sys.setenv(CHROMOTE_CHROME = "/path/to/chrome")}',
        "3" = "Or download Chrome for Testing:",
        " " = "{.code ensure_chrome(download = TRUE)}"
      ))
    }
    return(FALSE)
  }
  
  # Attempt to download Chrome for Testing
  if (verbose) cli::cli_alert_info("Downloading Chrome for Testing...")
  
  if (!requireNamespace("chromote", quietly = TRUE)) {
    if (verbose) {
      cli::cli_alert_danger("Package {.pkg chromote} is required.")
      cli::cli_alert_info("Install with: {.code install.packages('chromote')}")
    }
    return(FALSE)
  }
  
  # Get platform-specific download URL
  download_info <- get_chrome_download_info()
  if (is.null(download_info)) {
    if (verbose) cli::cli_alert_danger("Could not determine download URL for this platform.")
    return(FALSE)
  }
  
  # Create cache directory
  cache_dir <- chrome_cache_dir()
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  zip_path <- file.path(cache_dir, download_info$filename)
  chrome_dir <- file.path(cache_dir, "chrome")
  
  # Download if not already cached
  if (!dir.exists(chrome_dir)) {
    if (verbose) cli::cli_progress_step("Downloading Chrome for Testing (~150MB)...")
    
    tryCatch({
      download.file(download_info$url, zip_path, mode = "wb", quiet = !verbose)
      
      if (verbose) cli::cli_progress_step("Extracting...")
      unzip(zip_path, exdir = cache_dir)
      
      # Rename extracted folder
      extracted <- list.dirs(cache_dir, recursive = FALSE, full.names = TRUE)
      extracted <- extracted[grepl("chrome", basename(extracted), ignore.case = TRUE)]
      if (length(extracted) > 0 && extracted[1] != chrome_dir) {
        file.rename(extracted[1], chrome_dir)
      }
      
      # Clean up zip
      unlink(zip_path)
      
    }, error = function(e) {
      if (verbose) cli::cli_alert_danger("Download failed: {e$message}")
      return(FALSE)
    })
  }
  
  # Find the executable
  chrome_exe <- find_chrome_in_dir(chrome_dir)
  
  if (!is.null(chrome_exe) && file.exists(chrome_exe)) {
    # Make executable on Unix
    if (.Platform$OS.type == "unix") {
      Sys.chmod(chrome_exe, mode = "0755")
    }
    
    # Set environment variable
    Sys.setenv(CHROMOTE_CHROME = chrome_exe)
    
    if (verbose) {
      cli::cli_alert_success("Chrome for Testing installed successfully!")
      cli::cli_alert_info("Location: {.path {chrome_exe}}")
    }
    return(TRUE)
  }
  
  if (verbose) cli::cli_alert_danger("Could not find Chrome executable after extraction.")
  FALSE
}

#' Get Chrome cache directory
#' @keywords internal
chrome_cache_dir <- function() {
  tryCatch(
    tools::R_user_dir("cardargus", which = "cache"),
    error = function(e) file.path(path.expand("~"), ".cache", "cardargus")
  )
}

#' Get Chrome for Testing download info
#' @keywords internal
get_chrome_download_info <- function() {
  # Chrome for Testing URLs (stable channel)
  # These are official Google-provided builds for testing/automation
  base_url <- "https://storage.googleapis.com/chrome-for-testing-public"
  version <- "131.0.6778.87"  # Stable version
  
  platform <- get_chrome_platform()
  if (is.null(platform)) return(NULL)
  
  list(
    url = sprintf("%s/%s/%s/chrome-%s.zip", base_url, version, platform, platform),
    filename = sprintf("chrome-%s.zip", platform),
    platform = platform
  )
}

#' Get platform identifier for Chrome downloads
#' @keywords internal
get_chrome_platform <- function() {
  os <- Sys.info()["sysname"]
  arch <- Sys.info()["machine"]
  
  if (os == "Linux") {
    return("linux64")
  } else if (os == "Darwin") {
    if (arch == "arm64") {
      return("mac-arm64")
    } else {
      return("mac-x64")
    }
  } else if (os == "Windows") {
    if (grepl("64", arch)) {
      return("win64")
    } else {
      return("win32")
    }
  }
  
  NULL
}

#' Find Chrome executable in a directory
#' @keywords internal
find_chrome_in_dir <- function(dir) {
  if (!dir.exists(dir)) return(NULL)
  
  os <- Sys.info()["sysname"]
  
  if (os == "Windows") {
    candidates <- list.files(dir, pattern = "chrome\\.exe$", 
                             recursive = TRUE, full.names = TRUE)
  } else if (os == "Darwin") {
    # macOS app bundle structure
    candidates <- list.files(dir, pattern = "^Google Chrome for Testing$|^Chromium$|^chrome$", 
                             recursive = TRUE, full.names = TRUE)
    # Also check for the actual binary inside .app
    app_dirs <- list.dirs(dir, recursive = TRUE)
    for (app in app_dirs) {
      if (grepl("\\.app/Contents/MacOS$", app)) {
        bins <- list.files(app, full.names = TRUE)
        candidates <- c(candidates, bins)
      }
    }
  } else {
    candidates <- list.files(dir, pattern = "^chrome$", 
                             recursive = TRUE, full.names = TRUE)
  }
  
  # Return first executable
  for (cand in candidates) {
    if (file.exists(cand)) {
      info <- file.info(cand)
      if (!info$isdir) return(cand)
    }
  }
  
  NULL
}

# ------------------------------------------------------------------------------
# Internal helper: wrap SVG in minimal HTML
# ------------------------------------------------------------------------------

#' Create a temporary HTML file wrapping an SVG
#'
#' @param svg_string SVG content as character string.
#' @param width_px Width in pixels. If NULL, extracted from SVG.
#' @param height_px Height in pixels. If NULL, extracted from SVG.
#' @param background Background color (default "transparent").
#'
#' @return A list with path, width, and height.
#' @keywords internal
write_svg_html_temp <- function(svg_string,
                                width_px = NULL,
                                height_px = NULL,
                                background = "transparent") {
  svg_string <- as.character(svg_string)
  
  # Extract dimensions from SVG if not provided
  if (is.null(width_px)) {
    width_px <- parse_svg_root_dim(svg_string, "width")
  }
  if (is.null(height_px)) {
    height_px <- parse_svg_root_dim(svg_string, "height")
  }
  
  # Fallback defaults
  if (is.na(width_px) || is.null(width_px)) width_px <- 800
  if (is.na(height_px) || is.null(height_px)) height_px <- 500
  
  width_px <- as.integer(ceiling(width_px))
  height_px <- as.integer(ceiling(height_px))
  
  # Minimal HTML wrapper - Chrome renders webfonts and @font-face perfectly
html <- sprintf(
    '<!doctype html>
<html>
<head>
<meta charset="utf-8"/>
<style>
  html, body {
    margin: 0; padding: 0;
    width: %dpx; height: %dpx;
    background: %s;
    overflow: hidden;
  }
  .wrap {
    width: %dpx; height: %dpx;
    display: flex;
    align-items: flex-start;
    justify-content: flex-start;
  }
  svg { display: block; }
</style>
</head>
<body>
  <div class="wrap">
    %s
  </div>
</body>
</html>',
    width_px, height_px, background,
    width_px, height_px,
    svg_string
  )
  
  tf <- tempfile(fileext = ".html")
  writeLines(html, tf, useBytes = TRUE)
  list(path = tf, width = width_px, height = height_px)
}

# ------------------------------------------------------------------------------
# Public API: Chrome-based conversion
# ------------------------------------------------------------------------------

#' Convert SVG to PNG using headless Chrome
#'
#' @description
#' Renders an SVG to PNG using headless Chrome via the chromote package.
#' This method provides superior font rendering compared to librsvg/ImageMagick,
#' as Chrome properly handles @font-face rules, web fonts, and CSS features.
#'
#' @param svg_input SVG string or path to an SVG file.
#' @param output_path Output path for the PNG file. If NULL, a temp file is used.
#' @param dpi Resolution in dots per inch (default 300). Chrome uses 96 DPI as base,
#'   so dpi = 300 results in approximately 3.125x scaling.
#' @param background Background color for the HTML page (default "transparent").
#'   Use "white", "#FFFFFF", etc. for a solid background.
#' @param timeout Maximum time in seconds to wait for page load (default 30).
#'
#' @return Path to the generated PNG file.
#' @export
#'
#' @examples
#' svg <- svg_card("FAR", list(), list())
#' file_name <- tempfile(fileext = ".png")
#' # High-quality PNG with Chrome rendering
#' \dontrun{ # Requires an external Chrome/Chromium installation.
#' if (chrome_available()) {
#'   png_path <- svg_to_png_chrome(svg, file_name, dpi = 300)
#' }
#' }
svg_to_png_chrome <- function(svg_input,
                               output_path = NULL,
                               dpi = 300,
                               background = "transparent",
                               timeout = 30) {
  
  if (!requireNamespace("chromote", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg chromote} is required.",
      "i" = "Install with: {.code install.packages('chromote')}"
    ))
  }
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg base64enc} is required.",
      "i" = "Install with: {.code install.packages('base64enc')}"
    ))
  }
  
  # Read SVG content
  if (is.character(svg_input) && length(svg_input) == 1 && file.exists(svg_input)) {
    svg_content <- paste(readLines(svg_input, warn = FALSE), collapse = "\n")
  } else {
    svg_content <- as.character(svg_input)
  }
  
  # Sanitize SVG (remove problematic metadata, but keep @import for Chrome)
  # We only remove Inkscape/sodipodi stuff, not the font imports
  svg_content <- sanitize_svg_metadata(svg_content)
  
  if (is.null(output_path)) {
    output_path <- tempfile(fileext = ".png")
  }
  ensure_output_dir(output_path)
  
  # Calculate scale factor from DPI (Chrome base is 96 DPI)
  scale <- dpi / 96
  
  # Create HTML wrapper
  page <- write_svg_html_temp(svg_content, background = background)
  
  # Start Chrome session
  b <- chromote::ChromoteSession$new()
  on.exit({
    try(b$close(), silent = TRUE)
    try(chromote::default_chromote_object()$close(), silent = TRUE)
  }, add = TRUE)
  
  
  # Navigate to HTML file
  file_url <- paste0("file://", normalizePath(page$path, winslash = "/"))
  b$Page$navigate(file_url)
  b$Page$loadEventFired(timeout = timeout)
  
  # Small delay to ensure fonts are loaded
  Sys.sleep(0.3)
  
  # Set viewport with scale factor for DPI
  b$Emulation$setDeviceMetricsOverride(
    width = as.integer(page$width),
    height = as.integer(page$height),
    deviceScaleFactor = scale,
    mobile = FALSE
  )
  
  # Capture screenshot
  shot <- b$Page$captureScreenshot(format = "png", fromSurface = TRUE)
  raw <- base64enc::base64decode(shot$data)
  writeBin(raw, output_path)
  
  output_path
}

#' Convert SVG to PDF using headless Chrome
#'
#' @description
#' Renders an SVG to PDF using headless Chrome via the chromote package.
#' This method produces vector PDFs with perfect font rendering.
#'
#' @param svg_input SVG string or path to an SVG file.
#' @param output_path Output path for the PDF file.
#' @param background Background color for the HTML page (default "transparent").
#' @param print_background Whether to include CSS backgrounds in PDF (default TRUE).
#' @param timeout Maximum time in seconds to wait for page load (default 30).
#'
#' @return Path to the generated PDF file.
#' @export
#'
#' @examples
#' \dontrun{ # It requires an external Chrome/Chromium installation 
#' svg <- svg_card("FAR", list(), list())
#' if (chrome_available()) {
#'   pdf_path <- svg_to_pdf_chrome(svg, tempfile(fileext = ".pdf"))
#' }
#' }
svg_to_pdf_chrome <- function(svg_input,
                               output_path,
                               background = "transparent",
                               print_background = TRUE,
                               timeout = 30) {
  
  if (!requireNamespace("chromote", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg chromote} is required.",
      "i" = "Install with: {.code install.packages('chromote')}"
    ))
  }
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg base64enc} is required.",
      "i" = "Install with: {.code install.packages('base64enc')}"
    ))
  }
  
  # Read SVG content
  if (is.character(svg_input) && length(svg_input) == 1 && file.exists(svg_input)) {
    svg_content <- paste(readLines(svg_input, warn = FALSE), collapse = "\n")
  } else {
    svg_content <- as.character(svg_input)
  }
  
  # Sanitize SVG metadata only (keep @import for Chrome)
  svg_content <- sanitize_svg_metadata(svg_content)
  
  ensure_output_dir(output_path)
  
  # Create HTML wrapper
  page <- write_svg_html_temp(svg_content, background = background)
  
  # Start Chrome session
  b <- chromote::ChromoteSession$new()
  on.exit({
    try(b$close(), silent = TRUE)
  }, add = TRUE)
  
  # Navigate to HTML file
  file_url <- paste0("file://", normalizePath(page$path, winslash = "/"))
  b$Page$navigate(file_url)
  b$Page$loadEventFired(timeout = timeout)
  
  # Small delay for font loading
  Sys.sleep(0.3)
  
  # Generate PDF with exact page size (inches = pixels / 96)
  pdf <- b$Page$printToPDF(
    printBackground = print_background,
    marginTop = 0,
    marginBottom = 0,
    marginLeft = 0,
    marginRight = 0,
    paperWidth = page$width / 96,
    paperHeight = page$height / 96,
    preferCSSPageSize = TRUE
  )
  
  raw <- base64enc::base64decode(pdf$data)
  writeBin(raw, output_path)
  
  output_path
}

# ------------------------------------------------------------------------------
# Internal: sanitize SVG metadata only (not @import)
# ------------------------------------------------------------------------------

#' Sanitize SVG metadata for Chrome rendering
#'
#' @description
#' Removes problematic Inkscape/Sodipodi metadata that can cause issues,
#' but preserves @import rules since Chrome handles them correctly.
#'
#' @param svg_content SVG string.
#' @return Sanitized SVG string.
#' @keywords internal
sanitize_svg_metadata <- function(svg_content) {
  svg <- as.character(svg_content)
  
  # Remove problematic metadata blocks (safe for rendering)
  svg <- gsub("<sodipodi:namedview[^>]*?/\\s*>", "", svg, perl = TRUE)
  svg <- gsub("<sodipodi:namedview[\\s\\S]*?</sodipodi:namedview\\s*>", "", svg, perl = TRUE)
  svg <- gsub("<inkscape:page[^>]*?/\\s*>", "", svg, perl = TRUE)
  svg <- gsub("<inkscape:page[\\s\\S]*?</inkscape:page\\s*>", "", svg, perl = TRUE)
  svg <- gsub("<metadata[\\s\\S]*?</metadata\\s*>", "", svg, perl = TRUE)
  
  # Remove attributes with undefined prefixes
  svg <- gsub("\\s+sodipodi:[a-zA-Z0-9_.-]+\\s*=\\s*\"[^\"]*\"", "", svg, perl = TRUE)
  svg <- gsub("\\s+inkscape:[a-zA-Z0-9_.-]+\\s*=\\s*\"[^\"]*\"", "", svg, perl = TRUE)
  
  svg
}
