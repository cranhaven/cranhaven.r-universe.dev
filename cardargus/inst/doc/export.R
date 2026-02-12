## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----svg, eval=FALSE----------------------------------------------------------
# library(cardargus)
# 
# card <- svg_card(
#   title = "PROJECT",
#   badges_data = list(list(label = "Status", value = "Active")),
#   fields = list(list(list(label = "Name", value = "Test"))),
#   bg_color = "#3498db"
# )
# 
# # Save to file (with embedded fonts)
# save_svg(card, "my_card.svg")

## ----png-basic, eval=FALSE----------------------------------------------------
# # Convert SVG string to PNG
# svg_to_png(card, "my_card.png")
# 
# # With custom DPI (default is 300)
# svg_to_png(card, "my_card.png", dpi = 300)

## ----png-chrome, eval=FALSE---------------------------------------------------
# # Check if Chrome is available
# if (chrome_available()) {
#   # High-quality PNG with Chrome
#   svg_to_png_chrome(card, "my_card.png", dpi = 300)
# }
# 
# # Convert to PDF (vector output)
# svg_to_pdf_chrome(card, "my_card.pdf")

## ----chrome-install, eval=FALSE-----------------------------------------------
# # Install chromote package
# install.packages("chromote")
# 
# # Check if Chrome is available
# chrome_available()
# 
# # If Chrome is not installed, download it automatically
# ensure_chrome(download = TRUE)
# 
# # This downloads "Chrome for Testing" (~150MB) to your user cache
# # No admin/root permissions required!

## ----png-bg, eval=FALSE-------------------------------------------------------
# # Transparent background (preserves rounded corners)
# svg_to_png(card, "card_transparent.png", dpi = 300)
# 
# # White background
# svg_to_png(card, "card_white.png", dpi = 300, background = "white")
# 
# # Chrome also supports backgrounds
# svg_to_png_chrome(card, "card_white.png", dpi = 300, background = "white")

## ----png-margin, eval=FALSE---------------------------------------------------
# # Add 20px margin with white background
# svg_to_png_with_margin(
#   card,
#   output_path = "card_with_margin.png",
#   margin = 20,
#   background = "white",
#   dpi = 300
# )

## ----formats, eval=FALSE------------------------------------------------------
# # Export to SVG and PNG
# svg_to_formats(
#   card,
#   output_base = "exports/my_card",  # Without extension
#   formats = c("svg", "png"),
#   dpi = 300
# )
# # Creates: exports/my_card.svg, exports/my_card.png

## ----batch, eval=FALSE--------------------------------------------------------
# # Create multiple cards
# cards <- list(
#   svg_card(title = "Card 1", badges_data = list(), fields = list()),
#   svg_card(title = "Card 2", badges_data = list(), fields = list()),
#   svg_card(title = "Card 3", badges_data = list(), fields = list())
# )
# 
# # Batch convert to PNG
# batch_svg_to_png(
#   svg_list = cards,
#   output_dir = "output/cards",
#   prefix = "card",
#   dpi = 300,
#   background = "white"
# )
# # Creates: output/cards/card_001.png, card_002.png, card_003.png

## ----inline, eval=FALSE-------------------------------------------------------
# # Display card directly in document (HTML only)
# include_card(card)
# 
# # With custom width
# include_card(card, width = "80%")

## ----png-display, eval=FALSE--------------------------------------------------
# # Display as PNG with auto engine selection
# include_card_png(card, dpi = 150)
# 
# # Force Chrome for best font rendering
# include_card_png(card, dpi = 300, engine = "chrome")
# 
# # Force rsvg (faster, no Chrome dependency)
# include_card_png(card, dpi = 300, engine = "rsvg")

## ----knitr-save, eval=FALSE---------------------------------------------------
# # Save and get path for knitr::include_graphics
# path <- save_card_for_knitr(
#   card,
#   filename = "my_card",
#   format = "png",
#   dpi = 300,
#   engine = "chrome"  # or "auto", "rsvg"
# )
# knitr::include_graphics(path)

## ----engine, eval=FALSE-------------------------------------------------------
# # Register the cardargus engine
# register_cardargus_knitr()
# 
# # Then in a chunk with engine='cardargus':
# # ```{cardargus}
# # svg_card(title = "Dynamic Card", ...)
# # ```

## ----grob, eval=FALSE---------------------------------------------------------
# library(grid)
# 
# # Convert card to grob (auto-selects best engine)
# grob <- card_to_grob(card, dpi = 150)
# 
# # Force Chrome rendering
# grob <- card_to_grob(card, dpi = 150, engine = "chrome")
# 
# # Draw in a grid viewport
# grid.newpage()
# grid.draw(grob)

## ----size, eval=FALSE---------------------------------------------------------
# # Smaller file (lower quality)
# svg_to_png_chrome(card, "card_web.png", dpi = 96)
# 
# # Medium file (balanced)
# svg_to_png_chrome(card, "card_standard.png", dpi = 150)
# 
# # Larger file (high quality)
# svg_to_png_chrome(card, "card_print.png", dpi = 300)

## ----compare, eval=FALSE------------------------------------------------------
# # Same card, different engines
# svg_to_png(card, "card_rsvg.png", dpi = 300)
# svg_to_png_chrome(card, "card_chrome.png", dpi = 300)
# 
# # Chrome generally produces better results for:
# # - Google Fonts and custom web fonts
# # - Complex CSS styling
# # - Font weights and variants

## ----errors, eval=FALSE-------------------------------------------------------
# # Safe conversion with fallback
# convert_card <- function(card, output_path, dpi = 300) {
#   if (chrome_available()) {
#     svg_to_png_chrome(card, output_path, dpi = dpi)
#   } else {
#     message("Chrome not available, using rsvg")
#     svg_to_png(card, output_path, dpi = dpi)
#   }
# }
# 
# # With explicit error handling
# tryCatch({
#   svg_to_png_chrome(card, "output.png", dpi = 300)
#   message("Conversion successful!")
# }, error = function(e) {
#   message("Chrome conversion failed: ", e$message)
#   message("Falling back to rsvg...")
#   svg_to_png(card, "output.png", dpi = 300)
# })

## ----chrome-not-found, eval=FALSE---------------------------------------------
# # Check Chrome availability
# chrome_available()
# 
# # Option 1: Download Chrome for Testing automatically
# ensure_chrome(download = TRUE)
# 
# # Option 2: Set path to existing Chrome manually
# Sys.setenv(CHROMOTE_CHROME = "/path/to/chrome")
# 
# # Option 3: Find where Chrome is installed
# find_chrome_path()

## ----batch-tip, eval=FALSE----------------------------------------------------
# # For many cards, rsvg may be faster if fonts are embedded
# svg_to_png(card, "output.png", dpi = 300)
# 
# # Or accept the Chrome overhead for perfect rendering
# svg_to_png_chrome(card, "output.png", dpi = 300)

## ----timeout, eval=FALSE------------------------------------------------------
# # Increase timeout for complex SVGs
# svg_to_png_chrome(card, "output.png", timeout = 60)  # 60 seconds

