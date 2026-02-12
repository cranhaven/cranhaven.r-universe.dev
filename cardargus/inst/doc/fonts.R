## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval=FALSE--------------------------------------------------------
# library(cardargus)
# 
# # Automatically setup fonts (recommended)
# setup_fonts()

## ----check, eval=FALSE--------------------------------------------------------
# # Check if a font is available
# font_available("Jost")
# font_available("Arial")
# 
# # List cached fonts
# list_fonts()

## ----google, eval=FALSE-------------------------------------------------------
# # Register a specific Google Font
# register_google_font("Roboto")
# register_google_font("Open Sans")
# register_google_font("Lato")

## ----install, eval=FALSE------------------------------------------------------
# # Download and cache fonts
# install_fonts()
# 
# # Install specific fonts
# install_fonts(fonts = c("Jost", "Montserrat", "Roboto"))
# 
# # Check cache location
# font_cache_dir()

## ----custom, eval=FALSE-------------------------------------------------------
# # Use a different font in cards
# card <- svg_card(
#   title = "MY CARD",
#   font = "Montserrat",  # Use Montserrat instead of Jost
#   ...
# )

## ----css, eval=FALSE----------------------------------------------------------
# # Generate CSS for a font
# css <- get_font_css("Jost")
# cat(css)
# # @import url("https://fonts.googleapis.com/css2?family=Jost:wght@400;500;600;700&display=swap");
# 
# # Multiple weights
# css <- get_font_css("Montserrat", weights = c(400, 700))

## ----cache, eval=FALSE--------------------------------------------------------
# # Get cache directory
# font_cache_dir()
# #> "/home/user/.cache/cardargus"
# 
# # List cached fonts
# list_fonts()

## ----resources, eval=FALSE----------------------------------------------------
# # SVGs directory (bundled logos)
# svgs_dir()
# 
# # Path to a specific SVG
# get_svg_path("morar_bem.svg")
# 
# # List available SVGs
# list_bundled_svgs()

## ----packages, eval=FALSE-----------------------------------------------------
# install.packages(c("showtext", "sysfonts", "systemfonts"))

## ----png, eval=FALSE----------------------------------------------------------
# # Setup fonts first
# setup_fonts()
# 
# # Download fonts for embedding
# install_fonts()
# 
# # Create and export
# card <- svg_card(...)
# svg_to_png(card, "card.png", dpi = 300)

## ----chrome, eval=FALSE-------------------------------------------------------
# # Ensure Chrome is available
# ensure_chrome(download = TRUE)
# 
# # Create and export
# card <- svg_card(...)
# svg_to_png_chrome(card, "card.png", dpi = 300)

## ----server, eval=FALSE-------------------------------------------------------
# # Option 1: Download fonts locally
# install_fonts()
# 
# # Update font cache (Linux)
# system("fc-cache -fv")
# 
# # Option 2: Use Chrome rendering (recommended)
# ensure_chrome(download = TRUE)
# svg_to_png_chrome(card, "card.png")

