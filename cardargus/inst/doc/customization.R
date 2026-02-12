## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----bg-color, eval=FALSE-----------------------------------------------------
# library(cardargus)
# 
# # Orange card
# card_orange <- svg_card(title = "ALERT", bg_color = "#fab255", ...)
# 
# # Blue card
# card_blue <- svg_card(title = "INFO", bg_color = "#3498db", ...)
# 
# # Green card
# card_green <- svg_card(title = "SUCCESS", bg_color = "#2ecc71", ...)
# 
# # Red card
# card_red <- svg_card(title = "URGENT", bg_color = "#e74c3c", ...)
# 
# # Dark card
# card_dark <- svg_card(title = "PREMIUM", bg_color = "#2c3e50", ...)

## ----gradient, eval=FALSE-----------------------------------------------------
# # Horizontal gradient (left to right)
# card <- svg_card(
#   title = "HOUSING",
#   bg_color = "linear-gradient(to right, #1a5a3a, #2e7d32)",
#   ...
# )
# 
# # Diagonal gradient (135 degrees)
# card <- svg_card(
#   title = "PROGRAM",
#   bg_color = "linear-gradient(135deg, #667eea, #764ba2)",
#   ...
# )
# 
# # Vertical gradient (top to bottom)
# card <- svg_card(
#   title = "PROJECT",
#   bg_color = "linear-gradient(to bottom, #00c6ff, #0072ff)",
#   ...
# )
# 
# # Multi-color gradient
# card <- svg_card(
#   title = "RAINBOW",
#   bg_color = "linear-gradient(to right, #ff6b6b, #feca57, #48dbfb)",
#   ...
# )

## ----text-colors, eval=FALSE--------------------------------------------------
# card <- svg_card(
#   title = "MY CARD",
#   title_color = "#FFD700",      # Gold title
#   label_color = "#E0E0E0",      # Light gray labels
#   footer_color = "white",       # White footer
#   value_text_color = "#212529", # Dark gray values
#   value_bg_color = "#f8f8ff",   # Field background
#   ...
# )

## ----fonts, eval=FALSE--------------------------------------------------------
# # Using Roboto
# card <- svg_card(
#   title = "CARD",
#   font = "Roboto",
#   ...
# )
# 
# # Using Open Sans
# card <- svg_card(
#   title = "CARD",
#   font = "Open Sans",
#   ...
# )
# 
# # Using Montserrat
# card <- svg_card(
#   title = "CARD",
#   font = "Montserrat",
#   ...
# )

## ----font-sizes, eval=FALSE---------------------------------------------------
# card <- svg_card(
#   title = "CARD",
#   title_fontsize = 32,    # Larger title
#   label_fontsize = 12,    # Labels
#   value_fontsize = 11,    # Values
#   footer_fontsize = 8,    # Footer
#   ...
# )

## ----width, eval=FALSE--------------------------------------------------------
# # Narrow card
# card_narrow <- svg_card(width = 400, ...)
# 
# # Default card
# card_default <- svg_card(width = 500, ...)
# 
# # Wide card
# card_wide <- svg_card(width = 600, ...)

## ----padding, eval=FALSE------------------------------------------------------
# card <- svg_card(
#   padding = 25,         # Internal padding
#   corner_radius = 12,   # More rounded corners
#   ...
# )

## ----footer, eval=FALSE-------------------------------------------------------
# # With automatic timestamp
# card <- svg_card(
#   footer = NULL,  # Uses automatic timestamp
#   ...
# )
# 
# # With custom text
# card <- svg_card(
#   footer = "Source: Housing Dept. - Updated January 2026",
#   ...
# )
# 
# # Without footer
# card <- svg_card(
#   footer = "",
#   ...
# )

## ----logos, eval=FALSE--------------------------------------------------------
# # Without logos
# card <- svg_card(
#   logos = list(),
#   ...
# )
# 
# # With file paths
# card <- svg_card(
#   logos = c("path/to/logo1.svg", "path/to/logo2.svg"),
#   logos_height = 40,
#   ...
# )
# 
# # With bundled logos
# card <- svg_card(
#   logos = c(get_svg_path("morar_bem.svg")),
#   logos_height = 40,
#   bottom_logos = c(get_svg_path("gov_pe3.svg")),
#   bottom_logos_height = 35,
#   ...
# )

## ----complete, eval=FALSE-----------------------------------------------------
# card <- svg_card(
#   title = "HOUSING PROGRAM",
#   badges_data = list(
#     list(label = "Units", value = "1,000", color = "white"),
#     list(label = "Investment", value = "$100M", color = "#4CAF50")
#   ),
#   fields = list(
#     list(list(label = "Project", value = "Downtown Development", with_icon = TRUE)),
#     list(
#       list(label = "City", value = "Boston"),
#       list(label = "State", value = "MA")
#     )
#   ),
#   bg_color = "#2c3e50",
#   width = 550,
#   padding = 25,
#   corner_radius = 12,
#   font = "Roboto",
#   title_fontsize = 24,
#   title_color = "#ecf0f1",
#   label_color = "#bdc3c7",
#   footer = "Real-time updated data",
#   footer_color = "#95a5a6"
# )

## ----display, eval=FALSE------------------------------------------------------
# # In R Markdown or Quarto
# include_card(card)
# 
# # Or as PNG for better compatibility
# include_card_png(card, dpi = 150)

