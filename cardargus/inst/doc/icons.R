## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----icons, eval=FALSE--------------------------------------------------------
# library(cardargus)
# 
# # House icon (default for fields with with_icon = TRUE)
# icon_house(width = 50, height = 56)
# 
# # Building icon
# icon_building(width = 50, height = 56)
# 
# # Construction icon
# icon_construction(width = 50, height = 56)
# 
# # Map pin icon
# icon_map_pin(width = 50, height = 56)
# 
# # Money icon
# icon_money(width = 50, height = 56)

## ----field-icons, eval=FALSE--------------------------------------------------
# fields <- list(
#   # Default house icon
#   list(
#     list(label = "Project", value = "Housing Complex", with_icon = TRUE)
#   ),
#   # Built-in building icon
#   list(
#     list(label = "Building", value = "Tower A",
#          with_icon = icon_building())
#   ),
#   # Construction icon
#   list(
#     list(label = "Status", value = "Under Construction",
#          with_icon = icon_construction())
#   ),
#   # Custom SVG file path
#   list(
#     list(label = "Custom", value = "My Item",
#          with_icon = "/path/to/my_icon.svg")
#   )
# )
# 
# card <- svg_card(
#   title = "PROJECT",
#   badges_data = list(),
#   fields = fields,
#   bg_color = "#3498db"
# )

## ----custom-icons, eval=FALSE-------------------------------------------------
# # Custom colors
# icon <- icon_house(
#   width = 60,
#   height = 70,
#   stroke_color = "#e74c3c",  # Red outline
#   stroke_width = 3,
#   fill = "#ffebee"           # Light red fill
# )
# 
# # Building with custom size
# building <- icon_building(
#   width = 40,
#   height = 48,
#   stroke_color = "#2c3e50",
#   fill = "#ecf0f1"
# )

## ----bundled, eval=FALSE------------------------------------------------------
# # List all bundled SVGs
# list_bundled_svgs()
# #> [1] "gov_pe3.svg" "morar_bem.svg" "seduh.svg" "tree.svg" "example_card.svg"
# 
# # Get full path to a logo
# path <- get_svg_path("morar_bem.svg")
# 
# # Get the SVGs directory
# svgs_dir()

## ----logos-cards, eval=FALSE--------------------------------------------------
# card <- svg_card(
#   title = "FAR",
#   badges_data = list(
#     list(label = "Units", value = "192", color = "white")
#   ),
#   fields = list(...),
#   bg_color = "#fab255",
# 
#   # Top-right logos (bundled or custom paths)
#   logos = c(
#     get_svg_path("morar_bem.svg"),
#     get_svg_path("seduh.svg"),
#     "/path/to/my_custom_logo.svg"  # Any local file
#   ),
#   logos_height = 40,
# 
#   # Bottom-left logos
#   bottom_logos = c(
#     get_svg_path("gov_pe3.svg")
#   ),
#   bottom_logos_height = 35
# )

## ----external, eval=FALSE-----------------------------------------------------
# # Load and resize an external SVG
# logo_data <- load_svg_for_embed(
#   svg_path = "path/to/my_logo.svg",
#   target_height = 50
# )
# 
# # The result contains:
# # - svg_content: The processed SVG string
# # - width: Calculated width (maintains aspect ratio)
# # - height: The target height

## ----logo-rows, eval=FALSE----------------------------------------------------
# # Top-right logo row
# logo_row <- create_logo_row(
#   logos = c("logo1.svg", "logo2.svg"),
#   target_height = 40,
#   spacing = 15,
#   card_width = 500,
#   x_offset = 20,
#   y_offset = 20
# )
# 
# # Bottom-left logo row
# bottom_row <- create_bottom_logo_row(
#   logos = c("logo3.svg"),
#   target_height = 30,
#   spacing = 10,
#   x_offset = 20,
#   card_height = 400,
#   y_offset = 20
# )

## ----custom-svg, eval=FALSE---------------------------------------------------
# # Create a simple custom logo
# my_logo <- '<svg width="50" height="50" viewBox="0 0 50 50">
#   <circle cx="25" cy="25" r="20" fill="#3498db"/>
#   <text x="25" y="30" text-anchor="middle" fill="white" font-size="16">AB</text>
# </svg>'
# 
# # Use in a card
# card <- svg_card(
#   title = "PROJECT",
#   logos = list(my_logo),
#   logos_height = 40,
#   ...
# )

