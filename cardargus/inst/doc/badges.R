## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----single-badge, eval=FALSE-------------------------------------------------
# library(cardargus)
# 
# # Basic badge
# badge <- create_badge(
#   label = "Status",
#   value = "Active",
#   color = "#4CAF50"
# )
# 
# # Save to file
# writeLines(badge, "status_badge.svg")

## ----colors, eval=FALSE-------------------------------------------------------
# # Green for success
# badge_success <- create_badge("Build", "Passing", "#4CAF50")
# 
# # Red for failure
# badge_error <- create_badge("Build", "Failing", "#e74c3c")
# 
# # Blue for information
# badge_info <- create_badge("Version", "1.0.0", "#3498db")
# 
# # Orange for warning
# badge_warning <- create_badge("Coverage", "45%", "#fab255")
# 
# # White (default)
# badge_default <- create_badge("License", "MIT", "white")

## ----auto-color, eval=FALSE---------------------------------------------------
# # Dark background -> white text
# create_badge("Test", "Dark", "#2c3e50")
# 
# # Light background -> dark text
# create_badge("Test", "Light", "#f1c40f")

## ----badge-row, eval=FALSE----------------------------------------------------
# badges <- list(
#   list(label = "Units", value = "192"),
#   list(label = "Budget", value = "$36.4M"),
#   list(label = "Status", value = "Active", color = "#4CAF50")
# )
# 
# # All badges will have uniform height
# row <- create_badge_row(badges, default_color = "white", uniform_height = TRUE)

## ----in-cards, eval=FALSE-----------------------------------------------------
# card <- svg_card(
#   title = "PROJECT",
#   badges_data = list(
#     list(label = "Units", value = "500", color = "white"),
#     list(label = "Budget", value = "$50M", color = "#4CAF50"),
#     list(label = "Phase", value = "2/3", color = "#3498db")
#   ),
#   fields = list(...),
#   bg_color = "#fab255"
# )

## ----uniform, eval=FALSE------------------------------------------------------
# # These badges will all have the same height
# badges <- list(
#   list(label = "A", value = "1"),              # Short
#   list(label = "Long Label", value = "Value"), # Long
#   list(label = "X", value = "Y")               # Short
# )
# 
# row <- create_badge_row(badges, uniform_height = TRUE)

## ----customize, eval=FALSE----------------------------------------------------
# badge <- create_badge(
#   label = "Custom",
#   value = "Badge",
#   color = "#9b59b6",
#   font = "Roboto",          # Custom font
#   fontsize = 12,            # Font size
#   style = "margin: 5px;"    # Additional CSS
# )

## ----save, eval=FALSE---------------------------------------------------------
# # Single badge
# badge <- create_badge("Version", "1.0.0", "#3498db")
# writeLines(badge, "version.svg")
# 
# # Badge row
# row <- create_badge_row(badges)
# writeLines(row, "badges.svg")

## ----rmarkdown, eval=FALSE----------------------------------------------------
# # Display inline
# badge <- create_badge("Status", "Active", "#4CAF50")
# knitr::asis_output(badge)

