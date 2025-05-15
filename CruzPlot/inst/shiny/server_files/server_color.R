# Update select dropdown menus for greyscale vs normal colors

### Color input list
# Description: input names (default)
# Planned trasect:  planned_transects_color (grey)
# Land color:       color_land (bisque1)
# Water color:      color_water (white)
# Grid line color:  grid_line_color (black)
# Sighting symbol:            das_symbol_color (black)
# Effort - simplified effort: das_effort_simp_col (black)
# Effort - by Beaufort:       das_effort_det_bft_col (10 cols)
# Effort - standard:          das_effort_det_col_s (black)
# Effort - non-standard:      das_effort_det_col_n (black)
# Effort - fine:              das_effort_det_col_f (black)
# Non-das data - line:        ndas_line_col (black)
# Non-das data - point:       ndas_pt_col (black)


observeEvent(input$color_style, {
  if (!cruz.load.color.flag()) {
    if (input$color_style == 1) {
      palette("default")
      c.pal <- cruz.palette.color
      updateSelectInput(session, "planned_transects_color", choices = c.pal, selected = "grey")
      updateSelectInput(session, "color_land", choices = c.pal, selected = "bisque1")
      updateSelectInput(session, "color_water", choices = c.pal, selected = "white")
      updateSelectInput(session, "grid_line_color", choices = c.pal, selected = "black")
      updateSelectInput(session, "das_symbol_color", choices = c.pal, selected = "black")
      updateTextInput(session, "das_symbol_color_mult", value = "Black")
      updateSelectInput(session, "das_effort_simp_col", choices = c.pal, selected = "black")
      updateSelectInput(session, "das_effort_det_bft_col", choices = c.pal, selected = eff.bft.default)
      updateSelectInput(session, "das_effort_det_col_s", choices = c.pal, selected = "black")
      updateSelectInput(session, "das_effort_det_col_n", choices = c.pal, selected = "black")
      updateSelectInput(session, "das_effort_det_col_f", choices = c.pal, selected = "black")
      updateSelectInput(session, "ndas_line_col", choices = c.pal, selected = "black")
      updateSelectInput(session, "ndas_pt_col", choices = c.pal, selected = "black")

    } else if (input$color_style == 2) {
      palette(gray(0:5/5))
      c.pal <- cruz.palette.gray
      updateSelectInput(session, "planned_transects_color", choices = c.pal, selected = "grey")
      updateSelectInput(session, "color_land", choices = c.pal, selected = 4)
      updateSelectInput(session, "color_water", choices = c.pal, selected = 0)
      updateSelectInput(session, "grid_line_color", choices = c.pal, selected = 1)
      updateSelectInput(session, "das_symbol_color", choices = c.pal, selected = 1)
      updateTextInput(session, "das_symbol_color_mult", value = "Black")
      updateSelectInput(session, "das_effort_simp_col", choices = c.pal, selected = 1)
      updateSelectInput(session, "das_effort_det_bft_col", choices = c.pal, selected = 1)
      updateSelectInput(session, "das_effort_det_col_s", choices = c.pal, selected = 1)
      updateSelectInput(session, "das_effort_det_col_n", choices = c.pal, selected = 1)
      updateSelectInput(session, "das_effort_det_col_f", choices = c.pal, selected = 1)
      updateSelectInput(session, "ndas_line_col", choices = c.pal, selected = 1)
      updateSelectInput(session, "ndas_pt_col", choices = c.pal, selected = 1)
    }
  }

  cruz.load.color.flag(FALSE)
}, ignoreInit = TRUE)
