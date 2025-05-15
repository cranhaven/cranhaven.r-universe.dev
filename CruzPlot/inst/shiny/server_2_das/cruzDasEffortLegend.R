# Turn off effort legend when switching to Simplified Effort
observeEvent(input$das_effort, {
  if (input$das_effort == 2) {
    updateCheckboxInput(session, "eff_legend", value = FALSE)
    cruz.eff.leg(FALSE)

    if (input$eff_legend_title == "Effort by Beaufort") {
      updateTextInput(session, "eff_legend_title", value = "")
      cruz.eff.leg.title("")
    }
  }

  if (input$das_effort == 3) {
    updateCheckboxInput(session, "eff_legend", value = TRUE)
    cruz.eff.leg(TRUE)

    if (input$eff_legend_title == "") {
      updateTextInput(session, "eff_legend_title", value = "Effort by Beaufort")
      cruz.eff.leg.title("Effort by Beaufort")
    }
  }
})

# Use reactiveVals so that map doesn't have to plot twice due to update
observeEvent(input$eff_legend, {
  cruz.eff.leg(input$eff_legend)
})

observeEvent(input$eff_legend_title, {
  cruz.eff.leg.title(input$eff_legend_title)
})


# Get and return parameters for effort legend
cruzDasEffortLegend <- reactive({
  req(input$das_effort != 1)

  ### General parameters, set in Legends section
  eff.leg.pos <- input$eff_legend_pos
  if (eff.leg.pos == 1) {
    validate(
      need(!is.na(input$eff_legend_lat), "Please enter a valid effort legend latitude value"),
      need(!is.na(input$eff_legend_lon), "Please enter a valid effort legend longitude value")
    )
    eff.leg.x = input$eff_legend_lon
    eff.leg.y = input$eff_legend_lat

  } else {
    eff.leg.x <- eff.leg.pos
    eff.leg.y <- NULL
  }

  font.fam <- font.family.vals[as.numeric(input$eff_legend_font)]

  # eff.leg.title <- if (input$eff_legend_title == "") NULL else input$eff_legend_title
  eff.leg.title <- if (cruz.eff.leg.title() == "") NULL else cruz.eff.leg.title()

  eff.leg.bty <- ifelse(input$eff_legend_boxCol == 1, "n", "o")
  eff.leg.box.col <- ifelse(input$eff_legend_boxCol == 2, NA, "black")
  eff.leg.box.lwd <- ifelse(input$eff_legend_boxCol == 2, 0, 1)
  eff.leg.box.cex <- input$eff_legend_textSize


  ### Parameters that are effort-type specific
  if (input$das_effort == 2) {
    # Simplified effort
    eff.leg.lab <- "Simplified effort"
    eff.leg.col <- input$das_effort_simp_col
    eff.leg.lwd <- input$das_effort_simp_lwd

  } else if (input$das_effort == 3) {
    # Detailed effort
    if (input$das_effort_det_byBft) {
      # Detailed effort - plot by Bft
      bft.range <- cruzDasEffortFilterBeaufortVal()
      bft.which <- (bft.range[1]:bft.range[2]) + 1
      validate(
        need(length(bft.which) <= 10, "Beaufort legend error 1"),
        need(length(bft.which) <= length(input$das_effort_det_bft_col),
             "Beaufort legend error 2")
      )

      eff.leg.lab <- (0:9)[bft.which]
      eff.leg.col <- input$das_effort_det_bft_col[bft.which]
      eff.leg.lwd <- input$das_effort_det_bft_lwd

    } else {
      # Detailed effort - plot by S/N/F
      snf.idx <- which(c("S", "N", "F") %in% req(input$das_effort_snf))
      eff.leg.lab <- c("Standard", "Non-standard", "Fine")[snf.idx]
      eff.leg.col <- c(input$das_effort_det_col_s, input$das_effort_det_col_n, input$das_effort_det_col_f)[snf.idx]
      eff.leg.lwd <- c(input$das_effort_det_lwd_s, input$das_effort_det_lwd_n, input$das_effort_det_lwd_f)[snf.idx]
    }
  }


  ### Return list
  list(
    eff.leg.x = eff.leg.x, eff.leg.y = eff.leg.y,
    eff.leg.title = eff.leg.title, eff.leg.lab = eff.leg.lab,
    eff.leg.col = eff.leg.col, eff.leg.lwd = eff.leg.lwd,
    eff.leg.bty = eff.leg.bty, eff.leg.box.col = eff.leg.box.col,
    eff.leg.box.lwd = eff.leg.box.lwd,
    eff.leg.box.cex = eff.leg.box.cex, font.fam = font.fam
  )
})
