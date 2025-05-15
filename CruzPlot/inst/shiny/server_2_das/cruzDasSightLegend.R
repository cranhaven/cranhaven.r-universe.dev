# Returns parameters for sighting legend

cruzDasSightLegend <- reactive({
  symbol.list <- cruzDasSightSymbol()

  leg.df       <- symbol.list$leg.df
  sight.type   <- symbol.list$sight.type
  sp.codes     <- symbol.list$sp.codes
  sp.codes.len <- length(sp.codes)
  sp.count     <- symbol.list$sp.count
  das.sight    <- symbol.list$das.sight

  font.fam <- font.family.vals[as.numeric(input$das_legend_font)]

  names.lab <- input$das_legend_names

  if (sight.type %in% c(3, 4)) {
    leg.lab <- leg.df$SpCode
    if ("5" %in% names.lab) leg.lab <- paste0(leg.lab, ", n = ", sp.count)

  } else {
    sp.codes.all <- cruz.list$sp.codes
    temp.use <- vapply(
      sp.codes, function(i) which(sp.codes.all$Code == i), 1,
      USE.NAMES = FALSE
    )

    sp.codes.all.use <- sp.codes.all[temp.use, ]

    # # This piece cuts the common name at the first comma, which results in incorrect names for some codes
    # sp.codes.all.use$Name_Common <- vapply(sp.codes.all.use$Name_Common, function(i) {
    #   unlist(strsplit(i, ","))[1]
    # }, as.character(1))

    leg.lab <- NULL
    if ("1" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Code)
    if ("2" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Abbr)
    if ("3" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Name_Scientific)
    if ("4" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Name_Common)
    validate(
      need(leg.lab, "Please select species information to display in the sighting legend")
    )

    if (cruzDasSightEventResight()) leg.lab <- paste0(leg.lab, ", Event: ", input$das_sighting_events)

    if ("5" %in% names.lab) {
      if (cruzDasSightEventResight()) {
        validate(need(length(unique(das.sight$Event)) <= 2, "Error processing sighting legend"))
        # Primary sighting will always come before resight
        sp.count.res <- c(table(das.sight$Event)[input$das_sighting_events[1]],
                          table(das.sight$Event)[input$das_sighting_events[2]])
        sp.count.res[is.na(sp.count.res)] <- 0
        leg.lab <- paste0(leg.lab, ", n = ", sp.count.res)
        rm(sp.count.res)

      } else {
        leg.lab <- paste0(leg.lab, ", n = ", sp.count)
        # leg.lab <- if (any(names.lab %in% 1:4)) {
        #   paste0(leg.lab, ", n = ", sp.count)
        # } else {
        #   paste0("n = ", sp.count)
        # }
      }
    }
  }

  leg.title <- if (input$das_legend_title == "") NULL else input$das_legend_title
  leg.bty <-     ifelse(input$das_legend_boxCol == 1, "n", "o")
  leg.box.col <- ifelse(input$das_legend_boxCol == 2, NA, "black")
  leg.box.lwd <- ifelse(input$das_legend_boxCol == 2, 0, 1)
  leg.box.cex <- input$das_legend_textSize

  leg.pos <- input$das_legend_pos
  if (leg.pos == 1) {
    validate(
      need(!is.na(input$das_legend_lat), "Please enter a valid legend latitude value"),
      need(!is.na(input$das_legend_lon), "Please enter a valid legend longitude value")
    )
    leg.x <- input$das_legend_lon
    leg.y <- input$das_legend_lat

  } else {
    leg.x <- leg.pos
    leg.y <- NULL
  }

  list(
    leg.x = leg.x, leg.y = leg.y, leg.lab = leg.lab, leg.title = leg.title,
    leg.pch = leg.df$pch, leg.col = leg.df$col,
    leg.cex = leg.df$cex, leg.lwd = leg.df$lwd,
    leg.bty = leg.bty, leg.box.col = leg.box.col,
    leg.box.lwd = leg.box.lwd, leg.box.cex = leg.box.cex,
    font.fam = font.fam
  )
})
