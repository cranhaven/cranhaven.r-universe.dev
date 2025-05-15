# cruzDasSightSymbol for CruzPlot - step 4 of processing species data
#   cruzDasSightSymbol() returns list of sighting type, species count, and
#     point type, color, size, and linewidth for legend and points, respectively
#   cruzDasSightSymbolAnimalSelected() returns list of point type, color, size, and linewidth for selected mammal or turtle sightings
#   cruzDasSightSymbolAnimalAll() returns list of point type, color, size, and linewidth for all mammal or turtle sightings
#   cruzDasSightSymbolBoat() returns list of point type, color, size, and linewidth for boat sightings
#   cruzDasSightSymbolCPOD() returns list of point type, color, size, and linewidth for CPOD sightings

# Two things need to be done - get/format pch, col, cex, and lwd for 1) legend and 2) sighting points.
#   Boats and C-PODs al have singl value for each paramter, so (1) and (2) will be the same (and of length one).
#   For mammals and turtles, 1) legend stuff requires one value per species code,
#   while 2) sighting points require one value per sighting
#   For (2) for mammals/turtles, we can use the legend values to build a species - plot params key,
#   which is then joined with das.sight


###############################################################################
cruzDasSightSymbol <- reactive({
  data.list <- cruzDasSightFilter()

  sight.type   <- data.list$sight.type
  das.sight    <- data.list$das.sight
  sp.codes     <- data.list$sp.codes
  sp.count     <- data.list$sp.count
  sp.selection <- data.list$sp.selection

  # Species code sanity check
  if (sight.type %in% c(1, 2)) {
    stopifnot(isTRUE(all.equal(length(sp.codes), length(sp.count))))
  } else {
    stopifnot(is.null(sp.codes))
  }

  if (sight.type %in% c(3, 4)) {
    # Boats and C-Pods
    symbol.prop <- cruzDasSightSymbolBoat()
    leg.df <- data.frame(
      SpCode = ifelse(sight.type == 3, "Boat", "CPOD"),
      pch = symbol.prop$pt.pch,
      col = symbol.prop$pt.col,
      cex = symbol.prop$pt.cex,
      lwd = symbol.prop$pt.lwd,
      stringsAsFactors = FALSE
    )

  } else {
    # Mammals and turtles
    symbol.prop <- if (sp.selection) {
      cruzDasSightSymbolAnimalSelected()
    } else {
      cruzDasSightSymbolAnimalAll()
    }

    # For validate statements
    leg.pch <- symbol.prop$pt.pch
    leg.col <- symbol.prop$pt.col
    leg.cex <- symbol.prop$pt.cex
    leg.lwd <- symbol.prop$pt.lwd

    # Empty input check
    validate(
      need(leg.pch != "",
           "Please enter at least one number for symbol type"),
      need(leg.col != "",
           "Please enter at least one number for symbol color"),
      need(leg.cex != "",
           "Please enter at least one number for symbol size"),
      need(all(0 < leg.cex & leg.cex < 20),
           paste("Please ensure all symbol size entries are numbers",
                 "greater than zero and less than twenty")),
      need(leg.lwd != "",
           "Please enter at least one number for symbol line width"),
      need(all(0 < leg.lwd & leg.lwd < 20),
           paste("Please ensure all symbol line width entries are numbers",
                 "greater than zero and less than twenty"))
    )

    # For each parameter, rep() until it's length == sp.codes.len,
    #   and check that there aren't more selected than species
    if (cruzDasSightEventResight()) {
      # Requires that only one species is selected
      validate(
        need(length(leg.cex) == 1,
             "Please provide only one symbol size when plotting resights")
      )
      leg.df <- data.frame(
        SpCode = sp.codes,
        Event = sort(input$das_sighting_events, decreasing = TRUE),#Ensures S, s, or equivalent
        pch = .func_sight_symbol_pt(leg.pch, "type", 1),
        col = .func_sight_symbol_pt(leg.col, "color", 2),
        cex = .func_sight_symbol_pt(c(leg.cex, 0.75 * leg.cex), "size", 2),
        lwd = .func_sight_symbol_pt(leg.lwd, "line width", 1),
        stringsAsFactors = FALSE
      )

    } else {
      sp.codes.len <- length(sp.codes)
      leg.df <- data.frame(
        SpCode = sp.codes,
        pch = .func_sight_symbol_pt(leg.pch, "type", sp.codes.len),
        col = .func_sight_symbol_pt(leg.col, "color", sp.codes.len),
        cex = .func_sight_symbol_pt(leg.cex, "size", sp.codes.len),
        lwd = .func_sight_symbol_pt(leg.lwd, "line width", sp.codes.len),
        stringsAsFactors = FALSE
      )
    }
  }

  if (cruzDasSightEventResight()) {
    pt.df <- left_join(das.sight, leg.df, by = c("SpCode", "Event")) %>%
      select(.data$SpCode, .data$Lon, .data$Lat,
             .data$pch, .data$col, .data$cex, .data$lwd)
  } else {
    pt.df <- left_join(das.sight, leg.df, by = "SpCode") %>%
      select(.data$SpCode, .data$Lon, .data$Lat,
             .data$pch, .data$col, .data$cex, .data$lwd)
  }

  # # If specified, make the symbol color correspond to event code
  # if (sight.type %in% c(1, 2) & input$das_symbol_event) {
  #   pt.df
  # }

  list(
    sight.type = sight.type, sp.count = sp.count, sp.codes = sp.codes,
    leg.df = leg.df, pt.df = pt.df, das.sight = das.sight
  )
})



.func_sight_symbol_pt <- function(pt.x, pt.txt, sp.codes.len, valid.check = TRUE) {
  if (valid.check)
    validate(
      need(sp.codes.len >= length(pt.x),
           paste("There are more symbol", pt.txt, "entries than species"))
    )

  if (sp.codes.len > length(pt.x)) {
    pt.x <- rep(pt.x, ceiling(sp.codes.len / length(pt.x)))
    pt.x <- pt.x[1:sp.codes.len]
  }
  # else {
  #   pt.x <- pt.x[1:sp.codes.len]
  # }

  pt.x
}


###############################################################################
# Mammal or turtle symbol properties
# Plot parameters for plotting selected mammal/turtle species
cruzDasSightSymbolAnimalSelected <- reactive({
  if (!input$das_symbol_mult) {
    pt.pch <- as.numeric(input$das_symbol_type)
    pt.col <- input$das_symbol_color

  } else {
    pt.pch <- as.numeric(unlist(strsplit(input$das_symbol_type_mult, ",")))
    pt.col <- str_trim(unlist(strsplit(input$das_symbol_color_mult, ",")))
    # pt.pch <- as.numeric(unlist(strsplit(cruz.das.symbol.type(), ",")))
    # pt.col <- str_trim(unlist(strsplit(cruz.das.symbol.color(), ",")))
    validate(
      need(length(pt.pch) > 0, # duplicate of check in main function
           "Please enter at least one value for symbol type"),
      need(all(pt.pch %in% 0:20),
           paste("Not all symbol type entries are valid." ,
                 "Please be sure all entries are a whole number from 0 to 20")),
      need(length(pt.col) > 0,
           "Please enter at least one value for symbol color")
    )

    # Covert color names to color codes-codes established in server file,
    #   keeping them in order
    valid.message <- paste(
      "Not all symbol color entries are valid. Please be sure each entry",
      "matches a color in the Color and Formatting Options page"
    )
    if (input$color_style == 1) {
      validate(need(all(pt.col %in% symbol.col), valid.message))
      pt.col <- vapply(pt.col, function(i) which(symbol.col %in% i), 1)
      pt.col <- symbol.col.code[pt.col]

    } else if (input$color_style == 2) {
      validate(need(all(pt.col %in% symbol.col.gray), valid.message))
      pt.col <- vapply(pt.col, function(i) which(symbol.col.gray %in% i), 1)
      pt.col <- symbol.col.code.gray[pt.col]
    }
  }

  # Same whether input$das_symbol_mult is checked or not
  pt.cex <- as.numeric(unlist(strsplit(input$das_symbol_size, ",")))
  pt.lwd <- as.numeric(unlist(strsplit(input$das_symbol_linewidth, ",")))

  list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd)
})

# Plot parameters for plotting all mammal/turtle species
cruzDasSightSymbolAnimalAll <- reactive({
  sp.codes.len <- length(cruzDasSightFilter()$sp.codes)
  pch.all <- unname(unlist(cruz.symbol.type))
  col.all <- c(
    "black", "red", "forestgreen", "orange",
    "blue", "tan4", "yellow", "aquamarine2", "bisque1", "hotpink",
    "green", "wheat3", "lightblue", "indianred2", "gray"
  )

  pt.pch <- rep(pch.all, ceiling(sp.codes.len / length(pch.all)))
  pt.pch <- pt.pch[1:sp.codes.len]

  pt.col <- rep(col.all, each = length(pch.all))
  if (length(pt.col) < sp.codes.len) stop("Sight symbol error - report as issue")
  pt.col <- pt.col[1:sp.codes.len]

  pt.cex <- 1
  pt.lwd <- 1

  stopifnot(
    length(pt.pch) == length(unique(cruzDasSightFilter()$das.sight$SpCode)),
    length(pt.col) == length(unique(cruzDasSightFilter()$das.sight$SpCode))
  )

  list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd)
})

# Plot parameters for plotting boats or CPODs
cruzDasSightSymbolBoat <- reactive({
  pt.pch <- as.numeric(input$das_symbol_type_boat)
  pt.col <- input$das_symbol_color_boat
  pt.cex <- as.numeric(input$das_symbol_size_boat)
  pt.lwd <- as.numeric(input$das_symbol_linewidth_boat)

  list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd)
})

# CPOD symbol properties
# cruzDasSightSymbolCPOD <- reactive({
#   pt.pch <- as.numeric(input$das.symbol.type.cpod)
#   pt.col <- input$das.symbol.color.cpod
#   pt.cex <- as.numeric(input$das.symbol.size.cpod)
#   pt.lwd <- as.numeric(input$das.symbol.linewidth.cpod)
#
#   list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd)
# })

###############################################################################
