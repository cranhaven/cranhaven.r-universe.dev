# All code for drawing static plot

##############################################################################
##############################################################################
# draw_setVals - Set map param values and call reactive functions for drawMap.R

#------------------------------------------------------------------------------
# Map range
lon.range <- cruz.map.range$lon.range
lat.range <- cruz.map.range$lat.range
world2 <- cruz.map.range$world2
stopifnot("world2 param is not a logical" = inherits(world2, "logical"))

validate( #lats
  need(-90 <= lat.range[1] & lat.range[1]<= 90,
       "The bottom latitude must be a number between -90 and 90"),
  need(-90 <= lat.range[2] & lat.range[2]<= 90,
       "The top latitude must be a number between -90 and 90")
)
if ((0 <= lon.range[1] & 0 <= lon.range[2]) || (lon.range[1] < 0 & lon.range[2] < 0))
  validate( #lons
    need(lon.range[1] < lon.range[2],
         message = paste("Left longitude must be less than right longitude,",
                         "unless left longitude s positive and right longitude",
                         "is negative (Pacific-centered map)"))
  )
if (world2) {
  validate(
    need(0 <= lon.range[1] & lon.range[1]<= 360,
         "The left longtiude must be a number between -180 and 180"),
    need(0 <= lon.range[2] & lon.range[2]<= 360,
         "The right longtiude must be a number between -180 and 180")
  )
} else { #!cruz.map.range$world2
  validate(
    need(-180 <= lon.range[1] & lon.range[1]<= 180,
         "The left longtiude must be a number between -180 and 180"),
    need(-180 <= lon.range[2] & lon.range[2]<= 180,
         "The right longtiude must be a number between -180 and 180")
  )
}


#------------------------------------------------------------------------------
# Other map paramters - not tick/grid
map.name <- cruz.map.range$map.name
map.water.col <- cruzMapColorWater()
map.land.col <- cruzMapColorLand()
if (input$color_lakes_rivers) map.river <- cruzMapRiver()

map.coastline <- NULL
if (input$coast & !is.null(cruz.list$coastline))
  map.coastline <- cruz.list$coastline

if (input$bar) {
  scale.bar <- cruzMapScaleBar()

  validate(
    need(lon.range[1] <= scale.bar$x1,
         "Start of scale bar must be after left longitude value"),
    need(lon.range[2] >= (scale.bar$x2),
         paste("End of scale bar must be before right longitude value -",
               "please extend the map range or decrease the scale bar length")),
    need(lat.range[1] <= scale.bar$y,
         "Scale bar latitude must be greater than bottom latitude value"),
    need(lat.range[2] >= scale.bar$y,
         "Scale bar latitude must be less than top latitude value")
  )
}

validate(need(!is.na(input$label_title_size), "Please enter a valid title size value"))
validate(need(!is.na(input$label_axis_size), "Please enter a valid axis label size value"))
validate(need(input$label_title_size > 0, "Please enter a title size greater than zero"))
validate(need(input$label_axis_size > 0, "Please enter an axis label size greater than zero"))
title.info <- cruzMapLabelTitle()
axes.info <- cruzMapLabelAxes()


#------------------------------------------------------------------------------
# Other map paramters - tick/grid
if (input$tick || input$grid) {
  ### Error checks
  # Check that tick intervals are possibly valid
  validate(need(!is.na(cruz.tick$tick.interval.major), "Please enter a valid major tick interval value"))
  validate(need(!is.na(input$tick_interval_minor), "Please enter a valid minor tick interval value"))
  validate(need(cruz.tick$tick.interval.major > 0, "Please enter a major tick interval value greater than zero"))
  validate(need(input$tick_interval_minor >= 0, "Please enter a minor tick interval value greater than or equal to zero"))

  # Check that tick label size is a valid entry
  validate(need(!is.na(input$label_tick_size), "Please enter a valid tick label size value"))
  validate(need(input$label_tick_size >= 0, "Please enter a tick label size value greater than or equal to zero"))

  if (!world2) { #Check that actual longitude values are valid given map rnages
    validate(
      need(lon.range[1] <= as.numeric(cruz.tick$label.lon.start),
           message = "Start of longitude tick labels must be after left longitude value"),
      need(lon.range[2] >= as.numeric(cruz.tick$label.lon.start),
           message = "Start of longitude tick labels must be before right longitude value")
    )
  } else { #world2
    validate(
      need(as.numeric(cruz.tick$label.lon.start) != 0,
           message = "Please use '180' rather than '0' for the start of longitude tick labels")
    )
    if (as.numeric(cruz.tick$label.lon.start) < 0)
    {
      validate(
        need((as.numeric(cruz.tick$label.lon.start) + 180) <= lon.range[2],
             message = "Start of longitude tick labels must be before right longitude value")
      )
    }
    if (as.numeric(cruz.tick$label.lon.start) > 0)
    {
      validate(
        need(lon.range[1] <= (as.numeric(cruz.tick$label.lon.start)),
             message = "Start of longitude tick labels must be after left longitude value")
      )
    }
  }
  validate( #Check that actual latitude values are valid given map rnages
    need(lat.range[1] <= cruz.tick$label.lat.start,
         message = "Start of latitude tick labels must be greater than bottom latitude value"),
    need(lat.range[2] >= cruz.tick$label.lat.start,
         message = "Start of latitude tick labels must be less than top latitude value"),
    need(!is.na(input$tick_length),
         message = "Please enter a valid tick length value")
  )

  # Assignments
  tick.lon <- cruzMapIntervalLon()
  tick.lat <- cruzMapIntervalLat()
}

if (input$tick) {
  tick.lon.bool <- cruzMapTickLonBool()
  tick.lat.bool <- cruzMapTickLatBool()
  tick.lon$label <- cruzMapTickLonLab()
  tick.lat$label <- cruzMapTickLatLab()
  tick.param <- cruzMapTickParam()
}

if (input$grid) grid.param <- cruzMapGrid()


#------------------------------------------------------------------------------
### Planned transects
if (input$planned_transects_plot) {
  validate(
    need(input$planned_transects_toplot,
         "Please select at least one class of planned transects to plot")
  )
  #So that renderUI()'s can catch up
  req(input$planned_transects_color, input$planned_transects_lty)

  # Get user inputs
  pltrans <- cruz.list$planned.transects
  pltrans.which <- as.numeric(input$planned_transects_toplot)
  pltrans.which2 <- as.numeric(input$planned_transects_toplot2)
  pltrans.colors <- input$planned_transects_color
  pltrans.lty <- as.numeric(input$planned_transects_lty)
  pltrans.lwd <- input$planned_transects_lwd

  # Process user inputs
  if (length(pltrans.colors) == 1) {
    pltrans.colors <- rep(pltrans.colors, length(pltrans.which))
  }

  validate(
    need(length(pltrans.colors) == length(pltrans.which),
         paste("The number of selected planned transect colors must either be",
               "1 or equal to than the number of selected planned transects"))
  )

  pltrans.class1 <- planned_transects_class1()[pltrans.which]
  names(pltrans.colors) <- pltrans.class1

  pltrans <- dplyr::filter(pltrans, class1 %in% pltrans.class1)

  if (anyNA(planned_transects_class2())) {
    # Class 2 was not specified
    pltrans.list <- lapply(pltrans.class1, function(i) {
      x <- dplyr::filter(pltrans, class1 == i)

      lapply(unique(x$num), function(k) {
        x <- dplyr::filter(x, num == k)
        if (nrow(x) == 0) {
          NULL
        } else if (nrow(x) == 1){
          validate(need(FALSE, "Error in planned transect processing"))
        } else {
          list(x$lon, x$lat, unname(pltrans.colors[as.character(i)]), pltrans.lty)
        }
      })
    })

  } else {
    # Class 2 was specified
    validate(
      need(pltrans.which2,
           "Please select at least one class 2 type to plot")
    )

    pltrans.class2 <- planned_transects_class2()[pltrans.which2]
    pltrans <- dplyr::filter(pltrans, class2 %in% pltrans.class2)

    if (length(pltrans.lty) == 1) {
      pltrans.lty <- rep(pltrans.lty, length(pltrans.class2))
    }
    validate(
      need(length(pltrans.lty) == length(pltrans.class2),
           paste("The number of selecetd planned transect line types must either be",
                 "1 or equal to than the number unique class 2 values"))
    )
    names(pltrans.lty) <- pltrans.class2

    pltrans.list <- lapply(pltrans.class1, function(i) {
      x <- dplyr::filter(pltrans, class1 == i)

      lapply(pltrans.class2, function(j) {
        x <- dplyr::filter(x, class2 == j)

        lapply(unique(x$num), function(k) {
          x <- dplyr::filter(x, num == k)
          if (nrow(x) == 0) {
            NULL
          } else if (nrow(x) == 1){
            validate(need(FALSE, "Error in planned transect processing"))
          } else {
            list(x$lon, x$lat,
                 unname(pltrans.colors[as.character(i)]),
                 unname(pltrans.lty[as.character(j)]))
          }
        })
      })
    })
  }
}


###############################################################################
# Set data values and call reactive functions for drawData.R

#------------------------------------------------------------------------------
### Non-DAS
data.ndas <- if (input$ndas_plot) cruzNonDas() else NULL

#------------------------------------------------------------------------------
### DAS
if (isTruthy(cruz.list$das.data)) {
  req(input$das_sight_dateRange, input$das_effort_dateRange)

  #### TODO Add validate() checks for lat/long info
  # Sightings
  if (input$das_sightings) {
    # Error checks are in cruzDasSight... functions
    das.sight <- cruzDasSightFilter()$das.sight
    sight.type <- cruzDasSightFilter()$sight.type
    das.sight.pt <- cruzDasSightSymbol()$pt.df
    if (input$das_legend) das.sight.legend <- cruzDasSightLegend()
  }

  # Effort
  if (as.numeric(input$das_effort) != 1) {
    das.eff.lines <- cruzDasEffortFilter()

    eff.col <- cruzDasEffortParams()$eff.col
    eff.lwd <- cruzDasEffortParams()$eff.lwd
    # das.eff.legend <- if (input$eff_legend) cruzDasEffortLegend() else NULL
    das.eff.legend <- if (cruz.eff.leg()) cruzDasEffortLegend() else NULL


    # Adjust data.effort$Lon points as needed for world/world2
    validate(
      need(!(any(is.na(das.eff.lines$st_lat) | is.na(das.eff.lines$end_lat) |
                   is.na(das.eff.lines$st_lon) | is.na(das.eff.lines$end_lon))),
           "Some of the lat/long data for the effort is 'NA'")
    )
  }
}



##############################################################################
##############################################################################
##############################################################################
### drawMap for CruzPlot
## Creates window and then plots water and land (map)
## Plots scale bar, tick marks/labels, map labels, and grid lines as specified

###############################################################################
# Range and map window triggers for redrawing the map
input$map_replot
input$map_size
input$dimension

###############################################################################
#------------------------------------------------------------------------------
### Window
mar1 <- ifelse(nchar(axes.info$lab.lon) > 0, 7, 3)
mar2 <- ifelse(nchar(axes.info$lab.lat) > 0, 7, 5)
mar3 <- ifelse(nchar(title.info$lab)    > 0, 7, 2)

x.try <- try(map(map.name[[1]], xlim = lon.range[1:2], ylim = lat.range[1:2],
                 mar = c(mar1, mar2, mar3, 4)),
             silent = TRUE)
validate(need(x.try, "Error - there must be some land in the map area"))

x.1 <- map(map.name[[1]], xlim = lon.range[1:2], ylim = lat.range[1:2],
           mar = c(mar1, mar2, mar3, 4))
param <- cruzMapParam()$param.unit

#------------------------------------------------------------------------------
### Water
rect(param[1], param[3], param[2], param[4], col = map.water.col[[1]])

# Depth
map.depth <- map.water.col[[2]]
if (isTruthy(map.depth))
  plot(map.depth, image = TRUE, land = TRUE, add = TRUE,
       axes = FALSE, xlab = NA, ylab = NA, lwd = 0.0,
       bpal = list(c(0, max(map.depth), "grey"),
                   c(min(map.depth), 0, bathy.col)))

#------------------------------------------------------------------------------
### Land
if (input$coast) {
  # Coastline
  validate(
    need(isTruthy(map.coastline),
         message = "Please input a valid coastline file")
  )

  polygon(x = map.coastline$lon, y = map.coastline$lat, col = map.land.col)
  lines(x = map.coastline$lon, y = map.coastline$lat)

} else {
  # Default from maps package
  map(map.name[[1]], regions = map.name[[2]],
      xlim = lon.range[1:2], ylim = lat.range[1:2],
      fill = TRUE, col = map.land.col, add = TRUE)
}

#------------------------------------------------------------------------------
### Rivers and Lakes
if (input$color_lakes_rivers)
  map(map.river, col = map.water.col[[1]], add = TRUE)

graphics::box()

#------------------------------------------------------------------------------
### Tick marks and labels
if (input$tick) {
  # Draw major and minor tick marks
  if (tick.lon.bool$bot[1]) {
    axis(1, at = tick.lon$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale,
         family = tick.param$font)
    axis(1, at = tick.lon$min, labels = FALSE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") *0.4*tick.param$len)
  }
  if (tick.lat.bool$left[1]) {
    axis(2, at = tick.lat$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale,
         family = tick.param$font)
    axis(2, at = tick.lat$min, labels = FALSE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") * 0.4*tick.param$len)
  }
  if (tick.lon.bool$top[1]) {
    axis(3, at = tick.lon$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale,
         family = tick.param$font)
    axis(3, at = tick.lon$min, labels = FALSE, lwd = 0,  lwd.ticks = 1,
         tcl = par("tcl") * 0.4*tick.param$len)
  }
  if (tick.lat.bool$right[1]) {
    axis(4, at = tick.lat$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale,
         family = tick.param$font)
    axis(4, at = tick.lat$min, labels = FALSE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") * 0.4*tick.param$len)
  }

  # Draw tick labels
  if (tick.lon.bool$bot[2])
    axis(1, at = tick.lon$label.loc, labels = tick.lon$label, tick = FALSE,
         cex.axis = tick.param$scale, family = tick.param$font)
  if (tick.lat.bool$left[2])
    axis(2, at = tick.lat$label.loc, labels = tick.lat$label, tick = FALSE,
         las = 1, cex.axis = tick.param$scale, family = tick.param$font)
  if (tick.lon.bool$top[2])
    axis(3, at = tick.lon$label.loc, labels = tick.lon$label, tick = FALSE,
         cex.axis = tick.param$scale, family = tick.param$font)
  if (tick.lat.bool$right[2])
    axis(4, at = tick.lat$label.loc, labels = tick.lat$label, tick = FALSE,
         las = 1, cex.axis = tick.param$scale, family = tick.param$font)
}

#------------------------------------------------------------------------------
### Grid lines
if (input$grid) {
  abline(v = tick.lon$maj, col = grid.param$col, lwd = grid.param$lwd,
         lty = as.numeric(grid.param$lty))
  abline(h = tick.lat$maj, col = grid.param$col, lwd = grid.param$lwd,
         lty = as.numeric(grid.param$lty))
}

#------------------------------------------------------------------------------
### Scale bar
if (input$bar) {
  lines(c(scale.bar$x1, scale.bar$x2), c(scale.bar$y, scale.bar$y),
        lwd = scale.bar$lwd)
  text(mean(c(scale.bar$x1, scale.bar$x2)),
       scale.bar$y-0.04*abs(lat.range[2]-lat.range[1]),
       paste(scale.bar$len, scale.bar$units.str))
}

#------------------------------------------------------------------------------
### Labels
# Title
if (!is.null(title.info$lab)) {
  title(main = title.info$lab, line = 3, family = title.info$fam,
        cex.main = title.info$cex)
}

# Longitude axis
if (!is.null(axes.info$lab.lon)) {
  title(xlab = axes.info$lab.lon, family = axes.info$fam,
        cex.lab = axes.info$cex)
}
# Latitude axis
if (!is.null(axes.info$lab.lat)) {
  title(ylab = axes.info$lab.lat, family = axes.info$fam,
        cex.lab = axes.info$cex, line = 4)
}

#------------------------------------------------------------------------------
### Transect lines
# Not in drawData since this isn't DAS data
if (input$planned_transects_plot) {
  if (anyNA(planned_transects_class2())) {
    # No class2
    for (i in pltrans.list) {
      for (k in i) {
        graphics::lines(
          x = k[[1]], y = k[[2]], col = k[[3]], lty = k[[4]], lwd = pltrans.lwd
        )
      }
    }

  } else {
    # Yes class 2
    for (i in pltrans.list) {
      for (j in i) {
        for (k in j) {
          graphics::lines(
            x = k[[1]], y = k[[2]], col = k[[3]], lty = k[[4]], lwd = pltrans.lwd
          )
        }
      }
    }
  }
}



##############################################################################
##############################################################################
###############################################################################
# drawData for CruzPlot
#   Plots selected sightings, legends, and effort lines from DAS file
#   Plots non-DAS data (lines or points)


### Plot non-DAS data
if (isTruthy(data.ndas)) {
  # Plot lines
  data.ndas.l <- data.ndas[[1]]
  if (length(data.ndas.l) > 0) {
    for(i in seq_along(data.ndas.l)) {
      data.ndas.l.curr <- data.ndas.l[[i]]
      lines(x = data.ndas.l.curr$x, y = data.ndas.l.curr$y,
            lty = data.ndas.l.curr$type, col = data.ndas.l.curr$col,
            lwd = data.ndas.l.curr$lwd)
    }
  }

  # Plot points
  data.ndas.p <- data.ndas[[2]]
  if (length(data.ndas.p) > 0) {
    for(j in seq_along(data.ndas.p)) {
      data.ndas.p.curr <- data.ndas.p[[j]]
      points(x = data.ndas.p.curr$x, y = data.ndas.p.curr$y,
             pch = data.ndas.p.curr$type, col = data.ndas.p.curr$col,
             cex = data.ndas.p.curr$cex, lwd = data.ndas.p.curr$lwd)
    }
  }
}


### Plot DAS data
if (isTruthy(cruz.list$das.data)) {
  ## Plot effort segments
  if (input$das_effort != "1")
    segments(x0 = das.eff.lines$st_lon, x1 = das.eff.lines$end_lon,
             y0 = das.eff.lines$st_lat, y1 = das.eff.lines$end_lat,
             col = eff.col, lwd = eff.lwd)


  ## Plot sighting points and legend
  if (input$das_sightings) {
    points(das.sight.pt$Lon, das.sight.pt$Lat,
           pch = das.sight.pt$pch, col = das.sight.pt$col,
           cex = das.sight.pt$cex, lwd = das.sight.pt$lwd)

    if (input$das_legend) {
      op <- par(family = das.sight.legend$font.fam)
      legend(x = das.sight.legend$leg.x,
             y = das.sight.legend$leg.y,
             legend = das.sight.legend$leg.lab,
             title = das.sight.legend$leg.title,
             pch = das.sight.legend$leg.pch,
             col = das.sight.legend$leg.col,
             pt.cex = das.sight.legend$leg.cex,
             pt.lwd = das.sight.legend$leg.lwd,
             bty = das.sight.legend$leg.bty,
             box.col = das.sight.legend$leg.box.col,
             box.lwd = das.sight.legend$leg.box.lwd,
             cex = das.sight.legend$leg.box.cex,
             bg = "white")
      par(op)
    }
  }


  ## Plot effort legend
  if (input$das_effort != 1) {
    if (isTruthy(das.eff.legend)) {
      op <- par(family = das.eff.legend$font.fam)
      legend(x = das.eff.legend$eff.leg.x,
             y = das.eff.legend$eff.leg.y,
             title = das.eff.legend$eff.leg.title,
             legend = das.eff.legend$eff.leg.lab,
             lwd = das.eff.legend$eff.leg.lwd,
             col = das.eff.legend$eff.leg.col,
             bty = das.eff.legend$eff.leg.bty,
             box.col = das.eff.legend$eff.leg.box.col,
             box.lwd = das.eff.legend$eff.leg.box.lwd,
             cex = das.eff.legend$eff.leg.box.cex,
             bg = "white")
      par(op)
    }
  }
}

graphics::box() # Added in case legend takes out some of the map border


##############################################################################
##############################################################################
###############################################################################
# drawInteractive for CruzPlot: Plot interactive points

### Prep
lon.range <- cruz.map.range$lon.range
lat.range <- cruz.map.range$lat.range

lon.diff <- abs(lon.range[2]-lon.range[1])
lat.diff <- abs(lat.range[2]-lat.range[1])

lon.mult <- 0.02
lat.mult1 <- 0.05
lat.mult2 <- 0.1


validate(
  need((input$das_effort_interactive == 1) || (input$das_sight_interactive == 1),
       message = "Cannot have both sighting and effort interactive plots selected")
)
if ((input$das_effort_interactive != 1) || (input$das_sight_interactive != 1)) {
  validate(
    need(cruz.list$das.data,
         "Please load a DAS data file before selecting an interactive plot")
  )
}


### Sighting labels
if (isTruthy(sight$click)) {
  for(i in seq_along(sight$click)) {
    text(x = sight$click[[i]][1], y = sight$click[[i]][2], labels = sight$lab[i], pos = 1)
  }
}

if (sight$miss)
  text(x = (lon.diff*lon.mult) + lon.range[1], y = (lat.diff*lat.mult1) + lat.range[1],
       labels = "Click was too far from a sighting", pos = 4)

if (isTruthy(sight$hover)) {
  if (sight$hover.miss) {
    text(x = (lon.diff*lon.mult) + lon.range[1], y = (lat.diff*lat.mult2) + lat.range[1],
         labels = "Cursor is too far from a sighting", pos = 4)
  } else {
    text(x = sight$hover[1], y = sight$hover[2], labels = sight$hover.lab, pos = 1)
  }
}


### Effort labels
if (isTruthy(effort$click)) {
  for(i in seq_along(effort$click)) {
    text(x = effort$click[[i]][1], y = effort$click[[i]][2], labels = effort$lab[i], pos = 1)
  }
}

if (effort$miss)
  text(x = (lon.diff*lon.mult) + lon.range[1], y = (lat.diff*lat.mult1) + lat.range[1],
       labels = "Click was too far from an effort start or end point", pos = 4)

if (isTruthy(effort$hover)) {
  if (effort$hover.miss) {
    text(x = (lon.diff*lon.mult) + lon.range[1], y = (lat.diff*lat.mult2) + lat.range[1],
         labels = "Cursor is too far from an effort start or end point", pos = 4)
  } else {
    text(x = effort$hover[1], y = effort$hover[2], labels = effort$hover.lab, pos = 1)
  }
}
