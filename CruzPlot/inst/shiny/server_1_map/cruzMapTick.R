# Processing for Tick tab of Create and Save Map tab
#   update: major tick interval, start of longitude tick labels, start of latitude tick labels
#   cruzMapTickLonBool() returns boolean list of whether bottom and top tick marks and tick labels are drawn, respectively
#   cruzMapTickLatBool() returns boolean list of whether left and right tick marks and tick labels are drawn, respectively
#   cruzMapTickLon() returns labels for longitude tick marks
#   cruzMapTickLat() returns labels for latitude tick marks
#   cruzMapTickParam() returns list of tick length, font, and scale


###############################################################################
#  Return list of longitude values of major tick marks/grid lines and minor tick marks
cruzMapIntervalLon <- reactive({
  lon.range <- cruz.map.range$lon.range
  tick.maj <- cruz.tick$tick.interval.major
  tick.min <- input$tick_interval_minor
  lon.start <- cruz.tick$label.lon.start

  if (cruz.map.range$world2) lon.start <- ifelse(lon.start < 0, lon.start + 360, lon.start)

  tick.lon <- list(label.loc = seq(lon.start, lon.range[2], by = tick.maj))
  temp.tick <- rev(seq(lon.start, lon.range[1], by = -tick.maj))
  tick.lon$maj <- sort(unique(c(tick.lon$label.loc, temp.tick)))
  tick.lon$min <- cruzTickMinor(deg.range = lon.range, maj.ticks = tick.lon$maj,
                                tick.maj.interval = tick.maj, n=tick.min)

  tick.lon
})

# Return list of latitude values of major tick marks/grid lines and minor tick marks
cruzMapIntervalLat <- reactive({
  lat.range <- cruz.map.range$lat.range
  tick.maj <- cruz.tick$tick.interval.major
  tick.min <- input$tick_interval_minor
  lat.start <- cruz.tick$label.lat.start

  tick.lat <- list(label.loc = seq(lat.start, lat.range[2], by = tick.maj))
  temp.tick <- rev(seq(lat.start, lat.range[1], by = -tick.maj))
  tick.lat$maj <- sort(unique(c(tick.lat$label.loc, temp.tick)))
  tick.lat$min <- cruzTickMinor(deg.range = lat.range, maj.ticks = tick.lat$maj,
                                tick.maj.interval = tick.maj, n=tick.min)

  tick.lat
})


###############################################################################
# Update reactiveValues cruz.tick at start (cruz.tick's = NULL) and
#    if inputs change and are different from cruz.tick

observe({
  req(input$tick_interval_major)

  in.tick.interval.major <- input$tick_interval_major
  isolate({
    if (cruz.tick$tick.interval.major != in.tick.interval.major)
      cruz.tick$tick.interval.major <- in.tick.interval.major
  })
})

observe({
  req(input$label_lon_start)

  in.label.lon.start <- as.numeric(input$label_lon_start)
  isolate({
    if (cruz.tick$label.lon.start != in.label.lon.start)
      cruz.tick$label.lon.start <- in.label.lon.start
  })
})

observe({
  req(input$label_lat_start)

  in.label.lat.start <- as.numeric(input$label_lat_start)
  isolate({
    if (cruz.tick$label.lat.start != in.label.lat.start)
      cruz.tick$label.lat.start <- in.label.lat.start
  })
})

###############################################################################
# Update inputs

# Tick major interval
observe({
  lon.range <- cruz.map.range$lon.range
  lat.range <- cruz.map.range$lat.range
  tick.val <- cruzTickUpdate(lon.range, lat.range)

  updateNumericInput(session, "tick.interval.major", value = tick.val)
  cruz.tick$tick.interval.major <- tick.val
}, priority = 2)

# Tick label longitude start
observe({
  b <- cruz.tick$tick.interval.major
  if (b != 0 && !is.na(b)) {
    lon.range <- cruz.map.range$lon.range
    lon.start <- cruzTickStart(lon.range, b)

    updateTextInput(session, "label.lon.start", value = paste(lon.start))
    cruz.tick$label.lon.start <- lon.start
  }
}, priority = 1)

# Tick label latitude start
observe({
  b <- cruz.tick$tick.interval.major
  if (b != 0 && !is.na(b)) {
    lat.range <- cruz.map.range$lat.range
    lat.start <- cruzTickStart(lat.range, b)

    updateTextInput(session, "label.lat.start", value = paste(lat.start))
    cruz.tick$label.lat.start <- lat.start
  }
}, priority = 1)

###############################################################################
# Reactive functions

# Plot longtiude tick marks
cruzMapTickLonBool <- reactive({
  bot <- c(input$tick_bot, input$tick_bot_lab)
  top <- c(input$tick_top, input$tick_top_lab)
  list(bot = bot, top = top)
})

# Plot latitude tick marks
cruzMapTickLatBool <- reactive({
  left <- c(input$tick_left, input$tick_left_lab)
  right <- c(input$tick_right, input$tick_right_lab)
  list(left = left, right = right)
})

# Plot longtiude tick labels
cruzMapTickLonLab <- reactive({
  tick.lab.loc <- cruzMapIntervalLon()$label.loc
  format <- input$tick_style

  tick.lab <- parse(text = sapply(tick.lab.loc, function(i) {
    i <- ifelse(i > 180, i - 360, i)
    i <- ifelse(i < -180, 360 - i, i)
    a <- ifelse(i < 0 & !(format %in% c(1, 3)), -1 * i, i)
    b <- ifelse(i < 0, "~W", "~E")
    b <- ifelse(a %in% c(0, 180), "", b)
    b <- ifelse((format == 2 || format == 4), b, "")
    l <- ifelse((format == 3 || format == 4), "*degree", "")
    paste(a, l, b, sep = "")
  }))

  tick.lab
})

# Plot latitude tick labels
cruzMapTickLatLab <- reactive({
  tick.lab.loc <- cruzMapIntervalLat()$label.loc
  format <- input$tick_style

  tick.lab <- parse(text = sapply(tick.lab.loc, function(i) {
    a <- ifelse(i < 0 & !(format %in% c(1, 3)), -1 * i, i)
    b <- ifelse(i < 0, "~S", "~N")
    b <- ifelse(a %in% c(0, 90), "", b)
    b <- ifelse((format == 2 || format == 4), b, "")
    l <- ifelse((format == 3 || format == 4), "*degree", "")
    paste(a, l, b, sep = "")
  }))

  tick.lab
})

cruzMapTickParam <- reactive({
  tick.len <- input$tick_length
  lab.font <- font.family.vals[as.numeric(input$label_tick_font)]
  lab.scale <- input$label_tick_size
  list(len = tick.len, font = lab.font, scale = lab.scale)
})
