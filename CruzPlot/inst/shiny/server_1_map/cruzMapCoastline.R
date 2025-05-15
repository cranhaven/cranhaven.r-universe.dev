# Processing for Coastline section of Range tab of Create and Save Map tab
#   Load coastline data and save it to reactiveValue
#   Updates map limits to coastline file extent


observeEvent(input$coast_file, {
  # TODO: validate checks fo file type
 coastline <- read.csv(input$coast_file$datapath)
 coastline <- rbind(c(NA, NA, NA), coastline, c(NA, NA, NA))

 cruz.list$coastline <- coastline
}, ignoreInit = TRUE)

observeEvent(cruz.list$coastline, {
  if (isTruthy(cruz.list$coastline)) {
    map.coastline <- cruz.list$coastline
    x <- map.coastline$lon[!is.na(map.coastline$lon)]
    y <- map.coastline$lat[!is.na(map.coastline$lat)]
    validate(
      need(length(x) == length(y),
           "Coastline lon and lat columns have difference number of non-NA values"),
      need(all(x >= -180) & all(x < 0),
           "CruzPlot can currently only handle coastline data with a longitude range of -180 to 0 degrees")
    )

    # Update inputs
    updateNumericInput(session, "lon.left", value = min(x))
    updateNumericInput(session, "lon.right", value = max(x))
    updateNumericInput(session, "lat.bot", value = min(y))
    updateNumericInput(session, "lat.top", value = max(y))

    # Update reactiveValues - this doesn't handle world2 coastline now
    cruz.map.range$lon.range <- c(min(x), max(x))
    cruz.map.range$lat.range <- c(min(y), max(y))
    cruz.map.range$world2 <- FALSE
  }
})
