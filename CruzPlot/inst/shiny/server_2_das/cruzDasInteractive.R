# Initialize interactive reactive values, and reset them when appropriate

###############################################################################
sight <- reactiveValues(
  click = NULL,
  hover = NULL,
  hover.lab = NULL,
  lab = NULL,
  miss = FALSE,
  hover.miss = FALSE
)

effort <- reactiveValues(
  click = NULL,
  hover = NULL,
  lab = NULL,
  hover.lab = NULL,
  miss = FALSE,
  hover.miss = FALSE
)


###############################################################################
### When changed to a new page or tab: 1) Turn plot to not-interactive,
###   2) reset hover info, and 3) remove miss labels
observe({
  input$tabs
  input$tabset2

  updateRadioButtons(session, "das_sight_interactive", selected = 1)
  updateRadioButtons(session, "das_effort_interactive", selected = 1)

  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE

  effort$hover <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})


###############################################################################
# Reset as needed

### If DAS file or map range changes, reset interactive everything
observe({
  cruz.list$das.data
  cruz.map.range$lon.range
  cruz.map.range$lat.range
  cruz.map.range$world2

  # Reset all things
  sight$click <- NULL
  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE

  effort$click <- NULL
  effort$hover <- NULL
  effort$lab <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})

### If interactive selection changes, remove hover and miss
observeEvent(input$das_sight_interactive, {
  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE
})
observeEvent(input$das_effort_interactive, {
  effort$hover <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})

### If sightings selections change, reset interactive sighting things
observe({
  # Sightings to plot and filters
  input$das_sighting_type
  input$das_sighting_code_1_all
  input$das_sighting_code_2_all
  input$das_sighting_events
  input$das_sighting_code_1
  input$das_sighting_code_2

  input$das_sight_effort
  input$das_sight_cp
  input$das_sight_snf
  input$das_sight_minBft
  input$das_sight_maxBft
  input$das_sight_dateRange
  input$das_sight_cruise
  input$das_sight_trunc

  # Reset sighting things
  sight$click <- NULL
  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE
})


### If effort selections change, reset interactive effort things
observe({
  # Effort to plot and filters
  input$das_effort
  input$das_effort_cp
  input$das_effort_snf

  input$das_effort_filter_same
  input$das_effort_minBft
  input$das_effort_maxBft
  input$das_effort_dateRange
  input$das_effort_cruise

  # Reset effort things
  effort$click <- NULL
  effort$hover <- NULL
  effort$lab <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})



###############################################################################
###############################################################################
# cruzDasInteractiveSight for CruzPlot

### Interactive sighting click
observeEvent(input$sight_click, {
  click.curr <- c(input$sight_click$x, input$sight_click$y)
  das.sight <- cruzDasSightFilter()$das.sight

  param.unit <- cruzMapParam()$param.unit
  param.unit.diff <- c(param.unit[2]-param.unit[1], param.unit[4]-param.unit[3])
  # ^ Works because for world2 map, x range is 0, 360
  param.inch <- cruzMapParam()$param.inch
  x.ratio <- param.inch[1]/param.unit.diff[1]
  y.ratio <- param.inch[2]/param.unit.diff[2]

  # Determine closest point and return information to print
  sight.type <- cruzDasSightFilter()$sight.type
  close.info <- if (sight.type == 1) {
    # type = 1 means mammal sighting for function cruzClosestPt
    cruzClosestPt(click.curr, type = 1, das.sight$Lat, das.sight$Lon,
                  das.sight$DateTime, das.sight$SightNo)
  } else {
    # type = 2 means non-mammal sighting for function cruzClosestPt
    cruzClosestPt(click.curr, type = 2, das.sight$Lat, das.sight$Lon,
                  das.sight$DateTime)
  }

  dist.inch <- sqrt(
    (as.numeric(close.info[1])*x.ratio)^2 + (as.numeric(close.info[2])*y.ratio)^2
  )
  if (dist.inch <= 0.2) {
    isolate({
      sight$click <- c(sight$click, list(click.curr))
      sight$lab <- c(sight$lab, close.info[3])
    })
    sight$miss <- FALSE
  } else{
    sight$miss <- TRUE
  }
})


### Hover to display sighitng information
observeEvent(input$sight_hover, {
  sight$hover <- c(input$sight_hover$x, input$sight_hover$y)
  das.sight <- cruzDasSightFilter()$das.sight

  param.unit <- cruzMapParam()$param.unit
  param.unit.diff <- c(param.unit[2]-param.unit[1], param.unit[4]-param.unit[3])
  # ^ Works because for world2 map, x range is 0, 360
  param.inch <- cruzMapParam()$param.inch
  x.ratio <- param.inch[1]/param.unit.diff[1]
  y.ratio <- param.inch[2]/param.unit.diff[2]

  # Determine closest point and return information to print
  sight.type <- cruzDasSightFilter()$sight.type
  close.info <- if (sight.type == 1) {
    # type = 1 means mammal sighting for function cruzClosestPt
    cruzClosestPt(sight$hover, type = 1, das.sight$Lat, das.sight$Lon,
                  das.sight$DateTime, das.sight$SightNo)
  } else {
    # type = 2 means non-mammal sighting for function cruzClosestPt
    cruzClosestPt(sight$hover, type = 2, das.sight$Lat, das.sight$Lon,
                  das.sight$DateTime)
  }

  dist.inch <- sqrt(
    (as.numeric(close.info[1])*x.ratio)^2 + (as.numeric(close.info[2])*y.ratio)^2
  )
  if (dist.inch <= 0.3) {
    isolate(sight$hover.lab <- close.info[3])
    sight$hover.miss <- FALSE
  } else {
    sight$hover.miss <- TRUE
  }
})


### Remove last point
observeEvent(input$das_sight_interactive_reset_last, {
  sight$click <- if (length(sight$click) == 1) NULL else head(sight$click, -1)
  sight$lab <- if (length(sight$lab) == 1) NULL else head(sight$lab, -1)
  sight$miss <- FALSE

  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$hover.miss <- FALSE
})


### Remove all points
observeEvent(input$das_sight_interactive_reset_all, {
  sight$click <- NULL
  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE
})



###############################################################################
###############################################################################
# cruzDasInteractiveEffort for CruzPlot

### Click to add labels to map
observeEvent(input$effort_click, {
  click.curr <- c(input$effort_click$x, input$effort_click$y)
  das.effort <- cruzDasEffortFilter()
  # browser()

  param.unit <- cruzMapParam()$param.unit
  param.unit.diff <- c(param.unit[2]-param.unit[1], param.unit[4]-param.unit[3])
  # ^ Works because for world2 map, x range is 0, 360
  param.inch <- cruzMapParam()$param.inch
  x.ratio <- param.inch[1]/param.unit.diff[1]
  y.ratio <- param.inch[2]/param.unit.diff[2]

  # Determine closest point and if applicable get label information
  close.info <- cruzClosestPt(
    click.curr, 3, das.effort$st_lat, das.effort$st_lon, das.effort$DateTime,
    das.lat2 = das.effort$end_lat, das.lon2 = das.effort$end_lon
  )

  dist.inch <- sqrt(
    (as.numeric(close.info[1])*x.ratio)^2 + (as.numeric(close.info[2])*y.ratio)^2
  )
  if (dist.inch <= 0.2) {
    isolate({
      effort$click <- c(effort$click, list(click.curr))
      effort$lab <- c(effort$lab, close.info[3])
    })
    effort$miss <- FALSE
  } else {
    effort$miss <- TRUE
  }
})


### Hover to see R and E lat/lon coordinates
observeEvent(input$effort_hover, {
  effort$hover <- c(input$effort_hover$x, input$effort_hover$y)
  das.effort <- cruzDasEffortFilter()

  param.unit <- cruzMapParam()$param.unit
  param.unit.diff <- c(param.unit[2]-param.unit[1], param.unit[4]-param.unit[3])
  # ^ Works because for world2 map, x range is 0, 360
  param.inch <- cruzMapParam()$param.inch
  x.ratio <- param.inch[1]/param.unit.diff[1]
  y.ratio <- param.inch[2]/param.unit.diff[2]

  # Determine closest point and if applicable get label information
  close.info <- cruzClosestPt(
    effort$hover, 3, das.effort$st_lat, das.effort$st_lon, das.effort$DateTime,
    das.lat2 = das.effort$end_lat, das.lon2 = das.effort$end_lon
  )

  dist.inch <- sqrt(
    (as.numeric(close.info[1])*x.ratio)^2 + (as.numeric(close.info[2])*y.ratio)^2
  )
  if (dist.inch <= 0.3) {
    isolate(effort$hover.lab <- close.info[3])
    effort$hover.miss <- FALSE
  } else {
    effort$hover.miss <- TRUE
  }
})


### Remove last point
observeEvent(input$das_effort_interactive_reset_last, {
  effort$click <- if (length(effort$click) == 1) NULL else head(effort$click, -1)
  effort$lab <- if (length(effort$lab) == 1) NULL else head(effort$lab, -1)
  effort$miss <- FALSE

  effort$hover <- NULL
  effort$hover.lab <- NULL
  effort$hover.miss <- FALSE
})


### Remove all points
observeEvent(input$das_effort_interactive_reset_all, {
  effort$click <- NULL
  effort$hover <- NULL
  effort$lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})
