# File 2 of effort processing
#   cruzDasEffortRange() subsets effort data to map range
#   cruzDasEffortFilter() returns filtered effort data - with helper functions
#   cruzDasEffortParams() takes filtered data and returns plotting params


###############################################################################
###############################################################################
### Final effort function - gets filtered data from cruzDasEffortFilter()
cruzDasEffortRange <- reactive({
  das.eff.lines <- cruzDasEffortEvent()

  # Check for any effort lines with NA coordinates
  ll.na <- sum(is.na(das.eff.lines$st_lat) | is.na(das.eff.lines$st_lon) |
                 is.na(das.eff.lines$end_lat) | is.na(das.eff.lines$end_lon))
  validate(
    need(ll.na == 0,
         "Error processing effort line positions - please report this as an issue")
  )

  # Adjust longitudes if world2 map is being used
  if (cruz.map.range$world2) {
    das.eff.lines <- das.eff.lines %>%
      mutate(st_lon = ifelse(.data$st_lon < 0, .data$st_lon + 360, .data$st_lon),
             end_lon = ifelse(.data$end_lon < 0, .data$end_lon + 360, .data$end_lon))
  }

  # Remove any effort lines with both st and end points outside map range
  lon.range <- cruz.map.range$lon.range
  lat.range <- cruz.map.range$lat.range

  das.eff.lines.range <- das.eff.lines %>%
    filter(between(.data$st_lat, lat.range[1], lat.range[2]),
           between(.data$st_lon, lon.range[1], lon.range[2]),
           between(.data$end_lat, lat.range[1], lat.range[2]),
           between(.data$end_lon, lon.range[1], lon.range[2]))

  validate(
    need(nrow(das.eff.lines.range) > 0,
         "No effort lines are completely within the map boundaries")
  )

  # Return
  das.eff.lines.range
})


###############################################################################
###############################################################################
observeEvent(input$das_sightings, {
  if (!input$das_sightings) {
    updateCheckboxInput(session, "das_effort_filter_same", value = FALSE)
  }
})


###############################################################################
# Top-level function for filtering effort line data
cruzDasEffortFilter <- reactive({
  #Called in draw_setVals

  das.eff.lines <- cruzDasEffortRange()

  if (input$das_effort_filter_same) {
    validate(
      need(input$das_sightings,
           "Sightings must be plotted to use the sighting filters for effort lines")
    )
  }

  ### Collect logical vectors
  keep1 <- cruzDasEffortFilterMode()
  keep2 <- cruzDasEffortFilterEfftype()
  keep3 <- if (input$das_effort == 3) cruzDasEffortFilterBeaufort() else TRUE
  keep4 <- cruzDasEffortFilterDate()
  keep5 <- cruzDasEffortFilterCruise()

  keep.all <- keep1 & keep2 & keep3 & keep4 & keep5
  keep.all.na <- which(is.na(keep.all))

  ### Show modal if any filter values are NA
  # Can be this simple b/c (NA & F) output is (F)
  if (length(keep.all.na) > 0) {
    table.out <- das.eff.lines %>%
      slice(keep.all.na) %>%
      mutate(DateTime = as.character(DateTime),
             Cruise = as.character(Cruise)) %>%
      select(Event, DateTime, OnEffort, Cruise, Mode, EffType, Bft,
             # File = file_das, #Can take up too much space
             `Line number` = line_num)
    if (input$das_effort != 3) table.out <- table.out %>% select(Bft)

    txt.out <- ifelse(nrow(table.out) == 1, "line had", "lines have")
    txt.out2 <- ifelse(nrow(table.out) == 1, "This line", "These lines")

    showModal(modalDialog(
      title = "CruzPlot notice",
      tags$h5("The following effort", txt.out, "at least one NA filter value.",
              txt.out2, "will be removed (filtered) and thus not plotted",
              "or included in tabular output:"),
      tags$br(), tags$br(),
      renderTable(table.out),
      easyClose = TRUE,
      footer = "Click anywhere or press any button to close this notice",
      size = "l"
    ))
  }

  keep.all[is.na(keep.all)] <- FALSE
  das.eff.lines.filt <- das.eff.lines[keep.all, ]

  ### Final checks and return
  validate(
    need(sum(is.na(das.eff.lines.filt$Event)) == 0,
         "Error in CruzPlot effort filtering - please report this as an issue")
  )
  validate(need(nrow(das.eff.lines.filt) > 0, "No effort lines match the provided filters"))

  das.eff.lines.filt
})


###############################################################################
# Individual filters filter for indicies of data.effort to keep

#------------------------------------------------------------------------------
.func_eff_filt_validate <- function(x, x.txt) {
  if (anyNA(x)) warning(paste("some", x.txt, "filter values were NA"))

  validate(
    need(any(x), paste("No effort lines within the map range",
                       "match the given", x.txt, "filter"))
  )
  x
}


#------------------------------------------------------------------------------
### Closing/passing mode filter
cruzDasEffortFilterMode <- reactive ({
  das.eff.lines <- cruzDasEffortRange()

  keep <- das.eff.lines$Mode %in% input$das_effort_cp
  .func_eff_filt_validate(keep, "mode (closing/passing)")
})


#------------------------------------------------------------------------------
### S/N/F effort type filter
cruzDasEffortFilterEfftype <- reactive ({
  das.eff.lines <- cruzDasEffortRange()

  keep <- das.eff.lines$EffType %in% input$das_effort_snf
  .func_eff_filt_validate(keep, "effort type (standard/non-standard/fine)")
})


#------------------------------------------------------------------------------
### Beaufort filter
cruzDasEffortFilterBeaufortVal <- reactive({
  # Separate function to be used in Legend
  if (input$das_effort_filter_same) {
    eff.bft.min <- as.numeric(input$das_sight_minBft)
    eff.bft.max <- as.numeric(input$das_sight_maxBft)
  } else {
    eff.bft.min <- as.numeric(input$das_effort_minBft)
    eff.bft.max <- as.numeric(input$das_effort_maxBft)
  }

  validate(
    need(eff.bft.min <= eff.bft.max,
         "Effort filter: minimum Beaufort must be less than or equal to maximum Beaufort")
  )

  c(eff.bft.min, eff.bft.max)
})

cruzDasEffortFilterBeaufort <- reactive ({
  das.eff.lines <- cruzDasEffortRange()
  bft.vals <- cruzDasEffortFilterBeaufortVal()

  keep <- if (identical(bft.vals, c(0, 9))) {
    TRUE
  } else {
    between(das.eff.lines$Bft, bft.vals[1], bft.vals[2])
  }
  .func_eff_filt_validate(keep, "Beaufort")
})


#------------------------------------------------------------------------------
### Date Filter
cruzDasEffortFilterDate <- reactive({
  das.eff.lines <- cruzDasEffortRange()

  eff.date.vals <- if (input$das_effort_filter_same) {
    input$das_sight_dateRange
  } else {
    input$das_effort_dateRange
  }

  validate(
    need(eff.date.vals[1] <= eff.date.vals[2],
         "Effort filter: minimum date must be less than or equal to maximum date")
  )

  keep <- between(
    as.Date(das.eff.lines$DateTime), eff.date.vals[1], eff.date.vals[2]
  )
  .func_eff_filt_validate(keep, "date")
})


#------------------------------------------------------------------------------
### Cruise number filter
cruzDasEffortFilterCruise <- reactive ({
  das.eff.lines <- cruzDasEffortRange()

  if (input$das_effort_filter_same) {
    if (is.null(input$das_sight_cruiseNum)) {
      TRUE
    } else {
      eff.cruise.vals <- as.numeric(input$das_sight_cruise)
      keep <- das.eff.lines$Cruise %in% eff.cruise.vals
      .func_eff_filt_validate(keep, "cruise number") }

  } else {
    if (is.null(input$das_effort_cruise)) {
      TRUE
    } else {
      eff.cruise.vals <- as.numeric(input$das_effort_cruise)
      keep <- das.eff.lines$Cruise %in% eff.cruise.vals
      .func_eff_filt_validate(keep, "cruise number")}
  }
})


###############################################################################
###############################################################################
### Get effort plotting colors and line widths
cruzDasEffortParams <- reactive({
  if (input$das_effort == 2) {
    ## If simplified effort, simple results
    eff.col <- input$das_effort_simp_col
    eff.lwd <- input$das_effort_simp_lwd

  } else if (input$das_effort == 3) {
    ## If detailed effort, not as simple
    das.eff.lines <- cruzDasEffortFilter()

    # Use Beaufort or effort type values to generate colors
    if (input$das_effort_det_byBft) {
      bft.cols <- input$das_effort_det_bft_col
      bft.col.num <- cruzDasEffortFilterBeaufortVal()[2] + 1
      validate(
        need(length(bft.cols) >= bft.col.num,
             paste("Please choose at least", bft.col.num,
                   "colors, one for each possible Beaufort value between 0",
                   "and the specified maximum Beaufort"))
      )

      eff.col <- bft.cols[das.eff.lines$Bft + 1]
      eff.lwd <- input$das_effort_det_bft_lwd

    } else {
      eff.col <- case_when(
        das.eff.lines$EffType == "S" ~ input$das_effort_det_col_s,
        das.eff.lines$EffType == "N" ~ input$das_effort_det_col_n,
        das.eff.lines$EffType == "F" ~ input$das_effort_det_col_f
      )
      eff.lwd <- case_when(
        das.eff.lines$EffType == "S" ~ input$das_effort_det_lwd_s,
        das.eff.lines$EffType == "N" ~ input$das_effort_det_lwd_n,
        das.eff.lines$EffType == "F" ~ input$das_effort_det_lwd_f
      )
    }
  }

  list(eff.col = eff.col, eff.lwd = eff.lwd)
})

###############################################################################

