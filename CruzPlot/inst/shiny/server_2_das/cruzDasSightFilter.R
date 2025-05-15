# cruzDasSightFilter for CruzPlot - step 3 of processing species data
#   cruiseDasSightFilter() pulls individual filters together

#   cruzDasSightFilterEffort() returns a logical indicating which rows satisfy the effort filter
#   cruzDasSightFilterBeaufort() returns a logical indicating which rows satisfy the beaufort filter
#   cruzDasSightFilterDate() returns a logical indicating which rows satisfy the date filter
#   cruzDasSightFilterCruise() returns a logical indicating which rows are from the given cruise number(s)
#   cruzDasSightFilterTrunc() returns a logical indicating which rows are within the given truncation distance


###############################################################################
### Top-level function for filtering
cruzDasSightFilter <- reactive({
  data.list <- cruzDasSightRange()

  das.sight    <- data.list$das.sight
  sight.type   <- data.list$sight.type
  sp.codes     <- data.list$sp.codes
  sp.selection <- data.list$sp.selection

  ### Collect logical vectors
  keep1 <- cruzDasSightFilterEffort()
  keep2 <- if (input$das_sight_effort == 2) cruzDasSightFilterMode() else TRUE
  keep3 <- if (input$das_sight_effort == 2) cruzDasSightFilterEfftype() else TRUE
  keep4 <- cruzDasSightFilterBeaufort()
  keep5 <- cruzDasSightFilterDate()
  keep6 <- cruzDasSightFilterCruise()
  keep7 <- cruzDasSightFilterTrunc()

  keep.all <- keep1 & keep2 & keep3 & keep4 & keep5 & keep6 & keep7
  keep.all.na <- which(is.na(keep.all)) #works b/c (NA & F) output is (F)

  ### Verbosely change NA filter values to FALSE
  if (length(keep.all.na) > 0) {
    table.out <- das.sight %>%
      slice(keep.all.na) %>%
      mutate(DateTime = as.character(DateTime),
             Cruise = as.character(Cruise)) %>%
      select(Event, DateTime, OnEffort, Cruise, Mode, EffType, Bft,
             SightNo, SpCode, PerpDistKm,
             # File = file_das, #Can take up too much space
             `Line number` = line_num) %>%
      distinct()
    if (input$das_sight_effort == 2)
      table.out <- table.out %>% select(-Mode, -EffType)

    txt.out <- ifelse(nrow(table.out) == 1, "sighting", "sightings")
    txt.out2 <- ifelse(nrow(table.out) == 1, "This sighting", "These sightings")

    showModal(modalDialog(
      title = "CruzPlot notice",
      tags$h5("The following", txt.out, "had at least one NA filter value.",
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
  das.sight.filt <- das.sight[keep.all, ]

  ### Final checks, calculate sp.count, and return
  validate(
    need(sum(is.na(das.sight.filt$Event)) == 0,
         "Error in CruzPlot sighting filtering - please report this as an issue")
  )
  validate(need(nrow(das.sight) > 0, "No sightings match the given filters"))

  # Check that at least one sighting is still within map range, and get sp.count
  if (sight.type %in% c(1, 2)) {
    if (!sp.selection) sp.codes <- base::intersect(sp.codes, das.sight.filt$SpCode)

    # Calculate count for each species
    sp.count <- vapply(sp.codes, function(i, j) {
      sum(j$SpCode == i)
    }, 1, j = das.sight.filt, USE.NAMES = FALSE)

  } else {
    sp.count <- nrow(das.sight.filt)
  }

  list(
    das.sight = das.sight.filt, sight.type = sight.type,
    sp.codes = sp.codes, sp.selection = sp.selection, sp.count = sp.count
  )
})


###############################################################################
### Helper functions that filter sightings data by single property

.func_sight_filt_validate <- function(x, x.txt) {
  validate(
    need(any(x), paste("None of the specified sightings within the map range",
                       "match the given", x.txt, "filter"))
  )
  x
}

#------------------------------------------------------------------------------
# On/off effort
cruzDasSightFilterEffort <- reactive({
  das.sight <- cruzDasSightRange()$das.sight
  effort.val <- switch(as.numeric(input$das_sight_effort), c(0, 1), 1, 0)
  keep <- as.numeric(das.sight$OnEffort) %in% effort.val
  .func_sight_filt_validate(keep, "on/off effort")
})

# Mode: C/P
cruzDasSightFilterMode <- reactive({
  das.sight <- cruzDasSightRange()$das.sight
  keep <- das.sight$Mode %in% input$das_sight_cp
  .func_sight_filt_validate(keep, "mode (closing/passing)")
})

# Effort type: S/N/F
cruzDasSightFilterEfftype <- reactive({
  das.sight <- cruzDasSightRange()$das.sight
  keep <- das.sight$EffType %in% input$das_sight_snf
  .func_sight_filt_validate(keep, "effort type (standard/non-standard/fine)")
})

#------------------------------------------------------------------------------
# Beaufort
cruzDasSightFilterBeaufort <- reactive({
  das.sight <- cruzDasSightRange()$das.sight
  bft.min <- as.numeric(input$das_sight_minBft)
  bft.max <- as.numeric(input$das_sight_maxBft)

  validate(
    need(input$das_sight_minBft <= input$das_sight_maxBft,
         "Sightings filter: minimum Beaufort must be less than or equal to maximum Beaufort")
  )

  keep <- if (identical(c(bft.min, bft.max), c(0, 9))) {
    TRUE
  } else {
    between(das.sight$Bft, bft.min, bft.max)
  }
  .func_sight_filt_validate(keep, "Beaufort")
})

#------------------------------------------------------------------------------
# Dates
cruzDasSightFilterDate <- reactive({
  das.sight <- cruzDasSightRange()$das.sight
  date.vals <- input$das_sight_dateRange

  validate(
    need(input$das_sight_dateRange[1] <= input$das_sight_dateRange[2],
         "Sightings filter: minimum date must be less than or equal to maximum date")
  )

  keep <- between(
    as.Date(das.sight$DateTime), date.vals[1], date.vals[2]
  )
  .func_sight_filt_validate(keep, "date")
})

#------------------------------------------------------------------------------
# Cruise numbers
cruzDasSightFilterCruise <- reactive({
  das.sight <- cruzDasSightRange()$das.sight

  if (is.null(input$das_sight_cruise)) {
    # Return here to keep records that have 'NA' value
    TRUE

  } else {
    cruise.vals <- as.numeric(input$das_sight_cruise)
    keep <- das.sight$Cruise %in% cruise.vals
    .func_sight_filt_validate(keep, "cruise number")
  }
})

#------------------------------------------------------------------------------
# Perpendicular distance truncation
cruzDasSightFilterTrunc <- reactive({
  das.sight <- cruzDasSightRange()$das.sight

  pdist.val <- ifelse(
    input$das_sight_trunc_units == 1,
    input$das_sight_trunc, input$das_sight_trunc * 1.852
  )

  if (is.na(pdist.val)) {
    TRUE

  } else {
    keep <- das.sight$PerpDistKm <= pdist.val
    validate(
      need(any(keep),
           "There are no selected sightings within the given truncation distance")
    )
    .func_sight_filt_validate(keep, "truncation (perpendicular distance)")
  }
})

###############################################################################
