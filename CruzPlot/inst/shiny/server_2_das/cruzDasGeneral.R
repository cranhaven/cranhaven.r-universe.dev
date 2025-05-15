# cruzDasGeneral for CruzPlot
#   read and process DAS file
#   update: symbol type and color for when 'Input symbol properties as text' is clicked


###############################################################################
# Output flag indicating if DAS data has been loaded
output$das_loaded_flag <- reactive(isTruthy(cruz.list$das.data))
outputOptions(output, "das_loaded_flag", suspendWhenHidden = FALSE)

# Output flag indicating if SpCodes has been loaded
output$das_spcodes_loaded_flag <- reactive(isTruthy(cruz.list$sp.codes))
outputOptions(output, "das_spcodes_loaded_flag", suspendWhenHidden = FALSE)


###############################################################################
### Read and process DAS file(s)
das_file_load <- eventReactive(input$das_file, {
  # Clear reactive vals, and reset plot sightings and effort selections
  cruz.list$das.data <- NULL
  cruz.list$das.data.name <- NULL
  updateCheckboxInput(session, "das_sightings", value = FALSE)
  updateRadioButtons(session, "das_effort", selected = 1)

  # Get and check additional parameters
  skip <- input$das_file_skip
  days.gap <- input$das_file_days_gap
  reset.event  <- input$das_file_reset_event == 1
  reset.effort <- input$das_file_reset_effort == 1
  # reset.day    <- input$das_file_reset_day == 1

  v.tmp <- "skip must be a valid, whole number greater than or equal to 0"
  validate(need(!is.na(skip), v.tmp))
  validate(need(isTRUE(all.equal(skip %% 1, 0)), v.tmp))
  validate(need(skip >= 0, v.tmp))
  rm(v.tmp)

  v.tmp <- "days.gap must be a valid, whole number greater than or equal to 0"
  validate(need(!is.na(days.gap), v.tmp))
  validate(need(isTRUE(all.equal(days.gap %% 1, 0)), v.tmp))
  validate(need(days.gap >= 0, v.tmp))
  rm(v.tmp)

  # Process DAS file
  withProgress(message = "Processing DAS file", value = 0.6, {
    das.proc <- try(suppressWarnings(
      swfscDAS::das_process(
        input$das_file$datapath, skip = skip, days.gap = days.gap,
        reset.event = reset.event, reset.effort = reset.effort,
        reset.days = TRUE
      )
    ), silent = TRUE)
  })

  validate(
    need(isTruthy(das.proc),
         "Error: unable to read and process the provided DAS file(s)")
  )

  # Correct filename
  filename.key <- data.frame(
    tmp = basename(input$das_file$datapath),
    actual = input$das_file$name,
    stringsAsFactors = FALSE
  )

  das.proc <- das.proc %>%
    left_join(filename.key, by = c("file_das" = "tmp")) %>%
    mutate(file_das = .data$actual) %>%
    select(-.data$actual)

  # Save reactive values
  cruz.list$das.data <- das.proc
  # cruz.list$das.data.name <- input$das_file$name

  ""
}, ignoreInit = TRUE)


### Conditional flag for UI code for truthy cruz.list$das.data
output$cruzDasFile_Conditional <- reactive({
  isTruthy(cruz.list$das.data)
})
outputOptions(output, "cruzDasFile_Conditional", suspendWhenHidden = FALSE)


###############################################################################
# Help pages

### Help page for das file load
observeEvent(input$das_file_help, {
  showModal(modalDialog(
    tags$h5(tags$strong("Lines to skip:"),
            "The number of lines that are ignored before starting to read data in the provided file."),
    tags$h5(tags$strong("days.gap argument:"),
            "This argument helps the user keep information from spilling from",
            "one cruise into the next in concatenated DAS files.",
            "For instance, if days.gap is 20, then all of the DAS state/condition information",
            "(Cruise, Mode, Beaufort, Visibility, etc.)",
            "are reset when there are 20 or more days between records in the file."),
    tags$h5(tags$strong("reset.event argument:"),
            "This argument specifies what happens if a state/condition is not entered in the DAS data.",
            "For example, say the Beaufort value is left blank in a V event.",
            "If reset.event is TRUE, then the Beaufort value is set to NA",
            "in the processed data until the next V event.",
            "If reset.event is FALSE, then the previous Beaufort value will be carried through,",
            "meaning the NA value will be ignored.",
            "This argument should be FALSE only if state/condition information (e.g. Beaufort value)",
            "was purposefully not entered if it did not change."),
    tags$h5(tags$strong("reset.effort argument:"),
            "When using WinCruz, an R event or BR event series signifying the start of a new continuous effort section",
            "is generally immediately followed by a PVNW event sequence in which all state/condition values are updated.",
            "If reset.effort is TRUE, then the state/condition arguments are all reset to NA at the R event,",
            "or at the B event is it immediately precedes said R event.",
            "This argument should be FALSE only if 1) R events are not always followed by a PVNW event sequence or",
            "2) state/condition information was purposefully not entered if it did not change (similar to reset.event)."),
    tags$h5(tags$strong("reset.day argument:"),
            "This argument is always set to TRUE - the user cannot specify it when using CruzPlot.",
            "When TRUE, all of the state and condition information is reset at the beginning of each new day,",
            "rather than being carried over from the previous day."),
    easyClose = FALSE
  ))
})


### Help page for sighting events
observeEvent(input$das_sighting_events_help, {
  showModal(modalDialog(
    tags$h5(tags$strong("S event:"), "Cetacean sighting - standard"),
    tags$h5(tags$strong("G event:"), "Cetacean subgroup sighting"),
    tags$h5(tags$strong("K event:"), "Cetacean sighting - tracker. Historically only used during cruise number 1611 in 1998"),
    tags$h5(tags$strong("M event:"), "Cetacean sighting - matched. Historically only used during cruise number 1608 in 1997"),
    tags$h5(tags$strong("p event:"), "Pinniped sighting. Used beginning in 2018"),
    tags$h5(tags$strong("s event:"), "Standard cetacean resight. Corresponds to a previous S event"),
    tags$h5(tags$strong("g event:"), "Cetacean subgroup resight. Corresponds to a previous G event"),
    tags$h5(tags$strong("k event:"), "Tracker cetacean resight. Corresponds to a previous K event"),
    tags$br(), tags$br(),
    tags$h5("When plotting resights (s, k, or g events): 1) you can only plot a single sighting type (e.g. S and s event)",
            "and a single species at one time,",
            "and 2) The 'Symbol color...' entry corresponds the the order of the selected events.",
            "In addition, for resight plotting please ensure that the loaded DAS file is not a concatenated file,
            i.e. each sighting number in the file corresponds to a single sighting/resighting group."),
    easyClose = FALSE
  ))
})




###############################################################################
# Code for keeping current inputs the same when switching
#    from or to text symbol properties input
observeEvent(input$das_symbol_mult, {
  if (input$das_symbol_mult) {
    ### Convert codes to text
    # Covert numerics to symbols
    curr.pch <- as.numeric(input$das_symbol_type)
    if (length(curr.pch) == 0) curr.pch <- "1"

    # Covert color codes to color names
    curr.col <- input$das_symbol_color
    if (is.null(curr.col)) {
      curr.col <- "Black"

    } else {
      # This keeps numbers in order
      if (input$color_style == 1) {
        curr.col.idx <- vapply(curr.col, function(i) which(symbol.col.code %in% i), 1)
        curr.col <- symbol.col[curr.col.idx]

      } else if (input$color_style == 2) {
        curr.col.idx <- vapply(curr.col, function(i) which(symbol.col.code.gray %in% i), 1)
        curr.col <- symbol.col.gray[curr.col.idx]
      }
    }

    updateTextInput(session, "das_symbol_type_mult", value = paste(curr.pch, collapse = ", "))
    updateTextInput(session, "das_symbol_color_mult", value = paste(curr.col, collapse = ", "))


  } else {
    ### Convert from text to codes
    # Convert symbol codes to symbols
    curr.pch <- suppressWarnings(
      as.numeric(unlist(strsplit(input$das_symbol_type_mult, ", ")))
    )
    if (length(curr.pch) == 0) {
      curr.pch <- 1
    } else {
      if (!all(curr.pch %in% unname(cruz.symbol.type))) curr.pch <- 1
    }
    updateSelectInput(session, "das_symbol_type", selected = curr.pch)


    # Covert color names to color codes
    curr.col <- unlist(strsplit(input$das_symbol_color_mult, ", "))

    if (is.null(curr.col)) {
      curr.col <- "black"

    } else {
      if (!all(curr.col %in% symbol.col)) {
        curr.col <- "black"
      } else {
        # This keeps numbers in order
        if (input$color_style == 1) {
          curr.col.idx <- vapply(curr.col, function(i) which(symbol.col %in% i), 1)
          curr.col <- symbol.col.code[curr.col.idx]

        } else if (input$color_style == 2) {
          curr.col.idx <- vapply(curr.col, function(i) which(symbol.col.gray %in% i), 1)
          curr.col <- symbol.col.code.gray[curr.col.idx]
        }
      }
    }
    updateSelectInput(session, "das_symbol_color", selected = curr.col)
  }
}, ignoreInit = TRUE)


###############################################################################
# Flags for inputs for plotting detailed effort not by Beaufort

output$das_effort_det_s_flag <- reactive({
  flag <- FALSE
  if (input$das_effort == 3) {
    if (!input$das_effort_det_byBft) {
      if ("S" %in% input$das_effort_snf) flag <- TRUE
    }
  }

  flag
})
outputOptions(output, "das_effort_det_s_flag", suspendWhenHidden = FALSE)

output$das_effort_det_n_flag <- reactive({
  flag <- FALSE
  if (input$das_effort == 3) {
    if (!input$das_effort_det_byBft) {
      if ("N" %in% input$das_effort_snf) flag <- TRUE
    }
  }

  flag
})
outputOptions(output, "das_effort_det_n_flag", suspendWhenHidden = FALSE)

output$das_effort_det_f_flag <- reactive({
  flag <- FALSE
  if (input$das_effort == 3) {
    if (!input$das_effort_det_byBft) {
      if ("F" %in% input$das_effort_snf) flag <- TRUE
    }
  }

  flag
})
outputOptions(output, "das_effort_det_f_flag", suspendWhenHidden = FALSE)

###############################################################################
