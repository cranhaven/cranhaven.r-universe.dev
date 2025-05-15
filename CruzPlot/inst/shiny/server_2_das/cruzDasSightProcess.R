# Step 1 of processing species data
#   cruzDasSightSpeciesMammals() returns mammal species codes selected by user
#   cruzDasSightSpeciesTurtles() returns turtle species codes selected by user
#   cruzDasSightProcess() - run das_sight only once
#   cruzDasSightPosition - handle ship vs sighting position, verbosely remove NA positions
#   cruzDasSightSpecies() returns list of data frames containing data for selected species sightings,
#	    sighting type, and species codes; also computes sighting location based on angle and distance;
#     adds sight.lat, sight.lon, angle, distance (nmi) to data.sight dataframe


###############################################################################
# Extract and process species codes
cruzDasSightSpeciesMammals <- reactive({
  sp.codes <- if (input$das_sighting_code_1_all == 1) {
    cruzSpeciesMammals()$Code
  } else if (input$das_sighting_code_1_all == 2) {
    gsub(" ", "", substring(input$das_sighting_code_1, 1, 3))
  } else {
    stop("Invalid CruzPlot input$das_sighting_code_1_all value. ",
         "Please report this as an issue")
  }

  validate(
    need(length(sp.codes) > 0, "Please choose at least one valid mammal species code")
  )

  sp.codes
})

cruzDasSightSpeciesTurtles <- reactive({
  sp.codes <- if (input$das_sighting_code_2_all == 1) {
    cruzSpeciesTurtles()$Code
  } else if (input$das_sighting_code_2_all == 2) {
    substring(input$das_sighting_code_2, 1, 2)
  } else {
    stop("Invalid CruzPlot input$das_sighting_code_2_all value. ",
         "Please report this as an issue")
  }

  validate(
    need(length(sp.codes) > 0, "Please choose at least one valid turtle species code")
  )

  sp.codes
})


###############################################################################
# So that das_sight is run once
cruzDasSightSpeciesProcess <- reactive({
  swfscDAS::das_sight(req(cruz.list$das.data), return.format = "default")
})



###############################################################################
# Series of sighting-processing reactive functions
#   1) NA position filter, 2) Species/event filter

### Checks on NA positions to print modal, and 'select' selected position
cruzDasSightPosition <- reactive({
  das.sight <- cruzDasSightSpeciesProcess()

  #----------------------------------------------------------------------------
  # Verbosely remove sightings with NA positions
  ll.na <- if (input$das_sightings_position == 1) {
    which(is.na(das.sight$Lat) | is.na(das.sight$Lon))
  } else (
    which(
      is.na(das.sight$Lat) | is.na(das.sight$Lon) | is.na(das.sight$Course) |
        is.na(das.sight$Bearing) | is.na(das.sight$DistNm)
    )
  )

  if (length(ll.na) > 0) {
    table.out <- das.sight %>%
      slice(ll.na) %>%
      mutate(DateTime = as.character(DateTime),
             Cruise = as.character(Cruise),
             Resight = Event %in% c("s", "k", "g")) %>%
      select(Event, DateTime, Lat, Lon, OnEffort, Cruise,
             SightNo, SpCode, Resight,
             # File = file_das,
             `Line number` = line_num) %>%
      distinct()
    txt.out1 <- ifelse(nrow(table.out) == 1, "sighting has", "sightings have")
    txt.out2 <- ifelse(nrow(table.out) == 1, "This sighting", "These sightings")

    showModal(modalDialog(
      title = "CruzPlot notice",
      tags$h5("The following", txt.out1, "an NA value that causes the",
              "specified plotted position to be NA.",
              txt.out2, "will be automatically removed (filtered) and thus",
              "not plotted or included in tabular output:"),
      tags$br(), tags$br(),
      renderTable(table.out),
      tags$br(),
      tags$h5("This notice will not be shown again unless a new DAS file is loaded or",
              "'Position to plot' is changed.",
              "See the manual for more details"),
      easyClose = FALSE,
      size = "l"
    ))
  }

  das.sight <- if (input$das_sightings_position == 1) {
    das.sight %>% filter(!is.na(.data$Lat), !is.na(.data$Lon))
  } else {
    das.sight %>%
      filter(!is.na(.data$Lat), !is.na(.data$Lon),
             !is.na(.data$Course), !is.na(.data$Bearing), !is.na(.data$DistNm))
  }

  #----------------------------------------------------------------------------
  # Calculate sighting location,, select selected position, and return
  bearing2 <- (das.sight$Course + das.sight$Bearing) %% 360
  ll.sight <- geosphere::destPoint(
    matrix(c(das.sight$Lon, das.sight$Lat), ncol = 2),
    bearing2, das.sight$DistNm * 1852
  )

  # # Calculate sighting location using swfscMisc
  # ll.sight.dest <- apply(das.sight, 1, function(i) {
  # i <- as.numeric(i[c("Lat", "Lon", "bearing2", "DistNm")])
  #   swfscMisc::destination(i["Lat"], i["Lon"], i["bearing2"], i["DistNm"],
  #                          units = "nm", type = "ellipsoid")
  # })

  das.sight <- das.sight %>%
    mutate(Lat_ship = .data$Lat, Lon_ship = .data$Lon,
           Lat_sight = ll.sight[, "lat"],
           Lon_sight = ll.sight[, "lon"])


  # 'Select' ship or sighting position.
  #   Might as well do this here since position input is already being used
  if (input$das_sightings_position == 1) {
    das.sight$Lat <- das.sight$Lat_ship
    das.sight$Lon <- das.sight$Lon_ship

  } else if (input$das_sightings_position == 2) {
    das.sight$Lat <- das.sight$Lat_sight
    das.sight$Lon <- das.sight$Lon_sight
  }

  # Adjust longitudes if world2 map is being used
  # #this is done in cruzDasSightRange()
  # if (cruz.map.range$world2)
  #   das.sight$Lon <- ifelse(das.sight$Lon < 0, das.sight$Lon + 360, das.sight$Lon)

  das.sight
})


###############################################################################
# Filter for events specified either directly or by sighting type
#   And associated things that depend on the selected event
cruzDasSightEventResight <- reactive({
  (input$das_sighting_type == 1) &
    any(c("s", "k", "g") %in% input$das_sighting_events)
})

output$cruzDasSightEventResight_uiOut_message <- renderUI({
  req(cruzDasSightEventResight())
  tags$h5("When plotting resights, symbol color entries correspond to selected events")
})


cruzDasSightEvent <- reactive({
  das.sight <- cruzDasSightPosition()
  sight.type <- input$das_sighting_type

  if (sight.type == 2) {
    ### Turtle sightings
    das.sight <- das.sight %>% filter(.data$Event == "t")
    validate(
      need(nrow(das.sight) > 0,
           "There are no turtle sightings (t events) in the loaded DAS file(s)")
    )

  } else if (sight.type == 3) {
    ### Boat sightings
    das.sight <- das.sight %>% filter(.data$Event == "F")
    validate(
      need(nrow(das.sight) > 0,
           "There are no boat sightings (F events) in the loaded DAS file(s)")
    )

  } else if (sight.type == 1) {
    ### Marine mammal sightings
    sp.events <- input$das_sighting_events
    validate(need(sp.events, "Please select at least one event code to plot"))

    das.sight <- das.sight %>%
      filter(.data$Event %in% sp.events) %>%
      mutate(idx = seq_along(.data$Event))

    # Resights
    if (cruzDasSightEventResight()) {
      # Checks - other
      validate(
        need(length(unique(na.omit(cruz.list$das.data$file_das))) == 1,
             "You can only process resights when plotting data from a single DAS file")
        # ^b/c different files could have same SightNo, etc.
        # Doesn't solve concatenated files..
      )
      validate(
        need(sum(c("s", "k", "g") %in% sp.events) == 1,
             "You can only plot one type of resight at a time")
      )
      validate(
        if ("s" %in% sp.events) need("S" %in% sp.events, "To plot s events, S events must also be plotted"),
        if ("k" %in% sp.events) need("K" %in% sp.events, "To plot k events, K events must also be plotted"),
        if ("g" %in% sp.events) need("G" %in% sp.events, "To plot g events, G events must also be plotted"),
        need(length(sp.events) == 2,
             "When plotting resights, you can only plot the resight and the corresponding primary sighting event")
      )

      # Get species, etc., for s and k events
      if (any(c("s", "k") %in% sp.events)) {
        das.sight.main <- das.sight %>% filter(!(.data$Event %in% c("s", "k")))
        das.sight.res <- das.sight %>% filter(.data$Event %in% c("s", "k"))
        validate(
          need(all(das.sight.res$SightNo %in% das.sight.main$SightNo),
               paste("Not all of the selected s/k resight event(s) have primary sightings with",
                     "the same sighting numbers -",
                     "this is a DAS error that needs to be fixed to plot s/k events"))
        )

        col.names <- c("Prob", "SpCode", "SpCodeProb")
        d.toadd <- das.sight.main %>%
          select(SightNo, !!col.names) %>%
          filter(SightNo %in% das.sight.res$SightNo) %>%
          full_join(select(das.sight.res, -!!col.names), by = "SightNo") %>%
          select(!!names(das.sight.main))

        das.sight <- bind_rows(das.sight.main, d.toadd) %>% arrange(idx)
        rm(das.sight.main, das.sight.res, col.names, d.toadd)
      }

      # Get species, etc., for g events
      if (any("g" %in% sp.events)) {
        das.sight.main <- das.sight %>%
          filter(!(.data$Event %in% c("g"))) %>%
          mutate(ss_id = paste(SightNo, Subgroup, sep = "_"))
        das.sight.res <- das.sight %>%
          filter(.data$Event %in% c("g")) %>%
          mutate(ss_id = paste(SightNo, Subgroup, sep = "_"))
        validate(
          need(all(das.sight.res$ss_id %in% das.sight.main$ss_id),
               paste("Not all of the selected g resight event(s) have primary sightings with",
                     "the same sighting numbers/subgroup identified",
                     "- this is a DAS error that needs to be fixed to plot g events"))
        )

        col.names <- c("Prob", "SpCode", "SpCodeProb")
        d.toadd <- das.sight.main %>%
          select(.data$ss_id, !!col.names) %>%
          filter(.data$ss_id %in% das.sight.res$ss_id) %>%
          full_join(select(das.sight.res, -!!col.names), by = c("ss_id")) %>%
          select(!!names(das.sight.main))

        das.sight <- bind_rows(das.sight.main, d.toadd) %>%
          select(-ss_id) %>%
          arrange(idx)
        rm(das.sight.main, das.sight.res, col.names, d.toadd)
      }
    }

    validate(
      need(nrow(das.sight) > 0,
           paste("There are no mammal sightings for the selected event(s)",
                 "in the loaded DAS file(s)"))
    )

    das.sight <- das.sight %>% select(-idx)

  } else  {
    ### Error
    validate("Invalid input$das_sighting_type value")
  }

  das.sight
})


###############################################################################
# Filter for specfied species and (if applicable) events
cruzDasSightSpecies <- reactive({
  das.proc <- req(cruz.list$das.data)

  ### Sightings to plot
  sight.type <- input$das_sighting_type
  stopifnot(sight.type %in% 1:4)
  sp.selection <- isTRUE(
    (sight.type == 1 && input$das_sighting_code_1_all == 2) ||
      (sight.type == 2 && input$das_sighting_code_2_all == 2)
  )

  das.sight <- cruzDasSightEvent()

  #----------------------------------------------------------------------------
  if (sight.type == 1) {
    # 1: Mammals
    # Get species codes - also does validate() check for valid species
    sp.codes <- cruzDasSightSpeciesMammals()
    if (cruzDasSightEventResight()) {
      validate(
        need(length(sp.codes) == 1,
             "You currently can only plot resights for one species at a time")
      )
    }

    # Update probable sightings species if necessary
    if (input$das_sighting_probable) {
      das.sight$Prob[das.sight$Event %in% c("p", "s", "k", "g")] <- FALSE
      if (any(is.na(das.sight$Prob)))
        warning("A marine mammal sighting has an unexpected NA 'Prob' value")

      validate(
        need(sum(das.sight$Prob) > 0,
             "There are no probable sightings in the loaded DAS file(s)")
      )

      # 977 used as probable vaquita sighting on some cruises
      das.sight <- das.sight %>%
        mutate(SpCode = ifelse(.data$SpCode == "977", "041", .data$SpCode),
               SpCode = ifelse(.data$Prob, .data$SpCodeProb, .data$SpCode))
    }

    # Filter for selected species, and check that all selected species are in data
    das.sight <- das.sight %>% filter(.data$SpCode %in% sp.codes)

    if (input$das_sighting_code_1_all == 2) {
      sp.codes.none <- base::setdiff(sp.codes, das.sight$SpCode)
      validate(
        need(length(sp.codes.none) == 0,
             paste("The following species code(s) does (do) not",
                   "have any sightings in the loaded DAS file(s):",
                   paste(sp.codes.none, collapse = ", ")))
      )
    }


    #--------------------------------------------------------------------------
  } else if (sight.type == 2) {
    # 2: Turtles
    sp.codes <- cruzDasSightSpeciesTurtles()

    das.sight <- das.sight %>%
      filter(.data$SpCode %in% sp.codes)

    if (input$das_sighting_code_2_all == 2) {
      sp.codes.none <- base::setdiff(sp.codes, das.sight$SpCode)
      validate(
        need(length(sp.codes.none) == 0,
             paste("The following species code(s) does (do) not",
                   "have any sightings in the loaded DAS file(s):",
                   paste(sp.codes.none, collapse = ", ")))
      )
    }


    #--------------------------------------------------------------------------
  } else if (sight.type == 3) {
    # 3: Boats
    das.sight <- das.sight %>% mutate(SpCode = "Boat")
    sp.codes <- NULL


    #--------------------------------------------------------------------------
  } else {
    validate("Invlaid sighting type (input$das_sighting_type) selection")
  }


  #   #--------------------------------------------------------------------------
  # # SMW: Added for vaquita-specifc cruise. Does not seem applicable anymore
  # } else if (sight.type == 4) {
  #   # 4: C-PODs
  #   # C-POD sightings are entered as objects with sighting angle and distance
  #   # the string "cpod" in the comment on the next line indicates object is a CPOD
  #   sp.codes <- NULL
  #
  #   validate(
  #     need(sum(das.proc$Event == "X") > 0,
  #          "There are no C-POD sightings in the loaded DAS file(s)")
  #   )
  #
  #
  #   ndx.x <- which(das.proc$Event == "X")
  #   comm.x1 <- apply(das.proc[ndx.X + 1, paste0("Data", 1:7)], 1, function(i) {
  #     paste(na.omit(i), collapse = "")
  #   })
  #   comm.x1.cpod <- grepl("cpod", comm.x1, ignore.case = TRUE)
  #   stopifnot(length(ndx.x) == length(comm.x1))
  #
  #   ndx.x <- ndx.x[comm.x1.cpod]
  #
  #   das.sight <- das.proc %>%
  #     slice(ndx.x) %>%
  #     mutate(Sp = "CPOD",
  #            Bearing = as.numeric(.data$Data2),
  #            DistNm = as.numeric(.data$Data4),
  #            PerpDistKm = abs(sin(.data$Bearing*pi/180) * .data$DistNm) * 1.852)
  #
  #   validate(
  #     need(nrow(das.sight) > 0,
  #          "There are no C-POD sightings in the loaded DAS file(s)")
  #   )
  #   # comment.str.df <- data.all[,6:13]
  #   # comment.str <- apply(comment.str.df,1,paste,collapse="")
  #   # ndx.cpod <- grep("cpod",comment.str)
  #   # ndx <- ndx.X[(ndx.X+1) %in% ndx.cpod]
  #   # data.sight <- data.all[ndx,]
  #   # angle <- as.numeric(data.sight$Data2)
  #   # dist.nmi <- as.numeric(data.sight$Data4)
  # }


  #----------------------------------------------------------------------------
  # Final check to ensure some sightings match provided selection
  validate(
    need(nrow(das.sight) > 0,
         "No sightings exist for the selected type and code(s)")
  )



  # If "Plot all..." was selected, only return codes in sighting data
  if ((sight.type == 1 && input$das_sighting_code_1_all == 1) |
      (sight.type == 2 && input$das_sighting_code_2_all == 1)) {
    sp.codes <- base::intersect(sp.codes, das.sight$SpCode)
  }

  # Return list
  list(das.sight = das.sight, sight.type = sight.type,
       sp.codes = sp.codes, sp.selection = sp.selection)
})

###############################################################################
