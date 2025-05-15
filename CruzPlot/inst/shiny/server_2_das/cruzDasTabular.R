### cruzDasOutTabular
## Code for tabular output of sighitngs and effort


###############################################################################
# Table of all sightings in file
cruzDasOutSight_TotTable <- reactive({
  req(cruzDasSightFilter())
  das.sight <- cruzDasSightSpeciesProcess()
  sight.type <- input$das_sighting_type

  if (sight.type == 2) {
    ### Turtles
    das.sight <- das.sight %>% filter(.data$Event == "t")
    df.join <- NULL
    df.out <- if (input$das_sighting_code_2_all == 1) {
      das.sight %>%
        group_by(Event) %>%
        summarise(Count = n(), .groups = "drop")
    } else {
      das.sight %>%
        filter(SpCode %in% input$das_sighting_code_2) %>%
        group_by(Event, SpCode) %>%
        summarise(Count = n(), .groups = "drop")
    }

  } else if (sight.type == 3) {
    ### Boats
    das.sight <- das.sight %>% filter(.data$Event == "F")
    df.join <- NULL
    df.out <- das.sight %>%
      group_by(Event) %>%
      summarise(Count = n(), .groups = "drop")

  } else if (sight.type == 1) {
    ### Marine mammals
    das.sight <- das.sight <- das.sight %>%
      filter(.data$Event %in% input$das_sighting_events)
    df.join <- data.frame(Event = input$das_sighting_events, stringsAsFactors = FALSE)

    df.out <- if (input$das_sighting_code_1_all == 1) {
      das.sight %>%
        group_by(Event) %>%
        summarise(Count = n(), .groups = "drop")
    } else {
      das.sight %>%
        filter(SpCode %in% input$das_sighting_code_1) %>%
        group_by(Event, SpCode) %>%
        summarise(Count = n(), .groups = "drop")
    }
  }

  if (isTruthy(df.join)) {
    df.out <- df.out %>%
      full_join(df.join, by = "Event") %>%
      mutate(Count = ifelse(is.na(.data$Count), 0, .data$Count)) %>%
      arrange(.data$Event)
  }

  t(df.out)
})

# Sightings table
cruzDasOutSight_Table <- reactive({
  req(cruz.list$das.data)
  validate(
    need(input$das_sightings,
         "'Plot sightings' must be selected to generate tabular sightings output")
  )


  # Get filtered data, which also handles error checks
  data.list <- cruzDasSightFilter()
  das.sight <- data.list$das.sight

  das.sight.summ <- das.sight %>%
    group_by(.data$SpCode) %>%
    summarise(std = sum(.data$OnEffort & .data$EffType == "S", na.rm = TRUE),
              #na.rm=TRUE b/c off effort sightings might not have effort type
              nstd = sum(.data$OnEffort & .data$EffType == "N", na.rm = TRUE),
              fine = sum(.data$OnEffort & .data$EffType == "F", na.rm = TRUE),
              off_eff = sum(!.data$OnEffort),
              total = n(),
              .groups = "drop")

  # 'Filter' summary for columns specified by sighting filters, and rename
  #   Data has already been filtered, just need to make the table pretty
  if (input$das_sight_effort == 3) {
    das.sight.summ$std <- NA
    das.sight.summ$nstd <- NA
    das.sight.summ$fine <- NA

  } else {
    if (input$das_sight_effort == 2) das.sight.summ$off_eff <- NA

    if (!("S" %in% input$das_sight_snf)) das.sight.summ$std <- NA
    if (!("N" %in% input$das_sight_snf)) das.sight.summ$nstd <- NA
    if (!("F" %in% input$das_sight_snf)) das.sight.summ$fine <- NA
  }

  das.sight.summ.all <- if (input$das_out_allcheck) {
    c(list(`Species code` = "All"), lapply(select(das.sight.summ, -SpCode), sum))
  } else {
    NULL
  }

  # Add in selected species identifiers, join, and do name wrangling
  das.sight.sp <- das.sight.summ %>%
    select(.data$SpCode) %>%
    left_join(req(cruz.list$sp.codes), by = c("SpCode" = "Code"))

  das.sight.sp %>%
    rename("Species code" = .data$SpCode, "Abbreviation" = .data$Abbr,
           "Scientific name" = .data$Name_Scientific,
           "Common name" = .data$Name_Common) %>%
    select(c(1, as.numeric(input$das_out_sciname))) %>%
    left_join(das.sight.summ, by = c("Species code" = "SpCode")) %>%
    bind_rows(das.sight.summ.all) %>%
    rename("Standard" = .data$std, "Non-standard" = .data$nstd,
           "Fine" = .data$fine, "Off effort" = .data$off_eff, "Total" = .data$total)
})


# Download sightings table
output$das_out_sight_save <- downloadHandler(
  filename = function() {
    gsub("-", "", paste0("CruzPlot_sightings_", Sys.Date(), ".csv"))
  },

  content = function(file) {
    write.csv(cruzDasOutSight_Table(), file = file, row.names = FALSE)
  }
)


###############################################################################
# Effort
cruzDasOutEffort_Table <- reactive({
  das.eff.lines <- cruzDasEffortFilter() %>%
    mutate(st_lon = ifelse(.data$st_lon > 180, .data$st_lon - 360, .data$st_lon),
           end_lon = ifelse(.data$end_lon > 180, .data$end_lon - 360, .data$end_lon))

  # Calculate distance
  dist.effort.km <- geosphere::distVincentyEllipsoid(
    cbind(das.eff.lines$st_lon, das.eff.lines$st_lat),
    cbind(das.eff.lines$end_lon, das.eff.lines$end_lat)
  ) / 1000

  das.eff.lines$dist <-  if (input$das_out_effort_units == 2) {
    dist.effort.km / 1.852
  } else {
    dist.effort.km
  }

  # Create summary tables
  if (input$das_effort == 2) {
    eff.out <- data.frame(
      Bft = "All",
      std = sum(das.eff.lines$dist[das.eff.lines$EffType == "S"]),
      nstd = sum(das.eff.lines$dist[das.eff.lines$EffType == "N"]),
      fine = sum(das.eff.lines$dist[das.eff.lines$EffType == "F"]),
      total = sum(das.eff.lines$dist),
      stringsAsFactors = FALSE
    )

  } else if (input$das_effort == 3) {
    eff.summ <- das.eff.lines %>%
      group_by(Bft) %>%
      summarise(std = sum(dist[EffType == "S"]),
                nstd = sum(dist[EffType == "N"]),
                fine = sum(dist[EffType == "F"]),
                total = sum(dist),
                .groups = "drop")

    eff.out <- rbind(eff.summ, vapply(eff.summ, sum, 1)) %>%
      mutate(Bft = c(head(Bft, -1), "All"))

  } else {
    validate("Error: invalid 'Effort to plot' (das_effort) selection")
  }

  # 'Filter' summary for columns specified by effort type filters, and rename
  if (!("S" %in% input$das_effort_snf)) eff.out$std <- NA
  if (!("N" %in% input$das_effort_snf)) eff.out$nstd <- NA
  if (!("F" %in% input$das_effort_snf)) eff.out$fine <- NA

  eff.out %>%
    rename(Beaufort = Bft, Standard = std, `Non-standard` = nstd,
           Fine = fine, Total = total)


  # name.total <- paste("Total", ifelse(input$das_out_effort_units == 1, "(km)", "(nmi)"))
  # names(eff.out) <- c(head(names(eff.out), -1), name.total)
  # eff.out
})


# Save effort table
output$das_out_effort_save <- downloadHandler(
  filename = function() {
    u.txt <- switch(as.numeric(input$das_out_effort_units), "km", "nmi")
    gsub("-", "", paste0("CruzPlot_effort_", u.txt, "_", Sys.Date(), ".csv"))
  },

  content = function(file) {
    write.csv(cruzDasOutEffort_Table(), file = file, row.names = FALSE)
  }
)

###############################################################################
