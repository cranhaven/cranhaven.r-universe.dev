# cruzDasEffort for CruzPlot - file 1 of effort processing
#   cruzDasEffortEvent() returns selected and filtered effort data
#   Do filter stuff in cruzDasEffortFilter()

###############################################################################
cruzDasEffortEvent <- reactive({
  #----------------------------------------------------------------------------
  req(input$das_effort != 0)
  das.proc <- req(cruz.list$das.data)

  eff.events <- if (input$das_effort == 2) c("R", "E") else c("R", "V", "E")

  das.eff <- das.proc %>%
    filter(.data$OnEffort | .data$Event == "E",
           .data$Event %in% eff.events)

  validate(
    need(sum(das.eff$Event == "R") == sum(das.eff$Event == "E"),
         "There are not an equal number of R and E events in the data")
  )
  validate(
    need(all((which(das.eff$Event == "E") - which(das.eff$Event == "R")) > 0),
         "R and E events do not properly alternate"),
    need(identical(tail(das.eff$Event, 1), "E"),
         "The DAS data effort must end with an E event")
  )


  # For simplified effort, we don't need Beaufort values
  # For detailed effort, we remove R to V events; distance should be 0
  # Each continuous effort section will end with an E event
  das.eff.lines <- das.eff %>%
    mutate(st_lat = .data$Lat,
           st_lon = .data$Lon,
           end_lat = c(.data$Lat[-1], NA),
           end_lon = c(.data$Lon[-1], NA)) %>%
    filter(.data$Event != "E")

  if (input$das_effort == 3)
    das.eff.lines <- das.eff.lines %>% filter(Event != "R")


  #----------------------------------------------------------------------------
  # Verbosely remove any effort lines with NA lat/lon and return
  ll.na <- which(
    is.na(das.eff.lines$st_lat) | is.na(das.eff.lines$end_lat) |
      is.na(das.eff.lines$st_lon) | is.na(das.eff.lines$end_lon)
  )
  if (length(ll.na) > 0) {
    table.out <- das.eff.lines %>%
      slice(ll.na) %>%
      select(Event, DateTime, Lat, Lon, OnEffort,
             Cruise, file_das, line_num) %>%
      mutate(DateTime = as.character(DateTime),
             Cruise = as.character(Cruise))
    txt.out <- ifelse(nrow(table.out) == 1, "line had", "lines have")
    txt.out2 <- ifelse(nrow(table.out) == 1, "This line", "These lines")

    showModal(modalDialog(
      title = "CruzPlot notice",
      tags$h5("The following effort", txt.out, "had an NA latitude or longitude value.",
              txt.out2, "will be removed (filtered) and thus not plotted",
              "or included in tabular output:"),
      tags$br(), tags$br(),
      renderTable(table.out),
      tags$br(),
      tags$h5("This notice will not be shown again unless a new DAS file is loaded"),
      easyClose = FALSE,
      size = "l"
    ))
  }


  # Filter for non-NA lines
  das.eff.lines %>%
    filter(!is.na(.data$st_lat), !is.na(.data$end_lat),
           !is.na(.data$st_lon), !is.na(.data$end_lon))
})

###############################################################################
