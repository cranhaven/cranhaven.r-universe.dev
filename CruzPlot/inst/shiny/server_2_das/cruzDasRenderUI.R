## renderUI()'s for Plot DAS Data tab


###############################################################################
### Codes for mammals and turtles
# renderUIs for mammal and turtle species
output$das_sighting_code_1_uiOut_select <- renderUI({
  sp.mammals <- cruzSpeciesMammals()
  sp.codes.list <- as.list(sp.mammals$Code)
  names(sp.codes.list) <- paste(sp.mammals$Code, sp.mammals$Abbr, sep = " - ")

  selectInput("das_sighting_code_1", tags$h5("Select mammal species"),
              choices = sp.codes.list, multiple = TRUE,
              selected = NULL)
})
outputOptions(output, "das_sighting_code_1_uiOut_select", suspendWhenHidden = FALSE)

output$das_sighting_code_2_uiOut_select <- renderUI({
  sp.turtles <- cruzSpeciesTurtles()
  sp.codes.list <- as.list(sp.turtles$Code)
  names(sp.codes.list) <- paste(sp.turtles$Code, sp.turtles$Name_Scientific, sep = " - ")

  selectInput("das_sighting_code_2", tags$h5("Select turtle species"),
              choices = sp.codes.list, multiple = TRUE, selected = NULL)
})
outputOptions(output, "das_sighting_code_2_uiOut_select", suspendWhenHidden = FALSE)


###############################################################################
# Filters

# Date widgets
### Get min and max datws in DAS file
dateRange_min_max <- reactive({
  x <- req(cruz.list$das.data)
  input$das.file

  min.date <- as.character(as.Date(min(x$Date, na.rm = T)) - 1)
  max.date <- as.character(as.Date(max(x$Date, na.rm = T)) + 1)

  c(min.date, max.date)
})

### renderUI for date range filter for plotting sightings and effort
output$das_sight_dateRange_uiOut_date <- renderUI({
  req(cruz.list$das.data)

  dates <- dateRange_min_max()

  dateRangeInput("das_sight_dateRange", label = tags$h5("Date range"),
                 start = dates[1], end = dates[2])
})
outputOptions(output, "das_sight_dateRange_uiOut_date", suspendWhenHidden = FALSE, priority = 3)

output$das_effort_dateRange_uiOut_date <- renderUI({
  req(cruz.list$das.data)
  dates <- dateRange_min_max()

  dateRangeInput("das_effort_dateRange", label = tags$h5("Date range"),
                 start = dates[1], end = dates[2])
})
outputOptions(output, "das_effort_dateRange_uiOut_date", suspendWhenHidden = FALSE, priority = 3)


###############################################################################
# Cruise number

### Reactive returning cruise numbers in the file
das_cruise_nums <- reactive({
  x <- req(cruz.list$das.data)
  input$das.file

  unique(na.omit(x$Cruise))
})

### Cruise number - sight
output$das_sight_cruise_uiOut_select <- renderUI({
  req(cruz.list$das.data)
  cruises <- das_cruise_nums()

  selectInput("das_sight_cruise", tags$h5("Cruise number(s)"),
              choices = cruises, multiple = TRUE, selected = NULL)
})
outputOptions(output, "das_sight_cruise_uiOut_select", suspendWhenHidden = FALSE, priority = 3)

### Cruise number - effort
output$das_effort_cruise_uiOut_select <- renderUI({
  req(cruz.list$das.data)
  cruises <- das_cruise_nums()

  selectInput("das_effort_cruise", tags$h5("Cruise number(s)"),
              choices = cruises, multiple = TRUE, selected = NULL)
})
outputOptions(output, "das_effort_cruise_uiOut_select", suspendWhenHidden = FALSE, priority = 3)

###############################################################################
### Truncation input
output$das_sight_trunc_uiOut_numeric <- renderUI({
  isolate(curr.value <- input$das_sight_trunc)

  trunc.units <- input$das_sight_trunc_units
  if(trunc.units == 1) widget.name <- "Truncation (km)"
  if(trunc.units == 2) widget.name <- "Truncation (nmi)"

  numericInput("das_sight_trunc", label = tags$h5(widget.name), value = curr.value)
})
outputOptions(output, "das_sight_trunc_uiOut_numeric", suspendWhenHidden = FALSE, priority = 3)

##############################################################################
