# Processing for Planned Transects tab of Create and Save Map tab


###############################################################################
###############################################################################
# Indicator for if any planned transects are loaded

### Conditional flag for UI code
output$cruzMapPlannedTransects_Conditional <- reactive({
  isTruthy(cruz.list$planned.transects)
})
outputOptions(output, "cruzMapPlannedTransects_Conditional", suspendWhenHidden = FALSE)


### Turn plot checkbox on if planned transects are added
observe({
  if (isTruthy(cruz.list$planned.transects))
    updateCheckboxInput(session, "planned_transects_plot", value = TRUE)
})

### Turn plot checkbox off if all planned transects are removed
observe({
  if (is.null(cruz.list$planned.transects))
    updateCheckboxInput(session, "planned_transects_plot", value = FALSE)
})

###############################################################################
###############################################################################
# renderUI()s for loading planned transects

#----------------------------------------------------------
### Get names from loaded csv file
planned_transects_file_names <- reactive({
  req(planned_transects_read_csv())

  csv.names <- names(planned_transects_read_csv()[[2]])
  choices.list <- seq_along(csv.names)
  names(choices.list) <- csv.names

  choices.list
})

### Create ui inputs for selecting lon/lat columns
output$planned_transects_lon_uiOut_select <- renderUI({
  selectInput("planned_transects_lon",  h5("Longitude column"),
              choices = planned_transects_file_names(), selected = 1)
})

output$planned_transects_lat_uiOut_select <- renderUI({
  selectInput("planned_transects_lat",  h5("Latitude column"),
              choices = planned_transects_file_names(), selected = 2)
})

### Widgets for transect numbers
output$planned_transects_num_uiOut_select<- renderUI({
  selectInput("planned_transects_num",  h5("Transect number column"),
              choices = planned_transects_file_names(), selected = 3)
})

### Widget for transect classes 1
output$planned_transects_class1_uiOut_select<- renderUI({
  selectInput("planned_transects_class1",  h5("Transect class column"),
              choices = planned_transects_file_names(), selected = 4)
})

### Widget for transect classes 2
output$planned_transects_class2_uiOut_select<- renderUI({
  choices.list <- planned_transects_file_names()
  choices.list <- c("N/A - No class 2 info" = 0, choices.list)

  selectInput("planned_transects_class2",  h5("Transect class 2 column"),
              choices = choices.list, selected = 0)
})


#----------------------------------------------------------
### Button to add selected transect data to CruzPlot
output$planned_transects_execute_uiOut_button <- renderUI({
  req(planned_transects_read_csv())

  actionButton("planned_transects_execute", "Add data to CruzPlot")
})


###############################################################################
###############################################################################
# Process transect file and data and output widget to select ones to plot

### Read csv file
planned_transects_read_csv <- reactive({
  req(input$planned_transects_file)

  file.all <- input$planned_transects_file
  file.name <- file.all$name
  file.data <- try(read.csv(file.all$datapath, stringsAsFactors = FALSE),
                   silent = TRUE)

  validate(
    need(file.data, "Error loading planned transects CSV")
  )

  list(file.name, file.data)
})


### Add transect data to reactiveValue
planned_transects <- eventReactive(input$planned_transects_execute, {
  validate(
    need(input$planned_transects_lon != input$planned_transects_lat,
         "Error: The longitude column cannot be the same as the latitude column")
  )

  x <- planned_transects_read_csv()[[2]] %>%
    dplyr::select(lon = as.numeric(input$planned_transects_lon),
                  lat = as.numeric(input$planned_transects_lat),
                  num = as.numeric(input$planned_transects_num),
                  class1 = as.numeric(input$planned_transects_class1))

  validate(
    need(all(dplyr::between(x$lon, -180, 180)),
         "Error: Planned transect longitude data must be in range [-180, 180]"),
    need(all(dplyr::between(x$lat, -90, 90)),
         "Error: Planned transect latitude data must be in range [-90, 90]"),
    need(!anyNA(x$num),
         "Error: Planned transect 'number' column cannot have any NA values"),
    need(!anyNA(x$class1),
         "Error: Planned transect 'class' column cannot have any NA values")
  )

  if (as.numeric(input$planned_transects_class2) != 0) {
    x <- cbind(
      x, dplyr::select(planned_transects_read_csv()[[2]],
                       class2 = as.numeric(input$planned_transects_class2))
    )
    validate(
      need(!anyNA(x$class2),
           "Error: Planned transect 'class 2' column cannot have any NA values")
    )
  } else {
    x <- cbind(x, class2 = NA)
  }

  cruz.list$planned.transects <- x

  ""
})


###############################################################################
###############################################################################
# Processing loaded planned transects

planned_transects_class1 <- reactive({
  unique(cruz.list$planned.transects$class1)
})

planned_transects_class2 <- reactive({
  unique(cruz.list$planned.transects$class2)
})


###############################################################################
### Widgets for selecting planned transect class(es) to plot and their color
output$planned_transects_toplot_uiOut_select <- renderUI({
  req(cruz.list$planned.transects)

  choices.list.names <- planned_transects_class1()
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names

  isolate({
    choices.sel <- if (isTruthy(cruz.pt.load.toplot())) {
      cruz.pt.load.toplot()
    } else {
      choices.list
    }
    cruz.pt.load.toplot(NULL)
  })

  selectInput("planned_transects_toplot",
              tags$h5("Class(es) to plot"),
              choices = choices.list, selected = choices.sel,
              multiple = TRUE)
})


output$planned_transects_color_uiOut_select <- renderUI({
  req(cruz.list$planned.transects)

  isolate({
    choices.sel <- if (isTruthy(cruz.pt.load.color())) {
      cruz.pt.load.color()
    } else {
      "gray"
    }
    cruz.pt.load.color(NULL)
  })

  selectInput("planned_transects_color", label = tags$h5("Color(s)"),
              choices = cruz.palette.color, selected = choices.sel,
              multiple = TRUE)
})


#----------------------------------------------------------
### Widgets for selecting planned transect class 2(s) to plot and their lty
output$planned_transects_toplot2_uiOut_select <- renderUI({
  req(cruz.list$planned.transects)

  y <- planned_transects_class2()

  if (anyNA(y)) {
    helpText("No class 2 column was selected, and thus you can only specify",
             "a single line type for all planned transects")

  } else {
    choices.list.names <- y
    choices.list <- seq_along(choices.list.names)
    names(choices.list) <- choices.list.names

    isolate({
      choices.sel <- if (isTruthy(cruz.pt.load.toplot2())) {
        cruz.pt.load.toplot2()
      } else {
        choices.list
      }
      cruz.pt.load.toplot2(NULL)
    })

    selectInput("planned_transects_toplot2", tags$h5("Class 2(s) to plot"),
                choices = choices.list, selected = choices.sel,
                multiple = TRUE)
  }
})

# output$planned_transects_lty_uiOut_message <- renderUI({
#   req(cruz.list$planned.transects, input$planned_transects_plot)
#
#   if (!anyNA(planned_transects_class2())) {
#     helpText("Select either one line type or the same number of line types as transect class 2s.",
#              "When multiple line types are selected, the order in which transect class 2(s) are",
#              "selected to be plotted corresponds to order of specified line type(s).")
#   } else {
#     NULL
#   }
# })

output$planned_transects_lty_uiOut_select <- renderUI({
  req(cruz.list$planned.transects)

  input.lab <- ifelse(
    anyNA(planned_transects_class2()), "Line type", "Line type(s)"
  )

  isolate({
    choices.sel <- if (isTruthy(cruz.pt.load.lty())) {
      cruz.pt.load.lty()
    } else {
      1
    }
    cruz.pt.load.lty(NULL)
  })

  selectInput("planned_transects_lty", label = tags$h5(input.lab),
              choices = cruz.line.type, selected = choices.sel,
              multiple = !anyNA(planned_transects_class2()))

})

###############################################################################
# Removing loaded transects
#   Currently only allows one transect file to be loaded

# ### Widget for selecting planned transect(s) to remove
# output$planned_transects_toremove_uiOut_select <- renderUI({
#   req(cruz.list$planned.transects)
#
#   choices.list.names <- planned_transects_class1()
#   choices.list <- seq_along(choices.list.names)
#   names(choices.list) <- choices.list.names
#
#   selectInput("planned_transects_toremove",
#                  tags$h5("Select planned transect class(es) to remove"),
#                  choices = choices.list, multiple = TRUE)
# })
#
# output$planned_transects_toremove_execute_uiOut_button <- renderUI({
#   req(cruz.list$planned.transects)
#
#   actionButton("planned_transects_toremove_execute", "Remove")
# })
#
#
# ### Remove selected transects
# planned_transects_remove <- eventReactive(input$planned_transects_toremove_execute, {
#   req(cruz.list$planned.transects)
#   y <- as.numeric(input$planned_transects_toremove)
#
#   validate(
#     need(length(y) != 0,
#          "Please select at least one set of transects to remove")
#   )
#
#   x <- cruz.list$planned.transects %>%
#     filter(!(class1 %in% planned_transects_class1()[y]))
#
#   if (nrow(x) == 0) {
#     cruz.list$planned.transects <- NULL
#   } else {
#     cruz.list$planned.transects <- x
#   }
#
#   "Planned transects removed"
# })

###############################################################################
###############################################################################
