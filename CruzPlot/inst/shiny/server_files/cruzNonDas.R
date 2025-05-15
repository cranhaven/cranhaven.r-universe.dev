# Load and store non-DAS data


###############################################################################
# Update inputs

### Turn plot non-DAS off if all objects are removed
observe({
  if (length(cruz.list$ndas.data) == 0)
    updateCheckboxInput(session, "ndas_plot", value = FALSE)
})


###############################################################################
# Non-DAS file loading and processing

### Read in csv file
cruzNonDasFileLoad <- reactive({
  if (is.null(input$ndas_file)) return(NULL)
  read.csv(input$ndas_file$datapath, stringsAsFactors = FALSE)
})

### Get long/lat data
cruzNonDasFile_LonLat <- reactive({
  req(cruzNonDasFileLoad())
  ndas.file <- cruzNonDasFileLoad()

  lon.names <- c("Longitude", "longitude", "LONGITUDE", "Lon","lon","LON")
  ndx.lon <- which(names(ndas.file) %in% lon.names)

  lat.names <- c("Latitude", "latitude", "LATITUDE", "Lat","lat","LAT")
  ndx.lat <- which(names(ndas.file) %in% lat.names)

  validate(
    need(length(ndx.lon) < 2,
         paste("Multiple columns of loaded non-DAS data have one of the",
               "following headings:", paste(lon.names, collapse = ", "))),
    need(length(ndx.lon) > 0,
         paste("None columns of loaded non-DAS data have one of the",
               "following headings:", paste(lon.names, collapse = ", "))),
    need(length(ndx.lat) < 2,
         paste("Multiple columns of loaded non-DAS data have one of the",
               "following headings:", paste(lat.names, collapse = ", "))),
    need(length(ndx.lon) > 0,
         paste("None of loaded non-DAS data have one of the",
               "following headings:", paste(lat.names, collapse = ", ")))
  )

  ndas.x <- ndas.file[, ndx.lon]
  ndas.y <- ndas.file[, ndx.lat]

  list(ndas.x = ndas.x, ndas.y = ndas.y)
})

### Conditional flag for UI code
output$cruzNonDasFile_Conditional <- reactive({
  # For error messages about column names
  if (isTruthy(cruzNonDasFile_LonLat())) isTruthy(cruzNonDasFileLoad()) else NULL
})
outputOptions(output, "cruzNonDasFile_Conditional", suspendWhenHidden = FALSE)



###############################################################################
# Non-DAS data processing when adding data

### Add list of data to cruz.list and return data frame for display table
# Lines
cruzNonDasAdd_Line <- reactive({
  req(cruzNonDasFileLoad())

  ndas.x <- cruzNonDasFile_LonLat()[[1]]
  ndas.y <- cruzNonDasFile_LonLat()[[2]]

  line.lty <- as.numeric(input$ndas_line_lty)
  line.col <- input$ndas_line_col
  line.col.which <- which(cruz.palette.color == line.col)
  line.lwd <- input$ndas_line_lwd

  list.curr <- list(ind = 1, x = ndas.x, y = ndas.y, type = line.lty,
                    col = line.col, cex = NA, lwd = line.lwd)
  list.curr.df <- data.frame(
    a = length(ndas.x) - 1, b = names(cruz.line.type)[line.lty],
    c = names(cruz.palette.color)[line.col.which], d = NA, e = line.lwd,
    stringsAsFactors = FALSE
  )

  cruz.list$ndas.data <- c(cruz.list$ndas.data, list(list.curr))

  list.curr.df
})

# Points
cruzNonDasAdd_Point <- reactive({
  req(cruzNonDasFileLoad())

  ndas.x <- cruzNonDasFile_LonLat()[[1]]
  ndas.y <- cruzNonDasFile_LonLat()[[2]]

  pt.pch <- as.numeric(input$ndas_pt_pch)
  pt.col <- input$ndas_pt_col
  pt.col.which <- which(cruz.palette.color == pt.col)
  pt.cex <- input$ndas_pt_cex
  pt.lwd <- input$ndas_pt_lwd

  list.curr <- list(ind = 2, x = ndas.x, y = ndas.y, type = pt.pch,
                    col = pt.col, cex = pt.cex, lwd = pt.lwd)
  list.curr.df <- data.frame(
    a = length(ndas.x), b = names(cruz.symbol.type)[pt.pch + 1],
    c = names(cruz.palette.color)[pt.col.which], d = pt.cex, e = pt.lwd,
    stringsAsFactors = FALSE
  )

  cruz.list$ndas.data <- c(cruz.list$ndas.data, list(list.curr))

  list.curr.df
})


### Add non-DAS file info to CruzPlot
cruzNonDasAdd <- eventReactive(input$ndas_load_execute, {
  ndas.file <- cruzNonDasFileLoad()
  ndas.type <- input$ndas_plot_type
  ndas.type.name <- ifelse(ndas.type == 1, "Line", "Point")

  # Data added to cruz.list below
  list.curr.df <- if (ndas.type == 1) cruzNonDasAdd_Line() else cruzNonDasAdd_Point()

  ndas.df.curr <- cbind(data.frame("File_name" = input$ndas_file$name,
                                   "Type" = ndas.type.name,
                                   stringsAsFactors = FALSE),
                        list.curr.df)
  names(ndas.df.curr) <- c("File name", "Type", "Count", "Line or point type",
                           "Color", "Size", "Line width")

  cruz.list$ndas.df <- rbind(cruz.list$ndas.df, ndas.df.curr)

  ""
})


### Remove specified non-DAS data
output$ndas_remove_execute <- renderUI({
  req(cruz.list$ndas.df)

  actionButton("ndas_remove_execute", "Remove selected non-DAS data")
})

cruzNonDasRemove <- eventReactive(input$ndas_remove_execute, {
  req(cruz.list$ndas.df)

  which.toremove <- input$cruzNonDasLoaded_rows_selected
  validate(
    need(length(which.toremove) > 0,
         "Please select at least one row to remove")
  )

  cruz.list$ndas.data <- cruz.list$ndas.data[-which.toremove]

  cruz.list$ndas.df <- cruz.list$ndas.df[-which.toremove,]
  if (nrow(cruz.list$ndas.df) == 0) cruz.list$ndas.df <- NULL

  x <- cruz.list$ndas.toplot
  cruz.list$ndas.toplot <- x[!(x %in% which.toremove)]
  cruz.list$ndas.toplot <- unname(
    sapply(cruz.list$ndas.toplot,
           function(i) {
             i.num <- as.numeric(i)
             ifelse(i.num > which.toremove, as.character(i.num - 1), i)
           })
  )
  if (length(cruz.list$ndas.toplot) == 0) cruz.list$ndas.toplot <- NULL
  updateSelectInput(session, "ndas_toplot", selected = cruz.list$ndas.toplot)

  ""
})


###############################################################################
# Non-DAS data processing for plotting

observe({
  req(input$ndas_toplot)

  isolate({
    if (is.null(cruz.list$ndas.toplot)) {
      cruz.list$ndas.toplot <- input$ndas_toplot
    } else {
      if (!all(suppressWarnings(cruz.list$ndas.toplot != input$ndas_toplot)))
        cruz.list$ndas.toplot <- input$ndas_toplot
    }
  })
})

output$ndas_toplot_uiOut_select <- renderUI({
  df <- cruz.list$ndas.df
  validate(need(df, "Please load non-DAS data"))

  isolate(curr.sel <- cruz.list$ndas.toplot)

  choices.list.names <- apply(df, 1, function(i) paste(i[1], i[2], sep = " || "))
  choices.list <- seq_along(choices.list.names)
  choices.list.names <- paste0(choices.list, ": ", choices.list.names)

  names(choices.list) <- choices.list.names

  selectInput("ndas_toplot", tags$h5("Select non-DAS data to be plotted"),
              choices = choices.list, multiple = TRUE,
              selected = curr.sel)
})
outputOptions(output, "ndas_toplot_uiOut_select", suspendWhenHidden = FALSE)

### Get non-DAS data for plotting
cruzNonDas <- reactive({
  req(input$ndas_plot)

  all.data <- cruz.list$ndas.data

  validate(
    need(length(all.data) > 0,
         "Please load at least one non-DAS object before plotting")
  )

  all.data.sel <- all.data[as.numeric(cruz.list$ndas.toplot)]

  validate(
    need(length(all.data.sel) > 0,
         "Please select at least one non-DAS object to plot")
  )

  all.data.sel.ind <- sapply(all.data.sel, function(i) i$ind)

  # Get line and point data to plot
  x <- all.data.sel[all.data.sel.ind == 1]
  y <- all.data.sel[all.data.sel.ind == 2]

  list(data.line = x, data.point = y)
})
