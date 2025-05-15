# Processing for Color and Grid tabs of Create and Save Map tab
#   cruzMapRiver() returns river data, adjusted for world2 map if necessary
#   cruzMapColorLand() returns land color
#   cruzMapGrid() returns grid line parameters
#   cruzMapColorWater() returns water color and depth data
#   Color style updating is done in server_color


###############################################################################
# River values
cruzMapRiver <- reactive({
  world2 <- cruz.map.range$world2
  rivs <- map("rivers", plot = FALSE)
  if (world2) rivs$x <- ifelse(rivs$x < 0, rivs$x+360, rivs$x)

  rivs
})

# Land
cruzMapColorLand <- reactive({
  ifelse(input$color_land_all == TRUE, input$color_land, "white")
})


###############################################################################
# Grid values
cruzMapGrid <- reactive({
  list(
    col = input$grid_line_color, lwd = input$grid_line_width,
    lty = input$grid_line_type
  )
})

###############################################################################
# Water color

# Load bathymetry data
cruzMapBathyLoad <- eventReactive(input$depth_file, {
  req(input$depth_file)
  file.in <- input$depth_file

  cruz.list$bathy.xyz <- NULL
  bathy.xyz <- read.csv(file.in$datapath)

  validate(
    need(ncol(bathy.xyz) >= 3,
         "The bathymetric CSV file must contain at least 3 columns")
  )

  cruz.list$bathy.xyz <- bathy.xyz

  NULL
})

# Get color value and bathymetry data for water color
cruzMapColorWater <- reactive({
  if (input$color_water_style == 1) {
    bathy <- NULL

  } else { #if (input$color_water_style == 2) {
    bathy.xyz <- cruz.list$bathy.xyz
    validate(need(bathy.xyz, "Please load a CSV file with bathymetric data"))

    # Make sure lat/lon range matches world2 flag
    bathy.xyz[[1]] <- if (cruz.map.range$world2) {
      ifelse(bathy.xyz[[1]] < 0, bathy.xyz[[1]] + 360, bathy.xyz[[1]])
    } else {
      ifelse(bathy.xyz[[1]] > 180, bathy.xyz[[1]] - 360, bathy.xyz[[1]])
    }

    # Trim, and check that depth file lat/lon spans any map range
    lon.range <- cruz.map.range$lon.range
    lat.range <- cruz.map.range$lat.range
    bathy.xyz.keep <- between(bathy.xyz[[1]], lon.range[1], lon.range[2]) &
      between(bathy.xyz[[2]], lat.range[1], lat.range[2])

    bathy <- try(
      marmap::as.bathy(bathy.xyz[bathy.xyz.keep, ]),
      silent = TRUE
    )

    validate(need(inherits(bathy, "bathy"),
                  paste("Unable to convert the loaded CSV file into a bathy object;",
                        "see `maramp::as.bathy` for data format requirements")))
    validate(need(length(bathy) > 0,
                  "The loaded bathymetric data does not cover any of the current map area")
    )
  }

  list(input$color_water, bathy)
})


###############################################################################
# Download bathymetric data

# Download button for downloading bathymetric file
output$depth_download_button <- renderUI({
  v.val <- input$depth_res
  v.mess <- "Bathymetric data resolution must be a whole number between 0 and 60"

  validate(need(!is.na(v.val), v.mess))
  validate(need(isTRUE(all.equal(v.val %% 1, 0)), v.mess))
  validate(need(between(v.val, 0, 60), v.mess))

  downloadButton("depth_download", "Download bathymetric file")
})

# Message indicating if download using marmap::getNOAA.bathy failed
output$depth_download_message <- renderUI({
  if (cruz.list$bathy.download) {
    validate(
      paste("CruzPlot was not able to resolve host: gis.ngdc.noaa.gov.",
            "Please check your internet connection and try again")
    )
  } else {
    NULL
  }
})

# 'Reset' message if user leaves the page
observe({
  input$tabs
  input$tabset1

  isolate(cruz.list$bathy.download <- FALSE)
})

# Download bathymetric file
output$depth_download <- downloadHandler(
  filename = function() {
    # Defaults maramp file name: "marmap_coord_-135;29;-117;52_res_10.csv"
    paste0(
      paste(
        "marmap_coord",
        paste(cruz.map.range$lon.range[1], cruz.map.range$lon.range[2],
              cruz.map.range$lat.range[1], cruz.map.range$lat.range[2], sep = ";"),
        "res", input$depth_res,
        sep = "_"),
      ".csv"
    )
  },

  content = function(file) {
    cruz.list$bathy.download <- FALSE
    lon.range <- cruz.map.range$lon.range
    lat.range <- cruz.map.range$lat.range
    world2 <- cruz.map.range$world2

    # getNOAA.bathy() operates on -180 to 180 scale; use user inputs not lon.range
    bathy <- try(marmap::getNOAA.bathy(
      lon1 = input$lon_left, lon2 = input$lon_right,
      lat1 = lat.range[1], lat2 = lat.range[2],
      resolution = input$depth_res, antimeridian = world2,
      keep = FALSE
    ), silent = TRUE)

    if (!isTruthy(bathy)) cruz.list$bathy.download <- TRUE
    validate(need(bathy, "Download did not work"))

    write.csv(marmap::as.xyz(bathy), file = file, row.names = FALSE)

  }
)

###############################################################################
