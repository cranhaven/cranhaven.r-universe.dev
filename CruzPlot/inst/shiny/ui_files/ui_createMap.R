# UI code for Create Map tab in CruzPlot

ui.createMap <- function() {
  tabItem(
    tabName = "createmap",
    fluidRow(
      box(
        status = "primary", width = 6,
        conditionalPanel("input.tabset1 == 'Range'", plotOutput("plot1", height = "auto", brush = "map_brush")),
        conditionalPanel("input.tabset1 != 'Range'", plotOutput("plot1b", height = "auto"))
      ),
      tabBox(
        title = "Map", width = 6, id = "tabset1",

        ################################## Panel 1
        tabPanel(
          title = "Range",
          fluidRow(
            box(
              title = "Map range", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              helpText("For longitude values, please use the range -180 to 180.",
                       "For instance, use left and right longitudes of 130 and -110, respectively,",
                       "for a map of the northern Pacific.", tags$br(),
                       "Click the 'Replot map' button after changing map range values,",
                       "or if the map isn't properly sized in the window.", tags$br(),
                       "In addition, users can automatically change the map range input values",
                       "by clicking and holding to draw a box on map, although users still must click 'Replot map'.",
                       "To clear the box, click within the plot outside of the box."),
              fluidRow( #Separate to keep input boxes in line even if labels spill over
                column(3, tags$h5("Left longitude")),
                column(3, tags$h5("Right longitude")),
                column(3, tags$h5("Bottom latitude")),
                column(3, tags$h5("Top latitude"))
              ),
              fluidRow(
                column(3, numericInput("lon_left", NULL, value = start.ll$X[1])),
                column(3, numericInput("lon_right", NULL, value = start.ll$X[2])),
                column(3, numericInput("lat_bot", NULL, value = start.ll$X[3])),
                column(3, numericInput("lat_top", NULL, value = start.ll$X[4]))
              ),
              fluidRow(
                column(3, selectInput("resolution", label = tags$h5("Resolution"),
                                      choices = list("Low" = 1, "High" = 2), selected = start.ll$X[5])),
                column(3, tags$br(), tags$br(), actionButton("map_replot", "Replot map"))
              ),
              tags$span(htmlOutput("map_range_message"), style = "color: red;"),
              tags$h5("Set the map range to a default study area and replot:"),
              actionButton("map_replot_cce", "CCE"),
              actionButton("map_replot_cce2", "Extended CCE"),
              actionButton("map_replot_etp", "ETP"),
              actionButton("map_replot_hawaii", "Hawaii"),
              actionButton("map_replot_hawaiimain", "Main Hawaiian Islands"),
              actionButton("map_replot_marianas", "Marianas")
            ),
            box(
              title = "Scale bar", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              checkboxInput("bar", "Plot scale bar", value = FALSE),
              conditionalPanel(
                condition = "input.bar",
                helpText("Provide the coordinates for the left edge of the scale bar.",
                         "The coordinates must have the same range as the map range coordinates."),
                fluidRow(
                  column(4, uiOutput("scale_lon_uiOut_numeric")),
                  column(4, uiOutput("scale_lat_uiOut_numeric")),
                  column(4, numericInput("scale_width", tags$h5("Width of bar"), value = 2, min = 1, max = 6, step = 1)),
                ),
                fluidRow(
                  column(4, radioButtons("scale_units", tags$h5("Scale bar units"),
                                         choices = list("Kilometers" = 1, "Nautical miles" = 2),
                                         selected = 2)),
                  column(4, uiOutput("out_scale_len"))
                )
              )
            ),
            box(
              title = "Coastline", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              checkboxInput("coast", label = "Use coastline file", value = FALSE),
              conditionalPanel(
                condition = "input.coast",
                helpText("Map limits will automatically be updated to the extent of the",
                         "coastline file. Note: CruzPlot can only process coastline files",
                         "with points are between -180 and 0"),
                fileInput("coast_file", label = tags$h5("Coastline file"), width = "50%")
              )
            )
          )
        ),

        ################################### Panel 2
        tabPanel(
          title = "Planned Transects", value = "planned_transects",
          fluidRow(
            box(
              title = "Planned transects", status = "warning", solidHeader = FALSE, width = 12, collapsible = FALSE,
              fluidRow(
                box(
                  width = 12,
                  tags$strong("Load planned transects"),
                  helpText(paste("Longitudes must be in -180 to 180 range. See the manual for the required CSV file format")),
                  fileInput("planned_transects_file", tags$h5("Load planned transects CSV file"), accept = ".csv"),
                  fluidRow(
                    column(3, uiOutput("planned_transects_lon_uiOut_select")),
                    column(3, uiOutput("planned_transects_lat_uiOut_select")),
                    column(3, uiOutput("planned_transects_num_uiOut_select")),
                    column(3, uiOutput("planned_transects_class1_uiOut_select"))
                  ),
                  fluidRow(
                    column(3, uiOutput("planned_transects_class2_uiOut_select")),
                    column(3, offset = 1, tags$br(), tags$br(), uiOutput("planned_transects_execute_uiOut_button")),
                    column(5, tags$br(), tags$br(), textOutput("planned_transects_text"))
                  ),
                  tags$span(textOutput("planned_transects_message"), style = "color: blue;")
                ),
                conditionalPanel(
                  condition = "output.cruzMapPlannedTransects_Conditional",
                  box(
                    width = 12,
                    tags$strong("Plot loaded planned transects"),
                    checkboxInput("planned_transects_plot", "Plot planned transect lines", value = FALSE),
                    conditionalPanel(
                      condition = "input.planned_transects_plot",
                      column(12, helpText("For the color(s) and (if a class 2 column is specified) the line type(s),",
                                          "select either one or the same number as transect classes or class 2s, respectively.",
                                          "When multiple colors or line types are selected,",
                                          "the order in which transect classes and class 2s are selected to be plotted",
                                          "corresponds to order of specified colors and line types, respectively.")),
                      box(
                        width = 12,
                        ui.select.instructions(),
                        fluidRow(
                          column(6, uiOutput("planned_transects_toplot_uiOut_select")),
                          column(6, uiOutput("planned_transects_color_uiOut_select"))
                        ),
                        fluidRow(
                          column(4, uiOutput("planned_transects_toplot2_uiOut_select")),
                          column(4, uiOutput("planned_transects_lty_uiOut_select")),
                          column(4, numericInput("planned_transects_lwd", tags$h5("Line width"),
                                                 value = 1, min = 0, step = 1))
                        )
                      )
                    )
                  )
                  # box(
                  #   width = 12,
                  #   tags$strong("Remove loaded planned transects"),
                  #   uiOutput("planned_transects_toremove_uiOut_select"),
                  #   uiOutput("planned_transects_toremove_execute_uiOut_button"),
                  #   textOutput("planned_transects_remove_text")
                  # )
                )
              )
            )
          )
        ),

        ################################### Panel 3
        tabPanel(
          title = "Ticks & Labels",
          fluidRow(
            box(
              title = NULL, status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              checkboxInput("tick", label = "Plot tick marks and/or their labels", value = TRUE)
            )
          ),
          fluidRow(
            conditionalPanel(
              condition = "input.tick",
              box(
                title = "Tick marks", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, height = 437,
                fluidRow(
                  column(
                    width = 6,
                    checkboxInput("tick_left", label = "Left", value = TRUE),
                    checkboxInput("tick_bot", label = "Bottom", value = TRUE),
                    numericInput("tick_interval_major", label = tags$h5("Degrees between each major tick"),
                                 value = start.tick$interval, min = 0, max = 45, step = 5),
                    selectInput("tick_style", label = tags$h5("Tick label style"),
                                choices = list("120" = 1, "120W" = 2, "120\u00B0" = 3, "120\u00B0W" = 4),
                                selected = 4)
                  ),
                  column(
                    width = 6,
                    checkboxInput("tick_right", label = "Right", value = TRUE),
                    checkboxInput("tick_top", label = "Top", value = TRUE),
                    numericInput("tick_interval_minor", label = tags$h5("Minor ticks between each major tick"),
                                 value = 4, min = 0, max = 45, step = 1),
                    numericInput("tick_length", label = tags$h5("Tick length"), value = 1.0, min = 0, max = 2.5, step = 0.1)
                  )
                )
              ),
              box(
                title = "Tick labels", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, height = 437,
                fluidRow(
                  column(
                    width = 6,
                    checkboxInput("tick_left_lab", label = "Left", value = TRUE),
                    checkboxInput("tick_bot_lab", label = "Bottom", value = TRUE),
                    numericInput("label_lon_start", tags$h5("Start longitude tick labels at"), value = as.character(start.tick$lon)),
                    selectInput("label_tick_font", label = tags$h5("Tick label font"), choices = font.family, selected = 1)
                  ),
                  column(
                    width = 6,
                    checkboxInput("tick_right_lab", label = "Right", value = TRUE),
                    checkboxInput("tick_top_lab", label = "Top", value = TRUE),
                    numericInput("label_lat_start", tags$h5("Start latitude tick labels at"), value = as.character(start.tick$lat)),
                    numericInput("label_tick_size", label = tags$h5("Tick label size"), value = 1.0, min = 0.1, max = 3, step = 0.1)
                  )
                )
              )
            )
          )
        ),

        #################################### Panel 4
        tabPanel(
          title = "Map Labels",
          fluidRow(
            box(
              title = "Title", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, height = 315,
              textInput("label_title", tags$h5("Map title"), value = ""),
              fluidRow(
                column(6, selectInput("label_title_font", label = tags$h5("Title font"), choices = font.family, selected = 1)),
                column(6, numericInput("label_title_size", label = tags$h5("Title size"), value = 1.5, min = 0.1, max = 3, step = 0.1))
              )
            ),
            box(
              title = "Axis labels", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, height = 402,
              textInput("label_axis_lon", tags$h5("Longitude axis label"), value = ""),
              textInput("label_axis_lat", tags$h5("Latitude axis label"), value = ""),
              fluidRow(
                column(6, selectInput("label_axis_font", label = tags$h5("Axis label font"), choices = font.family, selected = 1)),
                column(6, numericInput("label_axis_size", label = tags$h5("Axis label size"), value = 1.2, min = 0.1, max = 3, step = 0.1))
              )
            )
          )
        ),

        #################################### Panel 5
        tabPanel(
          title = "Color",
          fluidRow(
            box(
              title = "Color style", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,
              helpText("This color style selection will affect the palette options for all color selections in CruzPlot"),
              tags$br(),
              radioButtons("color_style", label = NULL, choices = list("Color" = 1, "Gray scale" = 2),
                           selected = 1)
            ),
            box(
              title = "Land", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,
              fluidRow(
                column(6, checkboxInput("color_land_all", label = "Color all land", value = TRUE)),
                column(
                  width = 6,
                  conditionalPanel(
                    condition = "input.color_land_all",
                    selectInput("color_land", label = tags$h5("Land color"), choices = cruz.palette.color, selected = "bisque1")
                  )
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Water", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,
              checkboxInput("color_lakes_rivers", label = "Color lakes and rivers", value = FALSE),
              selectInput("color_water", label = tags$h5("Water (background) color"),
                          choices = cruz.palette.color, selected = "white"),
              radioButtons("color_water_style", label = tags$h5("Ocean color style"),
                           choices = list("Single color" = 1, "Depth (bathymetric) shading" = 2),
                           selected = 1),
              conditionalPanel(
                condition = "input.color_water_style==2",
                helpText("Load a CSV file with exactly 3 columns: latitude, longitude, and depth"),
                fileInput("depth_file", tags$h5("Bathymetric CSV file"), accept = ".csv"),
                textOutput("bathy_load_text"),
                tags$span(textOutput("bathy_message_text"), style = "color: blue;")
              )
            ),
            box(
              title = "Download bathymetric data", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,
              helpText("Download bathymetric data from NOAA website (see the documentation for",
                       tags$a(href = "https://CRAN.R-project.org/package=marmap",
                              "marmap function 'getNOAA.bathy'"),
                       "for more details).",
                       "The coordinates of the downloaded data will be the same as the current map range.",
                       "After downloading, you must load the CSV file into CruzPlot in the 'Water: Ocean color style' section"),
              numericInput("depth_res", tags$h5("Bathymetric data resolution, in minutes (range: 0-60)"),
                           value = 10, min = 0, max = 60, step = 5),
              uiOutput("depth_download_button"),
              uiOutput("depth_download_message")
            )
          )
        ),

        #################################### Panel 6
        tabPanel(
          title = "Grid",
          fluidRow(
            box(
              title = "Grid", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, height = 385,
              checkboxInput("grid", label = "Include grid lines at major tick marks", value = FALSE),
              conditionalPanel(
                condition = "input.grid",
                fluidRow(
                  column(3, selectInput("grid_line_color", label = tags$h5("Line color"),
                                        choices = cruz.palette.color, selected = "black")),
                  column(3, numericInput("grid_line_width", label = tags$h5("Line width"),
                                         value = 1, min = 1, max = 6, step = 1)),
                  column(3, selectInput("grid_line_type", label = tags$h5("Line type"),
                                        choices = cruz.line.type, selected = 1))
                )
              )
            )
          )
        ),

        #################################### Panel 6
        tabPanel(
          title = "Save",
          fluidRow(
            box(
              title = "Save map", status = "warning", solidHeader = FALSE, width = 12,
              fluidRow(
                column(3, radioButtons("download_format", label = tags$h5("File format"),
                                       choices = list("JPEG" = 1, "PDF" = 2, "PNG" = 3),
                                       selected = 3)),
                column(
                  width = 8,
                  fluidRow(
                    column(6, radioButtons("download_dim", tags$h5("File dimensions"),
                                           choices = list("Use dimensions of plot window" = 1, "Specify dimensions" = 2),
                                           selected = 1)),
                    column(6, numericInput("download_res", tags$h5("Resolution (ppi)"),
                                           value = 300, step = 50, min = 0))
                  ),
                  conditionalPanel(
                    condition = "input.download_dim == 1",
                    helpText("Downloaded map will have the same dimensions as the displayed map")
                  ),
                  conditionalPanel(
                    condition = "input.download_dim == 2",
                    fluidRow(
                      column(6, numericInput("download_width", tags$h5("File width (inches)"),
                                             value = 10, step = 1, min = 0)),
                      column(6, numericInput("download_height", tags$h5("File height (inches)"),
                                             value = 10, step = 1, min = 0))
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.download_format != 1",
                checkboxInput("background_transparent", "Make plot background transparent",
                              value = FALSE)
              ),
              uiOutput("downloadMap_button")
            )
          )
        )
      )
    )
  )
}
