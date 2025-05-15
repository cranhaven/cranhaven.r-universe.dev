# UI code for Plot DAS Data tab in CruzPlot

ui.dasPlot <- function() {
  tabItem(
    tabName = "DASplot",
    fluidRow(
      box(
        status = "primary", width = 6,
        conditionalPanel(
          condition = "input.das_sight_interactive==1",
          conditionalPanel(
            condition = "input.das_effort_interactive==1",
            plotOutput("plot2", height = "auto")
          )
        ),
        conditionalPanel(
          condition = "input.das_sight_interactive==2",
          conditionalPanel(
            condition = "input.das_effort_interactive==1",
            plotOutput("plot3", height = "auto", click = "sight_click", hover = "sight_hover")
          )
        ),
        conditionalPanel(
          condition = "input.das_effort_interactive==2",
          plotOutput("plot4", height = "auto", click = "effort_click", hover = "effort_hover")
        ),
      ),
      tabBox(
        title = "Plot DAS Sightings and Effort", id = "tabset2", width = 6,
        ##############################################################################################################
        tabPanel(
          title = "Data",
          fluidRow(
            box(
              title = "DAS Data", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              fluidRow(
                column(
                  width = 6,
                  helpText("For details about these parameters,",
                           actionLink("das_file_help", "click here"), "or see the official",
                           tags$a(href = "https://swfsc.github.io/swfscDAS/reference/index.html",
                                  "swfscDAS documentation")),
                  fluidRow(
                    column(6, numericInput("das_file_skip", tags$h5("Number of lines to skip before reading each file"),
                                           value = 0, min = 0)),
                    column(6, numericInput("das_file_days_gap", tags$h5("days.gap argument of das_process()"),
                                           value = 20, min = 0, step = 1))
                  ),
                  fluidRow(
                    column(6, selectInput("das_file_reset_event", tags$h5("reset.event argument of das_process()"),
                                          choices = list("TRUE" = 1, "FALSE" = 2), selected = TRUE)),
                    column(6, selectInput("das_file_reset_effort", tags$h5("reset.effort argument of das_process()"),
                                          choices = list("TRUE" = 1, "FALSE" = 2), selected = TRUE))
                  )
                ),
                column(
                  width = 6,
                  helpText("To load DAS data file(s), first set the desired parameters, and then click the \"Browse...\" button and",
                           "select the file(s) you want to load. Hold the Shift key to select multiple files.",
                           "If you change the parameters, you will have to Browse and select the file(s) again.",
                           "To 'remove' a file, browse again and select only the desired DAS file(s)"),
                  fileInput("das_file", label = tags$h5("DAS file input"), multiple = TRUE),
                  textOutput("das_file_load_text"),
                  tags$span(textOutput("das_loaded_text"), style = "color: blue;")
                )
              )
            ),
            box(
              title = "Species codes", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              helpText("Processing DAS sightings requires a species codes file, typically named SpCodes.dat,",
                       "to translate the species codes to scientific or common species names.",
                       "CruzPlot contains a default SpCodes.dat file (last modified 26 May 2020),",
                       "but you can also load your own species codes file.",
                       "This file must follow the same format as the default SpCodes.dat; see the manual for details"),
              ui.new.line(),
              fluidRow(
                column(5, fileInput("das_spcodes_file", tags$h5("Load species codes file"), accept = ".dat")),
                column(6, offset = 1, tags$br(), tags$br(), actionButton("das_spcodes_default", "Load default species codes"))
              ),
              textOutput("spcodes_user_read_text"),
              textOutput("spcodes_default_read_text"),
              tags$span(textOutput("spcodes_message"), style = "color: blue;")
            )
          )
        ),
        ##############################################################################################################
        tabPanel(
          title = "Sightings",
          fluidRow(
            box(
              title = "Sightings", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              conditionalPanel(
                condition = "output.das_loaded_flag == false | output.das_spcodes_loaded_flag == false",
                tags$span(tags$h5("Please load at least one DAS file and a species codes file to use this section"),
                          style = "color: red;")
              ),
              conditionalPanel(
                condition = "output.das_loaded_flag & output.das_spcodes_loaded_flag",
                fluidRow(
                  column(6, checkboxInput("das_sightings", label = tags$h5("Plot sightings"), value = FALSE)),
                  column(
                    width = 6,
                    conditionalPanel(
                      condition = "input.das_sightings",
                      radioButtons("das_sightings_position", tags$h5("Position to plot"),
                                   choices = list("Plot ship position" = 1, "Plot sighting position" = 2),
                                   selected = 1)
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.das_sightings",
                textOutput("das_sight_spcodes_message"),
                helpText("Sighting position is calculated using the ship position, ship course, sighting bearing (angle),",
                         "and radial distance to the sighting.",
                         "If any of these values are NA, then the sighting position will be NA")
              )
            ),
            conditionalPanel(
              condition = "input.das_sightings",
              column(
                width = 12,
                fluidRow(
                  box(
                    title = "Sighting type & species", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,
                    selectInput("das_sighting_type", label = tags$h5("Sighting type"),
                                choices = list("Mammals" = 1, "Turtles" = 2, "Boats" = 3), #, "CPODs" = 4),
                                selected = 1),
                    conditionalPanel(
                      condition = "input.das_sighting_type==1",
                      radioButtons("das_sighting_code_1_all", label = NULL,
                                   choices = list("Plot all mammal sightings" = 1, "Plot selected mammal sightings" = 2),
                                   selected = 2),
                      conditionalPanel(
                        condition = "input.das_sighting_code_1_all==2",
                        ui.select.instructions(),
                        uiOutput("das_sighting_code_1_uiOut_select")
                      ),
                      checkboxInput("das_sighting_probable", label = "Use probable species code", value = FALSE),
                      checkboxGroupInput("das_sighting_events",
                                         label = tags$h5("Plot sightings from",
                                                         actionLink("das_sighting_events_help", "(click here for details)")),
                                         choices = list("S events" = "S", "G events" = "G",
                                                        "K events" = "K", "M events" = "M", "p events" = "p",
                                                        "s events" = "s", "g events" = "g", "k events" = "k"),
                                         selected = c("S"), inline = TRUE),
                      tags$span(uiOutput("cruzDasSightEventResight_uiOut_message"), style = "color: blue;")
                    ),
                    conditionalPanel(
                      condition = "input.das_sighting_type==2",
                      radioButtons("das_sighting_code_2_all", label = NULL,
                                   choices = list("Plot all turtle sightings" = 1, "Plot selected turtle sightings" = 2),
                                   selected = 2),
                      conditionalPanel(
                        condition = "input.das_sighting_code_2_all==2",
                        ui.select.instructions(),
                        uiOutput("das_sighting_code_2_uiOut_select")
                      )
                    )
                  ),
                  box(
                    title = "Symbol properties", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
                    helpText("Not available when 'Plot all...sightings' is selected"),

                    # Mammal or turtle symbol properties for selected species
                    conditionalPanel(
                      condition = "(input.das_sighting_type == 1  & input.das_sighting_code_1_all == 2) | (input.das_sighting_type==2 & input.das_sighting_code_2_all == 2)",
                      helpText("To remove selected species, click the input(s) to remove and then click backspace or delete"),
                      helpText("The order in which species are selected to be plotted",
                               "corresponds to the order of specified symbol properties"),
                      conditionalPanel(
                        condition = "input.das_symbol_mult==false",
                        # checkboxInput("das_symbol_event", "Make symbol colors correspond to event code", value = FALSE),
                        selectInput("das_symbol_type", label = tags$h5("Symbol type(s)"),
                                    choices = cruz.symbol.type, selected = 1, multiple = TRUE),
                        selectInput("das_symbol_color", label = tags$h5("Symbol color(s)"),
                                    choices = cruz.palette.color, selected = "black", multiple = TRUE)
                      ),
                      conditionalPanel(
                        condition = "input.das_symbol_mult",
                        textInput("das_symbol_type_mult", tags$h5("Symbol type(s) - text input"), value = "1"),
                        textInput("das_symbol_color_mult", tags$h5("Symbol color(s) - text input (case sensitive)"), value = "Black")
                      ),
                      fluidRow(
                        column(6, textInput("das_symbol_size", tags$h5("Symbol size(s)"), "1")),
                        column(6, textInput("das_symbol_linewidth", tags$h5("Symbol line width(s)"), "1"))
                      ),
                      checkboxInput("das_symbol_mult", label = "Input symbol properties as text", value = FALSE)
                    ),

                    # Boat (or CPOD) symbol properties
                    conditionalPanel(
                      condition = "input.das_sighting_type==3 | input.das_sighting_type==4",
                      helpText("To remove selected species, click the input(s) to remove and then click backspace or delete"),
                      helpText("The order in which species are selected to be plotted",
                               "corresponds to the order of specified symbol properties"),
                      fluidRow(
                        column(
                          width = 7,
                          selectInput("das_symbol_type_boat", label = tags$h5("Symbol type"),
                                      choices = cruz.symbol.type, selected = 1),
                          numericInput("das_symbol_size_boat", label = tags$h5("Symbol size"),
                                       value = 1, min = 0.1, max = 6, step = 0.1)
                        ),
                        column(
                          width = 5,
                          selectInput("das_symbol_color_boat", label = tags$h5("Symbol color"),
                                      choices = cruz.palette.color, selected = "black"),
                          numericInput("das_symbol_linewidth_boat", label = tags$h5("Symbol line width"),
                                       value = 1, min = 1, max = 6, step = 1)
                        )
                      )
                    )
                    # # CPOD symbol properties
                    #             conditionalPanel(condition = "input.das_sighting_type==4",
                    #                  selectInput("das_symbol_type_cpod", label = tags$h5("Symbol type"),
                    #                              choices = cruz.symbol.type,
                    #                              selected = 1),
                    #                  selectInput("das_symbol_color_cpod", label = tags$h5("Symbol color"),
                    #                              choices = cruz.palette.color,
                    #                              selected = "black"),
                    #                  textInput("das_symbol_size_cpod", label = tags$h5("Symbol size"),
                    #                            value = "1"),
                    #                  textInput("das_symbol_linewidth_cpod", label = tags$h5("Symbol line width"),
                    #                            value = "1")
                    #             )
                    # )
                  )
                )
              ),
              box(
                title = "Interactive sighting labels", status = "warning", solidheader = FALSE, width = 12, collapsible = TRUE,
                fluidRow(
                  column(4, radioButtons("das_sight_interactive", label = NULL,
                                         choices = list("Non-interactive plot" = 1,
                                                        "View and label sightings interactively" = 2),
                                         selected = 1)),
                  column(
                    width = 8,
                    actionButton("das_sight_interactive_reset_last", "Remove last sighting label"),
                    actionButton("das_sight_interactive_reset_all", "Remove all sighting labels")
                  )
                )
              )
            )
          )
        ),
        ##############################################################################################################
        tabPanel(
          title = "Sighting Filters",
          fluidRow(
            conditionalPanel(
              condition = "input.das_sightings==false",
              column(12, helpText("Because 'Plot sightings' is not checked, there are no sightings to filter"))
            ),
            conditionalPanel(
              condition = "input.das_sightings",
              box(
                title = "Sightings to plot", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                fluidRow(
                  column(6, radioButtons("das_sight_effort", label = NULL,
                                         choices = list("On and off effort" = 1, "On effort only" = 2, "Off effort only" = 3),
                                         selected = 2)
                  ),
                  conditionalPanel(
                    condition = "input.das_sight_effort != 2",
                    column(5, helpText("Mode and effort type filters are only available when plotting strictly on effort sightings"))
                  ),
                  conditionalPanel(
                    condition = "input.das_sight_effort == 2",
                    column(3, checkboxGroupInput("das_sight_cp", label = tags$h5("Mode"), #inline = TRUE,
                                                 choices = list("Closing" = "C", "Passing" = "P"),
                                                 selected = c("C", "P"))),
                    column(3, checkboxGroupInput("das_sight_snf", label = tags$h5("Effort type"), #inline = TRUE,
                                                 choices = list("Standard" = "S", "Non-standard" = "N", "Fine" = "F"),
                                                 selected = c("S", "N", "F")))
                  )
                )
              ),
              box(
                title = "Sighting filters", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                fluidRow(
                  column(3, selectInput("das_sight_minBft", label = tags$h5("Minimum Beaufort"),
                                        choices = cruz.beaufort, selected = 0)),
                  column(3, selectInput("das_sight_maxBft", label = tags$h5("Maximum Beaufort"),
                                        choices = cruz.beaufort, selected = 9)),
                  column(6, uiOutput("das_sight_dateRange_uiOut_date")),
                  column(12, helpText("Note that if the min and max Beaufort values are 0 and 9, respectively,",
                                      "then sightings with NA Beaufort values will be plotted"))
                ),
                tags$br(),
                helpText("To stop applying the cruise number(s) and truncation (perpendicular distance) filters,",
                         "delete all text from their boxes"),
                fluidRow(
                  column(4, uiOutput("das_sight_cruise_uiOut_select")),
                  column(4, uiOutput("das_sight_trunc_uiOut_numeric")),
                  column(4, radioButtons("das_sight_trunc_units", tags$h5("Truncation distance units"),
                                         choices = list("Kilometers" = 1, "Nautical miles" = 2),
                                         selected = 2)),
                  column(4, helpText("Note that if any cruise numbers are selected,",
                                     "sightings with an NA cruise number will not be plotted")),
                  column(8, helpText("Only sightings less than or equal to this perpendicular distance from the trackline will be plotted.",
                                     "Sightings with NA perpendicular distance values will not be plotted"))
                )
              )
            )
          )
        ),

        ##############################################################################################################
        tabPanel(
          title = "Effort",
          fluidRow(
            box(
              title = "Effort to plot", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              conditionalPanel(
                condition = "output.das_loaded_flag == false",
                tags$span(tags$h5("Please load at least one DAS file to use this section"), style = "color: red;")
              ),
              conditionalPanel(
                condition = "output.das_loaded_flag",
                fluidRow(
                  column(6, radioButtons("das_effort", label = NULL,
                                         choices = list("No effort lines" = 1, "Simplified effort" = 2, "Detailed effort" = 3),
                                         selected = 1)),
                  conditionalPanel(
                    condition = "input.das_effort != 1",
                    column(3, checkboxGroupInput("das_effort_cp", label = tags$h5("Mode"), #inline = TRUE,
                                                 choices = list("Closing" = "C", "Passing" = "P"),
                                                 selected = c("C", "P"))),
                    column(3, checkboxGroupInput("das_effort_snf", label = tags$h5("Effort type"), #inline = TRUE,
                                                 choices = list("Standard" = "S", "Non-standard" = "N", "Fine" = "F"),
                                                 selected = c("S", "N", "F")))
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.das_effort != 1",
            fluidRow(
              box(
                title = "Line properties", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
                conditionalPanel(
                  condition = "input.das_effort == 2",
                  tags$h5("Simplified effort line color and width"),
                  fluidRow(
                    column(6, selectInput("das_effort_simp_col", NULL, choices = cruz.palette.color, selected = "black")),
                    column(6, numericInput("das_effort_simp_lwd", NULL, value = 2, min = 1, max = 6, step = 1))
                  )
                ),
                conditionalPanel(
                  condition = "input.das_effort == 3",
                  checkboxInput("das_effort_det_byBft", "Show effort by Beaufort", value = TRUE),
                  conditionalPanel(
                    condition = "input.das_effort_det_byBft",
                    helpText("Each selected color corresponds to a Beaufort value, from low to high.",
                             "You must choose at least as many colors as the maximum Beaufort value, plus one.",
                             "Additional selected colors will be ignored.",
                             "The first color corresponds to Beaufort 0, the second color to Beaufort 1, and so on,",
                             "even if the Beaufort filter values change."),
                    conditionalPanel(
                      condition = "input.color_style == 2",
                      tags$span(tags$h5("You cannot plot effort color-coded by Beaufort when using grey scale"),
                                style = "color: red;")
                    ),
                    selectInput("das_effort_det_bft_col", tags$h5("Beaufort colors"),
                                choices = cruz.palette.color, selected = eff.bft.default,
                                multiple = TRUE),
                    numericInput("das_effort_det_bft_lwd", tags$h5("Line width"), value = 2, min = 1, max = 6, step = 1)
                  )
                ),
                conditionalPanel(
                  condition = "input.das_effort_det_byBft == false",
                  conditionalPanel(
                    condition = "output.das_effort_det_s_flag",
                    tags$h5("Standard effort line color and width"),
                    fluidRow(
                      column(6, selectInput("das_effort_det_col_s", NULL, choices = cruz.palette.color, selected = "black")),
                      column(6, numericInput("das_effort_det_lwd_s", NULL, value = 2, min = 1, max = 6, step = 1))
                    )
                  ),
                  conditionalPanel(
                    condition = "output.das_effort_det_n_flag",
                    tags$h5("Non-standard effort line color and width"),
                    fluidRow(
                      column(6, selectInput("das_effort_det_col_n", NULL, choices = cruz.palette.color, selected = "dodgerblue2")),
                      column(6, numericInput("das_effort_det_lwd_n", NULL, value = 2, min = 1, max = 6, step = 1))
                    )
                  ),
                  conditionalPanel(
                    condition = "output.das_effort_det_f_flag",
                    tags$h5("Fine scale effort line color and width"),
                    fluidRow(
                      column(6, selectInput("das_effort_det_col_f", NULL, choices = cruz.palette.color, selected = "green")),
                      column(6, numericInput("das_effort_det_lwd_f", NULL, value = 2, min = 1, max = 6, step = 1))
                    )
                  )
                )
              ),
              box(
                title = "Effort filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
                checkboxInput("das_effort_filter_same", "Same as 'Sighting filters' for Beaufort, date range, and cruise numbers",
                              value = TRUE),
                conditionalPanel(
                  condition = "input.das_effort_filter_same == false",
                  conditionalPanel(
                    condition = "input.das_effort == 3",
                    fluidRow(
                      column(6, selectInput("das_effort_minBft", tags$h5("Minimum Beaufort"), choices = cruz.beaufort, selected = 0)),
                      column(6, selectInput("das_effort_maxBft", tags$h5("Maximum Beaufort"), choices = cruz.beaufort, selected = 9)),
                      column(12, helpText("Note that if the min and max Beaufort values are 0 and 9, respectively,",
                                          "then effort lines with NA Beaufort values will be plotted"))
                    )
                  ),
                  conditionalPanel("input.das_effort == 2", helpText("Only detailed effort lines can be plotted by Beaufort")),
                  uiOutput("das_effort_dateRange_uiOut_date"),
                  uiOutput("das_effort_cruise_uiOut_select"),
                  helpText("Note that if no cruise numbers are selected,",
                           "effort with NA cruise number values will be plotted")
                )
              )
            ),
            fluidRow(
              box(
                title = "Interactive effort labels", status = "warning", solidheader = FALSE, width = 12, collapsible = TRUE,
                conditionalPanel(
                  condition = "input.das_effort != 2",
                  tags$h5("Interactive effort labels can only be used with simplified effort")
                ),
                conditionalPanel(
                  condition = "input.das_effort == 2",
                  helpText("Note that to display interactive effort your cursor",
                           "must be near the start or end points of the effort,",
                           "rather than just the effort line displayed in the map"),
                  fluidRow(
                    column(4, radioButtons("das_effort_interactive", label = NULL,
                                           choices = list("Non-interactive plot" = 1,
                                                          "View and label effort interactively" = 2),
                                           selected = 1)),
                    column(
                      width = 8,
                      actionButton("das_effort_interactive_reset_last", "Remove last effort label"),
                      actionButton("das_effort_interactive_reset_all", "Remove all effort labels")
                    )
                  )
                )
              )
            )
          )
        ),

        ##############################################################################################################
        tabPanel(
          title = "Legends",
          fluidRow(
            box(
              title = "Sightings legend", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              conditionalPanel(
                condition = "input.das_sightings==false",
                helpText("*** No legend for sightings unless 'Plot sightings' is selected in the 'Sightings' tab")
              ),
              conditionalPanel(
                condition = "input.das_sightings",
                checkboxInput("das_legend", label = "Include legend for sightings", value = TRUE),
                conditionalPanel(
                  condition = "input.das_legend",
                  fluidRow(
                    column(
                      width = 4,
                      selectInput("das_legend_pos", label = tags$h5("Position"),
                                  choices = list("Specify" = 1, "Top Left" = "topleft", "Top Right"= "topright",
                                                 "Bottom Left" = "bottomleft", "Bottom Right" = "bottomright"),
                                  selected = "topright"),
                      conditionalPanel(
                        condition = "input.das_legend_pos == 1",
                        numericInput("das_legend_lon", label = tags$h5("Longitude"), value = 0),
                        numericInput("das_legend_lat", label = tags$h5("Latitude"), value = 0)
                      ),
                      selectInput("das_legend_boxCol", label = tags$h5("Box style"),
                                  choices = list("Transparent" = 1, "White" = 2, "White with border" = 3),
                                  selected = 3)
                    ),
                    column(
                      width = 3,
                      selectInput("das_legend_font", tags$h5("Font"), choices = font.family, selected = 1),
                      numericInput("das_legend_textSize", tags$h5("Legend size"), value = 1.0, min = 0.1, max = 3, step = 0.1)
                    ),
                    column(
                      width = 5,
                      textInput("das_legend_title", label = tags$h5("Title (optional)"), value = ""),
                      conditionalPanel(
                        condition = "input.das_sighting_type == 1 | input.das_sighting_type == 2",
                        checkboxGroupInput("das_legend_names", tags$h5("Legend information"),
                                           choices = list("Species code" = 1, "Species abbreviation" = 2,
                                                          "Scientific name" = 3, "Common name" = 4,
                                                          "Include number of sightings" = 5),
                                           selected = c(1, 2, 5))
                      )
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Effort legend", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              conditionalPanel(
                condition = "input.das_effort == 1",
                helpText("*** No legend for effort unless effort is plotted in the 'Effort' tab")
              ),
              conditionalPanel(
                condition = "input.das_effort != 1",
                checkboxInput("eff_legend", label = "Include legend for effort", value = TRUE),
                conditionalPanel(
                  condition = "input.eff_legend",
                  fluidRow(
                    column(4,  selectInput("eff_legend_pos", label = tags$h5("Position"),
                                           choices = list("Specify" = 1, "Top Left" = "topleft", "Top Right"= "topright",
                                                          "Bottom Left" = "bottomleft", "Bottom Right" = "bottomright"),
                                           selected = "bottomleft")),
                    column(4, textInput("eff_legend_title", label = tags$h5("Title (optional)"), value = "")),
                    column(2, selectInput("eff_legend_font", label = tags$h5("Font"), choices = font.family, selected = 1)),
                    column(2, numericInput("eff_legend_textSize", label = tags$h5("Legend size"),
                                           value = 1.0, min = 0.1, max = 3, step = 0.1))
                  ),
                  fluidRow(
                    conditionalPanel(
                      condition = "input.eff_legend_pos == 1",
                      column(4, numericInput("eff_legend_lon", label = tags$h5("Longitude"), value = 0)),
                      column(4, numericInput("eff_legend_lat", label = tags$h5("Latitude"), value = 0))
                    ),
                    column(4, selectInput("eff_legend_boxCol", label = tags$h5("Box color"),
                                          choices = list("Transparent" = 1, "White" = 2, "White with border" = 3),
                                          selected = 3))
                  )
                )
              )
            )
          )
        ),

        ##############################################################################################################
        tabPanel(
          title = "Tabular Output",
          fluidRow(
            box(
              title = "Effort", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              conditionalPanel("input.das_effort == 1", helpText("Effort must be plotted to generate tabular output for effort")),
              conditionalPanel(
                condition = "input.das_effort != 1",
                helpText("Reports the distance traveled while on effort, summarized by effort type.",
                         "The effort that is summarized is the same as the effort that is plotted,",
                         "i.e. it has been filtered using the same filters",
                         "(mode, effort type, date, Beaufort, and cruise number)",
                         "specified in the 'Effort' tab"),
                radioButtons("das_out_effort_units", label = "", #NULL puts the widget too close to the helpText
                             choices = list("Distance unit: kilometers" = 1, "Distance unit: nautical miles" = 2),
                             selected = 2),
                tableOutput("das_out_effort_table"),
                downloadButton("das_out_effort_save", "Save effort table")
              )
            ),
            box(
              title = "Sightings", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              conditionalPanel(
                condition = "input.das_sightings != true",
                helpText("Sightings must be selected to generate tabular output for sightings")
              ),
              conditionalPanel(
                condition = "input.das_sightings",
                helpText("Reports the number of sightings with the selected species, summarized by effort type.",
                         "The sightings that are summarized are those that are plotted,",
                         "i.e. they have been filtered using the same filters",
                         "(species, on or off effort, mode, effort type, date, Beaufort, cruise number, and truncation distance)",
                         "specified in the 'Filters' tab"),
                tags$br(),
                tags$h5("Total number of sightings in the DAS file for selected events and species,",
                        "with only a filter for species code applied (if applicable).",
                        "Note that for instance when plotting all marine mammal species,",
                        "an S sighting with two species counts as two in this sum.",
                        "These values are intended to be a reference for the total number of sightings in the DAS file(s)"),
                tableOutput("das_out_sight_tot_table"),
                checkboxGroupInput("das_out_sciname", tags$h5("Additional species information to include"),
                                   choices = list("Species abbreviation" = 2,
                                                  "Scientific name" = 3, "Common name" = 4),
                                   selected = NULL, inline = TRUE),
                checkboxInput("das_out_allcheck", "Include an 'All' summary row", value = TRUE),
                tableOutput("das_out_sight_table"),
                downloadButton("das_out_sight_save", "Save sightings table")
              )
            )
          )
        )
      )
    )
  )
}
