# app.R for CruzPlot

###############################################################################
# Check for and attach packages
# list.packages <- list(
#   "dplyr", "DT", "geosphere", "mapdata", "marmap", "maps", "shiny",
#   "shinydashboard", "shinyjs", "stringr", "swfscDAS"
# )
#
# p.check <- vapply(list.packages, requireNamespace, as.logical(1), quietly = TRUE)
# if (!all(p.check))
#   stop("To use CruzPlot, the following packages must be installed: ",
#        paste(list.packages, collapse = ", "), "\n",
#        "To install the missing packages, run the following:\n",
#        "install.packages(c(\"", paste(list.packages[!p.check],
#                                       collapse = "\", \""), "\"))")
#
# sapply(list.packages, require, character.only = TRUE, warn.conflicts = FALSE)

stopifnot(
  require(CruzPlot),
  require(dplyr),
  require(DT),
  require(geosphere),
  require(mapdata),
  require(marmap),
  require(maps),
  require(shiny),
  require(shinydashboard),
  require(shinyjs),
  require(stringr),
  require(swfscDAS)
)


###############################################################################
# ### Read default values for map - better way to do this..?
# source(file.path("server_files", "server_funcs.R"), local = TRUE, chdir = TRUE)
#
# if(file.exists("StarterVals.csv")) {
#   start.ll <- suppressWarnings(read.csv("StarterVals.csv", header = TRUE))
# } else {
#   start.ll <- data.frame(X = c(-180, -110, 0, 33, 1))
# }
#
# start.tick <- NULL
# start.tick$interval <- cruzTickUpdate(c(start.ll$X[2], start.ll$X[1]), c(start.ll$X[4], start.ll$X[3]))
# start.tick$lon <- cruzTickStart(c(start.ll$X[1], start.ll$X[2]), start.tick$interval)
# start.tick$lat <- cruzTickStart(c(start.ll$X[3], start.ll$X[4]), start.tick$interval)


### Set default values for map - keep this format in case they need to be user-provided
start.ll <- data.frame(X = c(-135, -117, 29, 52, 1))
start.tick <- list(interval = 5, lon = -135, lat = 30)


###############################################################################
##### Assorted other stuff...
old <- options()
on.exit(options(old))

options(shiny.maxRequestSize = 50 * 1024^2) # Max file size is 50MB
options("digits" = 5)   # for proper display of sighting and effort coordinates

plot.res <- 72 #Resolution of displayed plots; passed torenderPlot() calls

jscode <- "shinyjs.closeWindow = function() { window.close(); }"


source(file.path("app_vals.R"), local = TRUE, chdir = TRUE)


###############################################################################
##### UI
ui.new.line <- function() helpText(HTML("<br/>"))
ui.select.instructions <- function() {
  helpText("To remove selected input(s): click the input(s) to remove, ",
           "and then click backspace or delete")
}


# Load files with UI code
source(file.path("ui_files", "ui_createMap.R"), local = TRUE, chdir = TRUE)
source(file.path("ui_files", "ui_dasPlot.R"), local = TRUE, chdir = TRUE)
source(file.path("ui_files", "ui_nonDasPlot.R"), local = TRUE, chdir = TRUE)
source(file.path("ui_files", "ui_other.R"), local = TRUE, chdir = TRUE)


# UI function
ui <- dashboardPage(
  dashboardHeader(title = "CruzPlot", titleWidth = "200"),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Create and Save Map", tabName = "createmap", icon = icon("th", lib = "font-awesome")),
      menuItem("Plot DAS Data", tabName = "DASplot", icon = icon("th")),
      menuItem("Plot Non-DAS Data", tabName = "nonDASplot", icon = icon("th")),
      menuItem(HTML(paste0("Color and Formatting", "<br/>", "Options")), tabName = "dispColor", icon = icon("th")),
      menuItem("Species Information", tabName = "dispSp", icon = icon("th")),
      menuItem("CruzPlot Manual", tabName = "dispManual", icon = icon("th")),
      tags$br(),
      fileInput("load_app_envir_file", "Load workspace"),
      column(
        width = 12,
        textOutput("load_app_text"),
        downloadButton("save_app_envir", "Save workspace", style = "color: black")
      ),
      tags$br(), tags$br(), tags$br(),
      numericInput("map_size", "Map height (pixels)", value = 600, min = 0, step = 100),
      tags$br(),
      actionButton("stop", "Close CruzPlot"),
      column(12, tags$h5(paste0("CruzPlot v", packageVersion("CruzPlot"))))
    ), width = "200"
  ),

  dashboardBody(
    useShinyjs(),
    # See https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window
    extendShinyjs(text = jscode, functions = c("closeWindow")),

    # See https://stackoverflow.com/questions/59760316/change-the-color-of-text-in-validate-in-a-shiny-app
    tags$head( #validation text
      tags$style(HTML("
                      .shiny-output-error-validation {
                      color: red; font-weight: bold;
                      }
                      "))
    ),

    # See https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
    tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),

    tabItems(
      ui.createMap(),
      ui.dasPlot(),
      ui.nonDasPlot(),
      ui.dispColor(),
      ui.dispSp(),
      ui.dispManual()
    )
  )
)


###############################################################################
##### server
server <- function(input, output, session) {
  ### Quit GUI
  session$onSessionEnded(function() {
    stopApp(returnValue = "CruzPlot was closed")
  })

  observeEvent(input$stop, {
    js$closeWindow()
    stopApp(returnValue = "CruzPlot was closed")
  })


  #----------------------------------------------------------------------------
  ### Map tab
  map.height <- reactive(input$map_size)

  source(file.path("server_1_map", "cruzMapCoastline.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapColorGrid.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapLabel.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapPlannedTransects.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapRange.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapScaleBar.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapTick.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapSave.R"), local = TRUE, chdir = TRUE)


  #----------------------------------------------------------------------------
  ### DAS data tab
  # Read Species codes and renderUI for mammal and turtle codes
  source(file.path("server_files", "cruzSpeciesCodes.R"), local = TRUE, chdir = TRUE)

  # Load DAS file, and do related actions
  source(file.path("server_2_das", "cruzDasGeneral.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasRenderUI.R"), local = TRUE, chdir = TRUE)

  source(file.path("server_2_das", "cruzDasSightProcess.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasSightFilter.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasSightRange.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasSightSymbol.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasSightLegend.R"), local = TRUE, chdir = TRUE)

  source(file.path("server_2_das", "cruzDasEffortEvent.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasEffortOther.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasEffortLegend.R"), local = TRUE, chdir = TRUE)

  source(file.path("server_2_das", "cruzDasInteractive.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasTabular.R"), local = TRUE, chdir = TRUE)


  #----------------------------------------------------------------------------
  ### Non-DAS data tab
  source(file.path("server_files", "cruzNonDas.R"), local = TRUE, chdir = TRUE)


  #----------------------------------------------------------------------------
  ### Other
  output$manual_out <- renderUI({
    style.txt <- paste0("height:", map.height(), "px; width:100%; scrolling=yes")
    tags$iframe(style = style.txt, src = "CruzPlot_Manual_app.pdf")
  })
  source(file.path("server_files", "cruzDisplaySymbolProp.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzWorld2DataRange.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "server_reactiveValues.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "server_render.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "server_funcs.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "server_color.R"), local = TRUE, chdir = TRUE)


  ### Other output - static plot

  plotMap <- reactive({
    function() {
      # The on.exit call causes the coordinates, eg from a click or brush event,
      #   to not be scaled to the data space, aka their range is 0-1.
      #   This is ok because the only par calls in CruzPlot are around legend
      #   calls for the sake of the font family.
      # oldpar <- par(no.readonly = TRUE)
      # on.exit(par(oldpar))

      source(file.path("server_files", "cruzDraw.R"), local = TRUE, chdir = TRUE)
    }
  })
}

shiny::shinyApp(ui = ui, server = server)
