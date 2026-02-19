#' leaflet_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_leaflet_map_ui <- function(id){
  ns <- NS(id)

  tagList(

    #this deletes the map layer by layerId
    tags$head(tags$script(HTML("
  Shiny.addCustomMessageHandler(
    'removeleaflet',
    function(data){
      var map = HTMLWidgets.find('#' + data.elid).getMap();
      var layer = map._layers[data.layerId];
      if(layer) {
        map.removeLayer(layer);
      }
    }
  )
"))),

    #TODO need to look at moving scripts to external .js files
    #for putting JS externally
    #tags$head(tags$script(src = "handlers.js") ),

    fileInput(ns("kmz_file"), "Load A .kmz File:",
              accept = c(".kmz"), placeholder = "SECOND: Select a google earth (.kmz) file..."
    ) |> shinyhelper::helper(type = "markdown",
                             content = "kmz_file_loader_help",
                             icon = "question-circle"),
    textOutput(ns("fileDetails")),
    shinyWidgets::progressBar(id = ns("pb1"), value = 0, title = ""),
    leaflet::leafletOutput(ns("mymap"), height = 720),
    fileInput(ns("overlay_file"), "Load An Overlay:", accept = c(".kml"), placeholder = "Use this to add an overlay (.kml only...)"
    ) |> shinyhelper::helper(type = "markdown",
                             content = "kml_overlay_loader_help",
                             icon = "question-circle"),
    textOutput(ns("fileDetails"))
  )
}

#' leaflet_map Server Functions
#'
#' @noRd
mod_leaflet_map_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #output$mymap <- loadBaseLeafletMap()

    #shinyjs::disable("kmz_file")
    #shinyjs::hide("overlay_file")

    observe({
      shinyWidgets::updateProgressBar(session = session, id = "pb1", value = 50, title = "Checking files in kmz & loading map.")

      files_extracted <- unzipKmz(input$kmz_file$datapath)

      shinyWidgets::updateProgressBar(session = session, id = "pb1", value = 75, title = "Loading image metadata")
      #r$imgs_metadata <- load_image_metadata(app_sys("/app/www/files"))
      r$imgs_metadata <- load_image_metadata(file.path(tempdir(), "files"))
      #View(r$imgs_metadata)

      shinyWidgets::updateProgressBar(session = session, id = "pb1", value = 100, title = files_extracted)

      #r$imgs_lst <- get_image_files(app_sys("/app/www/files"))
      r$imgs_lst <- get_image_files(file.path(tempdir(), "files"))

      #fName <- paste0(app_sys("/app/www/doc.kml"))
      fName <- file.path(tempdir(), "doc.kml")

      myKml <- readr::read_file(fName)

      output$mymap <- loadBaseLeafletMap(kml=myKml)
      #shinyjs::show("overlay_file")
      golem::invoke_js("showid", "image_panel")

    }) %>% bindEvent(input$kmz_file)

    observe({
      #myOverlayMap <- readr::read_file(input$overlay_file$datapath)
      addMapOverlay(input$overlay_file)

      if(myEnv$config$showPopupAlerts == TRUE){
        shinyWidgets::show_alert(
          title = "Map Overlay Loaded!",
          text = "Added the map overlay to the map panel.",
          type = "info"
        )
      }
    })%>% bindEvent(input$overlay_file)


    #if the current image viewed changes
    observe({
      #print("current image changed: mod_leaflet_map")
      req(r$current_image_metadata, r$current_image)

      r$current_map_zoom <- input$mymap_zoom

      addCurrentImageToMap()

      previous_annotations_map <- check_for_annotations(r$user_annotations_data, r$current_image)

      if(nrow(previous_annotations_map > 1)){
        #print("annotations already exist")
        add_annotations_to_map()

      }

    }) %>% bindEvent(r$current_image)


    # triggered when item added using drawToolbar
    observe({
      feature <- input$mymap_draw_new_feature
      req(feature, r$current_image)  # Make sure there is a new feature before proceeding
      #print("mymap_draw_new_feature triggered: mod_leaflet_map")

      #utils::str(feature)
      #print(feature)
      layerId <- feature$properties$layerId

      clear_drawn_annotation_from_map(session, layerId)  # clear the item as it will be added back in the next step
      # Generate a unique ID for the feature
      myId <- gsub("\\.", "",format(Sys.time(), "%Y%m%d-%H%M%OS3"))
      # Generate an ID based on the current date and time only if there's no existing ID
      if (is.null(feature$properties$id)) {
        feature$properties$id <- myId
        feature$properties$feature_type <- paste0(feature$geometry$type, "-map")
      }

      # now add feature to reactive so it can trigger in other modules
      r$new_leafletMap_item <- feature

    }) %>% bindEvent(input$mymap_draw_new_feature)  # Make sure to bind to the drawing event

    # triggered when item edited using drawToolbar
    observe({
      editedFeatures <- input$mymap_draw_edited_features
      req(editedFeatures)  # Make sure there is an edited feature before proceeding
      #utils::str(editedFeatures)
      #str <- sprintf("Edited feature with layerId: %s", editedFeatures)
      #print(str)

      myMarker <- geojsonsf::geojson_sf(jsonify::to_json(editedFeatures, unbox = TRUE, digits=9))
      geom <- sf::st_as_text(myMarker$geometry, digits=9)

      myGeometry <- geom

      r$user_annotations_data <- edit_annotation_data(myUserAnnotationsData = r$user_annotations_data, myId = editedFeatures$properties$layerId, myGeometry=myGeometry)
      save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)

    }) %>% bindEvent(input$mymap_draw_edited_features)  # Ensure the observe event triggers upon feature edits

    # triggered to add a single item to the map from control form
    observe({
      #print("new map item: leaflet")
      req(r$new_leafletMap_item)
      add_annotations_to_map()
    }) %>% bindEvent(r$new_leafletMap_item)


    # remove_leafletMap_item
    observe({
      #print("remove_map_item: leaflet")
      req(r$remove_leafletMap_item)
      remove_map_item()
    }) %>% bindEvent(r$remove_leafletMap_item)

    # observe clicks on the map (kml) loaded when someone clicks on a yellow marker
    observe({
      req(input$mymap_geojson_click)
      click <- input$mymap_geojson_click
      #print("geojson marker clicked")
      #print(click$properties$name)
      r$current_image <- paste0(click$properties$name)
      r$current_image_metadata <- get_image_metadata(r$imgs_metadata, r$current_image)
    }) %>% bindEvent(input$mymap_geojson_click)

    # refresh_leaflet_item when user clicks ApplySettingsButton
    observe({
      #print("refresh_leaflet_item: map")
      req(r$refresh_user_config, r$current_image)
      #fName <- paste0(app_sys("/app/www/doc.kml"))
      temp_dir <- tempdir()
      fName <- file.path(temp_dir, "/doc.kml")
      myKml <- readr::read_file(fName)
      output$mymap <- loadBaseLeafletMap(kml=myKml)
      addCurrentImageToMap()
      add_annotations_to_map()
    }) %>% bindEvent(r$refresh_user_config)

  })
}

## To be copied in the UI
# mod_leaflet_map_ui("leaflet_map")

## To be copied in the server
# mod_leaflet_map_server("leaflet_map")
