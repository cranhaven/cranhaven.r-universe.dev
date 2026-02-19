#' 360_image UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_360_image_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(
        HTML(".shiny-notification {
              height: 50px;
              width: 200px;
              position:fixed;
              top: calc(50% - 25px);;
              left: calc(50% - 100px);;
            }"
        )
      )
    ),
    # Div to hold the select input and button, using flexbox for alignment
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
      # Modify selectInput style to include margin on the right for spacing
      div(shinyWidgets::pickerInput(
        inputId =  ns("img_dd"),
        label = "Image To Annotate:",
        choices = "",
        multiple = FALSE,
        width = "100%",
        #selected = 1
        options = list(title = "THIRD: Select an image to annotate....")
      ), #%>% shinyhelper::helper(type = "markdown", content = "image_loader", icon = "question-circle"),
      style = "flex-grow: 1; margin-right: 5px;"), # Adjusted for spacing and flexible width
      shiny::uiOutput(ns("toggleButton"), style = "align-self: flex-end; margin-bottom: 8px; width: 95%;")
    ),

    # Container to hold both the Leaflet output and the iframe
    div(style = "position: relative; height: 750px;", # Ensuring container has a defined height
        div(id = ns("leaflet360Container"),
            style = "position: absolute; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;",
            leaflet::leafletOutput(ns("leaflet360"), height = "100%")
        )
        ,
        div(id = ns("panoContainer"),
            style = "position: absolute; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;",
            # # Iframe output
            uiOutput(ns("pano_iframe"), style = "height: 100%;")
        )
    ),

  )
}

#' 360_image Server Functions
#'
#' @noRd
mod_360_image_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #set the image dropdown to load when the kmz is unzipped and r$imgs_lst is changed
    observe({
      req(r$imgs_lst)
      #changed this to automatcally select the first image as the pin dropping needs it to have one
      shinyWidgets::updatePickerInput(session = session, inputId = "img_dd", choices = r$imgs_lst, selected = r$imgs_lst[1], options = list(title = "Now Select an image to annotate it."))
      if(myEnv$config$showPopupAlerts == TRUE){
        shinyWidgets::show_alert(
          title = "ALMOST SET.. Now annotate the images",
          text = "You can change the image using the dropdown in the middle and use the buttons on the right to start annotating.",
          type = "info"
        )
      }

    }) %>% bindEvent(r$imgs_lst)

    #setup to watch for a new image loaded into the 360 viewer and change the metadata
    observe({
      #print("image dropdown changed")
      r$current_image <- input$img_dd
      req(r$current_image)
      #print(paste0(app_sys("/app/www/files/"),"/",input$img_dd))
      r$current_image_metadata <- get_image_metadata(r$imgs_metadata, r$current_image)
      #print(r$user_annotations_file_name)
    }) %>% bindEvent(input$img_dd)


    # Render the pano_iframe
    output$pano_iframe <- renderUI({

      ###############################################
      # version for if overlays drawn on 360 as png
      # Remove the .jpg extension and replace with .png to check for its existence
      #png_version <- gsub(".jpg", ".png", r$current_image)

      # Check if the PNG file exists in the 'files/' directory
      # if (file.exists(paste0(app_sys("app/www/files"), "/", png_version))) {
      #   image_to_use <- png_version
      # } else {
      #   image_to_use <- r$current_image
      #   print(r$current_image)
      #   #temp_dir <- tempdir()
      #   #image_to_use <- file.path(temp_dir, r$current_image)
      # }

      image_to_use <- paste0("/temp_dir/files/", r$current_image)
      #print(r$current_image)
      #temp_dir <- tools::R_user_dir("pannotator")

      # Construct the URL using the determined image file
      src_url <- paste0("www/pannellum.htm#panorama=", image_to_use,
                        "&autoLoad=true&autoRotate=0&ignoreGPanoXMP=true")

      #print(utils::URLencode(src_url))
      ###############################################

      ## Construct the iframe URL
      # src_url <- paste0("www/pannellum.htm#panorama=",
      #                   "files/",r$current_image,
      #                   "&autoLoad=true&autoRotate=0&ignoreGPanoXMP=true")

      tags$iframe(src = utils::URLencode(src_url), width = "100%", height = "750px")#%>% shinyhelper::helper(type = "markdown", content = "image_loader", icon = "question-circle fa-lg")
    })

    #just for changing the button label and icon
    toggleState <- reactiveVal(TRUE)  # TRUE for 'Draw', FALSE for 'View'
    # Dynamic UI for the action button
    output$toggleButton <- renderUI({
      shiny::div(style = "text-align: right;",  # Aligns all content in the div to the right
                 shiny::tagList(
                   # The main toggle button
                   shiny::actionButton(
                     inputId = ns("togglePano"),
                     label = if (toggleState()) "Switch To Drawing Mode" else "Switch To Viewing Mode",
                     icon = shiny::icon(if (toggleState()) "draw-polygon" else "globe"),
                     style = "margin-bottom: 6px; margin-right: 5px; width: 95%;"  # Added margin-right for spacing
                   ),

                   # Additional button for exporting polygons as images
                   if (!toggleState() && !is.null(r$current_annotation_360polygons) && nrow(r$current_annotation_360polygons) > 0) {
                     shinyFiles::shinyDirButton(id=ns("exportPolygonsAsImages"), label='Export Cropped Polygon Images', title='Please select a folder to export the cropped images into :)', icon=icon("download"), multiple=FALSE, viewtype="list", style = "margin-right: 5px; width: 95%;")
                   }
                 )
      )
    })


    # export Polygons button
    observe({
      #print("Export Polygons Clicked")
      if(length(r$current_annotation_360polygons) < 0) {
        #print( "no polygons to export")
      }

      # Determine the path to the Documents folder consistently across platforms
      #home_dir <- fs::path_home()
      #documents_dir <- file.path(home_dir)
      #volumes <- c(Documents = fs::path_home(), "R Installation" = R.home(),shinyFiles::getVolumes()())

      # Create volumes list containing only the Documents folder
      volumes <- c(shinyFiles::getVolumes()())

      if (is.integer(input$exportPolygonsAsImages)) {
        cat("No directory has been selected (shinyDirChoose)")
        shinyFiles::shinyDirChoose(input,"exportPolygonsAsImages", roots = volumes, session = session, defaultPath = "", defaultRoot = NULL, allowDirCreate = TRUE)
      } else {

        save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)

        annotations_export_dir <- shinyFiles::parseDirPath(volumes, input$exportPolygonsAsImages)
        #print(annotations_export_dir)

        #added progressIndicator in function
        create_cropped_polygons_from_360_images(annotations_export_dir)

        #export_success <-  create_cropped_polygons_from_360_images(annotations_export_dir)

        # if(export_success == "success"){
        #   print("the export was successful")
        # }

        shinyWidgets::show_alert(
          title = "Export Successful!",
          text = HTML(paste0("The cropped images are in:<br>", annotations_export_dir )),
          html = TRUE,
          type = "success"
        )

      }

    }) %>% bindEvent(input$exportPolygonsAsImages)

    # Toggle the visibility of the Leaflet360 map and the iframe
    observe({
      #print("toggling pano frame")
      # Toggle the current state
      toggleState(!toggleState())
      shinyjs::toggle(id="panoContainer", anim=TRUE)
    }) %>% bindEvent(input$togglePano)

    # triggered when the current image changes
    observe({
      #print("r$current_image changed: mod_360_image")
      req(r$imgs_lst, r$current_image)
      output$leaflet360 <- addCurrentImageToLeaflet360()

      #TODO check if this fixes the current_annotations
      r$current_annotation_360markers <- NULL
      r$current_annotation_360polygons <- NULL

      previous_annotations_360 <- check_for_annotations(r$user_annotations_data, r$current_image)

      if(nrow(previous_annotations_360 > 1)){
        #print("annotations already exist")
        add_annotations_to_360()
      }

      # code for auto updating dropdown if leaflet_map is clicked
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "img_dd",
        choices = r$imgs_lst,
        selected = r$current_image,  # Automatically select the current image
        options = list(title = "Now Select an image to annotate it.")
      )

    }) %>% bindEvent(r$current_image)


    # triggered when new leaflet item added to leaflet360
    observe({
      feature <- input$leaflet360_draw_new_feature
      req(feature, r$current_image)  # Make sure there is a new feature before proceeding
      #print("leaflet360_draw_new_feature triggered: mod_360_image")

      #utils::str(feature)
      layerId <- feature$properties$layerId
      #print(paste0("layerId: ", layerId))

      clear_drawn_annotation_from_360(session, layerId)

      # Generate a unique ID for the feature
      myId <- gsub("\\.", "",format(Sys.time(), "%Y%m%d-%H%M%OS3"))
      # Generate an ID based on the current date and time only if there's no existing ID
      if (is.null(feature$properties$id)) {
        feature$properties$id <- myId
        feature$properties$feature_type <- paste0(feature$geometry$type, "-360")
      }

      # now add feature to reactive so it can trigger in other modules
      r$new_leaflet360_item <- feature

    }) %>% bindEvent(input$leaflet360_draw_new_feature)  # Make sure to bind to the drawing event


    # triggered when item edited using drawToolbar
    observe({
      editedFeatures <- input$leaflet360_draw_edited_features
      req(editedFeatures)  # Make sure there is an edited feature before proceeding
      #utils::str(editedFeatures)
      #str <- sprintf("Edited feature with layerId: %s", editedFeatures)
      #print(str)

      #layer_id <- editedFeatures$properties$layerId
      #print("removing edited one")
      #clear_drawn_annotation_from_leaflet(session, layerId)  # pass the correct layer ID

      myMarker <- geojsonsf::geojson_sf(jsonify::to_json(editedFeatures, unbox = TRUE, digits=9))
      geom <- sf::st_as_text(myMarker$geometry, digits=9)

      myGeometry <- geom

      r$user_annotations_data <- edit_annotation_data(myUserAnnotationsData = r$user_annotations_data, myId = editedFeatures$properties$layerId, myGeometry=myGeometry)
      save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)

    }) %>% bindEvent(input$leaflet360_draw_edited_features)  # Ensure the observe event triggers upon feature edits


    # triggered to add a single item to the 360 from control form
    observe({
      #print("new 360 item: leaflet360")
      #print(r$new_leafletMap_item)

      add_annotations_to_360()

      #TODO NOT SURE THIS IS THE CORRECT PLACE TO HAVE THIS
      #call the function to add the overlay for an equirectangular
      #image to be drawn and generate a png to load in panellum

    }) %>% bindEvent(r$new_leaflet360_item)

    # remove_leaflet_item
    observe({
      #print("remove_leaflet_item: 360")
      req(r$remove_leaflet360_item)
      remove_360_item()

    }) %>% bindEvent(r$remove_leaflet360_item)

    # refresh user config settings on applySettingsButton click
    observe({
      #print("refresh_leaflet_item: 360")
      req(r$refresh_user_config, r$current_image)
      #output$leaflet360 <- addCurrentImageToLeaflet360()
      add_annotations_to_360()
    }) %>% bindEvent(r$refresh_user_config)

  })
}

## To be copied in the UI
# mod_360_image_ui("pano360_image")

## To be copied in the server
# mod_360_image_server("pano360_image")
