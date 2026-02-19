#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#'
# TODO
# fix panellum displaying error on first load without image option - (DONE)
# fix purple image (current image) staying when new new kmz - (DONE)
# check latitude and longitude have - for S etc...(DONE)
# check themes on all shinyWidgets - switching to bslib?
# warning popup on change of lookups in the settings
# possibly add a wait icon progressbar on export cropped images - (DONE)
# fix icon on whole image icon not showing (DONE)
# fix polygon fill opacity on export images (DONE)
# add export cropped polygons from image - (DONE)
# add geocode metadata to exported images - (DONE)
# add help rmds - (DONE)
# add text in control form for current image filename - (DONE)
# add function for drawing overlay to PNG FOR PANNELLUM (STARTED)
# look into polygon dissapearing after being drawn, annotation added but the polygon not drawn.
# need to look at all usernames being changed on edits from file happened when un-hooked username to rds.
# look at setting bounds for drawing on 360 images as currently one can draw polygons outside the pixels of the image
# fix stroke on polygons not show when stroke unchecked
# fix settings panel horizontal scroll bar from appearing
# make data load lowercase jpg and JPG when loading image metadata.
# move custom js to handlers.js file
# add stoke/fill options for overlay maps in settings panel
# add dashed line option for polygons
# add warning popup when changing lookup settings
# add click on images in the mapping panel to open them in the image panel
# add collection of annotation items so they can all be collapsed together
# add overlays to panellum
# add dashed line option to polygons
# check white space around exported images from crops
# add option to export png's and jpgs
# add functions/button to delete all annotations for image.
# add remove overlay button
# add zoom to overlay button
# zoom to extents of polygons drawn on map when r$current_image changes
# write unit test functions
# add 'restore defaults' button in settings for
# make kmz browse progress bars hide/switch to progressbar on exports.
# have warnings popup to reload page when changing username lookup file in settings
# fix it so that the lookups default and work even if someone deletes the files from the system.
# switch to |> instead of %>% pipes
# TODO

# save the user config
save_user_config <- function(config_var){
  #print("user config saved!")
  req(r$config, myEnv$project_config_file)
  configr::write.config(config.dat = r$config, file.path = myEnv$project_config_file, write.type = "yaml", indent = 4)
  return("user config saved!")
}

# called on clicking the 'Apply Settings' button in the settings form
refresh_user_config <- function(session){
  #print("refreshing user config")
  #session$reload()
  #Use runjs to run JavaScript code for reloading the page
  #shinyjs::runjs('window.location.reload();')
  r$refresh_user_config <- utils::timestamp()
  return("user config refreshed!")
}

# get image files from folder
get_image_files <- function(folderToUse){
  imgs_fn <- list.files(folderToUse, pattern = "JPG$|JPEG$", ignore.case = TRUE, recursive = FALSE, full.names = FALSE)
  #golem::invoke_js("showid", "image_panel")
  return(imgs_fn)
}

# use exiftools to read image metadata
load_image_metadata <- function(directory){
  file_extension <- "\\.jpg$"
  my_files <- list.files(directory, pattern=paste0(file_extension), all.files=FALSE, full.names=TRUE)
  files_df <- exiftoolr::exif_read(path=my_files, tags = c("-G1", "-a", "-s"))
  #View(files_df)
  return(files_df)
}

# get the Image meta data
get_image_metadata <- function(files_df, imageToGet){
  #print("get_image_metadata called")
  #print(imageToGet)
  colnames(files_df)[which(colnames(files_df)=="FileName")] <- "FileName"
  colnames(files_df)[which(colnames(files_df)=="GPSLatitudeRef")] <- "GPSLatitudeRef"
  colnames(files_df)[which(colnames(files_df)=="GPSLatitude")] <- "GPSLatitude"
  colnames(files_df)[which(colnames(files_df)=="GPSLongitudeRef")] <- "GPSLongitudeRef"
  colnames(files_df)[which(colnames(files_df)=="GPSLongitude")] <- "GPSLongitude"
  newdata <- files_df[which(files_df$FileName==imageToGet),]
  #View(newdata)
  return(newdata)
}

# Write latitude and longitude metadata back to an image
write_image_gps_metadata <- function(image_file, latitude, latitude_ref, longitude, longitude_ref) {
  #print("write_image_metadata called")
  # Construct the commands to update the GPS metadata
  gps_latitude_command <- paste0("-GPSLatitude=", latitude)
  gps_latitude_ref_command <- paste0("-GPSLatitudeRef=", latitude_ref)
  gps_longitude_command <- paste0("-GPSLongitude=", longitude)
  gps_longitude_ref_command <- paste0("-GPSLongitudeRef=", longitude_ref)

  # Execute the ExifTool command to update the image's GPS metadata
  exiftoolr::exif_call(
    args = c( "-overwrite_original",
              gps_latitude_command,
              gps_latitude_ref_command,
              gps_longitude_command,
              gps_longitude_ref_command,
              image_file
    )
  )
}

# generic function to load lookups to populate dropdown selects from a csv file
load_lookup <- function(fileToLoad, display_column, value_column){
  #print("load lookup called")
  #print(paste0("fileToLoad: ", fileToLoad))
  full_file_path <- normalizePath(paste0(myEnv$data_dir,"/", fileToLoad), mustWork = TRUE)
  #full_file_path <- normalizePath(fileToLoad, mustWork = TRUE)
  #print(full_file_path)
  lookup <- utils::read.csv(file = full_file_path, header = TRUE, sep = ',')
  my_list <- list()
  for(i in 1:nrow(lookup)) {
    my_list[[i]] <- lookup[i, value_column]
    names(my_list)[i] <- lookup[i, display_column]
  }
  return(my_list)
}

# check for saved annotations data
check_for_saved_data <- function(dataFileToFind){
  #print(paste0("looking for: ", dataFileToFind))
  if(file.exists(dataFileToFind)){
    #print("file found!")
    dataFile <- readRDS(dataFileToFind)
  } else {
    #print("No Saved User Data - creating New File!")
    dataFile <- create_user_dataframe()
  }
  return(dataFile)
}

# create blank annotation data file
create_user_dataframe <- function(){
  df <- data.frame(user=character(),id=double(),imagefile=character(),feature_type=character(),radius=numeric(),geometry=character(),dd1=character(),dd2=character(),dd3=character(),dd4=character(),stringsAsFactors=FALSE)

  return(df)
}

# check for annotations on image dropdown change
check_for_annotations <- function(myUserAnnotationsData, myCurrentImage){
  newdata <- myUserAnnotationsData[which(myUserAnnotationsData$imagefile==myCurrentImage), ]
  #utils::str(newdata)
  return(newdata)
}

# edit annotations data
edit_annotation_data <- function(myUserAnnotationsData, myId,
                                 myUser = NA, myImage = NA,
                                 myFeatureType = NA,
                                 myRadius = NA, myGeometry = NA,
                                 myDD1 = NA, myDD2 = NA, myDD3 = NA, myDD4 = NA) {

  # Identify the row to update
  row_to_update <- myUserAnnotationsData$id == myId

  # Function to check if a parameter was provided (is not NA)
  is_provided <- function(x) !is.na(x)

  # Update values only if they are provided
  if (is_provided(myUser)) myUserAnnotationsData[row_to_update, "user"] <- myUser
  if (is_provided(myImage)) myUserAnnotationsData[row_to_update, "imagefile"] <- myImage
  if (is_provided(myFeatureType)) myUserAnnotationsData[row_to_update, "feature_type"] <- myFeatureType
  if (is_provided(myRadius)) myUserAnnotationsData[row_to_update, "radius"] <- myRadius
  if (is_provided(myGeometry)) myUserAnnotationsData[row_to_update, "geometry"] <- myGeometry
  if (is_provided(myDD1)) myUserAnnotationsData[row_to_update, "dd1"] <- myDD1
  if (is_provided(myDD2)) myUserAnnotationsData[row_to_update, "dd2"] <- myDD2
  if (is_provided(myDD3)) myUserAnnotationsData[row_to_update, "dd3"] <- myDD3
  if (is_provided(myDD4)) myUserAnnotationsData[row_to_update, "dd4"] <- myDD4

  # Check if the row exists to update or a new row needs to be added
  if (!any(row_to_update)) {
    # Create a new row with provided values, using NA for unspecified fields
    new_values <- data.frame(user = myUser, id = myId, imagefile = myImage,
                             feature_type = myFeatureType, radius = myRadius, geometry = myGeometry,
                             dd1 = myDD1, dd2 = myDD2, dd3 = myDD3, dd4 = myDD4)
    myUserAnnotationsData <- rbind(myUserAnnotationsData, new_values)
    warning("No matching ID found. Adding as a new row instead.")
  }

  return(myUserAnnotationsData)
}

# delete annotations from data frame
delete_annotation_data <- function(myUserAnnotationsData, myId) {
  # Filter out the rows where the id matches the specified value
  newdf <- myUserAnnotationsData[myUserAnnotationsData$id != myId,]
  return(newdf)
}

# clear all annotations from data frame
clear_all_annotation_data <- function(myUserAnnotationsData) {
  # remove all rows
  newdf <- myUserAnnotationsData[0,]
  return(newdf)
}

# save annotations to file
save_annotations <- function(myAnnotations, myAnnotationFileName){
  saveRDS(myAnnotations, file = myAnnotationFileName)
}

# add a new annotation to the control form
add_annotations_form <- function(input, myActiveAnnotations, myId, myFeatureType, myGeometry, myRadius, myDD1, myDD2, myDD3, myDD4){

  #r$new_annotation_id <- myId
  myActiveAnnotations(c(myId, myActiveAnnotations()))
  #r$active_annotations <- c(current_id, r$active_annotations())
  #print(r$active_annotations())

  # check and set the icon for the form
  if(myFeatureType == "Point-whole-image-annotation"){
    myIcon <- myEnv$formIcons$wholeImageMapFormIcon
  } else if(myFeatureType == "Point-map"){
    myIcon <- myEnv$formIcons$pointMapFormIcon
  } else if(myFeatureType == "Polygon-map"){
    myIcon <- myEnv$formIcons$polygonMapFormIcon
  } else if(myFeatureType == "Point-360"){
    myIcon <- myEnv$formIcons$point360FormIcon
  } else if(myFeatureType == "Polygon-360"){
    myIcon <- myEnv$formIcons$polygon360FormIcon
  } else {
    myIcon <- myEnv$formIcons$wholeImageMapFormIcon
  }

  ui <- div(
    id = paste0("control_form-",myId),
    style = "margin-bottom: 20px; border: 1px solid #ccc; padding: 10px; box-shadow: 0px 2px 2px #eee; border-radius: 10px;",
    div(
      style = "position: relative",
      bslib::card(
        title = paste0(myFeatureType),
        div(
          div(
            # Use a span to wrap the icon and text for better inline display
            span(HTML(paste0(
              myIcon, # Use the myIcon directly which includes the icon HTML
              h5(myId, style = "display: inline; margin-left: 5px; vertical-align: middle;")
            ))),
            style = "float: left;" # Aligns myId and icon to the left
          ),
          div(
            id = "button_group", # Container for buttons
            style = "text-align: right; margin-bottom: 20px;", # Right align buttons and add space below
            actionButton(inputId = paste0("control_form-","collapse_", myId), label = "", icon("chevron-up"), class = "btn btn-info btn-sm"),
            actionButton(inputId = paste0("control_form-","close_", myId), label = "", icon("trash"), class = "btn btn-danger btn-sm")
          )
        ),
        div(
          id = paste0("control_form-","content_", myId),
          div(style = "position: absolute; top: 20px; visibility: collapse;",
              textInput(
                inputId = paste0("control_form-","geometry-", myId),
                label = NULL,
                value = paste0(myGeometry),
              ),
              textInput(
                inputId = paste0("control_form-","feature_type-", myId),
                label = NULL,
                value = paste0(myFeatureType)
              )
          ),
          selectInput(
            inputId = paste0("control_form-","dropdown1-", myId),
            label = paste0(myFeatureType, "-", myEnv$config$lookup1Label),
            choices = myEnv$var_dropdown1,
            selected = myDD1,
            multiple = FALSE,
            selectize = FALSE,
            width = NULL,
            size = NULL
          ),
          if(myEnv$config$lookup2Enabled == TRUE){
            selectInput(
              inputId = paste0("control_form-","dropdown2-", myId),
              label = paste0(myFeatureType, "-", myEnv$config$lookup2Label),
              choices = myEnv$var_dropdown2,
              selected = myDD2,
              multiple = FALSE,
              selectize = FALSE,
              width = NULL,
              size = NULL
            )
          },
          if(myEnv$config$lookup3Enabled == TRUE){
            selectInput(
              inputId = paste0("control_form-","dropdown3-", myId),
              label = paste0(myFeatureType, "-", myEnv$config$lookup3Label),
              choices = myEnv$var_dropdown3,
              selected = myDD3,
              multiple = FALSE,
              selectize = FALSE,
              width = NULL,
              size = NULL
            )
          },
          if(myEnv$config$lookup4Enabled == TRUE){
            selectInput(
              inputId = paste0("control_form-","dropdown4-", myId),
              label = paste0(myFeatureType, "-", myEnv$config$lookup4Label),
              choices = myEnv$var_dropdown4,
              selected = myDD4,
              multiple = FALSE,
              selectize = FALSE,
              width = NULL,
              size = NULL
            )
          },
        ),style = "overflow: visible; min-height: 50px;"
      ),
    )
  ) %>% insertUI(selector = "#add_here", where = "beforeEnd")

  # Create observer for deleting the annotation card
  observe({
    #print(paste0("close_clicked"))
    #print(paste0(myId))
    #utils::str(myId)
    indices_to_remove <- which(r$active_annotations() == myId)
    if (length(indices_to_remove) > 0) {
      # Update the list excluding the specified myId
      updated_annotations <- r$active_annotations()[-indices_to_remove]
      r$active_annotations(updated_annotations)
    }
    #print(r$active_annotations())
    removeUI(selector = paste0("#", "control_form-", myId))
    r$user_annotations_data <- delete_annotation_data(r$user_annotations_data, myId)
    r$remove_leafletMap_item <- myId
    r$remove_leaflet360_item <- myId
  }) %>% bindEvent(input[[paste0("close_", myId)]])

  # Create observer for collapsing the annotation card
  r$active_annotations_collapse[[myId]] <- observe({
    #print("collapse clicked")
    divID <- paste0("control_form-content_", myId)
    btnID <- paste0("control_form-collapse_", myId)

    # JavaScript to toggle the div visibility and button icon
    jsCode <- sprintf(
      "shinyjs.toggle(id='%s');
    var btn = document.getElementById('%s');
    var icon = btn.querySelector('i');
    if (icon.classList.contains('fa-chevron-up')) {
      icon.classList.remove('fa-chevron-up');
      icon.classList.add('fa-chevron-down');
    } else {
      icon.classList.remove('fa-chevron-down');
      icon.classList.add('fa-chevron-up');
    }",
      divID, btnID
    )
    shinyjs::runjs(jsCode)
  }) %>% bindEvent(input[[paste0("collapse_", myId)]])

  # Create observer for updating dd1
  observe({
    #print("DD1 changed")
    #print(input[[paste0("dropdown1-", myId)]])
    r$user_annotations_data <-
      edit_annotation_data(
        myUserAnnotationsData = r$user_annotations_data,
        myId = myId,
        myDD1 = paste0(input[[paste0("dropdown1-", myId)]])
      )
    save_annotations(
      myAnnotations = r$user_annotations_data,
      myAnnotationFileName = r$user_annotations_file_name
    )
  }) %>% bindEvent(input[[paste0("dropdown1-", myId)]])

  # Create observer for updating dd2
  if(myEnv$config$lookup2Enabled == TRUE){
    observe({
      #print("DD2 changed")
      #print(input[[paste0("dropdown2-", myId)]])
      r$user_annotations_data <-
        edit_annotation_data(
          myUserAnnotationsData = r$user_annotations_data,
          myId = myId,
          myDD2 = paste0(input[[paste0("dropdown2-", myId)]])
        )
      save_annotations(
        myAnnotations = r$user_annotations_data,
        myAnnotationFileName = r$user_annotations_file_name
      )
    }) %>% bindEvent(input[[paste0("dropdown2-", myId)]])
  }
  # Create observer for updating dd3
  if(myEnv$config$lookup3Enabled == TRUE){
    observe({
      #print("DD3 changed")
      #print(input[[paste0("dropdown2-", myId)]])
      r$user_annotations_data <-
        edit_annotation_data(
          myUserAnnotationsData = r$user_annotations_data,
          myId = myId,
          myDD3 = paste0(input[[paste0("dropdown3-", myId)]])
        )
      save_annotations(
        myAnnotations = r$user_annotations_data,
        myAnnotationFileName = r$user_annotations_file_name
      )
    }) %>% bindEvent(input[[paste0("dropdown3-", myId)]])
  }
  # Create observer for updating dd4
  if(myEnv$config$lookup4Enabled == TRUE){
    observe({
      #print("DD4 changed")
      #print(input[[paste0("dropdown2-", myId)]])
      r$user_annotations_data <-
        edit_annotation_data(
          myUserAnnotationsData = r$user_annotations_data,
          myId = myId,
          myDD4 = paste0(input[[paste0("dropdown4-", myId)]])
        )
      save_annotations(
        myAnnotations = r$user_annotations_data,
        myAnnotationFileName = r$user_annotations_file_name
      )
    }) %>% bindEvent(input[[paste0("dropdown4-", myId)]])
  }
  #add to r$annotations_data
  r$user_annotations_data <- edit_annotation_data(myUserAnnotationsData = r$user_annotations_data, myUser = r$user_name, myId = myId, myImage=r$current_image, myFeatureType=paste0(myFeatureType), myGeometry=myGeometry, myDD1 = myDD1, myDD2 = myDD2, myDD3 = myDD3, myDD4 = myDD4)

  #View(r$user_annotations_data)
}

# clear all annotations from the form NOT the data frame
clear_annotations_form <- function() {
  #print("clear_annotations_form called")
  # only remove a module if there is at least one active annotation shown
  if (length(r$active_annotations()) > 0) {
    #print(paste0("r$active_annotations: ", r$active_annotations()))
    for (current_id in r$active_annotations()) {
      #print(paste0("Removing annotation ID: ", current_id))
      removeUI(selector = paste0("#control_form-", current_id))
    }

    # Stop the observer for the collapse button
    for(t in r$active_annotations_collapse){
      #print(paste0("removing observer for collapse"))
      # Safely remove the observer
      t$destroy()
      t <- NULL
    }
    r$active_annotations_collapse <- NULL
  }
}

################################
# Functions for mapping panel

unzipKmz <- function(kmzFile){

  # Set the paths to temporary directory
  temp_dir <- tempdir()
  kmlFile <- file.path(temp_dir, "doc.kml")
  filesFolder <- file.path(temp_dir, "files")
  if(file.exists(kmlFile)){
    #print("A kmz has previously been loaded.....")
    unlink(kmlFile)
    unlink(filesFolder, recursive = TRUE)
    #print("The files have been deleted")
  }

  # Unzip the new KMZ file to tempdir
  utils::unzip(kmzFile, list = FALSE, exdir = temp_dir)

  num_files <- length(list.files(filesFolder))
  #print(paste0(num_files, " image files extracted"))
  return(paste0(num_files, " image files extracted"))
}

# removeKmzFiles <- function(){
#   #print("Removing old kmz files...")
#   unlink("/temp_dir/doc.kml", force = TRUE)
#   unlink("/temp_dir/files", recursive = TRUE, force = TRUE)
# }

#adds a map overlay to the map for fire scars etc.
addMapOverlay <- function(overlayMap){
  myOverlayMap <- readr::read_file(overlayMap$datapath)
  myMapProxy <- leaflet::leafletProxy("mymap") %>%
    leaflet.extras::addKMLChoropleth(
      myOverlayMap, layerId = "Overlay", group = "Overlay",
      valueProperty = NULL,
      color = "#a6f31f", weight = 5, fillOpacity = 0.5) %>%
    leaflet::addLayersControl(overlayGroups = c("360-Images", "Overlay", "Whole-Image-Annotations", "Map-Annotations"), options = leaflet::layersControlOptions(collapsed = TRUE))
  return(myMapProxy)
}


# Function to load the base map with three groups: '360 images', 'points', and 'polygons'
loadBaseLeafletMap <- function(kml="") {

  if(myEnv$config$mapPanelSource == "Google.Maps"){
    #print(myEnv$config$mapPanelSource)

    mymap <- leaflet::renderLeaflet({
      #print("loadBaseLeafletMap called")
      leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2, maxZoom = 18)) %>%
        leaflet::setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
        leaflet::addTiles(urlTemplate = paste0("https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={z}&key=", myEnv$config$mapAPIKey, maxZoom = 18), attribution = 'Map data &copy; Google') %>%
        leaflet.extras::addKML(kml, layerId = "my_kml", group ="360-Images" ,  markerType = "circleMarker",
                               stroke = FALSE, fillColor = "yellow", fillOpacity = 1,
                               markerOptions = leaflet::markerOptions(interactive = TRUE, clickable = TRUE, radius = 5, riseOnHover = TRUE, riseOffset = 250), labelProperty = "name") %>%
        leafpm::addPmToolbar(targetGroup = "Map-Annotations",
                             toolbarOptions = leafpm::pmToolbarOptions(drawMarker = TRUE,
                                                                       drawPolygon = TRUE,
                                                                       drawPolyline = FALSE,
                                                                       drawCircle = FALSE,
                                                                       editMode = TRUE,
                                                                       cutPolygon = FALSE,
                                                                       removalMode = FALSE
                             ),
                             drawOptions = leafpm::pmDrawOptions(snappable = FALSE),
                             editOptions = leafpm::pmEditOptions(snappable = FALSE, snapDistance = 20,
                                                                 allowSelfIntersection = FALSE, draggable = FALSE,
                                                                 preventMarkerRemoval = FALSE, preventVertexEdit = FALSE)
        ) %>%
        leafpm::removePmToolbar()  %>%
        leaflet::addLayersControl(overlayGroups = c("360-Images", "Overlay", "Whole-Image-Annotations", "Map-Annotations"), options = leaflet::layersControlOptions(collapsed = TRUE))

    })
  } else {
    mymap <- leaflet::renderLeaflet({
      #print("loadBaseLeafletMap called")
      leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2, maxZoom = 17)) %>%
        leaflet::setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
        leaflet::addProviderTiles(eval(parse(text=paste0("leaflet::providers$", myEnv$config$mapPanelSource)))) %>%
        leaflet.extras::addKML(kml, layerId = "my_kml", group ="360-Images" ,  markerType = "circleMarker",
                               stroke = FALSE, fillColor = "yellow", fillOpacity = 1,
                               markerOptions = leaflet::markerOptions(interactive = TRUE, clickable = TRUE, radius = 5, riseOnHover = TRUE, riseOffset = 250), labelProperty = "name") %>%
        leafpm::addPmToolbar(targetGroup = "Map-Annotations",
                             toolbarOptions = leafpm::pmToolbarOptions(drawMarker = TRUE,
                                                                       drawPolygon = TRUE,
                                                                       drawPolyline = FALSE,
                                                                       drawCircle = FALSE,
                                                                       editMode = TRUE,
                                                                       cutPolygon = FALSE,
                                                                       removalMode = FALSE
                             ),
                             drawOptions = leafpm::pmDrawOptions(snappable = FALSE),
                             editOptions = leafpm::pmEditOptions(snappable = FALSE, snapDistance = 20,
                                                                 allowSelfIntersection = FALSE, draggable = FALSE,
                                                                 preventMarkerRemoval = FALSE, preventVertexEdit = FALSE)
        ) %>%
        leafpm::removePmToolbar()  %>%
        leaflet::addLayersControl(overlayGroups = c("360-Images", "Overlay", "Whole-Image-Annotations", "Map-Annotations"), options = leaflet::layersControlOptions(collapsed = TRUE))

    })
  }

  return(mymap)
}

# triggered to add the current image to the map
addCurrentImageToMap <- function(){
  #print("addCurrentImageToMap called")
  req(r$current_image_metadata, r$current_map_zoom)

  lat <- as.numeric(paste0(r$current_image_metadata$GPSLatitude))
  long <- as.numeric(paste0(r$current_image_metadata$GPSLongitude))
  zoom <- as.numeric(r$current_map_zoom)

  myMapProxy <- leaflet::leafletProxy("mymap") %>%
    leaflet::clearMarkers() %>%
    leaflet::removeMarker(layerId = "currentImage") %>% # remove the purple cirlce marker
    leaflet::clearGroup("Map-Annotations") %>%
    leaflet::clearGroup("Whole-Image-Annotations") %>%
    leaflet::setView(lng = long, lat = lat, zoom = zoom) %>%
    leaflet::addCircleMarkers(lng = long, lat = lat, layerId = "currentImage", group= "360-Images", fillColor = "darkviolet", radius=12, fillOpacity = 0.1, stroke = T, color = "#03F", weight = 3, opacity = 0.4) %>%
    leafpm::addPmToolbar(targetGroup = "Map-Annotations",
                         toolbarOptions = leafpm::pmToolbarOptions(drawMarker = TRUE,
                                                                   drawPolygon = TRUE,
                                                                   drawPolyline = FALSE,
                                                                   drawCircle = FALSE,
                                                                   editMode = TRUE,
                                                                   cutPolygon = FALSE,
                                                                   removalMode = FALSE
                         ),
                         drawOptions = leafpm::pmDrawOptions(snappable = FALSE),
                         editOptions = leafpm::pmEditOptions(snappable = FALSE, snapDistance = 20,
                                                             allowSelfIntersection = FALSE, draggable = FALSE,
                                                             preventMarkerRemoval = FALSE, preventVertexEdit = FALSE)
    ) %>%
    leaflet::addMeasure(position = "topright",  primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters", activeColor = "#3D535D", completedColor = "#7D4479") %>%
    leaflet::setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)

  return(myMapProxy)
}

# clears annotations from the map on first draw with the toolbar
clear_drawn_annotation_from_map <- function(session, layerId) {
  #print("clear_drawn_annotation_from_leaflet called")
  session$sendCustomMessage("removeleaflet", list(elid = "leaflet_map-mymap", layerId = layerId))
}

# add annotation to map
add_annotations_to_map <- function(){
  #print("add_annotations_to_map called")

  #print("new map layer added")
  # check for whole image annotations
  r$current_annotation_whole_images <- r$user_annotations_data %>%
    dplyr::filter(imagefile == r$current_image & feature_type %in% c("Point-whole-image-annotation")) %>%
    sf::st_as_sf(., wkt = "geometry")
  # check for map annotations
  r$current_annotation_markers <- r$user_annotations_data %>%
    dplyr::filter(imagefile == r$current_image & feature_type %in% c("Point-map")) %>%
    sf::st_as_sf(., wkt = "geometry")
  # check for polygon annotations
  r$current_annotation_polygons <- r$user_annotations_data %>%
    dplyr::filter(imagefile == r$current_image & feature_type %in% c("Polygon-map")) %>%
    sf::st_as_sf(., wkt = "geometry")

  myMapProxy <- leaflet::leafletProxy("mymap")

  #Check and add markers if present
  if(any(sf::st_geometry_type(r$current_annotation_markers) %in% c("POINT", "MULTIPOINT"))) {
    myMapProxy <- myMapProxy %>%
      leaflet::addAwesomeMarkers(
        data = r$current_annotation_markers, #single_feature,
        layerId = ~id,  # Set layerId to the id column
        group = "Map-Annotations",
        icon = myEnv$mapIcons$pointMapIcon,
        label = ~id,
        popup = ~paste(myEnv$formIcons$pointMapFormIcon,
                       "ID:", id, "<br>"
        ),
        popupOptions = leaflet::popupOptions(
          maxWidth = 300,
          minWidth = 50,
          maxHeight = NULL,
          autoPan = FALSE,
          keepInView = TRUE,
          closeButton = FALSE,
          closeOnClick = TRUE
        )
      )
  }

  # Check if r$annotation_polygons contains polygons before adding them
  if(any(sf::st_geometry_type(r$current_annotation_polygons) %in% c("POLYGON", "MULTIPOLYGON"))) {
    myMapProxy <- myMapProxy %>%
      leaflet::addPolygons( data = r$current_annotation_polygons,
                            layerId = ~id,  # Set layerId to the id column
                            group = "Map-Annotations",
                            label = ~id,
                            stroke = myEnv$config$mapPolygonStroke,
                            color = myEnv$config$mapPolygonStrokeColour,
                            weight = myEnv$config$mapPolygonStrokeWeight,
                            opacity = myEnv$config$mapPolygonStrokeOpacity,
                            fill = myEnv$config$mapPolygonFill,
                            fillColor = myEnv$config$mapPolygonFillColour,
                            fillOpacity = myEnv$config$mapPolygonFillOpacity,
                            dashArray = NULL,
                            smoothFactor = 1,
                            popup = ~paste(myEnv$formIcons$polygonMapFormIcon,
                                           "ID:", id, "<br>"
                            ),
                            popupOptions = leaflet::popupOptions(
                              maxWidth = 300,
                              minWidth = 50,
                              maxHeight = NULL,
                              autoPan = FALSE,
                              keepInView = TRUE,
                              closeButton = FALSE,
                              closeOnClick = TRUE
                            )
      )
  }

  # Check and add whole image annotations if present
  if(any(sf::st_geometry_type(r$current_annotation_whole_images) %in% c("POINT", "MULTIPOINT"))) {
    myMapProxy <- myMapProxy %>%
      # Add markers with the Font Awesome "street view" icon
      leaflet::addAwesomeMarkers(
        data = r$current_annotation_whole_images,
        layerId = ~id,  # Set layerId to the id column
        group = "Whole-Image-Annotations",
        icon = myEnv$mapIcons$wholeImageMapIcon,
        label = ~id,
        popup = ~paste(myEnv$formIcons$wholeImageMapFormIcon,
                       "ID:", id, "<br>"
        ),
        popupOptions = leaflet::popupOptions(
          maxWidth = 300,
          minWidth = 50,
          maxHeight = NULL,
          autoPan = FALSE,
          keepInView = TRUE,
          closeButton = FALSE,
          closeOnClick = TRUE
        ),
        clusterOptions = leaflet::markerClusterOptions(
          showCoverageOnHover = TRUE,
          zoomToBoundsOnClick = TRUE,
          spiderfyOnMaxZoom = TRUE,
          removeOutsideVisibleBounds = TRUE,
          spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5),
          freezeAtZoom = FALSE
        ),
        clusterId = "Whole-Image-Annotations"
      )
  }
  return(myMapProxy)
}

remove_map_item <- function(){
  #print("remove_map_item called")
  myMapProxy <- leaflet::leafletProxy("mymap") %>%
    leaflet::removeMarkerFromCluster(layerId=r$remove_leafletMap_item, clusterId = "Whole-Image-Annotations") %>%
    leaflet::removeMarker(r$remove_leafletMap_item) %>%
    leaflet::removeShape(r$remove_leafletMap_item)

  return(myMapProxy)
}

remove_360_item <- function(){
  #print("remove_360_item called")
  my360Proxy <- leaflet::leafletProxy("leaflet360") %>%
    leaflet::removeMarker(r$remove_leaflet360_item) %>%
    leaflet::removeShape(r$remove_leaflet360_item)

  return(my360Proxy)
}
########################################
# Functions for 360 image panel
# Function to load the base 360 leaflet
loadBaseLeaflet360 <- function() {

  #print("LoadBase360 called")
  leaflet360 <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = -2, maxZoom = 4, crs = leaflet::leafletCRS(crsClass = "L.CRS.Simple")))
  })
  return(leaflet360)
}

# add current image to 360 leaflet
addCurrentImageToLeaflet360 <- function(){
  #print(paste0("addCurrentImageToLeaflet360 called: r$current_image: ", r$current_image))
  # Prepare the dynamic image URL
  imageURL <- paste0("'/temp_dir/files/", r$current_image, "'")
  # Define the bounds of the image
  imageWidth <- r$current_image_metadata$ImageWidth  # Width of the image
  imageHeight <- r$current_image_metadata$ImageHeight  # Height of the image
  imageBounds <- list(c(0, 0), c(imageHeight, imageWidth))
  # Calculate the center of the image
  imageCenter <- c(imageHeight / 2, imageWidth / 2)


  leaflet360 <- leaflet::renderLeaflet({
    leafletMap <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = -2, maxZoom = 5, crs = leaflet::leafletCRS(crsClass = "L.CRS.Simple"))) %>%
      leafpm::addPmToolbar(targetGroup = "360-Annotations",
                           toolbarOptions = leafpm::pmToolbarOptions(drawMarker = TRUE,
                                                                     drawPolygon = TRUE,
                                                                     drawPolyline = FALSE,
                                                                     drawCircle = FALSE,
                                                                     editMode = TRUE,
                                                                     cutPolygon = FALSE,
                                                                     removalMode = FALSE),
                           drawOptions = leafpm::pmDrawOptions(list(draggable = FALSE)),
                           editOptions = leafpm::pmEditOptions(snappable = FALSE, snapDistance = 20,
                                                               allowSelfIntersection = FALSE, draggable = FALSE,
                                                               preventMarkerRemoval = FALSE, preventVertexEdit = FALSE)) %>%
      leaflet::addLayersControl(overlayGroups = c("360-Annotations"), options = leaflet::layersControlOptions(collapsed = FALSE))


    leafletMap <- htmlwidgets::onRender(
      leafletMap,
      paste0("
        function(el, x) {
          var imageUrl = ", imageURL, ";
          var imageBounds = ", jsonlite::toJSON(imageBounds), ";
          L.imageOverlay(imageUrl, imageBounds, {
            opacity: 1,
            interactive: false
          }).addTo(this);

          Shiny.addCustomMessageHandler('removeleaflet360', function(data){
           var map = HTMLWidgets.find('#' + data.elid).getMap();
           var layer = map._layers[data.layerId];
            if(layer) {
              map.removeLayer(layer);
            }
          });

         }
      ")
    )

    # Set the initial view of the map outside the onRender function
    leafletMap <- leafletMap %>%
      leaflet::setView(lng = imageCenter[2], lat = imageCenter[1], zoom = -2)
    #TODO see if measuring in pixels is possible
    #%>%
    #leaflet::addMeasure(position = "topright",  primaryLengthUnit = "pixels", primaryAreaUnit = "sqpixels", activeColor = "#3D535D", completedColor = "#7D4479")
  })

  return(leaflet360)
}

# add annotation to leaflet 360
add_annotations_to_360 <- function(){
  #print("add_annotations_to_360 called")
  #print("new 360 layer added")

  req(r$user_annotations_data)
  # check for map annotations
  r$current_annotation_360markers <- r$user_annotations_data %>%
    dplyr::filter(imagefile == r$current_image & feature_type %in% c("Point-360")) %>%
    sf::st_as_sf(., wkt = "geometry")
  # check for polygon annotations
  r$current_annotation_360polygons <- r$user_annotations_data %>%
    dplyr::filter(imagefile == r$current_image & feature_type %in% c("Polygon-360")) %>%
    sf::st_as_sf(., wkt = "geometry")

  #View(r$current_annotation_360polygons)

  my360Proxy <- leaflet::leafletProxy("leaflet360")# %>%

  #Check and add markers if present
  if(any(sf::st_geometry_type(r$current_annotation_360markers) %in% c("POINT", "MULTIPOINT"))) {
    my360Proxy <- my360Proxy %>%
      leaflet::addAwesomeMarkers(
        data = r$current_annotation_360markers, #single_feature,
        layerId = ~id,  # Set layerId to the id column
        group = "360-Annotations",
        icon = myEnv$mapIcons$point360Icon,
        label = ~id,
        popup = ~paste(myEnv$formIcons$point360FormIcon,
                       "ID:", id, "<br>"
        ),
        popupOptions = leaflet::popupOptions(
          maxWidth = 300,
          minWidth = 50,
          maxHeight = NULL,
          autoPan = FALSE,
          keepInView = TRUE,
          closeButton = FALSE,
          closeOnClick = TRUE
        )
      )
  }

  # Check if r$annotation_polygons contains polygons before adding them
  if(any(sf::st_geometry_type(r$current_annotation_360polygons) %in% c("POLYGON", "MULTIPOLYGON"))) {
    my360Proxy <- my360Proxy %>%
      leaflet::addPolygons( data = r$current_annotation_360polygons,
                            layerId = ~id,  # Set layerId to the id column
                            group = "360-Annotations",
                            label = ~id,
                            stroke = myEnv$config$pano360PolygonStroke,
                            color = myEnv$config$pano360PolygonStrokeColour,
                            weight = myEnv$config$pano360PolygonStrokeWeight,
                            opacity = myEnv$config$pano360PolygonStrokeOpacity,
                            fill = myEnv$config$pano360PolygonFill,
                            fillColor = myEnv$config$pano360PolygonFillColour,
                            fillOpacity = myEnv$config$pano360PolygonFillOpacity,
                            dashArray = NULL,
                            smoothFactor = 1,
                            popup = ~paste(myEnv$formIcons$polygon360FormIcon,
                                           "ID:", id, "<br>"
                            ),
                            popupOptions = leaflet::popupOptions(
                              maxWidth = 300,
                              minWidth = 50,
                              maxHeight = NULL,
                              autoPan = FALSE,
                              keepInView = TRUE,
                              closeButton = FALSE,
                              closeOnClick = TRUE
                            )
      )
  }
  return(my360Proxy)
}

# clears drawn item from leaflet so it can be reloaded with date ID
clear_drawn_annotation_from_360 <- function(session, layerId) {
  #print("clear_drawn_annotation_from_360 called")
  session$sendCustomMessage("removeleaflet360", list(elid = "pano360_image-leaflet360", layerId = layerId))
}

########################################
# create map icons for use on the map
create_map_icons <- function() {

  myIcons <- leaflet::awesomeIconList(
    wholeImageMapIcon = leaflet::makeAwesomeIcon(icon = "ion-image", library = "ion", iconColor =  myEnv$config$mapIconColour, markerColor = myEnv$config$mapMarkerColour),#ios-world-outline
    pointMapIcon = leaflet::makeAwesomeIcon(icon = "map-marked-alt", library = "fa", iconColor =  myEnv$config$mapIconColour, markerColor = myEnv$config$mapMarkerColour),
    polygonMapIcon = leaflet::makeAwesomeIcon(icon = "draw-polygon", library = "fa",iconColor =  myEnv$config$mapIconColour, markerColor = myEnv$config$mapMarkerColour),
    point360Icon = leaflet::makeAwesomeIcon(icon = "map-marked-alt", library = "fa",iconColor =  myEnv$config$pano360IconColour, markerColor = myEnv$config$pano360MarkerColour),
    polygon360Icon = leaflet::makeAwesomeIcon(icon = "draw-polygon", library = "fa",iconColor =  myEnv$config$pano360IconColour, markerColor = myEnv$config$pano360MarkerColour)
  )
  return(myIcons)
}

#create from icons for using in the control form and map/360 popups
create_form_icons <- function() {
  #myEnv$config$mapIconColour <- "DarkRed"
  #myEnv$config$pano360IconColour <- "navy"
  formIcons <- list(
    wholeImageMapFormIcon = paste0("<i class='ionicons ion-image' style='color: ", myEnv$config$mapIconColour, "; background-color: transparent;'></i>"),
    pointMapFormIcon = paste0("<i class='fa fa-map-marked-alt' style='color: ", myEnv$config$mapIconColour, "; background-color: transparent;'></i>"),
    polygonMapFormIcon = paste0("<i class='fa fa-draw-polygon' style='color: ", myEnv$config$mapIconColour, "; background-color: transparent;'></i>"),
    point360FormIcon = paste0("<i class='fa fa-map-marked-alt' style='color: ", myEnv$config$pano360IconColour, "; background-color: transparent;'></i>"),
    polygon360FormIcon = paste0("<i class='fa fa-draw-polygon' style='color: ", myEnv$config$pano360IconColour, "; background-color: transparent;'></i>")
  )
  return(formIcons)
}


##############

# function for outputting cropped polygons
create_cropped_polygons_from_360_images <- function(annotations_export_dir){
  req(r$user_annotations_data, r$current_annotation_360polygons, r$current_image)

  df_polygons <- r$current_annotation_360polygons

  image_path <- paste0(tempdir(),"/files/", r$current_image)
  img <- jpeg::readJPEG(image_path)

  img_raster <- grDevices::as.raster(img)
  plot_width <- r$current_image_metadata$ImageWidth
  plot_height <- r$current_image_metadata$ImageHeight

  if (!is.null(df_polygons)) {
    polygons_sf <- sf::st_as_sf(df_polygons, wkt = "geometry", crs = 4326)  # Ensure to set a valid CRS
    num_polygons <- nrow(polygons_sf)

    withProgress(message = 'Creating crops', value = 0, {


      for (i in seq_len(nrow(polygons_sf))) {
        bbox <- sf::st_bbox(polygons_sf[i, ])

        # Update progress bar
        incProgress(1 / num_polygons, detail = paste("Processing image", i, "of", num_polygons))

        # Initialize the plot with the raster annotation
        p <- ggplot2::ggplot() +
          ggplot2::annotation_raster(img_raster, xmin=0, xmax=plot_width, ymin=0, ymax=plot_height) +
          ggplot2::coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE) +
          ggplot2::theme_void()

        # Add the polygon layer conditionally
        if (myEnv$config$showPano360PolygonStrokeInCropExport && myEnv$config$showPano360PolygonFillInCropExport) {
          # Both stroke and fill enabled
          p <- p + ggplot2::geom_sf(
            data = polygons_sf[i, ],
            color = scales::alpha(myEnv$config$pano360PolygonStrokeColour, myEnv$config$pano360PolygonStrokeOpacity),
            fill = myEnv$config$pano360PolygonFillColour,
            linewidth = myEnv$config$pano360PolygonStrokeWeight,
            alpha = myEnv$config$pano360PolygonFillOpacity
          )
        } else if (myEnv$config$showPano360PolygonStrokeInCropExport) {
          # Only stroke enabled
          p <- p + ggplot2::geom_sf(
            data = polygons_sf[i, ],
            color = scales::alpha(myEnv$config$pano360PolygonStrokeColour, myEnv$config$pano360PolygonStrokeOpacity),
            fill = NA,
            linewidth = myEnv$config$pano360PolygonStrokeWeight
          )
        } else if (myEnv$config$showPano360PolygonFillInCropExport) {
          # Only fill enabled
          p <- p + ggplot2::geom_sf(
            data = polygons_sf[i, ],
            color = NA,
            fill = myEnv$config$pano360PolygonFillColour,
            alpha = myEnv$config$pano360PolygonFillOpacity
          )
        }

        # Export each plot as a PNG image
        # add id to the filename
        cropped_image_path <- paste0(annotations_export_dir, "/", gsub("\\.\\w+$", paste0("_", polygons_sf[i, "id"], ".png"), r$current_image))
        #print(cropped_image_path)

        #ggplot2::ggsave(cropped_image_path, plot = p, width = plot_width, height = plot_height, units = "px", dpi = 96, limitsize = FALSE, bg = "transparent")
        grDevices::png(filename = cropped_image_path, units = "px", type = "cairo-png", bg = "transparent", res = 96)
        print(p)  # This will render the ggplot object to the PNG device
        grDevices::dev.off()

        # get the gps metadata from the r$current_image
        lat <- r$current_image_metadata$GPSLatitude
        long <- r$current_image_metadata$GPSLongitude
        lat_ref <- r$current_image_metadata$GPSLatitudeRef
        long_ref <- r$current_image_metadata$GPSLongitudeRef
        # now write the exiftool GPS metadata to the png
        #View(r$current_image_metadata)
        write_image_gps_metadata(image_file=cropped_image_path, latitude=lat, latitude_ref=lat_ref, longitude=long,longitude_ref=long_ref)

      }

    }) #withProgress
  }
  #return("success")
}
